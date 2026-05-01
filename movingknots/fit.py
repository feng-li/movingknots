"""Small fitting helpers for the current fixed-knot Gaussian model."""

from __future__ import annotations

import jax
import jax.numpy as jnp
import numpy as np

from movingknots.basis import design_matrix
from movingknots.data import set_crossvalid
from movingknots.model import (
    conditional_gaussian_beta_posterior,
    gaussian_log_likelihood,
    make_p_matrices,
    marginal_gaussian_log_likelihood,
)
from movingknots.parameters import (
    build_gaussian_parameter_schema,
    build_marginal_gaussian_parameter_schema,
    cholesky_to_sigma_log_jacobian,
    normalize_knots,
    pack_gaussian_parameters,
    pack_marginal_gaussian_parameters,
    unconstrained_cholesky_from_sigma,
    unpack_gaussian_parameters,
    unpack_marginal_gaussian_parameters,
)
from movingknots.priors import inverse_wishart_logpdf, normal_logpdf
from movingknots.variational import fit_mean_field, mean_field_scale, sample_mean_field


def fit_gaussian_vi(
    x,
    y,
    knots,
    spline_config,
    free_knots=False,
    **kwargs,
):
    """Fit a Gaussian VI model, optionally freeing one knot family."""
    if free_knots is False or free_knots is None or free_knots == "fixed":
        return fit_fixed_knots_gaussian_vi(
            x=x,
            y=y,
            knots=knots,
            spline_config=spline_config,
            **kwargs,
        )
    if free_knots == "additive":
        return fit_free_additive_knots_gaussian_vi(
            x=x,
            y=y,
            knots=knots,
            spline_config=spline_config,
            **kwargs,
        )
    if free_knots == "surface":
        return fit_free_surface_knots_gaussian_vi(
            x=x,
            y=y,
            knots=knots,
            spline_config=spline_config,
            **kwargs,
        )
    if free_knots == "full":
        return fit_full_gaussian_vi(
            x=x,
            y=y,
            knots=knots,
            spline_config=spline_config,
            **kwargs,
        )
    if free_knots == "marginal":
        return fit_marginal_gaussian_vi(
            x=x,
            y=y,
            knots=knots,
            spline_config=spline_config,
            **kwargs,
        )
    raise ValueError(
        "free_knots must be False, 'fixed', 'additive', 'surface', 'full', or 'marginal'"
    )


def fit_fixed_knots_gaussian_vi(
    x,
    y,
    knots,
    spline_config,
    n_steps: int = 1000,
    learning_rate: float = 0.03,
    key=None,
    n_samples: int = 4,
    beta_prior_variance: float = 25.0,
    log_sigma_prior_mean: float | None = None,
    log_sigma_prior_variance: float = 1.0,
    init_scale: float = 0.02,
):
    """Fit a univariate fixed-knot Gaussian model with mean-field VI."""
    x = jnp.asarray(x)
    y = jnp.asarray(y)
    if y.ndim == 1:
        y = y[:, None]
    if y.shape[1] != 1:
        raise ValueError("fit_fixed_knots_gaussian_vi currently supports p=1 only")

    key = jax.random.PRNGKey(0) if key is None else key
    x_design = design_matrix(x, knots, spline_config)
    n_coef = x_design.shape[1]
    init_beta, init_log_sigma = _initial_gaussian_position(x_design, y)
    if log_sigma_prior_mean is None:
        log_sigma_prior_mean = init_log_sigma
    init_position = jnp.concatenate([init_beta, jnp.array([init_log_sigma])])

    def log_prob(z):
        beta = z[:n_coef, None]
        log_sigma = z[n_coef]
        sigma = jnp.exp(log_sigma).reshape((1, 1))
        return (
            gaussian_log_likelihood(
                y=y,
                x=x,
                knots=knots,
                spline_config=spline_config,
                beta=beta,
                sigma=sigma,
            )
            + normal_logpdf(beta, mean=0.0, variance=beta_prior_variance)
            + normal_logpdf(
                log_sigma,
                mean=log_sigma_prior_mean,
                variance=log_sigma_prior_variance,
            )
        )

    vi = fit_mean_field(
        log_prob,
        init_position,
        key,
        n_steps=n_steps,
        learning_rate=learning_rate,
        n_samples=n_samples,
        init_scale=init_scale,
    )

    beta_mean = vi["mu"][:n_coef, None]
    log_sigma_mean = vi["mu"][n_coef]
    log_sigma_sd = mean_field_scale(vi["rho"])[n_coef]
    sigma_mean = jnp.exp(log_sigma_mean + 0.5 * log_sigma_sd**2)

    return {
        "kind": "fixed",
        "n_coef": n_coef,
        "beta_mean": beta_mean,
        "log_sigma_mean": log_sigma_mean,
        "sigma_mean": sigma_mean,
        "vi": vi,
        "knots": knots,
        "spline_config": spline_config,
    }


def fit_free_additive_knots_gaussian_vi(
    x,
    y,
    knots,
    spline_config,
    n_steps: int = 1000,
    learning_rate: float = 0.03,
    key=None,
    n_samples: int = 4,
    beta_prior_variance: float = 25.0,
    knot_prior_mean=None,
    knot_prior_variance: float = 10.0,
    log_sigma_prior_mean: float | None = None,
    log_sigma_prior_variance: float = 1.0,
    init_scale: float = 0.02,
):
    """Fit a univariate Gaussian model with free one-dimensional additive knots."""
    x = jnp.asarray(x)
    y = jnp.asarray(y)
    if y.ndim == 1:
        y = y[:, None]
    if y.shape[1] != 1:
        raise ValueError("fit_free_additive_knots_gaussian_vi currently supports p=1 only")
    if x.shape[1] != 1:
        raise ValueError("fit_free_additive_knots_gaussian_vi currently supports one covariate")
    if "thinplate.a" not in spline_config.get("comp", ()):
        raise ValueError("spline_config must include 'thinplate.a'")
    if "thinplate.s" in spline_config.get("comp", ()):
        raise ValueError("surface knots are not supported by this helper")

    key = jax.random.PRNGKey(0) if key is None else key
    initial_additive_knots = jnp.asarray(knots["thinplate.a"]).reshape(-1)
    n_additive_knots = initial_additive_knots.size
    if sum(spline_config["thinplate.a.locate"]) != n_additive_knots:
        raise ValueError("thinplate.a.locate does not match the number of additive knots")

    initial_knots = {**knots, "thinplate.a": initial_additive_knots[:, None]}
    x_design = design_matrix(x, initial_knots, spline_config)
    n_coef = x_design.shape[1]
    init_beta, init_log_sigma = _initial_gaussian_position(x_design, y)
    if log_sigma_prior_mean is None:
        log_sigma_prior_mean = init_log_sigma
    if knot_prior_mean is None:
        knot_prior_mean = initial_additive_knots

    init_position = jnp.concatenate(
        [init_beta, jnp.array([init_log_sigma]), initial_additive_knots]
    )

    def unpack(z):
        beta = z[:n_coef, None]
        log_sigma = z[n_coef]
        additive_knots = z[(n_coef + 1) : (n_coef + 1 + n_additive_knots)]
        current_knots = {**knots, "thinplate.a": additive_knots[:, None]}
        return beta, log_sigma, additive_knots, current_knots

    def log_prob(z):
        beta, log_sigma, additive_knots, current_knots = unpack(z)
        sigma = jnp.exp(log_sigma).reshape((1, 1))
        return (
            gaussian_log_likelihood(
                y=y,
                x=x,
                knots=current_knots,
                spline_config=spline_config,
                beta=beta,
                sigma=sigma,
            )
            + normal_logpdf(beta, mean=0.0, variance=beta_prior_variance)
            + normal_logpdf(
                log_sigma,
                mean=log_sigma_prior_mean,
                variance=log_sigma_prior_variance,
            )
            + normal_logpdf(
                additive_knots,
                mean=knot_prior_mean,
                variance=knot_prior_variance,
            )
        )

    vi = fit_mean_field(
        log_prob,
        init_position,
        key,
        n_steps=n_steps,
        learning_rate=learning_rate,
        n_samples=n_samples,
        init_scale=init_scale,
    )

    beta_mean, log_sigma_mean, additive_knots_mean, fitted_knots = unpack(vi["mu"])
    log_sigma_sd = mean_field_scale(vi["rho"])[n_coef]
    sigma_mean = jnp.exp(log_sigma_mean + 0.5 * log_sigma_sd**2)

    return {
        "kind": "free_additive",
        "n_coef": n_coef,
        "n_additive_knots": n_additive_knots,
        "beta_mean": beta_mean,
        "log_sigma_mean": log_sigma_mean,
        "sigma_mean": sigma_mean,
        "additive_knots_mean": additive_knots_mean[:, None],
        "vi": vi,
        "knots": fitted_knots,
        "initial_knots": initial_knots,
        "spline_config": spline_config,
    }


def fit_free_surface_knots_gaussian_vi(
    x,
    y,
    knots,
    spline_config,
    n_steps: int = 1000,
    learning_rate: float = 0.03,
    key=None,
    n_samples: int = 4,
    beta_prior_variance: float = 25.0,
    knot_prior_mean=None,
    knot_prior_variance: float = 10.0,
    log_sigma_prior_mean: float | None = None,
    log_sigma_prior_variance: float = 1.0,
    init_scale: float = 0.02,
):
    """Fit a univariate Gaussian model with free two-dimensional surface knots."""
    x = jnp.asarray(x)
    y = jnp.asarray(y)
    if y.ndim == 1:
        y = y[:, None]
    if y.shape[1] != 1:
        raise ValueError("fit_free_surface_knots_gaussian_vi currently supports p=1 only")
    if x.shape[1] != 2:
        raise ValueError("fit_free_surface_knots_gaussian_vi currently supports two covariates")
    if "thinplate.s" not in spline_config.get("comp", ()):
        raise ValueError("spline_config must include 'thinplate.s'")
    if "thinplate.a" in spline_config.get("comp", ()):
        raise ValueError("additive knots are not supported by this helper")

    key = jax.random.PRNGKey(0) if key is None else key
    initial_surface_knots = jnp.asarray(knots["thinplate.s"])
    if initial_surface_knots.ndim != 2 or initial_surface_knots.shape[1] != x.shape[1]:
        raise ValueError("thinplate.s knots must have shape (n_surface, 2)")
    expected_surface_dim = tuple(spline_config.get("thinplate.s.dim", initial_surface_knots.shape))
    if expected_surface_dim != tuple(initial_surface_knots.shape):
        raise ValueError("thinplate.s.dim does not match the surface knot shape")

    n_surface, n_dim = initial_surface_knots.shape
    initial_knots = {**knots, "thinplate.s": initial_surface_knots}
    x_design = design_matrix(x, initial_knots, spline_config)
    n_coef = x_design.shape[1]
    init_beta, init_log_sigma = _initial_gaussian_position(x_design, y)
    if log_sigma_prior_mean is None:
        log_sigma_prior_mean = init_log_sigma
    if knot_prior_mean is None:
        knot_prior_mean = initial_surface_knots

    init_position = jnp.concatenate(
        [init_beta, jnp.array([init_log_sigma]), initial_surface_knots.reshape(-1)]
    )

    def unpack(z):
        beta = z[:n_coef, None]
        log_sigma = z[n_coef]
        surface_knots = z[(n_coef + 1) :].reshape((n_surface, n_dim))
        current_knots = {**knots, "thinplate.s": surface_knots}
        return beta, log_sigma, surface_knots, current_knots

    def log_prob(z):
        beta, log_sigma, surface_knots, current_knots = unpack(z)
        sigma = jnp.exp(log_sigma).reshape((1, 1))
        return (
            gaussian_log_likelihood(
                y=y,
                x=x,
                knots=current_knots,
                spline_config=spline_config,
                beta=beta,
                sigma=sigma,
            )
            + normal_logpdf(beta, mean=0.0, variance=beta_prior_variance)
            + normal_logpdf(
                log_sigma,
                mean=log_sigma_prior_mean,
                variance=log_sigma_prior_variance,
            )
            + normal_logpdf(
                surface_knots,
                mean=knot_prior_mean,
                variance=knot_prior_variance,
            )
        )

    vi = fit_mean_field(
        log_prob,
        init_position,
        key,
        n_steps=n_steps,
        learning_rate=learning_rate,
        n_samples=n_samples,
        init_scale=init_scale,
    )

    beta_mean, log_sigma_mean, surface_knots_mean, fitted_knots = unpack(vi["mu"])
    log_sigma_sd = mean_field_scale(vi["rho"])[n_coef]
    sigma_mean = jnp.exp(log_sigma_mean + 0.5 * log_sigma_sd**2)

    return {
        "kind": "free_surface",
        "n_coef": n_coef,
        "surface_shape": (n_surface, n_dim),
        "beta_mean": beta_mean,
        "log_sigma_mean": log_sigma_mean,
        "sigma_mean": sigma_mean,
        "surface_knots_mean": surface_knots_mean,
        "vi": vi,
        "knots": fitted_knots,
        "initial_knots": initial_knots,
        "spline_config": spline_config,
    }


def full_gaussian_log_prob_components(
    z,
    x,
    y,
    base_knots,
    spline_config,
    schema,
    beta_prior_variance: float,
    log_shrinkage_prior_mean: float,
    log_shrinkage_prior_variance: float,
    sigma_prior_df: float,
    sigma_prior_scale,
    additive_knot_prior_mean=None,
    surface_knot_prior_mean=None,
    knot_prior_variance: float = 10.0,
    include_cholesky_jacobian: bool = True,
    p_matrix_types=None,
):
    """Return log-probability components for the unified Gaussian VI model."""
    params = unpack_gaussian_parameters(schema, z, base_knots)
    x_design = design_matrix(x, params["knots"], spline_config)
    p_matrices = make_p_matrices(x_design, schema, p_matrix_types)
    components = {
        "log_likelihood": gaussian_log_likelihood(
            y=y,
            x=x,
            knots=params["knots"],
            spline_config=spline_config,
            beta=params["beta"],
            sigma=params["sigma"],
        ),
        "beta_shrinkage_prior": _beta_shrinkage_log_prior(
            beta=params["beta"],
            sigma=params["sigma"],
            log_shrinkage=params["log_shrinkage"],
            schema=schema,
            beta_prior_variance=beta_prior_variance,
            p_matrices=p_matrices,
        ),
        "log_shrinkage_prior": normal_logpdf(
            params["log_shrinkage"],
            mean=log_shrinkage_prior_mean,
            variance=log_shrinkage_prior_variance,
        ),
        "sigma_prior": inverse_wishart_logpdf(
            params["sigma"],
            df=sigma_prior_df,
            scale=sigma_prior_scale,
        ),
    }
    if include_cholesky_jacobian:
        components["sigma_cholesky_log_jacobian"] = cholesky_to_sigma_log_jacobian(
            params["raw_cholesky"],
            schema.n_response,
        )
    else:
        components["sigma_cholesky_log_jacobian"] = jnp.asarray(0.0)

    if schema.free_additive:
        if additive_knot_prior_mean is None:
            raise ValueError("additive_knot_prior_mean is required for free additive knots")
        components["additive_knot_prior"] = normal_logpdf(
            params["additive_knots"],
            mean=additive_knot_prior_mean,
            variance=knot_prior_variance,
        )
    else:
        components["additive_knot_prior"] = jnp.asarray(0.0)

    if schema.free_surface:
        if surface_knot_prior_mean is None:
            raise ValueError("surface_knot_prior_mean is required for free surface knots")
        components["surface_knot_prior"] = normal_logpdf(
            params["surface_knots"],
            mean=surface_knot_prior_mean,
            variance=knot_prior_variance,
        )
    else:
        components["surface_knot_prior"] = jnp.asarray(0.0)

    return components


def full_gaussian_log_prob(*args, **kwargs):
    """Return the summed log probability for the unified Gaussian VI model."""
    components = full_gaussian_log_prob_components(*args, **kwargs)
    total = 0.0
    for value in components.values():
        total = total + value
    return total


def marginal_gaussian_log_prob_components(
    z,
    x,
    y,
    base_knots,
    spline_config,
    schema,
    beta_prior_variance: float,
    log_shrinkage_prior_mean: float,
    log_shrinkage_prior_variance: float,
    sigma_prior_df: float,
    sigma_prior_scale,
    additive_knot_prior_mean=None,
    surface_knot_prior_mean=None,
    knot_prior_variance: float = 10.0,
    include_cholesky_jacobian: bool = True,
    p_matrix_types=None,
):
    """Return log-probability components with coefficients integrated out."""
    params = unpack_marginal_gaussian_parameters(schema, z, base_knots)
    components = {
        "marginal_log_likelihood": marginal_gaussian_log_likelihood(
            y=y,
            x=x,
            knots=params["knots"],
            spline_config=spline_config,
            sigma=params["sigma"],
            log_shrinkage=params["log_shrinkage"],
            schema=schema,
            beta_prior_variance=beta_prior_variance,
            p_matrix_types=p_matrix_types,
        ),
        "log_shrinkage_prior": normal_logpdf(
            params["log_shrinkage"],
            mean=log_shrinkage_prior_mean,
            variance=log_shrinkage_prior_variance,
        ),
        "sigma_prior": inverse_wishart_logpdf(
            params["sigma"],
            df=sigma_prior_df,
            scale=sigma_prior_scale,
        ),
    }
    if include_cholesky_jacobian:
        components["sigma_cholesky_log_jacobian"] = cholesky_to_sigma_log_jacobian(
            params["raw_cholesky"],
            schema.n_response,
        )
    else:
        components["sigma_cholesky_log_jacobian"] = jnp.asarray(0.0)

    if schema.free_additive:
        if additive_knot_prior_mean is None:
            raise ValueError("additive_knot_prior_mean is required for free additive knots")
        components["additive_knot_prior"] = normal_logpdf(
            params["additive_knots"],
            mean=additive_knot_prior_mean,
            variance=knot_prior_variance,
        )
    else:
        components["additive_knot_prior"] = jnp.asarray(0.0)

    if schema.free_surface:
        if surface_knot_prior_mean is None:
            raise ValueError("surface_knot_prior_mean is required for free surface knots")
        components["surface_knot_prior"] = normal_logpdf(
            params["surface_knots"],
            mean=surface_knot_prior_mean,
            variance=knot_prior_variance,
        )
    else:
        components["surface_knot_prior"] = jnp.asarray(0.0)

    return components


def marginal_gaussian_log_prob(*args, **kwargs):
    """Return the summed marginal log probability with B integrated out."""
    components = marginal_gaussian_log_prob_components(*args, **kwargs)
    total = 0.0
    for value in components.values():
        total = total + value
    return total


def fit_full_gaussian_vi(
    x,
    y,
    knots,
    spline_config,
    free_additive: bool | None = None,
    free_surface: bool | None = None,
    n_steps: int = 1000,
    learning_rate: float = 0.03,
    key=None,
    n_samples: int = 4,
    beta_prior_variance: float = 25.0,
    initial_log_shrinkage=None,
    log_shrinkage_prior_mean: float = 0.0,
    log_shrinkage_prior_variance: float = 1.0,
    additive_knot_prior_mean=None,
    surface_knot_prior_mean=None,
    knot_prior_variance: float = 10.0,
    sigma_prior_df: float | None = None,
    sigma_prior_scale=None,
    init_scale: float = 0.02,
    p_matrix_types=None,
):
    """Fit a unified Gaussian VI model with beta, knots, shrinkage, and full Sigma."""
    x = jnp.asarray(x)
    y = jnp.asarray(y)
    if y.ndim == 1:
        y = y[:, None]

    key = jax.random.PRNGKey(0) if key is None else key
    initial_knots = normalize_knots(knots, spline_config)
    schema = build_gaussian_parameter_schema(
        x=x,
        y=y,
        knots=initial_knots,
        spline_config=spline_config,
        free_additive=free_additive,
        free_surface=free_surface,
    )

    x_design = design_matrix(x, initial_knots, spline_config)
    init_beta, init_sigma = _initial_multivariate_gaussian_position(x_design, y)
    init_raw_cholesky = unconstrained_cholesky_from_sigma(init_sigma)
    if initial_log_shrinkage is None:
        initial_log_shrinkage = jnp.zeros(schema.log_shrinkage_shape)
    else:
        initial_log_shrinkage = jnp.asarray(initial_log_shrinkage).reshape(
            schema.log_shrinkage_shape
        )

    init_position = pack_gaussian_parameters(
        schema=schema,
        beta=init_beta,
        raw_cholesky=init_raw_cholesky,
        log_shrinkage=initial_log_shrinkage,
        additive_knots=initial_knots.get("thinplate.a"),
        surface_knots=initial_knots.get("thinplate.s"),
    )

    n_response = schema.n_response
    if sigma_prior_df is None:
        sigma_prior_df = float(n_response + 3)
    if sigma_prior_scale is None:
        sigma_prior_scale = _sigma_prior_scale_from_init(init_sigma, sigma_prior_df)
    sigma_prior_scale = jnp.asarray(sigma_prior_scale)

    if schema.free_additive:
        if additive_knot_prior_mean is None:
            additive_knot_prior_mean = initial_knots["thinplate.a"]
        additive_knot_prior_mean = jnp.asarray(additive_knot_prior_mean).reshape(
            schema.additive_shape
        )
    if schema.free_surface:
        if surface_knot_prior_mean is None:
            surface_knot_prior_mean = initial_knots["thinplate.s"]
        surface_knot_prior_mean = jnp.asarray(surface_knot_prior_mean).reshape(
            schema.surface_shape
        )

    def log_prob(z):
        return full_gaussian_log_prob(
            z=z,
            x=x,
            y=y,
            base_knots=initial_knots,
            spline_config=spline_config,
            schema=schema,
            beta_prior_variance=beta_prior_variance,
            log_shrinkage_prior_mean=log_shrinkage_prior_mean,
            log_shrinkage_prior_variance=log_shrinkage_prior_variance,
            sigma_prior_df=sigma_prior_df,
            sigma_prior_scale=sigma_prior_scale,
            additive_knot_prior_mean=additive_knot_prior_mean,
            surface_knot_prior_mean=surface_knot_prior_mean,
            knot_prior_variance=knot_prior_variance,
            p_matrix_types=p_matrix_types,
        )

    vi = fit_mean_field(
        log_prob,
        init_position,
        key,
        n_steps=n_steps,
        learning_rate=learning_rate,
        n_samples=n_samples,
        init_scale=init_scale,
    )

    mean_params = unpack_gaussian_parameters(schema, vi["mu"], initial_knots)
    out = {
        "kind": "full",
        "schema": schema,
        "n_coef": schema.n_coef,
        "n_response": schema.n_response,
        "beta_mean": mean_params["beta"],
        "sigma_mean": mean_params["sigma"],
        "sigma_cholesky_mean": mean_params["sigma_cholesky"],
        "log_shrinkage_mean": mean_params["log_shrinkage"],
        "shrinkage_mean": mean_params["shrinkage"],
        "vi": vi,
        "knots": mean_params["knots"],
        "initial_knots": initial_knots,
        "spline_config": spline_config,
        "free_additive": schema.free_additive,
        "free_surface": schema.free_surface,
        "sigma_prior_df": sigma_prior_df,
        "sigma_prior_scale": sigma_prior_scale,
        "p_matrix_types": p_matrix_types,
    }
    if schema.free_additive:
        out["additive_knots_mean"] = mean_params["additive_knots"]
    if schema.free_surface:
        out["surface_knots_mean"] = mean_params["surface_knots"]
    return out


def fit_marginal_gaussian_vi(
    x,
    y,
    knots,
    spline_config,
    free_additive: bool | None = None,
    free_surface: bool | None = None,
    n_steps: int = 1000,
    learning_rate: float = 0.03,
    key=None,
    n_samples: int = 4,
    beta_prior_variance: float = 25.0,
    initial_log_shrinkage=None,
    log_shrinkage_prior_mean: float = 0.0,
    log_shrinkage_prior_variance: float = 1.0,
    additive_knot_prior_mean=None,
    surface_knot_prior_mean=None,
    knot_prior_variance: float = 10.0,
    sigma_prior_df: float | None = None,
    sigma_prior_scale=None,
    init_scale: float = 0.02,
    p_matrix_types=None,
):
    """Fit a Gaussian VI model using p(knots, shrinkage, Sigma | Y) with B integrated out."""
    x = jnp.asarray(x)
    y = jnp.asarray(y)
    if y.ndim == 1:
        y = y[:, None]

    key = jax.random.PRNGKey(0) if key is None else key
    initial_knots = normalize_knots(knots, spline_config)
    schema = build_marginal_gaussian_parameter_schema(
        x=x,
        y=y,
        knots=initial_knots,
        spline_config=spline_config,
        free_additive=free_additive,
        free_surface=free_surface,
    )

    x_design = design_matrix(x, initial_knots, spline_config)
    init_beta, init_sigma = _initial_multivariate_gaussian_position(x_design, y)
    init_raw_cholesky = unconstrained_cholesky_from_sigma(init_sigma)
    if initial_log_shrinkage is None:
        initial_log_shrinkage = jnp.zeros(schema.log_shrinkage_shape)
    else:
        initial_log_shrinkage = jnp.asarray(initial_log_shrinkage).reshape(
            schema.log_shrinkage_shape
        )

    init_position = pack_marginal_gaussian_parameters(
        schema=schema,
        raw_cholesky=init_raw_cholesky,
        log_shrinkage=initial_log_shrinkage,
        additive_knots=initial_knots.get("thinplate.a"),
        surface_knots=initial_knots.get("thinplate.s"),
    )

    n_response = schema.n_response
    if sigma_prior_df is None:
        sigma_prior_df = float(n_response + 3)
    if sigma_prior_scale is None:
        sigma_prior_scale = _sigma_prior_scale_from_init(init_sigma, sigma_prior_df)
    sigma_prior_scale = jnp.asarray(sigma_prior_scale)

    if schema.free_additive:
        if additive_knot_prior_mean is None:
            additive_knot_prior_mean = initial_knots["thinplate.a"]
        additive_knot_prior_mean = jnp.asarray(additive_knot_prior_mean).reshape(
            schema.additive_shape
        )
    if schema.free_surface:
        if surface_knot_prior_mean is None:
            surface_knot_prior_mean = initial_knots["thinplate.s"]
        surface_knot_prior_mean = jnp.asarray(surface_knot_prior_mean).reshape(
            schema.surface_shape
        )

    def log_prob(z):
        return marginal_gaussian_log_prob(
            z=z,
            x=x,
            y=y,
            base_knots=initial_knots,
            spline_config=spline_config,
            schema=schema,
            beta_prior_variance=beta_prior_variance,
            log_shrinkage_prior_mean=log_shrinkage_prior_mean,
            log_shrinkage_prior_variance=log_shrinkage_prior_variance,
            sigma_prior_df=sigma_prior_df,
            sigma_prior_scale=sigma_prior_scale,
            additive_knot_prior_mean=additive_knot_prior_mean,
            surface_knot_prior_mean=surface_knot_prior_mean,
            knot_prior_variance=knot_prior_variance,
            p_matrix_types=p_matrix_types,
        )

    vi = fit_mean_field(
        log_prob,
        init_position,
        key,
        n_steps=n_steps,
        learning_rate=learning_rate,
        n_samples=n_samples,
        init_scale=init_scale,
    )

    mean_params = unpack_marginal_gaussian_parameters(schema, vi["mu"], initial_knots)
    out = {
        "kind": "marginal",
        "schema": schema,
        "n_coef": schema.n_coef,
        "n_response": schema.n_response,
        "sigma_mean": mean_params["sigma"],
        "sigma_cholesky_mean": mean_params["sigma_cholesky"],
        "log_shrinkage_mean": mean_params["log_shrinkage"],
        "shrinkage_mean": mean_params["shrinkage"],
        "vi": vi,
        "knots": mean_params["knots"],
        "initial_knots": initial_knots,
        "spline_config": spline_config,
        "free_additive": schema.free_additive,
        "free_surface": schema.free_surface,
        "sigma_prior_df": sigma_prior_df,
        "sigma_prior_scale": sigma_prior_scale,
        "beta_prior_variance": beta_prior_variance,
        "p_matrix_types": p_matrix_types,
        "x_train": x,
        "y_train": y,
    }
    if schema.free_additive:
        out["additive_knots_mean"] = mean_params["additive_knots"]
    if schema.free_surface:
        out["surface_knots_mean"] = mean_params["surface_knots"]
    beta_posterior = marginal_fit_beta_posterior(out)
    out["beta_mean"] = beta_posterior["mean"]
    out["beta_covariance_at_mean"] = beta_posterior["covariance"]
    return out


def marginal_fit_beta_posterior(fit, z=None):
    """Return p(B | Y, knots, shrinkage, Sigma) for a marginalized fit."""
    if fit.get("kind") != "marginal":
        raise ValueError("marginal_fit_beta_posterior requires a marginalized fit")
    params = _marginal_fit_noncoefficient_params(fit, z)
    beta_posterior = conditional_gaussian_beta_posterior(
        y=fit["y_train"],
        x=fit["x_train"],
        knots=params["knots"],
        spline_config=fit["spline_config"],
        sigma=params["sigma"],
        log_shrinkage=params["log_shrinkage"],
        schema=fit["schema"],
        beta_prior_variance=fit["beta_prior_variance"],
        p_matrix_types=fit.get("p_matrix_types"),
    )
    return {
        "mean": beta_posterior["mean"],
        "covariance": _symmetrize_covariance(
            beta_posterior["covariance"],
            jitter=0.0,
        ),
    }


def marginal_fit_predictive_moments(fit, x_new, z=None, include_noise: bool = False):
    """Return analytic predictive mean and covariance for a marginalized fit."""
    if fit.get("kind") != "marginal":
        raise ValueError("marginal_fit_predictive_moments requires a marginalized fit")
    params = _marginal_fit_noncoefficient_params(fit, z)
    beta_posterior = marginal_fit_beta_posterior(fit, z)
    x_design = design_matrix(x_new, params["knots"], fit["spline_config"])
    mean = x_design @ beta_posterior["mean"]
    response_eye = jnp.eye(fit["schema"].n_response, dtype=x_design.dtype)
    beta_to_prediction = jnp.kron(x_design, response_eye)
    covariance = (
        beta_to_prediction
        @ beta_posterior["covariance"]
        @ beta_to_prediction.T
    )
    if include_noise:
        covariance = covariance + jnp.kron(
            jnp.eye(x_design.shape[0], dtype=x_design.dtype),
            params["sigma"],
        )
    return {
        "mean": mean,
        "covariance": _symmetrize_covariance(covariance, jitter=0.0),
    }


def marginal_fit_log_predictive_density(fit, x_new, y_new, z=None):
    """Return exact log p(y_new | x_new, Y, z) for a marginalized Gaussian fit."""
    y_new = jnp.asarray(y_new)
    if y_new.ndim == 1:
        y_new = y_new[:, None]
    moments = marginal_fit_predictive_moments(
        fit,
        x_new,
        z=z,
        include_noise=True,
    )
    if y_new.shape != moments["mean"].shape:
        raise ValueError("y_new must have shape matching the predictive mean")
    return _multivariate_normal_logpdf(
        y_new.reshape(-1),
        moments["mean"].reshape(-1),
        moments["covariance"],
    )


def predict_mean(fit, x_new):
    """Posterior mean prediction using the variational mean of beta."""
    x_design = design_matrix(x_new, fit["knots"], fit["spline_config"])
    return x_design @ fit["beta_mean"]


def predict_samples(fit, x_new, key, n_samples: int = 100, include_noise: bool = False):
    """Draw posterior mean or noisy predictive samples from the variational posterior."""
    if fit.get("kind") == "full":
        return _predict_full_samples(
            fit=fit,
            x_new=x_new,
            key=key,
            n_samples=n_samples,
            include_noise=include_noise,
        )
    if fit.get("kind") == "marginal":
        return _predict_marginal_samples(
            fit=fit,
            x_new=x_new,
            key=key,
            n_samples=n_samples,
            include_noise=include_noise,
        )

    z_samples = sample_mean_field(
        fit["vi"]["mu"],
        fit["vi"]["rho"],
        key,
        n_samples=n_samples,
    )
    z_samples = jnp.atleast_2d(z_samples)

    def one_sample(z):
        beta, log_sigma, knots = _unpack_fit_sample(fit, z)
        x_design = design_matrix(x_new, knots, fit["spline_config"])
        return x_design @ beta

    if include_noise:
        sample_keys = jax.random.split(key, n_samples)

        def one_noisy_sample(z, sample_key):
            beta, log_sigma, knots = _unpack_fit_sample(fit, z)
            x_design = design_matrix(x_new, knots, fit["spline_config"])
            mean = x_design @ beta
            sigma = jnp.exp(log_sigma)
            return mean + jnp.sqrt(sigma) * jax.random.normal(
                sample_key,
                shape=mean.shape,
                dtype=mean.dtype,
            )

        return jax.vmap(one_noisy_sample)(z_samples, sample_keys)

    return jax.vmap(one_sample)(z_samples)


def predict_summary(
    fit,
    x_new,
    key,
    n_samples: int = 500,
    include_noise: bool = False,
    probs=(0.05, 0.5, 0.95),
):
    """Summarize posterior predictive samples."""
    samples = predict_samples(
        fit,
        x_new,
        key,
        n_samples=n_samples,
        include_noise=include_noise,
    )
    probs = jnp.asarray(probs)
    return {
        "mean": jnp.mean(samples, axis=0),
        "sd": jnp.std(samples, axis=0, ddof=1),
        "quantiles": jnp.quantile(samples, probs, axis=0),
        "probs": probs,
    }


def evaluate_gaussian_fit(fit, x_test, y_test):
    """Evaluate a fitted Gaussian model on test data."""
    y_test = jnp.asarray(y_test)
    if y_test.ndim == 1:
        y_test = y_test[:, None]
    y_pred = predict_mean(fit, x_test)
    residual = y_test - y_pred
    sigma_mean = jnp.asarray(fit["sigma_mean"])
    if sigma_mean.ndim == 0:
        sigma_mean = sigma_mean.reshape((1, 1))
    return {
        "mse": float(jnp.mean(residual**2)),
        "mae": float(jnp.mean(jnp.abs(residual))),
        "log_likelihood_at_mean": float(
            gaussian_log_likelihood(
                y=y_test,
                x=x_test,
                knots=fit["knots"],
                spline_config=fit["spline_config"],
                beta=fit["beta_mean"],
                sigma=sigma_mean,
            )
        ),
    }


def gaussian_log_predictive_samples(fit, x_test, y_test, key=None, n_samples: int = 100):
    """Return posterior-sampled Gaussian test log likelihoods."""
    if n_samples < 1:
        raise ValueError("n_samples must be positive")
    key = jax.random.PRNGKey(0) if key is None else key
    x_test = jnp.asarray(x_test)
    y_test = jnp.asarray(y_test)
    if y_test.ndim == 1:
        y_test = y_test[:, None]

    if fit.get("kind") == "full":
        z_samples = sample_mean_field(
            fit["vi"]["mu"],
            fit["vi"]["rho"],
            key,
            n_samples=n_samples,
        )
        z_samples = jnp.atleast_2d(z_samples)
        schema = fit["schema"]
        base_knots = fit.get("initial_knots", fit["knots"])

        def one_sample(z):
            params = unpack_gaussian_parameters(schema, z, base_knots)
            return gaussian_log_likelihood(
                y=y_test,
                x=x_test,
                knots=params["knots"],
                spline_config=fit["spline_config"],
                beta=params["beta"],
                sigma=params["sigma"],
            )

        return jax.vmap(one_sample)(z_samples)

    if fit.get("kind") == "marginal":
        z_samples = sample_mean_field(
            fit["vi"]["mu"],
            fit["vi"]["rho"],
            key,
            n_samples=n_samples,
        )
        z_samples = jnp.atleast_2d(z_samples)

        def one_sample(z):
            return marginal_fit_log_predictive_density(
                fit,
                x_test,
                y_test,
                z=z,
            )

        return jax.vmap(one_sample)(z_samples)

    z_samples = sample_mean_field(
        fit["vi"]["mu"],
        fit["vi"]["rho"],
        key,
        n_samples=n_samples,
    )
    z_samples = jnp.atleast_2d(z_samples)

    def one_sample(z):
        beta, log_sigma, knots = _unpack_fit_sample(fit, z)
        sigma = jnp.exp(log_sigma).reshape((1, 1))
        return gaussian_log_likelihood(
            y=y_test,
            x=x_test,
            knots=knots,
            spline_config=fit["spline_config"],
            beta=beta,
            sigma=sigma,
        )

    return jax.vmap(one_sample)(z_samples)


def gaussian_lpds(fit, x_test, y_test, key=None, n_samples: int = 100):
    """Estimate the Gaussian log predictive density score for one test split."""
    log_pred_samples = gaussian_log_predictive_samples(
        fit=fit,
        x_test=x_test,
        y_test=y_test,
        key=key,
        n_samples=n_samples,
    )
    summary = summarize_log_predictive_samples(log_pred_samples)
    summary["log_pred_samples"] = log_pred_samples
    return summary


def summarize_log_predictive_samples(log_pred_matrix):
    """Summarize posterior log likelihood samples into LPDS and Monte Carlo SE."""
    log_pred_matrix = jnp.asarray(log_pred_matrix)
    if log_pred_matrix.ndim == 1:
        log_pred_matrix = log_pred_matrix[:, None]
    if log_pred_matrix.ndim != 2:
        raise ValueError("log_pred_matrix must be one- or two-dimensional")
    if log_pred_matrix.shape[0] < 1:
        raise ValueError("log_pred_matrix must contain at least one sample")

    scale = jnp.max(log_pred_matrix, axis=0)
    exp_scaled = jnp.exp(log_pred_matrix - scale)
    exp_mean = jnp.mean(exp_scaled, axis=0)
    fold_lpds = scale + jnp.log(exp_mean)
    lpds = jnp.mean(fold_lpds)

    if log_pred_matrix.shape[0] < 2:
        nse_lpds = jnp.asarray(jnp.nan)
    else:
        exp_var = jnp.var(exp_scaled, axis=0, ddof=1)
        var_mean = exp_var / log_pred_matrix.shape[0]
        n_folds = log_pred_matrix.shape[1]
        nvar_lpds = jnp.sum(var_mean / (exp_mean**2)) / (n_folds**2)
        nse_lpds = jnp.sqrt(nvar_lpds)

    return {
        "lpds": float(lpds),
        "nse_lpds": float(nse_lpds),
        "fold_lpds": fold_lpds,
        "log_pred_matrix": log_pred_matrix,
    }


def cross_validate_gaussian_vi(
    x,
    y,
    knots,
    spline_config,
    crossvalid_args=None,
    free_knots=False,
    key=None,
    n_predictive_samples: int = 100,
    fit_kwargs=None,
):
    """Run K-fold Gaussian VI fits and aggregate LPDS across folds."""
    x = jnp.asarray(x)
    y = jnp.asarray(y)
    if y.ndim == 1:
        y = y[:, None]
    if n_predictive_samples < 1:
        raise ValueError("n_predictive_samples must be positive")
    n_obs = int(y.shape[0])
    if int(x.shape[0]) != n_obs:
        raise ValueError("x and y must contain the same number of observations")

    if crossvalid_args is None:
        crossvalid_args = {
            "N.subsets": min(5, n_obs),
            "partiMethod": "systematic",
        }
    crossvalid = set_crossvalid(n_obs, crossvalid_args)
    n_folds = len(crossvalid["testing"])
    if n_folds < 2:
        raise ValueError("cross-validation requires at least two folds")

    key = jax.random.PRNGKey(0) if key is None else key
    fold_keys = jax.random.split(key, n_folds)
    fit_kwargs = {} if fit_kwargs is None else dict(fit_kwargs)

    fold_results = []
    log_pred_columns = []
    for fold_index, fold_key in enumerate(fold_keys):
        fit_key, score_key = jax.random.split(fold_key)
        training_idx = np.asarray(crossvalid["training"][fold_index], dtype=int)
        testing_idx = np.asarray(crossvalid["testing"][fold_index], dtype=int)

        fit = fit_gaussian_vi(
            x=x[training_idx],
            y=y[training_idx],
            knots=knots,
            spline_config=spline_config,
            free_knots=free_knots,
            key=fit_key,
            **fit_kwargs,
        )
        fold_score = gaussian_lpds(
            fit=fit,
            x_test=x[testing_idx],
            y_test=y[testing_idx],
            key=score_key,
            n_samples=n_predictive_samples,
        )
        log_pred_samples = jnp.asarray(fold_score["log_pred_samples"]).reshape(-1)
        log_pred_columns.append(log_pred_samples)
        fold_results.append(
            {
                "fold": fold_index,
                "training_indices": training_idx,
                "testing_indices": testing_idx,
                "fit": fit,
                "lpds": fold_score["lpds"],
                "nse_lpds": fold_score["nse_lpds"],
                "log_pred_samples": log_pred_samples,
            }
        )

    log_pred_matrix = jnp.column_stack(log_pred_columns)
    summary = summarize_log_predictive_samples(log_pred_matrix)
    summary.update(
        {
            "folds": fold_results,
            "crossvalid": crossvalid,
            "n_predictive_samples": n_predictive_samples,
        }
    )
    return summary


def summarize_fit(fit):
    """Return a plain Python/NumPy summary of a fitted model."""
    sigma_mean = np.asarray(fit["sigma_mean"], dtype=float)
    summary = {
        "beta_mean": np.asarray(fit["beta_mean"], dtype=float),
        "sigma_mean": float(sigma_mean.reshape(-1)[0]) if sigma_mean.size == 1 else sigma_mean,
        "final_elbo": float(fit["vi"]["elbo"][-1]),
        "n_parameters": int(fit["vi"]["mu"].size),
    }
    if "shrinkage_mean" in fit:
        summary["shrinkage_mean"] = np.asarray(fit["shrinkage_mean"], dtype=float)
    if "additive_knots_mean" in fit:
        summary["additive_knots_mean"] = np.asarray(
            fit["additive_knots_mean"], dtype=float
        )
    if "surface_knots_mean" in fit:
        summary["surface_knots_mean"] = np.asarray(fit["surface_knots_mean"], dtype=float)
    return summary


def _predict_full_samples(fit, x_new, key, n_samples: int, include_noise: bool):
    z_samples = sample_mean_field(
        fit["vi"]["mu"],
        fit["vi"]["rho"],
        key,
        n_samples=n_samples,
    )
    z_samples = jnp.atleast_2d(z_samples)
    schema = fit["schema"]
    base_knots = fit.get("initial_knots", fit["knots"])

    def one_latent_sample(z):
        params = unpack_gaussian_parameters(schema, z, base_knots)
        x_design = design_matrix(x_new, params["knots"], fit["spline_config"])
        return x_design @ params["beta"]

    if not include_noise:
        return jax.vmap(one_latent_sample)(z_samples)

    sample_keys = jax.random.split(key, n_samples)

    def one_noisy_sample(z, sample_key):
        params = unpack_gaussian_parameters(schema, z, base_knots)
        x_design = design_matrix(x_new, params["knots"], fit["spline_config"])
        mean = x_design @ params["beta"]
        noise = jax.random.normal(sample_key, shape=mean.shape, dtype=mean.dtype)
        return mean + noise @ params["sigma_cholesky"].T

    return jax.vmap(one_noisy_sample)(z_samples, sample_keys)


def _predict_marginal_samples(fit, x_new, key, n_samples: int, include_noise: bool):
    z_key, beta_key, noise_key = jax.random.split(key, 3)
    z_samples = sample_mean_field(
        fit["vi"]["mu"],
        fit["vi"]["rho"],
        z_key,
        n_samples=n_samples,
    )
    z_samples = jnp.atleast_2d(z_samples)
    beta_keys = jax.random.split(beta_key, n_samples)

    def one_latent_sample(z, beta_sample_key):
        params = _marginal_fit_noncoefficient_params(fit, z)
        beta_sample = _sample_beta_posterior(
            marginal_fit_beta_posterior(fit, z),
            beta_sample_key,
        )
        x_design = design_matrix(x_new, params["knots"], fit["spline_config"])
        return x_design @ beta_sample

    if not include_noise:
        return jax.vmap(one_latent_sample)(z_samples, beta_keys)

    noise_keys = jax.random.split(noise_key, n_samples)

    def one_noisy_sample(z, beta_sample_key, noise_sample_key):
        params = _marginal_fit_noncoefficient_params(fit, z)
        beta_sample = _sample_beta_posterior(
            marginal_fit_beta_posterior(fit, z),
            beta_sample_key,
        )
        x_design = design_matrix(x_new, params["knots"], fit["spline_config"])
        mean = x_design @ beta_sample
        noise = jax.random.normal(noise_sample_key, shape=mean.shape, dtype=mean.dtype)
        return mean + noise @ params["sigma_cholesky"].T

    return jax.vmap(one_noisy_sample)(z_samples, beta_keys, noise_keys)


def _marginal_fit_noncoefficient_params(fit, z=None):
    schema = fit["schema"]
    if z is None:
        z = fit["vi"]["mu"]
    base_knots = fit.get("initial_knots", fit["knots"])
    return unpack_marginal_gaussian_parameters(schema, z, base_knots)


def _sample_beta_posterior(beta_posterior, key):
    beta_mean = beta_posterior["mean"]
    beta_covariance = _symmetrize_covariance(
        beta_posterior["covariance"],
        jitter=1e-9,
    )
    beta_sample = jax.random.multivariate_normal(
        key,
        mean=beta_mean.reshape(-1),
        cov=beta_covariance,
    )
    return beta_sample.reshape(beta_mean.shape)


def _unpack_fit_sample(fit, z):
    n_coef = fit["n_coef"]
    beta = z[:n_coef, None]
    log_sigma = z[n_coef]
    kind = fit.get("kind", "fixed")
    if kind == "fixed":
        return beta, log_sigma, fit["knots"]
    if kind == "free_additive":
        n_additive_knots = fit["n_additive_knots"]
        additive_knots = z[(n_coef + 1) : (n_coef + 1 + n_additive_knots)]
        return beta, log_sigma, {**fit["knots"], "thinplate.a": additive_knots[:, None]}
    if kind == "free_surface":
        surface_shape = fit["surface_shape"]
        surface_knots = z[(n_coef + 1) :].reshape(surface_shape)
        return beta, log_sigma, {**fit["knots"], "thinplate.s": surface_knots}
    raise ValueError(f"Unknown fit kind: {kind}")


def _initial_gaussian_position(x_design, y):
    x_np = np.asarray(x_design, dtype=float)
    y_np = np.asarray(y, dtype=float)
    beta = np.linalg.lstsq(x_np, y_np, rcond=None)[0].reshape(-1)
    residual = y_np - x_np @ beta[:, None]
    variance = max(float(np.mean(residual**2)), 1e-4)
    return jnp.asarray(beta), jnp.asarray(np.log(variance))


def _initial_multivariate_gaussian_position(x_design, y):
    x_np = np.asarray(x_design, dtype=float)
    y_np = np.asarray(y, dtype=float)
    beta = np.linalg.lstsq(x_np, y_np, rcond=None)[0]
    residual = y_np - x_np @ beta
    n_response = y_np.shape[1]
    covariance = residual.T @ residual / max(y_np.shape[0], 1)
    covariance = covariance + np.eye(n_response) * 1e-4
    return jnp.asarray(beta), jnp.asarray(covariance)


def _sigma_prior_scale_from_init(init_sigma, df):
    init_sigma = jnp.asarray(init_sigma)
    n_response = init_sigma.shape[0]
    scale_multiplier = max(float(df) - n_response - 1.0, 1.0)
    return init_sigma * scale_multiplier


def _symmetrize_covariance(covariance, jitter: float = 1e-9):
    covariance = jnp.asarray(covariance)
    covariance = 0.5 * (covariance + covariance.T)
    return covariance + jitter * jnp.eye(covariance.shape[0], dtype=covariance.dtype)


def _multivariate_normal_logpdf(value, mean, covariance):
    value = jnp.asarray(value)
    mean = jnp.asarray(mean)
    covariance = jnp.asarray(covariance)
    residual = value - mean
    sign, logdet = jnp.linalg.slogdet(covariance)
    quadratic = residual @ jnp.linalg.solve(covariance, residual)
    log_density = (
        -0.5 * residual.size * jnp.log(2 * jnp.pi)
        -0.5 * logdet
        -0.5 * quadratic
    )
    return jnp.where(sign > 0, log_density, -jnp.inf)


def _beta_shrinkage_log_prior(
    beta,
    sigma,
    log_shrinkage,
    schema,
    beta_prior_variance: float,
    p_matrices=None,
):
    beta = jnp.asarray(beta)
    sigma = jnp.asarray(sigma)
    log_shrinkage = jnp.asarray(log_shrinkage)
    total = 0.0
    coef_blocks = np.asarray(schema.coef_block_indices)

    for block_index in range(schema.n_shrinkage_blocks):
        rows = np.flatnonzero(coef_blocks == block_index)
        if rows.size == 0:
            continue
        beta_block = beta[jnp.asarray(rows), :]
        shrinkage_scale = jnp.sqrt(jnp.exp(log_shrinkage[block_index]))
        beta_cov = beta_prior_variance * (
            shrinkage_scale[:, None] * sigma * shrinkage_scale[None, :]
        )
        if p_matrices is None:
            p_inv = jnp.eye(rows.size, dtype=beta_cov.dtype)
        else:
            p_inv = jnp.linalg.pinv(jnp.asarray(p_matrices[block_index]))
        block_cov = jnp.kron(p_inv, beta_cov)
        beta_vec = beta_block.reshape(-1)
        sign, logdet = jnp.linalg.slogdet(block_cov)
        quadratic = beta_vec @ jnp.linalg.solve(block_cov, beta_vec)
        log_density = (
            -0.5 * beta_vec.size * jnp.log(2 * jnp.pi)
            -0.5 * logdet
            -0.5 * quadratic
        )
        total = total + jnp.where(sign > 0, log_density, -jnp.inf)
    return total
