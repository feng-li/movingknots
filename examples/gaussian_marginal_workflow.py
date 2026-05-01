"""End-to-end Gaussian marginal moving-knot workflow."""

from __future__ import annotations

import jax

jax.config.update("jax_enable_x64", True)

import jax.numpy as jnp
import numpy as np

from movingknots.basis import design_matrix
from movingknots.fit import (
    fit_full_gaussian_vi,
    fit_marginal_gaussian_vi,
    gaussian_lpds,
    predict_mean,
)


def make_synthetic_data(n_obs: int = 24, noise_scale: float = 0.02):
    x = jnp.linspace(-1.0, 1.0, n_obs)[:, None]
    true_knots = {"thinplate.a": jnp.array([[-0.45]])}
    initial_knots = {"thinplate.a": jnp.array([[0.75]])}
    spline_config = {
        "comp": ("intercept", "covariates", "thinplate.a"),
        "thinplate.a.locate": (1,),
    }
    beta_true = jnp.array([[0.0], [0.0], [4.0]])
    signal = design_matrix(x, true_knots, spline_config) @ beta_true
    y = signal + noise_scale * jnp.sin(5.0 * x)
    return x, y, true_knots, initial_knots, spline_config


def bad_fixed_knot_fit(x, y, knots, spline_config):
    x_design = design_matrix(x, knots, spline_config)
    beta = np.linalg.lstsq(np.asarray(x_design), np.asarray(y), rcond=None)[0]
    residual = np.asarray(y) - np.asarray(x_design) @ beta
    variance = max(float(np.mean(residual**2)), 1e-6)
    position = jnp.concatenate(
        [jnp.asarray(beta).reshape(-1), jnp.asarray([jnp.log(variance)])]
    )
    return {
        "kind": "fixed",
        "n_coef": int(x_design.shape[1]),
        "beta_mean": jnp.asarray(beta),
        "sigma_mean": jnp.asarray([[variance]]),
        "vi": {
            "mu": position,
            "rho": jnp.full_like(position, -30.0),
            "elbo": jnp.asarray([jnp.nan]),
        },
        "knots": knots,
        "spline_config": spline_config,
    }


def fit_metrics(name, fit, x, y, key, n_predictive_samples: int):
    residual = predict_mean(fit, x) - y
    score = gaussian_lpds(
        fit,
        x,
        y,
        key=key,
        n_samples=n_predictive_samples,
    )
    elbo = fit["vi"]["elbo"][-1]
    out = {
        "model": name,
        "train_mse": float(jnp.mean(residual**2)),
        "lpds": float(score["lpds"]),
        "nse_lpds": float(score["nse_lpds"]),
        "final_elbo": None if bool(jnp.isnan(elbo)) else float(elbo),
        "n_variational_parameters": int(fit["vi"]["mu"].size),
    }
    if "additive_knots_mean" in fit:
        out["additive_knot"] = float(fit["additive_knots_mean"][0, 0])
    elif "thinplate.a" in fit["knots"]:
        out["additive_knot"] = float(fit["knots"]["thinplate.a"][0, 0])
    return out


def print_metrics(metrics):
    print("true knot:", metrics["true_knot"])
    print("initial knot:", metrics["initial_knot"])
    print(
        f"{'model':<14} {'knot':>9} {'mse':>12} {'lpds':>12} "
        f"{'elbo':>12} {'npar':>6}"
    )
    for row in metrics["models"].values():
        elbo = row["final_elbo"]
        elbo_text = "NA" if elbo is None else f"{elbo:.3f}"
        print(
            f"{row['model']:<14} {row.get('additive_knot', np.nan):>9.3f} "
            f"{row['train_mse']:>12.4f} {row['lpds']:>12.3f} "
            f"{elbo_text:>12} {row['n_variational_parameters']:>6}"
        )


def main(
    n_obs: int = 24,
    n_steps: int = 30,
    n_predictive_samples: int = 4,
    key_seed: int = 10,
    print_results: bool = True,
):
    x, y, true_knots, initial_knots, spline_config = make_synthetic_data(n_obs=n_obs)
    keys = jax.random.split(jax.random.PRNGKey(key_seed), 5)
    fit_kwargs = {
        "n_steps": n_steps,
        "learning_rate": 0.03,
        "n_samples": 1,
        "init_scale": 0.01,
        "knot_prior_variance": 20.0,
        "p_matrix_types": ("X'X", "identity"),
    }

    fixed_fit = bad_fixed_knot_fit(x, y, initial_knots, spline_config)
    full_fit = fit_full_gaussian_vi(
        x,
        y,
        knots=initial_knots,
        spline_config=spline_config,
        free_additive=True,
        free_surface=False,
        key=keys[0],
        **fit_kwargs,
    )
    marginal_fit = fit_marginal_gaussian_vi(
        x,
        y,
        knots=initial_knots,
        spline_config=spline_config,
        free_additive=True,
        free_surface=False,
        key=keys[1],
        **fit_kwargs,
    )

    metrics = {
        "true_knot": float(true_knots["thinplate.a"][0, 0]),
        "initial_knot": float(initial_knots["thinplate.a"][0, 0]),
        "models": {
            "bad_fixed": fit_metrics(
                "bad_fixed",
                fixed_fit,
                x,
                y,
                keys[2],
                n_predictive_samples,
            ),
            "full": fit_metrics("full", full_fit, x, y, keys[3], n_predictive_samples),
            "marginal": fit_metrics(
                "marginal",
                marginal_fit,
                x,
                y,
                keys[4],
                n_predictive_samples,
            ),
        },
    }
    if print_results:
        print_metrics(metrics)
    return metrics


if __name__ == "__main__":
    main()
