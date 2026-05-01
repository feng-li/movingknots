"""Flat parameter schemas for Gaussian moving-knot models."""

from __future__ import annotations

from dataclasses import dataclass

import jax.numpy as jnp

from movingknots.basis import design_matrix


@dataclass(frozen=True)
class GaussianParameterSchema:
    """Layout for a flat unconstrained Gaussian moving-knots parameter vector."""

    n_coef: int
    n_response: int
    n_cholesky: int
    shrinkage_block_names: tuple[str, ...]
    coef_block_indices: tuple[int, ...]
    additive_shape: tuple[int, ...] | None
    surface_shape: tuple[int, ...] | None
    free_additive: bool
    free_surface: bool
    beta_slice: slice
    cholesky_slice: slice
    log_shrinkage_slice: slice
    additive_slice: slice
    surface_slice: slice
    size: int

    @property
    def n_shrinkage_blocks(self):
        return len(self.shrinkage_block_names)

    @property
    def log_shrinkage_shape(self):
        return (self.n_shrinkage_blocks, self.n_response)


@dataclass(frozen=True)
class MarginalGaussianParameterSchema:
    """Layout for non-coefficient parameters with B integrated out."""

    n_coef: int
    n_response: int
    n_cholesky: int
    shrinkage_block_names: tuple[str, ...]
    coef_block_indices: tuple[int, ...]
    additive_shape: tuple[int, ...] | None
    surface_shape: tuple[int, ...] | None
    free_additive: bool
    free_surface: bool
    cholesky_slice: slice
    log_shrinkage_slice: slice
    additive_slice: slice
    surface_slice: slice
    size: int

    @property
    def n_shrinkage_blocks(self):
        return len(self.shrinkage_block_names)

    @property
    def log_shrinkage_shape(self):
        return (self.n_shrinkage_blocks, self.n_response)


def build_gaussian_parameter_schema(
    x,
    y,
    knots,
    spline_config,
    free_additive: bool | None = None,
    free_surface: bool | None = None,
):
    """Build the flat parameter layout for a Gaussian moving-knots model."""
    x = jnp.asarray(x)
    y = jnp.asarray(y)
    if y.ndim == 1:
        y = y[:, None]

    knots = normalize_knots(knots, spline_config)
    components = tuple(spline_config.get("comp", ("intercept", "covariates")))
    component_set = set(components)
    free_additive = (
        "thinplate.a" in component_set if free_additive is None else bool(free_additive)
    )
    free_surface = (
        "thinplate.s" in component_set if free_surface is None else bool(free_surface)
    )
    if free_additive and "thinplate.a" not in component_set:
        raise ValueError("free_additive=True requires 'thinplate.a' in spline_config")
    if free_surface and "thinplate.s" not in component_set:
        raise ValueError("free_surface=True requires 'thinplate.s' in spline_config")

    x_design = design_matrix(x, knots, spline_config)
    n_coef = int(x_design.shape[1])
    n_response = int(y.shape[1])
    n_cholesky = n_response * (n_response + 1) // 2

    block_names = []
    block_index_by_name = {}
    coef_block_indices = []
    for component in components:
        width = _component_width(component, x, knots, spline_config)
        block_name = _component_block_name(component)
        if block_name not in block_index_by_name:
            block_index_by_name[block_name] = len(block_names)
            block_names.append(block_name)
        coef_block_indices.extend([block_index_by_name[block_name]] * width)

    if len(coef_block_indices) != n_coef:
        raise ValueError("internal schema error: coefficient block layout is inconsistent")

    start = 0
    beta_slice = slice(start, start + n_coef * n_response)
    start = beta_slice.stop
    cholesky_slice = slice(start, start + n_cholesky)
    start = cholesky_slice.stop
    n_log_shrinkage = len(block_names) * n_response
    log_shrinkage_slice = slice(start, start + n_log_shrinkage)
    start = log_shrinkage_slice.stop

    additive_shape = None
    additive_slice = slice(start, start)
    if free_additive:
        additive_shape = tuple(jnp.asarray(knots["thinplate.a"]).shape)
        n_additive = int(jnp.asarray(knots["thinplate.a"]).size)
        additive_slice = slice(start, start + n_additive)
        start = additive_slice.stop

    surface_shape = None
    surface_slice = slice(start, start)
    if free_surface:
        surface_shape = tuple(jnp.asarray(knots["thinplate.s"]).shape)
        n_surface = int(jnp.asarray(knots["thinplate.s"]).size)
        surface_slice = slice(start, start + n_surface)
        start = surface_slice.stop

    return GaussianParameterSchema(
        n_coef=n_coef,
        n_response=n_response,
        n_cholesky=n_cholesky,
        shrinkage_block_names=tuple(block_names),
        coef_block_indices=tuple(coef_block_indices),
        additive_shape=additive_shape,
        surface_shape=surface_shape,
        free_additive=free_additive,
        free_surface=free_surface,
        beta_slice=beta_slice,
        cholesky_slice=cholesky_slice,
        log_shrinkage_slice=log_shrinkage_slice,
        additive_slice=additive_slice,
        surface_slice=surface_slice,
        size=start,
    )


def build_marginal_gaussian_parameter_schema(
    x,
    y,
    knots,
    spline_config,
    free_additive: bool | None = None,
    free_surface: bool | None = None,
):
    """Build the flat parameter layout after integrating out coefficients."""
    full_schema = build_gaussian_parameter_schema(
        x=x,
        y=y,
        knots=knots,
        spline_config=spline_config,
        free_additive=free_additive,
        free_surface=free_surface,
    )

    start = 0
    cholesky_slice = slice(start, start + full_schema.n_cholesky)
    start = cholesky_slice.stop
    n_log_shrinkage = full_schema.n_shrinkage_blocks * full_schema.n_response
    log_shrinkage_slice = slice(start, start + n_log_shrinkage)
    start = log_shrinkage_slice.stop

    additive_slice = slice(start, start)
    if full_schema.free_additive:
        n_additive = int(jnp.prod(jnp.asarray(full_schema.additive_shape)))
        additive_slice = slice(start, start + n_additive)
        start = additive_slice.stop

    surface_slice = slice(start, start)
    if full_schema.free_surface:
        n_surface = int(jnp.prod(jnp.asarray(full_schema.surface_shape)))
        surface_slice = slice(start, start + n_surface)
        start = surface_slice.stop

    return MarginalGaussianParameterSchema(
        n_coef=full_schema.n_coef,
        n_response=full_schema.n_response,
        n_cholesky=full_schema.n_cholesky,
        shrinkage_block_names=full_schema.shrinkage_block_names,
        coef_block_indices=full_schema.coef_block_indices,
        additive_shape=full_schema.additive_shape,
        surface_shape=full_schema.surface_shape,
        free_additive=full_schema.free_additive,
        free_surface=full_schema.free_surface,
        cholesky_slice=cholesky_slice,
        log_shrinkage_slice=log_shrinkage_slice,
        additive_slice=additive_slice,
        surface_slice=surface_slice,
        size=start,
    )


def normalize_knots(knots, spline_config):
    """Return knot arrays in the canonical shapes used by the schema."""
    knots = dict(knots)
    components = set(spline_config.get("comp", ()))
    if "thinplate.a" in components:
        if "thinplate.a" not in knots:
            raise ValueError("knots must include 'thinplate.a'")
        knots["thinplate.a"] = jnp.asarray(knots["thinplate.a"]).reshape((-1, 1))
        expected = sum(int(v) for v in spline_config.get("thinplate.a.locate", ()))
        if int(knots["thinplate.a"].shape[0]) != expected:
            raise ValueError("thinplate.a.locate does not match additive knots")
    if "thinplate.s" in components:
        if "thinplate.s" not in knots:
            raise ValueError("knots must include 'thinplate.s'")
        knots["thinplate.s"] = jnp.asarray(knots["thinplate.s"])
        if knots["thinplate.s"].ndim != 2:
            raise ValueError("thinplate.s knots must be a two-dimensional array")
        expected = tuple(spline_config.get("thinplate.s.dim", knots["thinplate.s"].shape))
        if tuple(knots["thinplate.s"].shape) != expected:
            raise ValueError("thinplate.s.dim does not match surface knots")
    return knots


def pack_marginal_gaussian_parameters(
    schema: MarginalGaussianParameterSchema,
    raw_cholesky,
    log_shrinkage,
    additive_knots=None,
    surface_knots=None,
):
    """Pack non-coefficient parameters into the marginal schema's flat vector."""
    parts = [
        jnp.asarray(raw_cholesky).reshape(-1),
        jnp.asarray(log_shrinkage).reshape(-1),
    ]
    if schema.free_additive:
        parts.append(jnp.asarray(additive_knots).reshape(-1))
    if schema.free_surface:
        parts.append(jnp.asarray(surface_knots).reshape(-1))
    out = jnp.concatenate(parts)
    if int(out.size) != schema.size:
        raise ValueError("packed parameter vector does not match marginal schema size")
    return out


def pack_gaussian_parameters(
    schema: GaussianParameterSchema,
    beta,
    raw_cholesky,
    log_shrinkage,
    additive_knots=None,
    surface_knots=None,
):
    """Pack structured parameters into the schema's flat vector."""
    parts = [
        jnp.asarray(beta).reshape(-1),
        jnp.asarray(raw_cholesky).reshape(-1),
        jnp.asarray(log_shrinkage).reshape(-1),
    ]
    if schema.free_additive:
        parts.append(jnp.asarray(additive_knots).reshape(-1))
    if schema.free_surface:
        parts.append(jnp.asarray(surface_knots).reshape(-1))
    out = jnp.concatenate(parts)
    if int(out.size) != schema.size:
        raise ValueError("packed parameter vector does not match schema size")
    return out


def unpack_marginal_gaussian_parameters(
    schema: MarginalGaussianParameterSchema,
    z,
    base_knots,
):
    """Unpack non-coefficient parameters and transformed covariance."""
    z = jnp.asarray(z)
    raw_cholesky = z[schema.cholesky_slice]
    sigma_cholesky = cholesky_from_unconstrained(raw_cholesky, schema.n_response)
    sigma = sigma_cholesky @ sigma_cholesky.T
    log_shrinkage = z[schema.log_shrinkage_slice].reshape(schema.log_shrinkage_shape)

    knots = dict(base_knots)
    additive_knots = None
    if schema.free_additive:
        additive_knots = z[schema.additive_slice].reshape(schema.additive_shape)
        knots["thinplate.a"] = additive_knots

    surface_knots = None
    if schema.free_surface:
        surface_knots = z[schema.surface_slice].reshape(schema.surface_shape)
        knots["thinplate.s"] = surface_knots

    return {
        "raw_cholesky": raw_cholesky,
        "sigma_cholesky": sigma_cholesky,
        "sigma": sigma,
        "log_shrinkage": log_shrinkage,
        "shrinkage": jnp.exp(log_shrinkage),
        "knots": knots,
        "additive_knots": additive_knots,
        "surface_knots": surface_knots,
    }


def unpack_gaussian_parameters(
    schema: GaussianParameterSchema,
    z,
    base_knots,
):
    """Unpack a flat vector into model parameters and transformed covariance."""
    z = jnp.asarray(z)
    beta = z[schema.beta_slice].reshape((schema.n_coef, schema.n_response))
    raw_cholesky = z[schema.cholesky_slice]
    sigma_cholesky = cholesky_from_unconstrained(raw_cholesky, schema.n_response)
    sigma = sigma_cholesky @ sigma_cholesky.T
    log_shrinkage = z[schema.log_shrinkage_slice].reshape(schema.log_shrinkage_shape)

    knots = dict(base_knots)
    additive_knots = None
    if schema.free_additive:
        additive_knots = z[schema.additive_slice].reshape(schema.additive_shape)
        knots["thinplate.a"] = additive_knots

    surface_knots = None
    if schema.free_surface:
        surface_knots = z[schema.surface_slice].reshape(schema.surface_shape)
        knots["thinplate.s"] = surface_knots

    return {
        "beta": beta,
        "raw_cholesky": raw_cholesky,
        "sigma_cholesky": sigma_cholesky,
        "sigma": sigma,
        "log_shrinkage": log_shrinkage,
        "shrinkage": jnp.exp(log_shrinkage),
        "knots": knots,
        "additive_knots": additive_knots,
        "surface_knots": surface_knots,
    }


def cholesky_from_unconstrained(raw_cholesky, n_response: int):
    """Map unconstrained lower-triangle values to a positive-diagonal Cholesky factor."""
    raw_cholesky = jnp.asarray(raw_cholesky)
    out = jnp.zeros((n_response, n_response), dtype=raw_cholesky.dtype)
    idx = 0
    for row in range(n_response):
        for col in range(row + 1):
            value = raw_cholesky[idx]
            if row == col:
                value = jnp.exp(value)
            out = out.at[row, col].set(value)
            idx += 1
    return out


def unconstrained_from_cholesky(cholesky):
    """Flatten a positive lower Cholesky factor into unconstrained parameters."""
    cholesky = jnp.asarray(cholesky)
    values = []
    for row in range(cholesky.shape[0]):
        for col in range(row + 1):
            value = cholesky[row, col]
            if row == col:
                value = jnp.log(value)
            values.append(value)
    return jnp.asarray(values)


def unconstrained_cholesky_from_sigma(sigma):
    """Return unconstrained Cholesky parameters for a positive definite covariance."""
    return unconstrained_from_cholesky(jnp.linalg.cholesky(jnp.asarray(sigma)))


def cholesky_to_sigma_log_jacobian(raw_cholesky, n_response: int):
    """Log Jacobian for unconstrained Cholesky parameters mapped to Sigma = L L'."""
    raw_cholesky = jnp.asarray(raw_cholesky)
    log_diag = []
    idx = 0
    for row in range(n_response):
        for col in range(row + 1):
            if row == col:
                log_diag.append(raw_cholesky[idx])
            idx += 1

    total = n_response * jnp.log(2.0)
    for i, value in enumerate(log_diag):
        total = total + (n_response + 1 - i) * value
    return total


def _component_width(component, x, knots, spline_config):
    if component == "intercept":
        return 1
    if component == "covariates":
        return int(x.shape[1])
    if component == "thinplate.a":
        return int(jnp.asarray(knots["thinplate.a"]).shape[0])
    if component == "thinplate.s":
        return int(jnp.asarray(knots["thinplate.s"]).shape[0])
    raise ValueError(f"Unknown spline component: {component}")


def _component_block_name(component):
    if component in ("intercept", "covariates"):
        return "linear"
    if component == "thinplate.a":
        return "additive"
    if component == "thinplate.s":
        return "surface"
    raise ValueError(f"Unknown spline component: {component}")
