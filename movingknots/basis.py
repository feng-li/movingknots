"""Spline basis construction."""

from __future__ import annotations

import jax.numpy as jnp


def thinplate_r2_log_r(squared_distance):
    """Evaluate r^2 log(r), with value 0 at r = 0."""
    squared_distance = jnp.asarray(squared_distance)
    safe_squared_distance = jnp.where(squared_distance > 0, squared_distance, 1.0)
    return jnp.where(
        squared_distance > 0,
        0.5 * squared_distance * jnp.log(safe_squared_distance),
        0.0,
    )


def thinplate_surface_basis(x, knots):
    """Return thin-plate surface basis values for observations and multivariate knots."""
    x = jnp.asarray(x)
    knots = jnp.asarray(knots)
    diff = x[:, None, :] - knots[None, :, :]
    squared_distance = jnp.sum(diff * diff, axis=-1)
    return thinplate_r2_log_r(squared_distance)


def thinplate_additive_basis(x, knots, additive_counts):
    """Return additive thin-plate basis values grouped by covariate."""
    x = jnp.asarray(x)
    knots = jnp.asarray(knots).reshape(-1)
    additive_counts = tuple(int(count) for count in additive_counts)

    columns = []
    start = 0
    for covariate_index, count in enumerate(additive_counts):
        stop = start + count
        if count:
            covariate = x[:, covariate_index : covariate_index + 1]
            covariate_knots = knots[start:stop][None, :]
            squared_distance = (covariate - covariate_knots) ** 2
            columns.append(thinplate_r2_log_r(squared_distance))
        start = stop

    if not columns:
        return jnp.zeros((x.shape[0], 0), dtype=x.dtype)
    return jnp.concatenate(columns, axis=1)


def design_matrix(x, knots, spline_config):
    """Build the moving-knots design matrix in the configured component order."""
    x = jnp.asarray(x)
    components = spline_config.get("comp", ("intercept", "covariates"))

    blocks = []
    for component in components:
        if component == "intercept":
            blocks.append(jnp.ones((x.shape[0], 1), dtype=x.dtype))
        elif component == "covariates":
            blocks.append(x)
        elif component == "thinplate.s":
            blocks.append(thinplate_surface_basis(x, knots["thinplate.s"]))
        elif component == "thinplate.a":
            blocks.append(
                thinplate_additive_basis(
                    x,
                    knots["thinplate.a"],
                    spline_config["thinplate.a.locate"],
                )
            )
        else:
            raise ValueError(f"Unknown spline component: {component}")

    if not blocks:
        return jnp.zeros((x.shape[0], 0), dtype=x.dtype)
    return jnp.concatenate(blocks, axis=1)
