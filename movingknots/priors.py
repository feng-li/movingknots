"""Prior densities for the moving-knots model."""

from __future__ import annotations

import jax.numpy as jnp
from jax.scipy.special import gammaln


def normal_logpdf(value, mean=0.0, variance=1.0):
    """Elementwise normal log density summed over all entries."""
    value = jnp.asarray(value)
    mean = jnp.asarray(mean)
    variance = jnp.asarray(variance)
    return jnp.sum(
        -0.5 * (jnp.log(2 * jnp.pi) + jnp.log(variance) + (value - mean) ** 2 / variance)
    )


def multigammaln(p: int, x):
    """Log multivariate gamma function."""
    x = jnp.asarray(x)
    total = p * (p - 1) / 4 * jnp.log(jnp.pi)
    for j in range(1, p + 1):
        total = total + gammaln(x + (1 - j) / 2)
    return total


def inverse_wishart_logpdf(value, df, scale):
    """Inverse-Wishart log density."""
    value = jnp.asarray(value)
    scale = jnp.asarray(scale)
    p = value.shape[0]

    sign_value, logdet_value = jnp.linalg.slogdet(value)
    sign_scale, logdet_scale = jnp.linalg.slogdet(scale)
    log_density = (
        -df * p / 2 * jnp.log(2.0)
        - multigammaln(p, df / 2)
        + df / 2 * logdet_scale
        - (df + p + 1) / 2 * logdet_value
        - 0.5 * jnp.trace(jnp.linalg.solve(value, scale))
    )
    return jnp.where((sign_value > 0) & (sign_scale > 0), log_density, -jnp.inf)


def log_prior(params, prior_config):
    """Return the summed log prior for a parameter dictionary."""
    total = 0.0

    if "beta" in params and "beta" in prior_config:
        beta_config = prior_config["beta"]
        beta_variance = beta_config.get("variance", 1.0)
        if beta_config.get("use_shrinkage", False):
            beta_variance = _beta_variance_from_shrinkage(
                params["beta"],
                params["log_shrinkage"],
                beta_config,
            )
        total = total + normal_logpdf(
            params["beta"],
            mean=beta_config.get("mean", 0.0),
            variance=beta_variance,
        )

    if "sigma" in params and "sigma" in prior_config:
        sigma_config = prior_config["sigma"]
        total = total + inverse_wishart_logpdf(
            params["sigma"],
            df=sigma_config["df"],
            scale=sigma_config["scale"],
        )

    if "log_shrinkage" in params and "log_shrinkage" in prior_config:
        shrinkage_config = prior_config["log_shrinkage"]
        total = total + normal_logpdf(
            params["log_shrinkage"],
            mean=shrinkage_config.get("mean", 0.0),
            variance=shrinkage_config.get("variance", 1.0),
        )

    if "knots" in params and "knots" in prior_config:
        total = total + _knots_log_prior(params["knots"], prior_config["knots"])

    return total


def _beta_variance_from_shrinkage(beta, log_shrinkage, beta_config):
    beta = jnp.asarray(beta)
    log_shrinkage = jnp.asarray(log_shrinkage)
    base_variance = jnp.asarray(beta_config.get("variance", 1.0))
    row_blocks = beta_config.get("row_blocks")

    if row_blocks is None:
        shrinkage = jnp.exp(log_shrinkage)
        if shrinkage.size == 1:
            return base_variance * shrinkage
        if shrinkage.shape == (beta.shape[1],):
            return base_variance * shrinkage[None, :]
        return base_variance * jnp.reshape(shrinkage, beta.shape)

    shrinkage = jnp.reshape(jnp.exp(log_shrinkage), (len(row_blocks), beta.shape[1]))
    variance = jnp.ones_like(beta) * base_variance
    for block_index, rows in enumerate(row_blocks):
        variance = variance.at[jnp.asarray(rows), :].set(base_variance * shrinkage[block_index])
    return variance


def _knots_log_prior(knots, knots_config):
    total = 0.0
    for name, value in knots.items():
        if name not in knots_config:
            continue
        config = knots_config[name]
        total = total + normal_logpdf(
            value,
            mean=config.get("mean", 0.0),
            variance=config.get("variance", 1.0),
        )
    return total
