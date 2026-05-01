"""Model likelihoods."""

from __future__ import annotations

import jax.numpy as jnp
import numpy as np

from movingknots.basis import design_matrix
from movingknots.priors import log_prior


def gaussian_log_likelihood(y, x, knots, spline_config, beta, sigma):
    """Return the Gaussian multivariate regression log likelihood."""
    y = jnp.asarray(y)
    x = jnp.asarray(x)
    beta = jnp.asarray(beta)
    sigma = jnp.asarray(sigma)

    if y.ndim == 1:
        y = y[:, None]
    if beta.ndim == 1:
        beta = beta[:, None]
    if sigma.ndim == 0:
        sigma = sigma.reshape((1, 1))

    x_design = design_matrix(x, knots, spline_config)
    residual = y - x_design @ beta
    n_obs, n_response = y.shape

    sign, logdet = jnp.linalg.slogdet(sigma)
    solved = jnp.linalg.solve(sigma, residual.T)
    quadratic = jnp.sum(residual.T * solved)

    log_likelihood = (
        -0.5 * n_obs * n_response * jnp.log(2 * jnp.pi)
        -0.5 * n_obs * logdet
        -0.5 * quadratic
    )
    return jnp.where(sign > 0, log_likelihood, -jnp.inf)


def log_joint(y, x, params, spline_config, prior_config):
    """Return log likelihood plus log prior."""
    return gaussian_log_likelihood(
        y=y,
        x=x,
        knots=params.get("knots", {}),
        spline_config=spline_config,
        beta=params["beta"],
        sigma=params["sigma"],
    ) + log_prior(params, prior_config)


def marginal_gaussian_log_likelihood(
    y,
    x,
    knots,
    spline_config,
    sigma,
    log_shrinkage,
    schema,
    beta_prior_variance: float = 25.0,
    beta_prior_mean=None,
    p_matrix_types=None,
):
    """Return log p(Y | knots, shrinkage, Sigma) with B integrated out."""
    y = jnp.asarray(y)
    x = jnp.asarray(x)
    sigma = jnp.asarray(sigma)
    log_shrinkage = jnp.asarray(log_shrinkage)
    if y.ndim == 1:
        y = y[:, None]
    if sigma.ndim == 0:
        sigma = sigma.reshape((1, 1))

    x_design = design_matrix(x, knots, spline_config)
    p_matrices = make_p_matrices(x_design, schema, p_matrix_types)
    beta_mean = _beta_prior_mean(schema, beta_prior_mean)
    design_for_vec = _row_major_design_for_beta(x_design, schema.n_response)
    y_vec = y.reshape(-1)
    beta_mean_vec = beta_mean.reshape(-1)
    residual_vec = y_vec - design_for_vec @ beta_mean_vec

    observation_cov = jnp.kron(jnp.eye(y.shape[0], dtype=sigma.dtype), sigma)
    beta_cov = coefficient_prior_covariance(
        schema=schema,
        sigma=sigma,
        log_shrinkage=log_shrinkage,
        beta_prior_variance=beta_prior_variance,
        p_matrices=p_matrices,
    )
    marginal_cov = observation_cov + design_for_vec @ beta_cov @ design_for_vec.T
    return _multivariate_normal_logpdf_zero_mean(residual_vec, marginal_cov)


def conditional_gaussian_beta_posterior(
    y,
    x,
    knots,
    spline_config,
    sigma,
    log_shrinkage,
    schema,
    beta_prior_variance: float = 25.0,
    beta_prior_mean=None,
    p_matrix_types=None,
):
    """Return p(B | Y, knots, shrinkage, Sigma) mean and covariance."""
    y = jnp.asarray(y)
    x = jnp.asarray(x)
    sigma = jnp.asarray(sigma)
    log_shrinkage = jnp.asarray(log_shrinkage)
    if y.ndim == 1:
        y = y[:, None]
    if sigma.ndim == 0:
        sigma = sigma.reshape((1, 1))

    x_design = design_matrix(x, knots, spline_config)
    p_matrices = make_p_matrices(x_design, schema, p_matrix_types)
    beta_mean = _beta_prior_mean(schema, beta_prior_mean)
    beta_mean_vec = beta_mean.reshape(-1)
    design_for_vec = _row_major_design_for_beta(x_design, schema.n_response)
    y_vec = y.reshape(-1)

    observation_cov = jnp.kron(jnp.eye(y.shape[0], dtype=sigma.dtype), sigma)
    beta_cov = coefficient_prior_covariance(
        schema=schema,
        sigma=sigma,
        log_shrinkage=log_shrinkage,
        beta_prior_variance=beta_prior_variance,
        p_matrices=p_matrices,
    )
    prior_precision = jnp.linalg.inv(beta_cov)
    observation_precision = jnp.linalg.inv(observation_cov)
    posterior_precision = (
        prior_precision + design_for_vec.T @ observation_precision @ design_for_vec
    )
    posterior_cov = jnp.linalg.inv(posterior_precision)
    posterior_mean_vec = posterior_cov @ (
        prior_precision @ beta_mean_vec
        + design_for_vec.T @ observation_precision @ y_vec
    )
    return {
        "mean": posterior_mean_vec.reshape((schema.n_coef, schema.n_response)),
        "covariance": posterior_cov,
    }


def coefficient_prior_covariance(
    schema,
    sigma,
    log_shrinkage,
    beta_prior_variance: float = 25.0,
    p_matrices=None,
):
    """Return Cov(vec_row_major(B) | Sigma, shrinkage) for the block prior."""
    sigma = jnp.asarray(sigma)
    log_shrinkage = jnp.asarray(log_shrinkage).reshape(schema.log_shrinkage_shape)
    dtype = jnp.result_type(sigma, log_shrinkage)
    p_matrices = _normalize_p_matrices(schema, p_matrices, dtype)
    out = jnp.zeros(
        (schema.n_coef * schema.n_response, schema.n_coef * schema.n_response),
        dtype=dtype,
    )
    coef_blocks = np.asarray(schema.coef_block_indices)
    for block_index in range(schema.n_shrinkage_blocks):
        rows = np.flatnonzero(coef_blocks == block_index)
        if rows.size == 0:
            continue
        shrinkage_scale = jnp.sqrt(jnp.exp(log_shrinkage[block_index]))
        response_cov = beta_prior_variance * (
            shrinkage_scale[:, None] * sigma * shrinkage_scale[None, :]
        )
        p_inv = jnp.linalg.pinv(p_matrices[block_index])
        block_cov = jnp.kron(p_inv, response_cov)
        indices = _row_major_block_indices(rows, schema.n_response)
        out = out.at[indices[:, None], indices[None, :]].set(block_cov)
    return out


def make_p_matrices(x_design, schema, p_matrix_types=None):
    """Build R-style component penalty matrices from a design matrix."""
    x_design = jnp.asarray(x_design)
    p_matrix_types = _normalize_p_matrix_types(schema, p_matrix_types)
    coef_blocks = np.asarray(schema.coef_block_indices)
    p_matrices = []
    for block_index, p_type in enumerate(p_matrix_types):
        rows = np.flatnonzero(coef_blocks == block_index)
        width = int(rows.size)
        if p_type == "identity":
            p_matrices.append(jnp.eye(width, dtype=x_design.dtype))
        elif p_type == "X'X":
            x_block = x_design[:, jnp.asarray(rows)]
            p_matrices.append(x_block.T @ x_block)
        else:
            raise ValueError("p_matrix_types entries must be 'identity' or \"X'X\"")
    return tuple(p_matrices)


def _row_major_design_for_beta(x_design, n_response: int):
    return jnp.kron(x_design, jnp.eye(n_response, dtype=x_design.dtype))


def _beta_prior_mean(schema, beta_prior_mean):
    if beta_prior_mean is None:
        return jnp.zeros((schema.n_coef, schema.n_response))
    return jnp.asarray(beta_prior_mean).reshape((schema.n_coef, schema.n_response))


def _multivariate_normal_logpdf_zero_mean(value, covariance):
    value = jnp.asarray(value)
    covariance = jnp.asarray(covariance)
    sign, logdet = jnp.linalg.slogdet(covariance)
    quadratic = value @ jnp.linalg.solve(covariance, value)
    log_density = (
        -0.5 * value.size * jnp.log(2 * jnp.pi)
        -0.5 * logdet
        -0.5 * quadratic
    )
    return jnp.where(sign > 0, log_density, -jnp.inf)


def _normalize_p_matrix_types(schema, p_matrix_types):
    if p_matrix_types is None:
        return ("identity",) * schema.n_shrinkage_blocks
    if isinstance(p_matrix_types, str):
        p_matrix_types = (p_matrix_types,) * schema.n_shrinkage_blocks
    p_matrix_types = tuple(p_matrix_types)
    if len(p_matrix_types) != schema.n_shrinkage_blocks:
        raise ValueError("p_matrix_types must have one entry per shrinkage block")

    normalized = []
    for p_type in p_matrix_types:
        p_type = str(p_type)
        if p_type.lower() == "identity":
            normalized.append("identity")
        elif p_type.upper() == "X'X":
            normalized.append("X'X")
        else:
            raise ValueError("p_matrix_types entries must be 'identity' or \"X'X\"")
    return tuple(normalized)


def _normalize_p_matrices(schema, p_matrices, dtype):
    if p_matrices is None:
        return tuple(
            jnp.eye(
                sum(1 for block in schema.coef_block_indices if block == block_index),
                dtype=dtype,
            )
            for block_index in range(schema.n_shrinkage_blocks)
        )
    p_matrices = tuple(jnp.asarray(p_matrix, dtype=dtype) for p_matrix in p_matrices)
    if len(p_matrices) != schema.n_shrinkage_blocks:
        raise ValueError("p_matrices must have one matrix per shrinkage block")

    coef_blocks = np.asarray(schema.coef_block_indices)
    for block_index, p_matrix in enumerate(p_matrices):
        width = int(np.sum(coef_blocks == block_index))
        if p_matrix.shape != (width, width):
            raise ValueError("p_matrices block shape does not match the schema")
    return p_matrices


def _row_major_block_indices(rows, n_response: int):
    return jnp.asarray(
        [row * n_response + response for row in rows for response in range(n_response)]
    )
