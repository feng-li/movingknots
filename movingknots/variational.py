"""Minimal mean-field variational inference utilities."""

from __future__ import annotations

import jax
import jax.numpy as jnp


def init_mean_field(position, init_scale: float = 0.1):
    """Initialize a diagonal Gaussian variational family around a flat position."""
    position = jnp.ravel(jnp.asarray(position))
    rho = jnp.full_like(position, _inverse_softplus(init_scale))
    return {"mu": position, "rho": rho}


def mean_field_scale(rho, jitter: float = 1e-6):
    """Convert unconstrained scale parameters to positive standard deviations."""
    return jax.nn.softplus(rho) + jitter


def sample_mean_field(mu, rho, key, n_samples: int = 1):
    """Draw reparameterized samples from q(z)."""
    mu = jnp.asarray(mu)
    scale = mean_field_scale(rho)
    eps = jax.random.normal(key, shape=(n_samples, mu.size), dtype=mu.dtype)
    samples = mu[None, :] + scale[None, :] * eps
    if n_samples == 1:
        return samples[0]
    return samples


def mean_field_logpdf(z, mu, rho):
    """Return log q(z) for a diagonal Gaussian."""
    z = jnp.asarray(z)
    mu = jnp.asarray(mu)
    scale = mean_field_scale(rho)
    return jnp.sum(
        -0.5
        * (
            jnp.log(2 * jnp.pi)
            + 2 * jnp.log(scale)
            + ((z - mu) / scale) ** 2
        )
    )


def elbo(mu, rho, log_prob_fn, key, n_samples: int = 1):
    """Monte Carlo estimate of E_q[log p(z) - log q(z)]."""
    samples = sample_mean_field(mu, rho, key, n_samples=n_samples)
    samples = jnp.atleast_2d(samples)
    values = jax.vmap(lambda z: log_prob_fn(z) - mean_field_logpdf(z, mu, rho))(samples)
    return jnp.mean(values)


def fit_mean_field(
    log_prob_fn,
    init_position,
    key,
    n_steps: int = 1000,
    learning_rate: float = 1e-2,
    n_samples: int = 8,
    init_scale: float = 0.1,
):
    """Optimize a mean-field Gaussian ELBO with a small Adam implementation."""
    state = init_mean_field(init_position, init_scale=init_scale)
    opt_state = _init_adam(state)
    elbo_history = []
    sample_key = key

    def objective(current_state, current_key):
        return -elbo(
            current_state["mu"],
            current_state["rho"],
            log_prob_fn,
            current_key,
            n_samples=n_samples,
        )

    value_and_grad = jax.value_and_grad(objective)

    for step in range(1, n_steps + 1):
        sample_key, step_key = jax.random.split(sample_key)
        loss, grads = value_and_grad(state, step_key)
        state, opt_state = _adam_update(
            state,
            grads,
            opt_state,
            step=step,
            learning_rate=learning_rate,
        )
        elbo_history.append(-loss)

    return {
        "mu": state["mu"],
        "rho": state["rho"],
        "elbo": jnp.asarray(elbo_history),
    }


def _inverse_softplus(value):
    value = jnp.asarray(value)
    return jnp.log(jnp.expm1(value))


def _init_adam(state):
    return {
        "m": {name: jnp.zeros_like(value) for name, value in state.items()},
        "v": {name: jnp.zeros_like(value) for name, value in state.items()},
    }


def _adam_update(
    state,
    grads,
    opt_state,
    step: int,
    learning_rate: float,
    beta1: float = 0.9,
    beta2: float = 0.999,
    eps: float = 1e-8,
):
    next_state = {}
    next_m = {}
    next_v = {}
    for name, value in state.items():
        grad = grads[name]
        m = beta1 * opt_state["m"][name] + (1 - beta1) * grad
        v = beta2 * opt_state["v"][name] + (1 - beta2) * (grad * grad)
        m_hat = m / (1 - beta1**step)
        v_hat = v / (1 - beta2**step)
        next_state[name] = value - learning_rate * m_hat / (jnp.sqrt(v_hat) + eps)
        next_m[name] = m
        next_v[name] = v
    return next_state, {"m": next_m, "v": next_v}
