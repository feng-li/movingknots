import unittest

import jax

jax.config.update("jax_enable_x64", True)

import jax.numpy as jnp
import numpy as np

from movingknots.model import gaussian_log_likelihood
from movingknots.priors import normal_logpdf
from movingknots.variational import (
    elbo,
    fit_mean_field,
    init_mean_field,
    mean_field_logpdf,
    sample_mean_field,
)


class VariationalTests(unittest.TestCase):
    def test_init_sample_and_logpdf_shapes(self):
        key = jax.random.PRNGKey(0)
        state = init_mean_field(jnp.array([1.0, 2.0, 3.0]), init_scale=0.2)

        sample = sample_mean_field(state["mu"], state["rho"], key)
        samples = sample_mean_field(state["mu"], state["rho"], key, n_samples=5)
        logq = mean_field_logpdf(sample, state["mu"], state["rho"])

        self.assertEqual(state["mu"].shape, (3,))
        self.assertEqual(state["rho"].shape, (3,))
        self.assertEqual(sample.shape, (3,))
        self.assertEqual(samples.shape, (5, 3))
        self.assertTrue(bool(jnp.isfinite(logq)))

    def test_elbo_returns_finite_value(self):
        key = jax.random.PRNGKey(1)
        state = init_mean_field(jnp.array([0.0]), init_scale=0.1)
        log_prob_fn = lambda z: normal_logpdf(z, mean=1.0, variance=2.0)

        actual = elbo(state["mu"], state["rho"], log_prob_fn, key, n_samples=4)

        self.assertTrue(bool(jnp.isfinite(actual)))

    def test_fixed_knot_linear_vi_moves_beta_toward_truth(self):
        key = jax.random.PRNGKey(2)
        x = jnp.linspace(-1.0, 1.0, 40)[:, None]
        y = 1.0 + 2.0 * x
        spline_config = {"comp": ("intercept", "covariates")}

        def log_prob(z):
            beta = z[:2, None]
            log_sigma = z[2]
            sigma = jnp.exp(log_sigma).reshape((1, 1))
            return (
                gaussian_log_likelihood(
                    y=y,
                    x=x,
                    knots={},
                    spline_config=spline_config,
                    beta=beta,
                    sigma=sigma,
                )
                + normal_logpdf(beta, mean=0.0, variance=25.0)
                + normal_logpdf(log_sigma, mean=jnp.log(0.05), variance=1.0)
            )

        initial = jnp.array([0.0, 0.0, jnp.log(0.5)])
        fit = fit_mean_field(
            log_prob,
            initial,
            key,
            n_steps=350,
            learning_rate=0.03,
            n_samples=4,
            init_scale=0.02,
        )

        beta_mean = np.asarray(fit["mu"][:2])

        self.assertLess(np.linalg.norm(beta_mean - np.array([1.0, 2.0])), 0.35)
        self.assertGreater(float(fit["elbo"][-1]), float(fit["elbo"][0]))


if __name__ == "__main__":
    unittest.main()
