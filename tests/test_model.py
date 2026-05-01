import unittest

import jax

jax.config.update("jax_enable_x64", True)

import jax.numpy as jnp
import numpy as np

from movingknots.model import (
    conditional_gaussian_beta_posterior,
    gaussian_log_likelihood,
    log_joint,
    marginal_gaussian_log_likelihood,
)
from movingknots.parameters import build_marginal_gaussian_parameter_schema
from movingknots.priors import log_prior


def numpy_gaussian_log_likelihood(y, mean, sigma):
    y = np.asarray(y, dtype=float)
    mean = np.asarray(mean, dtype=float)
    sigma = np.asarray(sigma, dtype=float)
    residual = y - mean
    n_obs, n_response = y.shape
    _, logdet = np.linalg.slogdet(sigma)
    solved = np.linalg.solve(sigma, residual.T)
    quadratic = np.sum(residual.T * solved)
    return (
        -0.5 * n_obs * n_response * np.log(2 * np.pi)
        -0.5 * n_obs * logdet
        -0.5 * quadratic
    )


class ModelTests(unittest.TestCase):
    def test_gaussian_log_likelihood_univariate_response(self):
        x = jnp.array([[0.0], [2.0]])
        y = jnp.array([[1.0], [5.0]])
        beta = jnp.array([[1.0], [2.0]])
        sigma = jnp.array([[4.0]])
        spline_config = {"comp": ("intercept", "covariates")}

        actual = gaussian_log_likelihood(y, x, {}, spline_config, beta, sigma)
        expected = -np.log(2 * np.pi) - np.log(4.0)

        self.assertAlmostEqual(float(actual), expected)

    def test_gaussian_log_likelihood_multivariate_response(self):
        x = jnp.array([[0.0], [1.0], [2.0]])
        y = jnp.array([[1.2, -0.7], [0.7, -1.1], [1.8, -0.2]])
        beta = jnp.array([[1.0, -1.0]])
        sigma = jnp.array([[2.0, 0.3], [0.3, 1.0]])
        spline_config = {"comp": ("intercept",)}

        actual = gaussian_log_likelihood(y, x, {}, spline_config, beta, sigma)
        expected = numpy_gaussian_log_likelihood(
            y=np.asarray(y),
            mean=np.tile(np.asarray(beta), (3, 1)),
            sigma=np.asarray(sigma),
        )

        self.assertAlmostEqual(float(actual), expected)

    def test_gaussian_log_likelihood_matches_numpy_hand_computation(self):
        x = jnp.array([[0.0], [1.0], [2.0]])
        y = jnp.array([[1.0], [1.5], [3.5]])
        beta = jnp.array([[0.5], [1.0]])
        sigma = jnp.array([[0.25]])
        spline_config = {"comp": ("intercept", "covariates")}

        mean = np.array([[0.5], [1.5], [2.5]])
        expected = numpy_gaussian_log_likelihood(np.asarray(y), mean, np.asarray(sigma))
        actual = gaussian_log_likelihood(y, x, {}, spline_config, beta, sigma)

        self.assertAlmostEqual(float(actual), expected)

    def test_gaussian_log_likelihood_has_beta_gradient(self):
        x = jnp.array([[0.0], [1.0], [2.0]])
        y = jnp.array([[1.0], [2.0], [2.5]])
        beta = jnp.array([[0.5], [1.0]])
        sigma = jnp.array([[1.0]])
        spline_config = {"comp": ("intercept", "covariates")}

        grad = jax.grad(
            lambda beta_value: gaussian_log_likelihood(
                y, x, {}, spline_config, beta_value, sigma
            )
        )(beta)

        self.assertEqual(grad.shape, beta.shape)
        self.assertTrue(bool(jnp.all(jnp.isfinite(grad))))

    def test_log_joint_is_likelihood_plus_prior(self):
        x = jnp.array([[0.0], [1.0]])
        y = jnp.array([[1.0], [2.0]])
        params = {
            "beta": jnp.array([[1.0], [1.0]]),
            "sigma": jnp.array([[1.5]]),
        }
        spline_config = {"comp": ("intercept", "covariates")}
        prior_config = {
            "beta": {"mean": 0.0, "variance": 10.0},
            "sigma": {"df": 4.0, "scale": jnp.array([[1.0]])},
        }

        actual = log_joint(y, x, params, spline_config, prior_config)
        expected = gaussian_log_likelihood(
            y,
            x,
            {},
            spline_config,
            params["beta"],
            params["sigma"],
        ) + log_prior(params, prior_config)

        self.assertAlmostEqual(float(actual), float(expected))

    def test_log_joint_has_beta_gradient(self):
        x = jnp.array([[0.0], [1.0], [2.0]])
        y = jnp.array([[1.0], [2.0], [2.5]])
        params = {
            "beta": jnp.array([[0.5], [1.0]]),
            "sigma": jnp.array([[1.0]]),
        }
        spline_config = {"comp": ("intercept", "covariates")}
        prior_config = {
            "beta": {"mean": 0.0, "variance": 10.0},
            "sigma": {"df": 4.0, "scale": jnp.array([[1.0]])},
        }

        grad = jax.grad(
            lambda beta_value: log_joint(
                y,
                x,
                {**params, "beta": beta_value},
                spline_config,
                prior_config,
            )
        )(params["beta"])

        self.assertEqual(grad.shape, params["beta"].shape)
        self.assertTrue(bool(jnp.all(jnp.isfinite(grad))))

    def test_marginal_gaussian_log_likelihood_matches_dense_numpy(self):
        x = jnp.array([[0.0], [1.0], [2.0]])
        y = jnp.array([[1.0], [1.5], [3.0]])
        knots = {}
        spline_config = {"comp": ("intercept",)}
        sigma = jnp.array([[0.7]])
        log_shrinkage = jnp.log(jnp.array([[1.4]]))
        beta_prior_variance = 2.5
        schema = build_marginal_gaussian_parameter_schema(x, y, knots, spline_config)

        actual = marginal_gaussian_log_likelihood(
            y=y,
            x=x,
            knots=knots,
            spline_config=spline_config,
            sigma=sigma,
            log_shrinkage=log_shrinkage,
            schema=schema,
            beta_prior_variance=beta_prior_variance,
        )

        y_np = np.asarray(y).reshape(-1)
        design = np.ones((3, 1))
        prior_var = beta_prior_variance * float(jnp.exp(log_shrinkage[0, 0])) * 0.7
        cov = 0.7 * np.eye(3) + prior_var * design @ design.T
        _, logdet = np.linalg.slogdet(cov)
        expected = (
            -0.5 * y_np.size * np.log(2 * np.pi)
            -0.5 * logdet
            -0.5 * y_np @ np.linalg.solve(cov, y_np)
        )

        self.assertAlmostEqual(float(actual), expected)

    def test_conditional_beta_posterior_matches_univariate_formula(self):
        x = jnp.array([[0.0], [1.0], [2.0]])
        y = jnp.array([[1.0], [1.5], [3.0]])
        knots = {}
        spline_config = {"comp": ("intercept",)}
        sigma = jnp.array([[0.7]])
        log_shrinkage = jnp.log(jnp.array([[1.4]]))
        beta_prior_variance = 2.5
        schema = build_marginal_gaussian_parameter_schema(x, y, knots, spline_config)

        posterior = conditional_gaussian_beta_posterior(
            y=y,
            x=x,
            knots=knots,
            spline_config=spline_config,
            sigma=sigma,
            log_shrinkage=log_shrinkage,
            schema=schema,
            beta_prior_variance=beta_prior_variance,
        )

        prior_var = beta_prior_variance * float(jnp.exp(log_shrinkage[0, 0])) * 0.7
        expected_var = 1.0 / (3.0 / 0.7 + 1.0 / prior_var)
        expected_mean = expected_var * float(jnp.sum(y)) / 0.7

        self.assertAlmostEqual(float(posterior["mean"][0, 0]), expected_mean)
        self.assertAlmostEqual(float(posterior["covariance"][0, 0]), expected_var)


if __name__ == "__main__":
    unittest.main()
