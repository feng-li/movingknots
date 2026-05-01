import math
import unittest

import jax

jax.config.update("jax_enable_x64", True)

import jax.numpy as jnp
import numpy as np

from movingknots.priors import inverse_wishart_logpdf, log_prior, normal_logpdf


class PriorTests(unittest.TestCase):
    def test_normal_logpdf_matches_hand_computation(self):
        value = jnp.array([0.0, 2.0])

        actual = normal_logpdf(value, mean=1.0, variance=4.0)
        expected = 2 * (-0.5 * math.log(2 * math.pi * 4.0)) - 0.25

        self.assertAlmostEqual(float(actual), expected)

    def test_inverse_wishart_univariate_matches_formula(self):
        x = jnp.array([[2.0]])
        df = 5.0
        scale = jnp.array([[3.0]])

        actual = inverse_wishart_logpdf(x, df=df, scale=scale)
        expected = (
            -df / 2 * math.log(2)
            - math.lgamma(df / 2)
            + df / 2 * math.log(3)
            - (df + 2) / 2 * math.log(2)
            - 3 / (2 * 2)
        )

        self.assertAlmostEqual(float(actual), expected)

    def test_log_prior_sums_configured_components(self):
        params = {
            "beta": jnp.array([[0.0], [1.0]]),
            "sigma": jnp.array([[2.0]]),
            "log_shrinkage": jnp.array([[0.25]]),
            "knots": {"thinplate.a": jnp.array([[0.5], [1.5]])},
        }
        prior_config = {
            "beta": {"mean": 0.0, "variance": 1.0},
            "sigma": {"df": 4.0, "scale": jnp.array([[1.0]])},
            "log_shrinkage": {"mean": 0.0, "variance": 2.0},
            "knots": {"thinplate.a": {"mean": 1.0, "variance": 4.0}},
        }

        actual = log_prior(params, prior_config)
        expected = (
            normal_logpdf(params["beta"], 0.0, 1.0)
            + inverse_wishart_logpdf(params["sigma"], 4.0, jnp.array([[1.0]]))
            + normal_logpdf(params["log_shrinkage"], 0.0, 2.0)
            + normal_logpdf(params["knots"]["thinplate.a"], 1.0, 4.0)
        )

        self.assertAlmostEqual(float(actual), float(expected))

    def test_beta_prior_can_use_shrinkage_by_response(self):
        params = {
            "beta": jnp.array([[0.0, 1.0], [2.0, 3.0]]),
            "log_shrinkage": jnp.log(jnp.array([1.0, 4.0])),
        }
        prior_config = {
            "beta": {"mean": 0.0, "variance": 1.0, "use_shrinkage": True}
        }

        actual = log_prior(params, prior_config)
        expected = normal_logpdf(
            params["beta"],
            mean=0.0,
            variance=jnp.array([[1.0, 4.0], [1.0, 4.0]]),
        )

        self.assertAlmostEqual(float(actual), float(expected))


if __name__ == "__main__":
    unittest.main()
