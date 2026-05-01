import math
from pathlib import Path
import unittest

import jax

jax.config.update("jax_enable_x64", True)

import jax.numpy as jnp
import numpy as np

from movingknots.basis import (
    design_matrix,
    thinplate_additive_basis,
    thinplate_surface_basis,
)


FIXTURES = Path(__file__).parent / "fixtures"


class BasisTests(unittest.TestCase):
    def test_thinplate_surface_basis_hand_computed(self):
        x = jnp.array([[0.0, 0.0], [1.0, 0.0]])
        knots = jnp.array([[0.0, 0.0], [1.0, 1.0]])

        actual = thinplate_surface_basis(x, knots)
        expected = np.array([[0.0, math.log(2.0)], [0.0, 0.0]])

        np.testing.assert_allclose(np.asarray(actual), expected, rtol=1e-8, atol=1e-8)

    def test_thinplate_additive_basis_groups_knots_by_covariate(self):
        x = jnp.array([[0.0, 0.0], [2.0, 3.0]])
        knots = jnp.array([0.0, 1.0, 2.0])

        actual = thinplate_additive_basis(x, knots, additive_counts=(2, 1))
        expected = np.array(
            [
                [0.0, 0.0, 4.0 * math.log(2.0)],
                [4.0 * math.log(2.0), 0.0, 0.0],
            ]
        )

        np.testing.assert_allclose(np.asarray(actual), expected, rtol=1e-8, atol=1e-8)

    def test_design_matrix_uses_configured_component_order(self):
        x = jnp.array([[0.0, 0.0], [1.0, 0.0]])
        knots = {
            "thinplate.s": jnp.array([[0.0, 0.0], [1.0, 1.0]]),
            "thinplate.a": jnp.array([0.0, 1.0]),
        }
        spline_config = {
            "comp": ("intercept", "covariates", "thinplate.s", "thinplate.a"),
            "thinplate.a.locate": (1, 1),
        }

        actual = design_matrix(x, knots, spline_config)
        expected = np.array(
            [
                [1.0, 0.0, 0.0, 0.0, math.log(2.0), 0.0, 0.0],
                [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0],
            ]
        )

        np.testing.assert_allclose(np.asarray(actual), expected, rtol=1e-8, atol=1e-8)

    def test_design_matrix_matches_r_dmatrix_fixture(self):
        x = jnp.array([[0.0, 0.0], [1.0, 0.0], [0.5, 1.5]])
        knots = {
            "thinplate.s": jnp.array([[0.0, 0.0], [1.0, 1.0]]),
            "thinplate.a": jnp.array([0.0, 0.5, 1.0]),
        }
        spline_config = {
            "comp": ("intercept", "covariates", "thinplate.s", "thinplate.a"),
            "thinplate.a.locate": (2, 1),
        }

        actual = design_matrix(x, knots, spline_config)
        expected = np.loadtxt(FIXTURES / "r_dmatrix_tiny.csv", delimiter=",")

        np.testing.assert_allclose(np.asarray(actual), expected, rtol=1e-12, atol=1e-12)

    def test_surface_basis_is_differentiable_with_respect_to_knots(self):
        x = jnp.array([[0.25, 0.5], [0.75, 0.25]])
        knots = jnp.array([[0.1, 0.2], [0.8, 0.9]])

        grad = jax.grad(lambda theta: thinplate_surface_basis(x, theta).sum())(knots)

        self.assertEqual(grad.shape, knots.shape)
        self.assertTrue(bool(jnp.all(jnp.isfinite(grad))))

    def test_surface_basis_has_finite_gradient_at_zero_distance(self):
        x = jnp.array([[0.0, 0.0], [1.0, 0.0]])
        knots = jnp.array([[0.0, 0.0], [1.0, 1.0]])

        grad = jax.grad(lambda theta: thinplate_surface_basis(x, theta).sum())(knots)

        self.assertEqual(grad.shape, knots.shape)
        self.assertTrue(bool(jnp.all(jnp.isfinite(grad))))


if __name__ == "__main__":
    unittest.main()
