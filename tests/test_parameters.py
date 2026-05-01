import unittest

import jax

jax.config.update("jax_enable_x64", True)

import jax.numpy as jnp

from movingknots.parameters import (
    build_gaussian_parameter_schema,
    build_marginal_gaussian_parameter_schema,
    pack_gaussian_parameters,
    pack_marginal_gaussian_parameters,
    unconstrained_cholesky_from_sigma,
    unpack_gaussian_parameters,
    unpack_marginal_gaussian_parameters,
)


class ParameterSchemaTests(unittest.TestCase):
    def test_full_schema_packs_and_unpacks_all_parameter_blocks(self):
        x = jnp.array([[0.0, 0.0], [1.0, 0.5], [-0.5, 1.0]])
        y = jnp.ones((3, 2))
        knots = {
            "thinplate.a": jnp.array([[-0.5], [0.5], [0.25], [0.75]]),
            "thinplate.s": jnp.array([[0.0, 0.0], [0.5, 0.5]]),
        }
        spline_config = {
            "comp": ("intercept", "covariates", "thinplate.a", "thinplate.s"),
            "thinplate.a.locate": (2, 2),
            "thinplate.s.dim": (2, 2),
        }

        schema = build_gaussian_parameter_schema(
            x,
            y,
            knots,
            spline_config,
            free_additive=True,
            free_surface=True,
        )
        beta = jnp.arange(schema.n_coef * schema.n_response, dtype=float).reshape(
            schema.n_coef,
            schema.n_response,
        )
        raw_cholesky = unconstrained_cholesky_from_sigma(
            jnp.array([[1.0, 0.2], [0.2, 0.8]])
        )
        log_shrinkage = jnp.zeros(schema.log_shrinkage_shape)

        z = pack_gaussian_parameters(
            schema,
            beta=beta,
            raw_cholesky=raw_cholesky,
            log_shrinkage=log_shrinkage,
            additive_knots=knots["thinplate.a"],
            surface_knots=knots["thinplate.s"],
        )
        params = unpack_gaussian_parameters(schema, z, knots)

        self.assertEqual(z.shape, (schema.size,))
        self.assertEqual(schema.shrinkage_block_names, ("linear", "additive", "surface"))
        self.assertEqual(params["beta"].shape, (schema.n_coef, 2))
        self.assertEqual(params["sigma"].shape, (2, 2))
        self.assertEqual(params["log_shrinkage"].shape, (3, 2))
        self.assertEqual(params["knots"]["thinplate.a"].shape, (4, 1))
        self.assertEqual(params["knots"]["thinplate.s"].shape, (2, 2))

    def test_marginal_schema_omits_beta_but_keeps_block_layout(self):
        x = jnp.array([[0.0, 0.0], [1.0, 0.5], [-0.5, 1.0]])
        y = jnp.ones((3, 2))
        knots = {
            "thinplate.a": jnp.array([[-0.5], [0.5]]),
            "thinplate.s": jnp.array([[0.0, 0.0]]),
        }
        spline_config = {
            "comp": ("intercept", "covariates", "thinplate.a", "thinplate.s"),
            "thinplate.a.locate": (1, 1),
            "thinplate.s.dim": (1, 2),
        }

        schema = build_marginal_gaussian_parameter_schema(
            x,
            y,
            knots,
            spline_config,
            free_additive=True,
            free_surface=True,
        )
        raw_cholesky = unconstrained_cholesky_from_sigma(
            jnp.array([[1.0, 0.2], [0.2, 0.8]])
        )
        log_shrinkage = jnp.zeros(schema.log_shrinkage_shape)
        z = pack_marginal_gaussian_parameters(
            schema,
            raw_cholesky=raw_cholesky,
            log_shrinkage=log_shrinkage,
            additive_knots=knots["thinplate.a"],
            surface_knots=knots["thinplate.s"],
        )
        params = unpack_marginal_gaussian_parameters(schema, z, knots)

        self.assertEqual(z.shape, (schema.size,))
        self.assertLess(schema.size, 3 + 3 * 2 + schema.n_coef * schema.n_response)
        self.assertEqual(schema.shrinkage_block_names, ("linear", "additive", "surface"))
        self.assertEqual(params["sigma"].shape, (2, 2))
        self.assertEqual(params["log_shrinkage"].shape, (3, 2))
        self.assertEqual(params["knots"]["thinplate.a"].shape, (2, 1))
        self.assertEqual(params["knots"]["thinplate.s"].shape, (1, 2))


if __name__ == "__main__":
    unittest.main()
