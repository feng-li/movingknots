import csv
from pathlib import Path
import unittest

import jax

jax.config.update("jax_enable_x64", True)

import jax.numpy as jnp
import numpy as np

from movingknots.basis import design_matrix
from movingknots.fit import (
    full_gaussian_log_prob,
    full_gaussian_log_prob_components,
    marginal_fit_beta_posterior,
    marginal_gaussian_log_prob,
    marginal_gaussian_log_prob_components,
)
from movingknots.model import (
    coefficient_prior_covariance,
    make_p_matrices,
    marginal_gaussian_log_likelihood,
)
from movingknots.parameters import (
    build_gaussian_parameter_schema,
    build_marginal_gaussian_parameter_schema,
    pack_gaussian_parameters,
    pack_marginal_gaussian_parameters,
    unconstrained_cholesky_from_sigma,
)


FIXTURES = Path(__file__).parent / "fixtures"


class RFixtureTests(unittest.TestCase):
    def test_full_gaussian_log_prob_matches_r_fixture(self):
        expected = _read_component_fixture(FIXTURES / "r_full_logprob_tiny.csv")
        x, y, knots, spline_config, beta, sigma, log_shrinkage = _full_fixture_inputs()
        schema = build_gaussian_parameter_schema(
            x,
            y,
            knots,
            spline_config,
            free_additive=True,
            free_surface=True,
        )
        z = pack_gaussian_parameters(
            schema,
            beta=beta,
            raw_cholesky=unconstrained_cholesky_from_sigma(sigma),
            log_shrinkage=log_shrinkage,
            additive_knots=knots["thinplate.a"],
            surface_knots=knots["thinplate.s"],
        )

        components = full_gaussian_log_prob_components(
            z=z,
            x=x,
            y=y,
            base_knots=knots,
            spline_config=spline_config,
            schema=schema,
            beta_prior_variance=1.7,
            log_shrinkage_prior_mean=0.1,
            log_shrinkage_prior_variance=1.3,
            sigma_prior_df=6.5,
            sigma_prior_scale=jnp.array([[1.1, 0.15], [0.15, 0.95]]),
            additive_knot_prior_mean=jnp.array([[-0.5], [0.25], [-0.15]]),
            surface_knot_prior_mean=jnp.array([[-0.3, 0.0], [0.7, -0.6]]),
            knot_prior_variance=2.2,
        )

        for name, value in components.items():
            self.assertAlmostEqual(float(value), expected[name], places=8, msg=name)

        actual_total = full_gaussian_log_prob(
            z=z,
            x=x,
            y=y,
            base_knots=knots,
            spline_config=spline_config,
            schema=schema,
            beta_prior_variance=1.7,
            log_shrinkage_prior_mean=0.1,
            log_shrinkage_prior_variance=1.3,
            sigma_prior_df=6.5,
            sigma_prior_scale=jnp.array([[1.1, 0.15], [0.15, 0.95]]),
            additive_knot_prior_mean=jnp.array([[-0.5], [0.25], [-0.15]]),
            surface_knot_prior_mean=jnp.array([[-0.3, 0.0], [0.7, -0.6]]),
            knot_prior_variance=2.2,
        )
        self.assertAlmostEqual(float(actual_total), expected["full_log_prob"], places=8)

    def test_p_matrix_prior_and_marginal_likelihood_match_r_fixture(self):
        expected = _read_matrix_fixture(FIXTURES / "r_p_matrix_prior_tiny.csv")
        x, y, knots, spline_config, beta, sigma, log_shrinkage = _full_fixture_inputs()
        del beta
        schema = build_marginal_gaussian_parameter_schema(
            x,
            y,
            knots,
            spline_config,
            free_additive=True,
            free_surface=True,
        )
        p_matrix_types = ("X'X", "identity", "identity")
        x_design = design_matrix(x, knots, spline_config)

        p_matrices = make_p_matrices(x_design, schema, p_matrix_types)
        for index, p_matrix in enumerate(p_matrices, start=1):
            np.testing.assert_allclose(
                np.asarray(p_matrix),
                expected[f"p_matrix_{index}"],
                rtol=1e-10,
                atol=1e-10,
            )

        beta_covariance = coefficient_prior_covariance(
            schema=schema,
            sigma=sigma,
            log_shrinkage=log_shrinkage,
            beta_prior_variance=1.7,
            p_matrices=p_matrices,
        )
        np.testing.assert_allclose(
            np.asarray(beta_covariance),
            expected["coefficient_prior_covariance"],
            rtol=1e-10,
            atol=1e-10,
        )

        actual_marginal = marginal_gaussian_log_likelihood(
            y=y,
            x=x,
            knots=knots,
            spline_config=spline_config,
            sigma=sigma,
            log_shrinkage=log_shrinkage,
            schema=schema,
            beta_prior_variance=1.7,
            p_matrix_types=p_matrix_types,
        )
        self.assertAlmostEqual(
            float(actual_marginal),
            expected["marginal_log_likelihood"][0, 0],
            places=8,
        )

    def test_marginal_beta_posterior_matches_r_fixture(self):
        expected = _read_matrix_fixture(FIXTURES / "r_marginal_beta_posterior_tiny.csv")
        x, y, knots, spline_config, beta, sigma, log_shrinkage = _full_fixture_inputs()
        del beta
        schema = build_marginal_gaussian_parameter_schema(
            x,
            y,
            knots,
            spline_config,
            free_additive=True,
            free_surface=True,
        )
        p_matrix_types = ("X'X", "identity", "identity")
        z = pack_marginal_gaussian_parameters(
            schema,
            raw_cholesky=unconstrained_cholesky_from_sigma(sigma),
            log_shrinkage=log_shrinkage,
            additive_knots=knots["thinplate.a"],
            surface_knots=knots["thinplate.s"],
        )
        fit = {
            "kind": "marginal",
            "schema": schema,
            "vi": {"mu": z, "rho": jnp.full_like(z, -30.0)},
            "knots": knots,
            "initial_knots": knots,
            "spline_config": spline_config,
            "beta_prior_variance": 1.7,
            "p_matrix_types": p_matrix_types,
            "x_train": x,
            "y_train": y,
        }

        x_design = design_matrix(x, knots, spline_config)
        p_matrices = make_p_matrices(x_design, schema, p_matrix_types)
        beta_covariance = coefficient_prior_covariance(
            schema=schema,
            sigma=sigma,
            log_shrinkage=log_shrinkage,
            beta_prior_variance=1.7,
            p_matrices=p_matrices,
        )
        posterior = marginal_fit_beta_posterior(fit)
        marginal_log_likelihood = marginal_gaussian_log_likelihood(
            y=y,
            x=x,
            knots=knots,
            spline_config=spline_config,
            sigma=sigma,
            log_shrinkage=log_shrinkage,
            schema=schema,
            beta_prior_variance=1.7,
            p_matrix_types=p_matrix_types,
        )

        np.testing.assert_allclose(
            np.asarray(beta_covariance),
            expected["coefficient_prior_covariance"],
            rtol=1e-10,
            atol=1e-10,
        )
        np.testing.assert_allclose(
            np.asarray(posterior["mean"]),
            expected["posterior_beta_mean"],
            rtol=1e-10,
            atol=1e-10,
        )
        np.testing.assert_allclose(
            np.asarray(posterior["covariance"]),
            expected["posterior_beta_covariance"],
            rtol=1e-10,
            atol=1e-10,
        )
        np.testing.assert_allclose(
            np.asarray(posterior["covariance"]),
            np.asarray(posterior["covariance"].T),
            rtol=0.0,
            atol=1e-12,
        )
        self.assertGreater(
            float(jnp.min(jnp.linalg.eigvalsh(posterior["covariance"]))),
            0.0,
        )
        self.assertAlmostEqual(
            float(marginal_log_likelihood),
            expected["marginal_log_likelihood"][0, 0],
            places=8,
        )

    def test_marginal_gaussian_log_prob_matches_r_fixture(self):
        expected = _read_component_fixture(FIXTURES / "r_marginal_logprob_tiny.csv")
        x, y, knots, spline_config, beta, sigma, log_shrinkage = _full_fixture_inputs()
        del beta
        schema = build_marginal_gaussian_parameter_schema(
            x,
            y,
            knots,
            spline_config,
            free_additive=True,
            free_surface=True,
        )
        z = pack_marginal_gaussian_parameters(
            schema,
            raw_cholesky=unconstrained_cholesky_from_sigma(sigma),
            log_shrinkage=log_shrinkage,
            additive_knots=knots["thinplate.a"],
            surface_knots=knots["thinplate.s"],
        )
        p_matrix_types = ("X'X", "identity", "identity")

        components = marginal_gaussian_log_prob_components(
            z=z,
            x=x,
            y=y,
            base_knots=knots,
            spline_config=spline_config,
            schema=schema,
            beta_prior_variance=1.7,
            log_shrinkage_prior_mean=0.1,
            log_shrinkage_prior_variance=1.3,
            sigma_prior_df=6.5,
            sigma_prior_scale=jnp.array([[1.1, 0.15], [0.15, 0.95]]),
            additive_knot_prior_mean=jnp.array([[-0.5], [0.25], [-0.15]]),
            surface_knot_prior_mean=jnp.array([[-0.3, 0.0], [0.7, -0.6]]),
            knot_prior_variance=2.2,
            p_matrix_types=p_matrix_types,
        )

        for name, value in components.items():
            self.assertAlmostEqual(float(value), expected[name], places=8, msg=name)

        actual_total = marginal_gaussian_log_prob(
            z=z,
            x=x,
            y=y,
            base_knots=knots,
            spline_config=spline_config,
            schema=schema,
            beta_prior_variance=1.7,
            log_shrinkage_prior_mean=0.1,
            log_shrinkage_prior_variance=1.3,
            sigma_prior_df=6.5,
            sigma_prior_scale=jnp.array([[1.1, 0.15], [0.15, 0.95]]),
            additive_knot_prior_mean=jnp.array([[-0.5], [0.25], [-0.15]]),
            surface_knot_prior_mean=jnp.array([[-0.3, 0.0], [0.7, -0.6]]),
            knot_prior_variance=2.2,
            p_matrix_types=p_matrix_types,
        )
        self.assertAlmostEqual(
            float(actual_total),
            expected["marginal_log_prob"],
            places=8,
        )


def _full_fixture_inputs():
    x = jnp.array(
        [
            [-0.8, -0.3],
            [0.2, 0.4],
            [0.7, -0.6],
            [1.1, 0.9],
            [-1.2, 0.8],
        ]
    )
    knots = {
        "thinplate.a": jnp.array([[-0.6], [0.3], [-0.2]]),
        "thinplate.s": jnp.array([[-0.4, 0.1], [0.8, -0.7]]),
    }
    spline_config = {
        "comp": ("intercept", "covariates", "thinplate.a", "thinplate.s"),
        "thinplate.a.locate": (2, 1),
        "thinplate.s.dim": (2, 2),
    }
    beta = jnp.array(
        [
            [0.20, -0.10],
            [1.10, 0.40],
            [-0.50, 0.70],
            [0.30, -0.20],
            [-0.25, 0.15],
            [0.45, -0.35],
            [-0.10, 0.25],
            [0.05, 0.30],
        ]
    )
    residual = jnp.array(
        [
            [0.10, -0.08],
            [-0.05, 0.02],
            [0.03, 0.06],
            [-0.07, 0.04],
            [0.02, -0.03],
        ]
    )
    y = design_matrix(x, knots, spline_config) @ beta + residual
    sigma = jnp.array([[1.3, 0.25], [0.25, 0.9]])
    log_shrinkage = jnp.log(jnp.array([[1.2, 0.7], [0.8, 1.5], [1.6, 0.9]]))
    return x, y, knots, spline_config, beta, sigma, log_shrinkage


def _read_component_fixture(path):
    with path.open(newline="") as fh:
        return {row["component"]: float(row["value"]) for row in csv.DictReader(fh)}


def _read_matrix_fixture(path):
    rows_by_kind = {}
    with path.open(newline="") as fh:
        for row in csv.DictReader(fh):
            kind = row["kind"]
            rows_by_kind.setdefault(kind, []).append(
                (int(row["row"]), int(row["col"]), float(row["value"]))
            )

    out = {}
    for kind, rows in rows_by_kind.items():
        n_row = max(row for row, _, _ in rows) + 1
        n_col = max(col for _, col, _ in rows) + 1
        if kind != "marginal_log_likelihood":
            n_row -= 1
            n_col -= 1
        matrix = np.zeros((n_row, n_col))
        for row, col, value in rows:
            if kind == "marginal_log_likelihood":
                matrix[row, col] = value
            else:
                matrix[row - 1, col - 1] = value
        out[kind] = matrix
    return out


if __name__ == "__main__":
    unittest.main()
