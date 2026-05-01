import unittest

import jax

jax.config.update("jax_enable_x64", True)

import jax.numpy as jnp
import numpy as np

from movingknots.basis import design_matrix
from movingknots.fit import (
    cross_validate_gaussian_vi,
    evaluate_gaussian_fit,
    fit_fixed_knots_gaussian_vi,
    fit_full_gaussian_vi,
    fit_gaussian_vi,
    fit_free_additive_knots_gaussian_vi,
    fit_free_surface_knots_gaussian_vi,
    fit_marginal_gaussian_vi,
    gaussian_log_predictive_samples,
    gaussian_lpds,
    marginal_fit_beta_posterior,
    marginal_fit_log_predictive_density,
    marginal_fit_predictive_moments,
    predict_mean,
    predict_samples,
    predict_summary,
    summarize_log_predictive_samples,
    summarize_fit,
)
from movingknots.model import gaussian_log_likelihood
from movingknots.parameters import (
    build_marginal_gaussian_parameter_schema,
    pack_marginal_gaussian_parameters,
    unconstrained_cholesky_from_sigma,
)


def _make_marginal_fit(
    x,
    y,
    *,
    knots=None,
    spline_config=None,
    sigma=None,
    log_shrinkage=None,
    beta_prior_variance=25.0,
    p_matrix_types=None,
):
    x = jnp.asarray(x)
    y = jnp.asarray(y)
    if y.ndim == 1:
        y = y[:, None]
    knots = {} if knots is None else knots
    spline_config = (
        {"comp": ("intercept",)}
        if spline_config is None
        else spline_config
    )
    schema = build_marginal_gaussian_parameter_schema(
        x=x,
        y=y,
        knots=knots,
        spline_config=spline_config,
        free_additive=False,
        free_surface=False,
    )
    if sigma is None:
        sigma = jnp.eye(y.shape[1])
    sigma = jnp.asarray(sigma)
    if sigma.ndim == 0:
        sigma = sigma.reshape((1, 1))
    if log_shrinkage is None:
        log_shrinkage = jnp.zeros(schema.log_shrinkage_shape)
    else:
        log_shrinkage = jnp.asarray(log_shrinkage).reshape(schema.log_shrinkage_shape)
    z = pack_marginal_gaussian_parameters(
        schema=schema,
        raw_cholesky=unconstrained_cholesky_from_sigma(sigma),
        log_shrinkage=log_shrinkage,
    )
    return {
        "kind": "marginal",
        "schema": schema,
        "n_coef": schema.n_coef,
        "n_response": schema.n_response,
        "sigma_mean": sigma,
        "sigma_cholesky_mean": jnp.linalg.cholesky(sigma),
        "log_shrinkage_mean": log_shrinkage,
        "shrinkage_mean": jnp.exp(log_shrinkage),
        "vi": {
            "mu": z,
            "rho": jnp.full_like(z, -30.0),
            "elbo": jnp.asarray([0.0]),
        },
        "knots": knots,
        "initial_knots": knots,
        "spline_config": spline_config,
        "free_additive": False,
        "free_surface": False,
        "beta_prior_variance": beta_prior_variance,
        "p_matrix_types": p_matrix_types,
        "x_train": x,
        "y_train": y,
    }


def _row_major_sample_covariance(samples):
    sample_matrix = np.asarray(samples).reshape(samples.shape[0], -1)
    return np.cov(sample_matrix, rowvar=False, ddof=1)


def _dense_gaussian_logpdf(value, mean, covariance):
    value = jnp.asarray(value)
    mean = jnp.asarray(mean)
    covariance = jnp.asarray(covariance)
    residual = value - mean
    sign, logdet = jnp.linalg.slogdet(covariance)
    quadratic = residual @ jnp.linalg.solve(covariance, residual)
    log_density = (
        -0.5 * residual.size * jnp.log(2 * jnp.pi)
        -0.5 * logdet
        -0.5 * quadratic
    )
    return jnp.where(sign > 0, log_density, -jnp.inf)


class FitTests(unittest.TestCase):
    def test_fit_gaussian_vi_dispatches_to_fixed_fitter(self):
        x = jnp.linspace(-1.0, 1.0, 12)[:, None]
        y = 1.0 + 2.0 * x

        fit = fit_gaussian_vi(
            x,
            y,
            knots={},
            spline_config={"comp": ("intercept", "covariates")},
            free_knots=False,
            key=jax.random.PRNGKey(11),
            n_steps=5,
            n_samples=1,
        )

        self.assertIn("beta_mean", fit)
        self.assertIn("sigma_mean", fit)
        self.assertNotIn("additive_knots_mean", fit)
        self.assertNotIn("surface_knots_mean", fit)

    def test_fit_gaussian_vi_dispatches_to_free_additive_fitter(self):
        x = jnp.linspace(-1.0, 1.0, 12)[:, None]
        knots = {"thinplate.a": jnp.array([[0.25]])}
        spline_config = {
            "comp": ("intercept", "covariates", "thinplate.a"),
            "thinplate.a.locate": (1,),
        }
        y = design_matrix(x, knots, spline_config) @ jnp.array([[0.0], [1.0], [2.0]])

        fit = fit_gaussian_vi(
            x,
            y,
            knots=knots,
            spline_config=spline_config,
            free_knots="additive",
            key=jax.random.PRNGKey(12),
            n_steps=5,
            n_samples=1,
        )

        self.assertIn("additive_knots_mean", fit)
        self.assertEqual(fit["additive_knots_mean"].shape, (1, 1))

    def test_fit_gaussian_vi_dispatches_to_free_surface_fitter(self):
        grid = jnp.linspace(-1.0, 1.0, 3)
        x1, x2 = jnp.meshgrid(grid, grid, indexing="xy")
        x = jnp.column_stack((x1.reshape(-1), x2.reshape(-1)))
        knots = {"thinplate.s": jnp.array([[0.25, -0.25]])}
        spline_config = {
            "comp": ("intercept", "covariates", "thinplate.s"),
            "thinplate.s.dim": (1, 2),
        }
        y = design_matrix(x, knots, spline_config) @ jnp.array([[0.0], [1.0], [-1.0], [2.0]])

        fit = fit_gaussian_vi(
            x,
            y,
            knots=knots,
            spline_config=spline_config,
            free_knots="surface",
            key=jax.random.PRNGKey(13),
            n_steps=5,
            n_samples=1,
        )

        self.assertIn("surface_knots_mean", fit)
        self.assertEqual(fit["surface_knots_mean"].shape, (1, 2))

    def test_fit_gaussian_vi_rejects_invalid_free_knots_value(self):
        x = jnp.linspace(-1.0, 1.0, 6)[:, None]
        y = 1.0 + x

        with self.assertRaises(ValueError):
            fit_gaussian_vi(
                x,
                y,
                knots={},
                spline_config={"comp": ("intercept", "covariates")},
                free_knots="both",
                n_steps=1,
            )

    def test_predict_samples_fixed_fit_shape_and_noise(self):
        x = jnp.linspace(-1.0, 1.0, 12)[:, None]
        y = 1.0 + 2.0 * x
        fit = fit_gaussian_vi(
            x,
            y,
            knots={},
            spline_config={"comp": ("intercept", "covariates")},
            free_knots=False,
            key=jax.random.PRNGKey(14),
            n_steps=5,
            n_samples=1,
        )
        x_new = jnp.array([[0.0], [1.0]])

        mean_samples = predict_samples(fit, x_new, jax.random.PRNGKey(15), n_samples=3)
        noisy_samples = predict_samples(
            fit,
            x_new,
            jax.random.PRNGKey(15),
            n_samples=3,
            include_noise=True,
        )

        self.assertEqual(mean_samples.shape, (3, 2, 1))
        self.assertEqual(noisy_samples.shape, (3, 2, 1))
        self.assertGreater(float(jnp.max(jnp.abs(mean_samples - noisy_samples))), 0.0)

    def test_predict_samples_free_additive_fit_shape(self):
        x = jnp.linspace(-1.0, 1.0, 12)[:, None]
        knots = {"thinplate.a": jnp.array([[0.25]])}
        spline_config = {
            "comp": ("intercept", "covariates", "thinplate.a"),
            "thinplate.a.locate": (1,),
        }
        y = design_matrix(x, knots, spline_config) @ jnp.array([[0.0], [1.0], [2.0]])
        fit = fit_gaussian_vi(
            x,
            y,
            knots=knots,
            spline_config=spline_config,
            free_knots="additive",
            key=jax.random.PRNGKey(16),
            n_steps=5,
            n_samples=1,
        )

        samples = predict_samples(fit, x[:3], jax.random.PRNGKey(17), n_samples=4)

        self.assertEqual(samples.shape, (4, 3, 1))

    def test_predict_samples_free_surface_fit_shape(self):
        grid = jnp.linspace(-1.0, 1.0, 3)
        x1, x2 = jnp.meshgrid(grid, grid, indexing="xy")
        x = jnp.column_stack((x1.reshape(-1), x2.reshape(-1)))
        knots = {"thinplate.s": jnp.array([[0.25, -0.25]])}
        spline_config = {
            "comp": ("intercept", "covariates", "thinplate.s"),
            "thinplate.s.dim": (1, 2),
        }
        y = design_matrix(x, knots, spline_config) @ jnp.array([[0.0], [1.0], [-1.0], [2.0]])
        fit = fit_gaussian_vi(
            x,
            y,
            knots=knots,
            spline_config=spline_config,
            free_knots="surface",
            key=jax.random.PRNGKey(18),
            n_steps=5,
            n_samples=1,
        )

        samples = predict_samples(fit, x[:4], jax.random.PRNGKey(19), n_samples=2)

        self.assertEqual(samples.shape, (2, 4, 1))

    def test_predict_summary_fixed_fit_shapes_and_quantile_order(self):
        x = jnp.linspace(-1.0, 1.0, 12)[:, None]
        y = 1.0 + 2.0 * x
        fit = fit_gaussian_vi(
            x,
            y,
            knots={},
            spline_config={"comp": ("intercept", "covariates")},
            free_knots=False,
            key=jax.random.PRNGKey(20),
            n_steps=5,
            n_samples=1,
        )
        x_new = jnp.array([[0.0], [1.0]])

        summary = predict_summary(
            fit,
            x_new,
            jax.random.PRNGKey(21),
            n_samples=20,
            probs=(0.05, 0.5, 0.95),
        )

        self.assertEqual(summary["mean"].shape, (2, 1))
        self.assertEqual(summary["sd"].shape, (2, 1))
        self.assertEqual(summary["quantiles"].shape, (3, 2, 1))
        self.assertTrue(bool(jnp.all(summary["quantiles"][0] <= summary["quantiles"][1])))
        self.assertTrue(bool(jnp.all(summary["quantiles"][1] <= summary["quantiles"][2])))

    def test_predict_summary_noise_increases_average_sd(self):
        x = jnp.linspace(-1.0, 1.0, 12)[:, None]
        y = 1.0 + 2.0 * x
        fit = fit_gaussian_vi(
            x,
            y,
            knots={},
            spline_config={"comp": ("intercept", "covariates")},
            free_knots=False,
            key=jax.random.PRNGKey(22),
            n_steps=5,
            n_samples=1,
        )
        x_new = jnp.array([[0.0], [1.0]])

        latent = predict_summary(
            fit,
            x_new,
            jax.random.PRNGKey(23),
            n_samples=80,
            include_noise=False,
        )
        noisy = predict_summary(
            fit,
            x_new,
            jax.random.PRNGKey(23),
            n_samples=80,
            include_noise=True,
        )

        self.assertGreaterEqual(float(jnp.mean(noisy["sd"])), float(jnp.mean(latent["sd"])))

    def test_evaluate_gaussian_fit_returns_finite_metrics(self):
        x = jnp.linspace(-1.0, 1.0, 20)[:, None]
        y = 1.0 + 2.0 * x
        fit = fit_gaussian_vi(
            x,
            y,
            knots={},
            spline_config={"comp": ("intercept", "covariates")},
            free_knots=False,
            key=jax.random.PRNGKey(24),
            n_steps=50,
            n_samples=1,
        )

        metrics = evaluate_gaussian_fit(fit, x, y)

        self.assertEqual(set(metrics), {"mse", "mae", "log_likelihood_at_mean"})
        self.assertLess(metrics["mse"], 0.05)
        self.assertTrue(np.isfinite(metrics["mae"]))
        self.assertTrue(np.isfinite(metrics["log_likelihood_at_mean"]))

    def test_evaluate_gaussian_fit_better_than_worse_predictions(self):
        x = jnp.linspace(-1.0, 1.0, 20)[:, None]
        y = 1.0 + 2.0 * x
        fit = fit_gaussian_vi(
            x,
            y,
            knots={},
            spline_config={"comp": ("intercept", "covariates")},
            free_knots=False,
            key=jax.random.PRNGKey(25),
            n_steps=50,
            n_samples=1,
        )
        worse_fit = {**fit, "beta_mean": fit["beta_mean"] + jnp.array([[3.0], [-3.0]])}

        metrics = evaluate_gaussian_fit(fit, x, y)
        worse_metrics = evaluate_gaussian_fit(worse_fit, x, y)

        self.assertLess(metrics["mse"], worse_metrics["mse"])

    def test_summarize_log_predictive_samples_matches_log_mean_exp(self):
        log_pred = jnp.log(jnp.array([[1.0, 4.0], [3.0, 2.0], [5.0, 8.0]]))

        summary = summarize_log_predictive_samples(log_pred)

        expected_fold_lpds = np.log(np.mean(np.exp(np.asarray(log_pred)), axis=0))
        np.testing.assert_allclose(
            np.asarray(summary["fold_lpds"]),
            expected_fold_lpds,
            rtol=1e-12,
            atol=1e-12,
        )
        self.assertAlmostEqual(summary["lpds"], float(np.mean(expected_fold_lpds)))
        self.assertTrue(np.isfinite(summary["nse_lpds"]))

    def test_gaussian_lpds_uses_posterior_likelihood_samples(self):
        x = jnp.linspace(-1.0, 1.0, 12)[:, None]
        y = 1.0 + 2.0 * x
        fit = fit_gaussian_vi(
            x,
            y,
            knots={},
            spline_config={"comp": ("intercept", "covariates")},
            free_knots=False,
            key=jax.random.PRNGKey(31),
            n_steps=5,
            n_samples=1,
        )

        log_samples = gaussian_log_predictive_samples(
            fit,
            x[:4],
            y[:4],
            key=jax.random.PRNGKey(32),
            n_samples=4,
        )
        score = gaussian_lpds(
            fit,
            x[:4],
            y[:4],
            key=jax.random.PRNGKey(32),
            n_samples=4,
        )

        self.assertEqual(log_samples.shape, (4,))
        self.assertEqual(score["log_pred_samples"].shape, (4,))
        self.assertTrue(np.isfinite(score["lpds"]))
        self.assertTrue(np.isfinite(score["nse_lpds"]))

    def test_gaussian_lpds_for_marginal_fit_uses_exact_density_at_fixed_z(self):
        x = jnp.array([[-1.0], [-0.25], [0.5], [1.25]])
        beta = jnp.array([[0.3], [1.1]])
        y = design_matrix(x, {}, {"comp": ("intercept", "covariates")}) @ beta
        y = y + jnp.array([[0.05], [-0.02], [0.03], [-0.04]])
        fit = _make_marginal_fit(
            x,
            y,
            spline_config={"comp": ("intercept", "covariates")},
            sigma=jnp.array([[0.7]]),
            log_shrinkage=jnp.log(jnp.array([[1.2]])),
            beta_prior_variance=1.8,
        )
        x_new = jnp.array([[-0.5], [0.75]])
        y_new = jnp.array([[0.0], [1.0]])
        exact = marginal_fit_log_predictive_density(fit, x_new, y_new)

        score = gaussian_lpds(
            fit,
            x_new,
            y_new,
            key=jax.random.PRNGKey(38),
            n_samples=5,
        )

        self.assertEqual(score["log_pred_samples"].shape, (5,))
        self.assertEqual(score["log_pred_matrix"].shape, (5, 1))
        self.assertEqual(score["fold_lpds"].shape, (1,))
        np.testing.assert_allclose(
            np.asarray(score["log_pred_samples"]),
            np.full((5,), float(exact)),
            rtol=0.0,
            atol=1e-5,
        )
        self.assertAlmostEqual(score["lpds"], float(exact), delta=1e-5)
        np.testing.assert_allclose(
            np.asarray(score["fold_lpds"]),
            np.asarray([float(exact)]),
            rtol=0.0,
            atol=1e-5,
        )
        self.assertTrue(np.isfinite(score["nse_lpds"]))

    def test_cross_validate_gaussian_vi_returns_fold_lpds(self):
        x = jnp.linspace(-1.0, 1.0, 12)[:, None]
        y = 1.0 + 2.0 * x

        result = cross_validate_gaussian_vi(
            x=x,
            y=y,
            knots={},
            spline_config={"comp": ("intercept", "covariates")},
            crossvalid_args={"N.subsets": 2, "partiMethod": "systematic"},
            key=jax.random.PRNGKey(33),
            n_predictive_samples=3,
            fit_kwargs={"n_steps": 3, "n_samples": 1, "learning_rate": 0.02},
        )

        self.assertEqual(result["log_pred_matrix"].shape, (3, 2))
        self.assertEqual(result["fold_lpds"].shape, (2,))
        self.assertEqual(len(result["folds"]), 2)
        self.assertTrue(np.isfinite(result["lpds"]))
        self.assertTrue(np.isfinite(result["nse_lpds"]))

    def test_cross_validate_gaussian_vi_marginal_smoke(self):
        x = jnp.linspace(-1.0, 1.0, 10)[:, None]
        y = 0.5 + 1.25 * x + 0.05 * jnp.sin(4.0 * x)

        result = cross_validate_gaussian_vi(
            x=x,
            y=y,
            knots={},
            spline_config={"comp": ("intercept", "covariates")},
            crossvalid_args={"N.subsets": 2, "partiMethod": "systematic"},
            free_knots="marginal",
            key=jax.random.PRNGKey(39),
            n_predictive_samples=2,
            fit_kwargs={
                "n_steps": 2,
                "n_samples": 1,
                "learning_rate": 0.02,
                "init_scale": 0.01,
            },
        )

        self.assertEqual(result["log_pred_matrix"].shape, (2, 2))
        self.assertEqual(result["fold_lpds"].shape, (2,))
        self.assertEqual(len(result["folds"]), 2)
        self.assertTrue(np.isfinite(result["lpds"]))
        self.assertTrue(np.isfinite(result["nse_lpds"]))
        for fold in result["folds"]:
            self.assertEqual(fold["fit"]["kind"], "marginal")
            self.assertEqual(fold["log_pred_samples"].shape, (2,))
            self.assertTrue(np.isfinite(fold["lpds"]))

    def test_fixed_knots_gaussian_vi_fit_and_predict(self):
        key = jax.random.PRNGKey(3)
        x = jnp.linspace(-1.0, 1.0, 40)[:, None]
        y = 1.0 + 2.0 * x
        spline_config = {"comp": ("intercept", "covariates")}

        fit = fit_fixed_knots_gaussian_vi(
            x,
            y,
            knots={},
            spline_config=spline_config,
            key=key,
            n_steps=120,
            learning_rate=0.02,
            n_samples=4,
        )

        beta_mean = np.asarray(fit["beta_mean"]).reshape(-1)
        y_pred = predict_mean(fit, jnp.array([[0.0], [1.0]]))

        self.assertLess(np.linalg.norm(beta_mean - np.array([1.0, 2.0])), 0.2)
        self.assertGreater(float(fit["sigma_mean"]), 0.0)
        np.testing.assert_allclose(np.asarray(y_pred).reshape(-1), [1.0, 3.0], atol=0.25)

    def test_summarize_fit_for_fixed_knots(self):
        key = jax.random.PRNGKey(8)
        x = jnp.linspace(-1.0, 1.0, 20)[:, None]
        y = 1.0 + 2.0 * x
        fit = fit_fixed_knots_gaussian_vi(
            x,
            y,
            knots={},
            spline_config={"comp": ("intercept", "covariates")},
            key=key,
            n_steps=20,
            learning_rate=0.02,
            n_samples=1,
        )

        summary = summarize_fit(fit)

        self.assertEqual(summary["beta_mean"].shape, (2, 1))
        self.assertGreater(summary["sigma_mean"], 0.0)
        self.assertTrue(np.isfinite(summary["final_elbo"]))
        self.assertEqual(summary["n_parameters"], 3)
        self.assertNotIn("additive_knots_mean", summary)

    def test_fixed_additive_knots_gaussian_vi_predicts_surface(self):
        key = jax.random.PRNGKey(4)
        x = jnp.linspace(-1.0, 1.0, 60)[:, None]
        knots = {"thinplate.a": jnp.array([[-0.5], [0.5]])}
        spline_config = {
            "comp": ("intercept", "covariates", "thinplate.a"),
            "thinplate.a.locate": (2,),
        }
        beta_true = jnp.array([[1.0], [2.0], [-1.0], [0.75]])
        y_true = design_matrix(x, knots, spline_config) @ beta_true
        y = y_true + 0.03 * jnp.sin(7.0 * x)

        fit = fit_fixed_knots_gaussian_vi(
            x,
            y,
            knots=knots,
            spline_config=spline_config,
            key=key,
            n_steps=70,
            learning_rate=0.02,
            n_samples=4,
        )
        y_pred = predict_mean(fit, x)
        mse = jnp.mean((y_pred - y_true) ** 2)

        self.assertLess(float(mse), 0.01)

    def test_fixed_surface_knots_gaussian_vi_predicts_surface(self):
        key = jax.random.PRNGKey(5)
        grid = jnp.linspace(-1.0, 1.0, 6)
        x1, x2 = jnp.meshgrid(grid, grid, indexing="xy")
        x = jnp.column_stack((x1.reshape(-1), x2.reshape(-1)))
        knots = {"thinplate.s": jnp.array([[-0.5, -0.5], [0.5, 0.5]])}
        spline_config = {
            "comp": ("intercept", "covariates", "thinplate.s"),
            "thinplate.s.dim": (2, 2),
        }
        beta_true = jnp.array([[0.25], [1.5], [-0.75], [0.8], [-0.4]])
        y_true = design_matrix(x, knots, spline_config) @ beta_true
        y = y_true + 0.02 * jnp.cos(5.0 * x[:, :1])

        fit = fit_fixed_knots_gaussian_vi(
            x,
            y,
            knots=knots,
            spline_config=spline_config,
            key=key,
            n_steps=80,
            learning_rate=0.02,
            n_samples=4,
        )
        y_pred = predict_mean(fit, x)
        mse = jnp.mean((y_pred - y_true) ** 2)

        self.assertLess(float(mse), 0.01)

    def test_additive_knot_gradient_flows_through_likelihood(self):
        x = jnp.linspace(-1.0, 1.0, 20)[:, None]
        y = jnp.sin(3.0 * x)
        beta = jnp.array([[0.0], [0.0], [1.0]])
        sigma = jnp.array([[1.0]])
        spline_config = {
            "comp": ("intercept", "covariates", "thinplate.a"),
            "thinplate.a.locate": (1,),
        }

        grad = jax.grad(
            lambda theta: gaussian_log_likelihood(
                y=y,
                x=x,
                knots={"thinplate.a": theta[:, None]},
                spline_config=spline_config,
                beta=beta,
                sigma=sigma,
            )
        )(jnp.array([0.25]))

        self.assertEqual(grad.shape, (1,))
        self.assertTrue(bool(jnp.all(jnp.isfinite(grad))))

    def test_surface_knot_gradient_flows_through_likelihood(self):
        grid = jnp.linspace(-1.0, 1.0, 5)
        x1, x2 = jnp.meshgrid(grid, grid, indexing="xy")
        x = jnp.column_stack((x1.reshape(-1), x2.reshape(-1)))
        y = jnp.sin(2.0 * x[:, :1]) + jnp.cos(3.0 * x[:, 1:2])
        beta = jnp.array([[0.0], [0.0], [0.0], [1.0]])
        sigma = jnp.array([[1.0]])
        spline_config = {
            "comp": ("intercept", "covariates", "thinplate.s"),
            "thinplate.s.dim": (1, 2),
        }

        grad = jax.grad(
            lambda theta: gaussian_log_likelihood(
                y=y,
                x=x,
                knots={"thinplate.s": theta.reshape((1, 2))},
                spline_config=spline_config,
                beta=beta,
                sigma=sigma,
            )
        )(jnp.array([0.25, -0.15]))

        self.assertEqual(grad.shape, (2,))
        self.assertTrue(bool(jnp.all(jnp.isfinite(grad))))

    def test_free_additive_knot_gaussian_vi_improves_over_initial_fixed_knots(self):
        key = jax.random.PRNGKey(6)
        x = jnp.linspace(-1.0, 1.0, 50)[:, None]
        true_knots = {"thinplate.a": jnp.array([[-0.5]])}
        initial_knots = {"thinplate.a": jnp.array([[0.8]])}
        spline_config = {
            "comp": ("intercept", "covariates", "thinplate.a"),
            "thinplate.a.locate": (1,),
        }
        beta_true = jnp.array([[0.0], [0.0], [4.0]])
        y = design_matrix(x, true_knots, spline_config) @ beta_true

        x_bad = np.asarray(design_matrix(x, initial_knots, spline_config))
        y_np = np.asarray(y)
        beta_bad = np.linalg.lstsq(x_bad, y_np, rcond=None)[0]
        mse_initial_fixed = np.mean((x_bad @ beta_bad - y_np) ** 2)

        fit = fit_free_additive_knots_gaussian_vi(
            x,
            y,
            knots=initial_knots,
            spline_config=spline_config,
            key=key,
            n_steps=40,
            learning_rate=0.03,
            n_samples=1,
            knot_prior_variance=20.0,
            init_scale=0.01,
        )
        mse_free = jnp.mean((predict_mean(fit, x) - y) ** 2)

        self.assertLess(float(mse_free), mse_initial_fixed)
        self.assertGreater(
            float(jnp.abs(fit["additive_knots_mean"][0, 0] - initial_knots["thinplate.a"][0, 0])),
            0.05,
        )

    def test_free_surface_knot_gaussian_vi_improves_over_initial_fixed_knots(self):
        key = jax.random.PRNGKey(10)
        grid = jnp.linspace(-1.0, 1.0, 6)
        x1, x2 = jnp.meshgrid(grid, grid, indexing="xy")
        x = jnp.column_stack((x1.reshape(-1), x2.reshape(-1)))
        true_knots = {"thinplate.s": jnp.array([[-0.4, 0.4]])}
        initial_knots = {"thinplate.s": jnp.array([[0.8, -0.8]])}
        spline_config = {
            "comp": ("intercept", "covariates", "thinplate.s"),
            "thinplate.s.dim": (1, 2),
        }
        beta_true = jnp.array([[0.0], [0.0], [0.0], [4.0]])
        y = design_matrix(x, true_knots, spline_config) @ beta_true

        x_bad = np.asarray(design_matrix(x, initial_knots, spline_config))
        y_np = np.asarray(y)
        beta_bad = np.linalg.lstsq(x_bad, y_np, rcond=None)[0]
        mse_initial_fixed = np.mean((x_bad @ beta_bad - y_np) ** 2)

        fit = fit_free_surface_knots_gaussian_vi(
            x,
            y,
            knots=initial_knots,
            spline_config=spline_config,
            key=key,
            n_steps=50,
            learning_rate=0.03,
            n_samples=1,
            knot_prior_variance=20.0,
            init_scale=0.01,
        )
        mse_free = jnp.mean((predict_mean(fit, x) - y) ** 2)

        self.assertLess(float(mse_free), mse_initial_fixed)
        self.assertEqual(fit["surface_knots_mean"].shape, (1, 2))

    def test_summarize_fit_for_free_additive_knots(self):
        key = jax.random.PRNGKey(9)
        x = jnp.linspace(-1.0, 1.0, 20)[:, None]
        knots = {"thinplate.a": jnp.array([[0.8]])}
        spline_config = {
            "comp": ("intercept", "covariates", "thinplate.a"),
            "thinplate.a.locate": (1,),
        }
        y = design_matrix(x, {"thinplate.a": jnp.array([[-0.5]])}, spline_config) @ jnp.array(
            [[0.0], [0.0], [2.0]]
        )
        fit = fit_free_additive_knots_gaussian_vi(
            x,
            y,
            knots=knots,
            spline_config=spline_config,
            key=key,
            n_steps=20,
            learning_rate=0.02,
            n_samples=1,
            knot_prior_variance=20.0,
        )

        summary = summarize_fit(fit)

        self.assertEqual(summary["beta_mean"].shape, (3, 1))
        self.assertEqual(summary["additive_knots_mean"].shape, (1, 1))
        self.assertGreater(summary["sigma_mean"], 0.0)
        self.assertTrue(np.isfinite(summary["final_elbo"]))
        self.assertEqual(summary["n_parameters"], 5)

    def test_full_gaussian_vi_represents_multivariate_combined_moving_knots(self):
        key = jax.random.PRNGKey(26)
        grid = jnp.linspace(-1.0, 1.0, 4)
        x1, x2 = jnp.meshgrid(grid, grid, indexing="xy")
        x = jnp.column_stack((x1.reshape(-1), x2.reshape(-1)))
        knots = {
            "thinplate.a": jnp.array([[-0.5], [0.5], [-0.25], [0.25]]),
            "thinplate.s": jnp.array([[0.0, 0.0]]),
        }
        spline_config = {
            "comp": ("intercept", "covariates", "thinplate.a", "thinplate.s"),
            "thinplate.a.locate": (2, 2),
            "thinplate.s.dim": (1, 2),
        }
        beta_true = jnp.array(
            [
                [0.5, -0.25],
                [1.0, 0.5],
                [-0.5, 0.75],
                [0.2, -0.1],
                [-0.1, 0.2],
                [0.3, -0.2],
                [-0.2, 0.1],
                [0.4, 0.3],
            ]
        )
        y = design_matrix(x, knots, spline_config) @ beta_true

        fit = fit_full_gaussian_vi(
            x,
            y,
            knots=knots,
            spline_config=spline_config,
            free_additive=True,
            free_surface=True,
            key=key,
            n_steps=5,
            learning_rate=0.02,
            n_samples=1,
            init_scale=0.01,
            p_matrix_types=("X'X", "identity", "identity"),
        )
        samples = predict_samples(fit, x[:3], jax.random.PRNGKey(27), n_samples=2)
        noisy_samples = predict_samples(
            fit,
            x[:3],
            jax.random.PRNGKey(28),
            n_samples=2,
            include_noise=True,
        )
        metrics = evaluate_gaussian_fit(fit, x, y)
        summary = summarize_fit(fit)

        self.assertEqual(fit["beta_mean"].shape, beta_true.shape)
        self.assertEqual(fit["sigma_mean"].shape, (2, 2))
        self.assertEqual(fit["shrinkage_mean"].shape, (3, 2))
        self.assertEqual(fit["additive_knots_mean"].shape, (4, 1))
        self.assertEqual(fit["surface_knots_mean"].shape, (1, 2))
        self.assertEqual(fit["p_matrix_types"], ("X'X", "identity", "identity"))
        self.assertEqual(samples.shape, (2, 3, 2))
        self.assertEqual(noisy_samples.shape, (2, 3, 2))
        self.assertTrue(np.isfinite(metrics["log_likelihood_at_mean"]))
        self.assertEqual(summary["sigma_mean"].shape, (2, 2))
        self.assertEqual(summary["shrinkage_mean"].shape, (3, 2))

    def test_marginal_gaussian_vi_integrates_beta_out_of_variational_state(self):
        key = jax.random.PRNGKey(29)
        x = jnp.linspace(-1.0, 1.0, 10)[:, None]
        knots = {"thinplate.a": jnp.array([[-0.25], [0.5]])}
        spline_config = {
            "comp": ("intercept", "covariates", "thinplate.a"),
            "thinplate.a.locate": (2,),
        }
        beta_true = jnp.array([[0.5, -0.25], [1.0, 0.5], [0.2, -0.1], [-0.1, 0.2]])
        y = design_matrix(x, knots, spline_config) @ beta_true

        fit = fit_marginal_gaussian_vi(
            x,
            y,
            knots=knots,
            spline_config=spline_config,
            free_additive=True,
            free_surface=False,
            key=key,
            n_steps=5,
            learning_rate=0.02,
            n_samples=1,
            init_scale=0.01,
            p_matrix_types=("X'X", "identity"),
        )
        samples = predict_samples(fit, x[:2], jax.random.PRNGKey(30), n_samples=2)

        self.assertEqual(fit["kind"], "marginal")
        self.assertEqual(fit["beta_mean"].shape, beta_true.shape)
        self.assertEqual(fit["sigma_mean"].shape, (2, 2))
        self.assertEqual(fit["shrinkage_mean"].shape, (2, 2))
        self.assertEqual(fit["additive_knots_mean"].shape, (2, 1))
        self.assertEqual(fit["p_matrix_types"], ("X'X", "identity"))
        self.assertLess(fit["vi"]["mu"].size, beta_true.size + 3 + 4 + 2)
        self.assertEqual(samples.shape, (2, 2, 2))

    def test_marginal_free_additive_knots_improve_bad_fixed_knot_baseline(self):
        x = jnp.linspace(-1.0, 1.0, 16)[:, None]
        true_knots = {"thinplate.a": jnp.array([[-0.45]])}
        initial_knots = {"thinplate.a": jnp.array([[0.75]])}
        spline_config = {
            "comp": ("intercept", "covariates", "thinplate.a"),
            "thinplate.a.locate": (1,),
        }
        beta_true = jnp.array([[0.0], [0.0], [4.0]])
        y = design_matrix(x, true_knots, spline_config) @ beta_true

        x_bad = np.asarray(design_matrix(x, initial_knots, spline_config))
        y_np = np.asarray(y)
        beta_bad = np.linalg.lstsq(x_bad, y_np, rcond=None)[0]
        mse_initial_fixed = np.mean((x_bad @ beta_bad - y_np) ** 2)

        marginal_fit = fit_marginal_gaussian_vi(
            x,
            y,
            knots=initial_knots,
            spline_config=spline_config,
            free_additive=True,
            free_surface=False,
            key=jax.random.PRNGKey(41),
            n_steps=5,
            learning_rate=0.03,
            n_samples=1,
            init_scale=0.01,
            knot_prior_variance=20.0,
            p_matrix_types=("X'X", "identity"),
        )
        full_fit = fit_full_gaussian_vi(
            x,
            y,
            knots=initial_knots,
            spline_config=spline_config,
            free_additive=True,
            free_surface=False,
            key=jax.random.PRNGKey(42),
            n_steps=5,
            learning_rate=0.03,
            n_samples=1,
            init_scale=0.01,
            knot_prior_variance=20.0,
            p_matrix_types=("X'X", "identity"),
        )

        marginal_mse = float(jnp.mean((predict_mean(marginal_fit, x) - y) ** 2))
        full_mse = float(jnp.mean((predict_mean(full_fit, x) - y) ** 2))
        marginal_knot_shift = float(
            jnp.abs(
                marginal_fit["additive_knots_mean"][0, 0]
                - initial_knots["thinplate.a"][0, 0]
            )
        )
        marginal_lpds = gaussian_lpds(
            marginal_fit,
            x[:4],
            y[:4],
            key=jax.random.PRNGKey(43),
            n_samples=2,
        )
        full_lpds = gaussian_lpds(
            full_fit,
            x[:4],
            y[:4],
            key=jax.random.PRNGKey(44),
            n_samples=2,
        )

        self.assertEqual(marginal_fit["kind"], "marginal")
        self.assertGreater(marginal_knot_shift, 0.1)
        self.assertLess(marginal_mse, mse_initial_fixed)
        self.assertLessEqual(marginal_mse, full_mse + 0.05)
        self.assertTrue(np.isfinite(marginal_lpds["lpds"]))
        self.assertTrue(np.isfinite(full_lpds["lpds"]))

    def test_marginal_fit_beta_posterior_matches_dense_formula(self):
        x = jnp.array([[0.0], [1.0], [2.0], [3.0]])
        y = jnp.array([[1.0], [1.5], [2.5], [4.0]])
        sigma = jnp.array([[0.6]])
        log_shrinkage = jnp.log(jnp.array([[1.3]]))
        beta_prior_variance = 2.0
        fit = _make_marginal_fit(
            x,
            y,
            spline_config={"comp": ("intercept", "covariates")},
            sigma=sigma,
            log_shrinkage=log_shrinkage,
            beta_prior_variance=beta_prior_variance,
        )

        posterior = marginal_fit_beta_posterior(fit)

        x_design = design_matrix(x, {}, fit["spline_config"])
        prior_variance = beta_prior_variance * jnp.exp(log_shrinkage[0, 0]) * sigma[0, 0]
        expected_precision = (
            jnp.eye(2) / prior_variance
            + x_design.T @ x_design / sigma[0, 0]
        )
        expected_covariance = jnp.linalg.inv(expected_precision)
        expected_mean = expected_covariance @ (x_design.T @ y[:, 0] / sigma[0, 0])
        covariance = posterior["covariance"]

        np.testing.assert_allclose(
            np.asarray(posterior["mean"]).reshape(-1),
            np.asarray(expected_mean),
            rtol=1e-12,
            atol=1e-12,
        )
        np.testing.assert_allclose(
            np.asarray(covariance),
            np.asarray(expected_covariance),
            rtol=1e-12,
            atol=1e-12,
        )
        np.testing.assert_allclose(
            np.asarray(covariance),
            np.asarray(covariance.T),
            rtol=0.0,
            atol=1e-12,
        )
        self.assertGreater(float(jnp.min(jnp.linalg.eigvalsh(covariance))), 0.0)

    def test_marginal_predict_samples_include_beta_uncertainty_and_noise(self):
        fit = _make_marginal_fit(
            jnp.array([[0.0]]),
            jnp.array([[0.0]]),
            sigma=jnp.array([[1.0]]),
            log_shrinkage=jnp.log(jnp.array([[1.0]])),
            beta_prior_variance=100.0,
        )
        x_new = jnp.array([[0.0], [0.0]])

        samples = predict_samples(
            fit,
            x_new,
            jax.random.PRNGKey(34),
            n_samples=800,
            include_noise=True,
        )
        row_mean = jnp.mean(samples[:, :, 0], axis=1)
        half_diff = 0.5 * (samples[:, 0, 0] - samples[:, 1, 0])

        self.assertEqual(samples.shape, (800, 2, 1))
        self.assertGreater(float(jnp.var(row_mean)), float(jnp.var(half_diff)) + 0.25)

    def test_marginal_predictive_moments_match_dense_formula(self):
        x = jnp.array(
            [
                [-1.0, 0.2],
                [-0.2, -0.4],
                [0.5, 0.8],
                [1.2, -0.7],
                [1.8, 0.3],
            ]
        )
        beta = jnp.array([[0.4, -0.2], [1.1, 0.3], [-0.5, 0.8]])
        y = design_matrix(x, {}, {"comp": ("intercept", "covariates")}) @ beta
        y = y + jnp.array(
            [
                [0.05, -0.02],
                [-0.03, 0.04],
                [0.02, 0.01],
                [-0.04, -0.03],
                [0.03, 0.02],
            ]
        )
        sigma = jnp.array([[0.9, 0.2], [0.2, 0.7]])
        log_shrinkage = jnp.log(jnp.array([[1.4, 0.8]]))
        fit = _make_marginal_fit(
            x,
            y,
            spline_config={"comp": ("intercept", "covariates")},
            sigma=sigma,
            log_shrinkage=log_shrinkage,
            beta_prior_variance=3.0,
        )
        x_new = jnp.array([[-0.5, 0.1], [0.25, -0.6], [1.5, 0.9]])

        latent_moments = marginal_fit_predictive_moments(fit, x_new)
        noisy_moments = marginal_fit_predictive_moments(
            fit,
            x_new,
            include_noise=True,
        )

        posterior = marginal_fit_beta_posterior(fit)
        x_new_design = design_matrix(x_new, {}, fit["spline_config"])
        expected_mean = x_new_design @ posterior["mean"]
        dense_map = jnp.kron(x_new_design, jnp.eye(2))
        expected_latent_covariance = (
            dense_map @ posterior["covariance"] @ dense_map.T
        )
        expected_noisy_covariance = expected_latent_covariance + jnp.kron(
            jnp.eye(x_new.shape[0]),
            sigma,
        )

        np.testing.assert_allclose(
            np.asarray(latent_moments["mean"]),
            np.asarray(expected_mean),
            rtol=1e-12,
            atol=1e-12,
        )
        np.testing.assert_allclose(
            np.asarray(noisy_moments["mean"]),
            np.asarray(expected_mean),
            rtol=1e-12,
            atol=1e-12,
        )
        np.testing.assert_allclose(
            np.asarray(latent_moments["covariance"]),
            np.asarray(expected_latent_covariance),
            rtol=1e-12,
            atol=1e-12,
        )
        np.testing.assert_allclose(
            np.asarray(noisy_moments["covariance"]),
            np.asarray(expected_noisy_covariance),
            rtol=1e-12,
            atol=1e-12,
        )

    def test_marginal_predict_samples_match_predictive_moments(self):
        x = jnp.array(
            [
                [-1.0, 0.2],
                [-0.2, -0.4],
                [0.5, 0.8],
                [1.2, -0.7],
                [1.8, 0.3],
            ]
        )
        beta = jnp.array([[0.4, -0.2], [1.1, 0.3], [-0.5, 0.8]])
        y = design_matrix(x, {}, {"comp": ("intercept", "covariates")}) @ beta
        y = y + jnp.array(
            [
                [0.05, -0.02],
                [-0.03, 0.04],
                [0.02, 0.01],
                [-0.04, -0.03],
                [0.03, 0.02],
            ]
        )
        sigma = jnp.array([[0.9, 0.2], [0.2, 0.7]])
        fit = _make_marginal_fit(
            x,
            y,
            spline_config={"comp": ("intercept", "covariates")},
            sigma=sigma,
            log_shrinkage=jnp.log(jnp.array([[1.4, 0.8]])),
            beta_prior_variance=3.0,
        )
        x_new = jnp.array([[-0.5, 0.1], [0.25, -0.6], [1.5, 0.9]])
        latent_moments = marginal_fit_predictive_moments(fit, x_new)
        noisy_moments = marginal_fit_predictive_moments(
            fit,
            x_new,
            include_noise=True,
        )

        latent_samples = predict_samples(
            fit,
            x_new,
            jax.random.PRNGKey(35),
            n_samples=4000,
            include_noise=False,
        )
        noisy_samples = predict_samples(
            fit,
            x_new,
            jax.random.PRNGKey(36),
            n_samples=4000,
            include_noise=True,
        )

        np.testing.assert_allclose(
            np.asarray(jnp.mean(latent_samples, axis=0)),
            np.asarray(latent_moments["mean"]),
            rtol=0.0,
            atol=0.08,
        )
        np.testing.assert_allclose(
            _row_major_sample_covariance(latent_samples),
            np.asarray(latent_moments["covariance"]),
            rtol=0.25,
            atol=0.08,
        )
        np.testing.assert_allclose(
            np.asarray(jnp.mean(noisy_samples, axis=0)),
            np.asarray(noisy_moments["mean"]),
            rtol=0.0,
            atol=0.08,
        )
        np.testing.assert_allclose(
            _row_major_sample_covariance(noisy_samples),
            np.asarray(noisy_moments["covariance"]),
            rtol=0.25,
            atol=0.12,
        )
        self.assertGreater(
            float(jnp.trace(noisy_moments["covariance"])),
            float(jnp.trace(latent_moments["covariance"])),
        )

    def test_marginal_log_predictive_density_matches_dense_formula(self):
        x = jnp.array(
            [
                [-1.0, 0.2],
                [-0.2, -0.4],
                [0.5, 0.8],
                [1.2, -0.7],
                [1.8, 0.3],
            ]
        )
        beta = jnp.array([[0.4, -0.2], [1.1, 0.3], [-0.5, 0.8]])
        y = design_matrix(x, {}, {"comp": ("intercept", "covariates")}) @ beta
        y = y + jnp.array(
            [
                [0.05, -0.02],
                [-0.03, 0.04],
                [0.02, 0.01],
                [-0.04, -0.03],
                [0.03, 0.02],
            ]
        )
        fit = _make_marginal_fit(
            x,
            y,
            spline_config={"comp": ("intercept", "covariates")},
            sigma=jnp.array([[0.9, 0.2], [0.2, 0.7]]),
            log_shrinkage=jnp.log(jnp.array([[1.4, 0.8]])),
            beta_prior_variance=3.0,
        )
        x_new = jnp.array([[-0.5, 0.1], [0.25, -0.6], [1.5, 0.9]])
        y_new = jnp.array([[0.15, -0.25], [0.7, -0.1], [1.1, 0.35]])

        actual = marginal_fit_log_predictive_density(fit, x_new, y_new)

        moments = marginal_fit_predictive_moments(
            fit,
            x_new,
            include_noise=True,
        )
        expected = _dense_gaussian_logpdf(
            y_new.reshape(-1),
            moments["mean"].reshape(-1),
            moments["covariance"],
        )
        self.assertAlmostEqual(float(actual), float(expected), places=12)

    def test_marginal_log_predictive_samples_use_exact_density_at_fixed_z(self):
        x = jnp.array([[-1.0], [-0.25], [0.5], [1.25]])
        beta = jnp.array([[0.3], [1.1]])
        y = design_matrix(x, {}, {"comp": ("intercept", "covariates")}) @ beta
        y = y + jnp.array([[0.05], [-0.02], [0.03], [-0.04]])
        fit = _make_marginal_fit(
            x,
            y,
            spline_config={"comp": ("intercept", "covariates")},
            sigma=jnp.array([[0.7]]),
            log_shrinkage=jnp.log(jnp.array([[1.2]])),
            beta_prior_variance=1.8,
        )
        x_new = jnp.array([[-0.5], [0.75]])
        y_new = jnp.array([[0.0], [1.0]])
        exact = marginal_fit_log_predictive_density(fit, x_new, y_new)

        log_samples = gaussian_log_predictive_samples(
            fit,
            x_new,
            y_new,
            key=jax.random.PRNGKey(37),
            n_samples=6,
        )

        self.assertEqual(log_samples.shape, (6,))
        np.testing.assert_allclose(
            np.asarray(log_samples),
            np.full((6,), float(exact)),
            rtol=0.0,
            atol=1e-5,
        )

    def test_fixed_knots_gaussian_vi_rejects_multivariate_y_for_now(self):
        x = jnp.array([[0.0], [1.0]])
        y = jnp.array([[1.0, 2.0], [3.0, 4.0]])

        with self.assertRaises(ValueError):
            fit_fixed_knots_gaussian_vi(
                x,
                y,
                knots={},
                spline_config={"comp": ("intercept", "covariates")},
                n_steps=1,
            )

    def test_free_additive_knots_gaussian_vi_rejects_multivariate_y_for_now(self):
        x = jnp.array([[0.0], [1.0]])
        y = jnp.array([[1.0, 2.0], [3.0, 4.0]])

        with self.assertRaises(ValueError):
            fit_free_additive_knots_gaussian_vi(
                x,
                y,
                knots={"thinplate.a": jnp.array([[0.0]])},
                spline_config={
                    "comp": ("intercept", "covariates", "thinplate.a"),
                    "thinplate.a.locate": (1,),
                },
                n_steps=1,
            )

    def test_free_surface_knots_gaussian_vi_rejects_multivariate_y_for_now(self):
        x = jnp.array([[0.0, 0.0], [1.0, 1.0]])
        y = jnp.array([[1.0, 2.0], [3.0, 4.0]])

        with self.assertRaises(ValueError):
            fit_free_surface_knots_gaussian_vi(
                x,
                y,
                knots={"thinplate.s": jnp.array([[0.0, 0.0]])},
                spline_config={
                    "comp": ("intercept", "covariates", "thinplate.s"),
                    "thinplate.s.dim": (1, 2),
                },
                n_steps=1,
            )


if __name__ == "__main__":
    unittest.main()
