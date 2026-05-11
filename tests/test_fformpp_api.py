import unittest

import jax

jax.config.update("jax_enable_x64", True)

import jax.numpy as jnp
import numpy as np

from movingknots.fformpp import (
    evaluate,
    fit,
    fit_fformpp,
    predict,
    predict_fformpp,
    summary,
)
from movingknots.fformpp.api import _make_spline_config
from movingknots.basis import design_matrix


class FFormPPApiTests(unittest.TestCase):
    def test_fit_and_predict_small_multivariate_errors(self):
        x = jnp.linspace(-1.0, 1.0, 8)[:, None]
        y = jnp.column_stack(
            (
                1.0 + 0.25 * x[:, 0],
                2.0 - 0.15 * x[:, 0],
            )
        )

        fitted = fit(
            x,
            y,
            model_names=("ets", "arima"),
            surface_knots=1,
            additive_knots=1,
            key=jax.random.PRNGKey(1),
            fit_kwargs={
                "n_steps": 1,
                "n_samples": 1,
                "learning_rate": 0.01,
                "init_scale": 0.01,
            },
        )
        pred = predict(
            fitted,
            x[:3],
            key=jax.random.PRNGKey(2),
            n_samples=2,
            estimate="mean",
        )

        self.assertEqual(pred.shape, (3, 2))
        self.assertEqual(fitted.model_names, ("ets", "arima"))
        self.assertEqual(fitted.feature_names, ("feature_0",))
        self.assertTrue(np.all(np.isfinite(pred)))

        fit_summary = summary(fitted)
        self.assertEqual(fit_summary["n_features"], 1)
        self.assertEqual(fit_summary["n_models"], 2)
        self.assertEqual(fit_summary["model_names"], ("ets", "arima"))
        self.assertEqual(fit_summary["n_response"], 2)
        self.assertTrue(np.isfinite(fit_summary["final_elbo"]))
        self.assertTrue(np.isfinite(fit_summary["training_mse"]))

        metrics = evaluate(
            fitted,
            x[:3],
            y[:3],
            key=jax.random.PRNGKey(3),
            n_samples=2,
            estimate="mean",
            return_predictions=True,
        )
        self.assertEqual(metrics["n_obs"], 3)
        self.assertEqual(metrics["n_models"], 2)
        self.assertEqual(metrics["predicted_errors"].shape, (3, 2))
        self.assertEqual(set(metrics["selected_model_counts"]), {"ets", "arima"})
        self.assertTrue(np.isfinite(metrics["mse"]))

    def test_legacy_function_names_remain_aliases(self):
        self.assertIs(fit_fformpp, fit)
        self.assertIs(predict_fformpp, predict)

    def test_historical_fformpp_spline_width_is_78(self):
        spline_config = _make_spline_config(
            n_features=25,
            surface_knots=2,
            additive_knots=2,
        )
        x = jnp.zeros((3, 25))
        knots = {
            "thinplate.s": jnp.zeros((2, 25)),
            "thinplate.a": jnp.zeros((50, 1)),
        }

        self.assertEqual(design_matrix(x, knots, spline_config).shape[1], 78)


if __name__ == "__main__":
    unittest.main()
