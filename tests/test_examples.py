import unittest

import jax

jax.config.update("jax_enable_x64", True)

import numpy as np

from examples import gaussian_marginal_workflow, rajan_paper_workflow


class ExampleTests(unittest.TestCase):
    def test_gaussian_marginal_workflow_returns_finite_metrics(self):
        metrics = gaussian_marginal_workflow.main(
            n_obs=8,
            n_steps=1,
            n_predictive_samples=2,
            print_results=False,
        )

        self.assertEqual(metrics["true_knot"], -0.45)
        self.assertEqual(metrics["initial_knot"], 0.75)
        self.assertEqual(set(metrics["models"]), {"bad_fixed", "full", "marginal"})
        for row in metrics["models"].values():
            self.assertTrue(np.isfinite(row["train_mse"]))
            self.assertTrue(np.isfinite(row["lpds"]))
            self.assertTrue(np.isfinite(row["nse_lpds"]))
            self.assertGreater(row["n_variational_parameters"], 0)
        self.assertIsNone(metrics["models"]["bad_fixed"]["final_elbo"])
        self.assertTrue(np.isfinite(metrics["models"]["full"]["final_elbo"]))
        self.assertTrue(np.isfinite(metrics["models"]["marginal"]["final_elbo"]))

    def test_rajan_paper_workflow_loads_data_and_returns_finite_metrics(self):
        data = rajan_paper_workflow.load_rajan_data()

        self.assertEqual(data["x"].shape[1], 4)
        self.assertEqual(data["y"].shape[1], 1)
        self.assertEqual(data["x_names"], ("tang", "mbtr", "logsale", "profit"))
        self.assertEqual(data["y_name"], "debtratio")

        metrics = rajan_paper_workflow.main(
            n_obs=12,
            n_surface_knots=1,
            n_additive_knots_per_covariate=1,
            n_steps=1,
            n_predictive_samples=2,
            print_results=False,
        )

        self.assertEqual(metrics["n_obs"], 12)
        self.assertEqual(metrics["n_surface_knots"], 1)
        self.assertEqual(metrics["n_additive_knots_per_covariate"], 1)
        self.assertEqual(set(metrics["models"]), {"fixed", "marginal"})
        for row in metrics["models"].values():
            self.assertTrue(np.isfinite(row["train_mse"]))
            self.assertTrue(np.isfinite(row["lpds"]))
            self.assertTrue(np.isfinite(row["nse_lpds"]))
            self.assertGreater(row["n_variational_parameters"], 0)
        self.assertIsNone(metrics["models"]["fixed"]["final_elbo"])
        self.assertTrue(np.isfinite(metrics["models"]["marginal"]["final_elbo"]))


if __name__ == "__main__":
    unittest.main()
