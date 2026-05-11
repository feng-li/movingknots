import unittest

import jax

jax.config.update("jax_enable_x64", True)

import numpy as np

from examples import fformpp_workflow
from movingknots import fformpp


class FFormPPDatasetTests(unittest.TestCase):
    def test_load_m3_and_m1_examples(self):
        m3 = fformpp.load_m3_example(n_rows=3)
        m1 = fformpp.load_m1_example(n_rows=4)

        self.assertEqual(m3.features.shape, (3, 25))
        self.assertEqual(m3.errors.shape, (3, 7))
        self.assertEqual(m1.features.shape, (4, 25))
        self.assertEqual(m1.errors.shape, (4, 7))
        self.assertEqual(m3.model_names, ("ets", "arima", "rw", "rwd", "wn", "theta", "nn"))
        self.assertEqual(m3.feature_names[0], "entropy")
        self.assertTrue(np.all(np.isfinite(m3.features)))
        self.assertTrue(np.all(np.isfinite(m1.errors)))

    def test_fformpp_workflow_smoke(self):
        metrics = fformpp_workflow.main(
            n_train=8,
            n_test=3,
            n_features=3,
            n_steps=1,
            n_predictive_samples=1,
            print_results=False,
        )

        self.assertEqual(metrics["predicted"].shape, (3, 7))
        self.assertEqual(len(metrics["selected_model_names"]), 3)
        self.assertTrue(np.isfinite(metrics["mean_selected_error"]))


if __name__ == "__main__":
    unittest.main()
