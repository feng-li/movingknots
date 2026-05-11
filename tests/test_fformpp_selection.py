import unittest

import numpy as np

from movingknots.fformpp import combination_forecast, individual_forecast


class FFormPPSelectionTests(unittest.TestCase):
    def test_individual_forecast_selects_minimum_predicted_error(self):
        predicted = np.array([[2.0, 1.0, 3.0], [0.5, 0.7, 0.6]])
        actual = np.array([[20.0, 10.0, 30.0], [5.0, 7.0, 6.0]])

        result = individual_forecast(
            predicted,
            actual_errors=actual,
            model_names=("ets", "arima", "rw"),
        )

        self.assertEqual(result["model_names"], [("arima",), ("ets",)])
        np.testing.assert_allclose(result["min_errors"], [10.0, 5.0])
        self.assertEqual(result["summary"]["columns"], ("our_method", "ets", "arima", "rw"))

    def test_individual_forecast_tracks_ties(self):
        predicted = np.array([[1.0, 1.0, 2.0]])
        actual = np.array([[3.0, 4.0, 9.0]])

        result = individual_forecast(
            predicted,
            actual_errors=actual,
            model_names=("a", "b", "c"),
        )

        self.assertEqual(result["model_names"], [("a", "b")])
        self.assertEqual(result["models"][0].tolist(), [0, 1])
        self.assertEqual(result["min_errors"][0], 3.0)

    def test_combination_forecast_uses_top_unique_predicted_errors(self):
        predicted = np.array([[3.0, 1.0, 1.0, 2.0]])

        result = combination_forecast(
            predicted,
            n_components=2,
            model_names=("a", "b", "c", "d"),
        )

        self.assertEqual(result["model_names"], [("b", "c", "d")])
        self.assertEqual(result["models"][0].tolist(), [1, 2, 3])


if __name__ == "__main__":
    unittest.main()
