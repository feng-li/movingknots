import unittest

import numpy as np

from movingknots.data import data_partition, make_knots, set_crossvalid, std_data


class DataTests(unittest.TestCase):
    def test_std_data_norm_uses_sample_sd(self):
        actual = std_data(np.array([1.0, 2.0, 3.0]), "norm-0-1")

        np.testing.assert_allclose(actual["data"], [-1.0, 0.0, 1.0])
        self.assertEqual(actual["config"]["method"], "norm-0-1")

    def test_std_data_minus_one_to_one(self):
        actual = std_data(np.array([[0.0, 2.0], [5.0, 4.0], [10.0, 6.0]]), "-1to1")

        np.testing.assert_allclose(actual["data"], [[-1.0, -1.0], [0.0, 0.0], [1.0, 1.0]])

    def test_data_partition_systematic_is_zero_based(self):
        actual = data_partition(10, {"N.subsets": 3, "partiMethod": "systematic"})
        expected = [np.array([0, 3, 6, 9]), np.array([1, 4, 7]), np.array([2, 5, 8])]

        for actual_part, expected_part in zip(actual, expected):
            np.testing.assert_array_equal(actual_part, expected_part)

    def test_set_crossvalid_returns_training_complements(self):
        actual = set_crossvalid(5, {"N.subsets": 2, "partiMethod": "systematic"})

        np.testing.assert_array_equal(actual["testing"][0], [0, 2, 4])
        np.testing.assert_array_equal(actual["training"][0], [1, 3])

    def test_make_knots_equal_spaced(self):
        x = np.array([[0.0, 0.0], [1.0, 2.0], [2.0, 4.0], [3.0, 6.0]])
        config = {
            "comp": ("thinplate.s", "thinplate.a"),
            "thinplate.s.dim": (2, 2),
            "thinplate.a.locate": (2, 1),
        }

        actual = make_knots(x, "equal-spaced", config)

        np.testing.assert_allclose(actual["thinplate.s"], [[1.0, 2.0], [2.0, 4.0]])
        np.testing.assert_allclose(actual["thinplate.a"].reshape(-1), [1.0, 2.0, 3.0])


if __name__ == "__main__":
    unittest.main()
