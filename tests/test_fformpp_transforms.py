import unittest

import numpy as np

from movingknots.fformpp import glogit, transform_features


class FFormPPTransformTests(unittest.TestCase):
    def test_glogit_matches_centered_logit(self):
        actual = glogit(np.array([0.25, 0.5, 0.75]), a=0.0, b=1.0)

        np.testing.assert_allclose(actual[1], 0.0, atol=1e-12)
        self.assertLess(actual[0], 0.0)
        self.assertGreater(actual[2], 0.0)

    def test_transform_features_supports_sqrt(self):
        np.testing.assert_allclose(
            transform_features(np.array([1.0, 4.0, 9.0]), "sqrt"),
            [1.0, 2.0, 3.0],
        )


if __name__ == "__main__":
    unittest.main()
