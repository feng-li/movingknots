import math
import unittest

import numpy as np

from movingknots.distributions import diwishart, multigammaln, riwishart


class DistributionTests(unittest.TestCase):
    def test_multigammaln_univariate_matches_lgamma(self):
        self.assertAlmostEqual(multigammaln(1, 2.5), math.lgamma(2.5))

    def test_diwishart_returns_finite_log_density_for_spd_matrix(self):
        x = np.array([[2.0, 0.2], [0.2, 1.0]])
        scale = np.eye(2)

        self.assertTrue(np.isfinite(diwishart(x, df=5, scale=scale)))

    def test_riwishart_shape(self):
        draw = riwishart(6, np.eye(2), rng=123)

        self.assertEqual(draw.shape, (2, 2))
        np.testing.assert_allclose(draw, draw.T)


if __name__ == "__main__":
    unittest.main()
