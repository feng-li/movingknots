import math
import unittest

import numpy as np

from movingknots.utils import (
    Md,
    apply_commutation,
    block_diag,
    commutation_matrix,
    dM,
    dMd,
    hessian_approx,
    mesh_grid,
    rdist,
    trace,
    vech,
    vech_to_matrix,
)


class UtilsTests(unittest.TestCase):
    def test_rdist_matches_r_behavior(self):
        x = np.array([[0.0, 0.0]])
        y = np.array([[3.0, 4.0], [0.0, 0.0]])

        np.testing.assert_allclose(rdist(x, y, log=False), [[5.0, 0.0]])
        actual_log = rdist(x, y, log=True)
        self.assertAlmostEqual(actual_log[0, 0], math.log(5.0))
        self.assertTrue(np.isneginf(actual_log[0, 1]))

    def test_mesh_grid_matches_flutils_order(self):
        actual = mesh_grid([1, 2], [10, 20, 30])
        expected = np.array([[1, 10], [2, 10], [1, 20], [2, 20], [1, 30], [2, 30]])

        np.testing.assert_array_equal(actual, expected)

    def test_vech_uses_r_column_major_order(self):
        matrix = np.array([[1, 2, 3], [4, 5, 6], [7, 8, 9]])

        actual = vech(matrix)
        expected = np.array([[1], [4], [7], [5], [8], [9]])

        np.testing.assert_array_equal(actual, expected)
        np.testing.assert_array_equal(
            vech_to_matrix(actual),
            np.array([[1, 4, 7], [4, 5, 8], [7, 8, 9]]),
        )

    def test_block_diag_and_trace(self):
        actual = block_diag([np.ones((2, 1)), 2 * np.eye(2)])
        expected = np.array([[1, 0, 0], [1, 0, 0], [0, 2, 0], [0, 0, 2]])

        np.testing.assert_array_equal(actual, expected)
        self.assertEqual(trace(np.eye(3)), 3)

    def test_diagonal_matrix_shortcuts(self):
        matrix = np.array([[1, 2], [3, 4]])
        d = np.array([10, 100])

        np.testing.assert_array_equal(dM(d, matrix), np.diag(d) @ matrix)
        np.testing.assert_array_equal(Md(matrix, d), matrix @ np.diag(d))
        np.testing.assert_array_equal(dMd(d, matrix), np.diag(d) @ matrix @ np.diag(d))

    def test_commutation_matrix_and_fast_application(self):
        matrix = np.array([[1, 3, 5], [2, 4, 6]])
        vec = matrix.reshape(-1, order="F")[:, None]
        expected = matrix.T.reshape(-1, order="F")[:, None]

        k = commutation_matrix(2, 3)

        np.testing.assert_array_equal(k @ vec, expected)
        np.testing.assert_array_equal(apply_commutation(2, 3, vec), expected)

    def test_hessian_approx(self):
        gradient = np.array([[2.0], [-3.0]])

        np.testing.assert_array_equal(hessian_approx(gradient, "outer"), np.diag([-4.0, -9.0]))
        np.testing.assert_array_equal(hessian_approx(gradient, "identity"), -np.eye(2))
        self.assertIsNone(hessian_approx(gradient, "skip"))


if __name__ == "__main__":
    unittest.main()
