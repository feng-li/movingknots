"""Small utility functions ported from `flutils`."""

from __future__ import annotations

import secrets
from typing import Iterable

import numpy as np


def rdist(x, y, log: bool = True):
    """Euclidean distance matrix between rows of `x` and `y`."""
    x = np.asarray(x, dtype=float)
    y = np.asarray(y, dtype=float)
    if x.ndim != 2 or y.ndim != 2 or x.shape[1] != y.shape[1]:
        raise ValueError("x and y must be two-dimensional with the same column count")

    squared = np.sum(x * x, axis=1)[:, None] + np.sum(y * y, axis=1)[None, :] - 2 * x @ y.T
    squared = np.maximum(squared, 0.0)
    if log:
        with np.errstate(divide="ignore"):
            return 0.5 * np.log(squared)
    return np.sqrt(squared)


def mesh_grid(x1, x2=None):
    """Return the two-column grid matrix used by the R `mesh.grid` helper."""
    x1 = np.asarray(x1)
    x2 = x1 if x2 is None else np.asarray(x2)
    return np.column_stack((np.tile(x1, len(x2)), np.repeat(x2, len(x1))))


def vech(x, diag: bool = True):
    """Vectorize the lower triangle of a square matrix in R column-major order."""
    x = np.asarray(x)
    if x.ndim != 2 or x.shape[0] != x.shape[1]:
        raise ValueError("x must be a square matrix")

    values = []
    offset = 0 if diag else 1
    for col in range(x.shape[1]):
        for row in range(col + offset, x.shape[0]):
            values.append(x[row, col])
    return np.asarray(values)[:, None]


def vech_to_matrix(values, diag: bool = True):
    """Convert a lower-triangular vector back to a symmetric matrix."""
    values = np.asarray(values).reshape(-1)
    count = len(values)
    if diag:
        size = int((np.sqrt(8 * count + 1) - 1) / 2)
        expected = size * (size + 1) // 2
    else:
        size = int((np.sqrt(8 * count + 1) + 1) / 2)
        expected = size * (size - 1) // 2
    if expected != count:
        raise ValueError("input length is not compatible with triangular vectorization")

    out = np.zeros((size, size), dtype=values.dtype)
    i = 0
    offset = 0 if diag else 1
    for col in range(size):
        for row in range(col + offset, size):
            out[row, col] = values[i]
            out[col, row] = values[i]
            i += 1
    return out


def block_diag(blocks: Iterable):
    """Create a dense block diagonal matrix."""
    blocks = [np.asarray(block) for block in blocks]
    if not blocks:
        return None
    rows = sum(block.shape[0] for block in blocks)
    cols = sum(block.shape[1] for block in blocks)
    out = np.zeros((rows, cols), dtype=np.result_type(*blocks))
    row = 0
    col = 0
    for block in blocks:
        next_row = row + block.shape[0]
        next_col = col + block.shape[1]
        out[row:next_row, col:next_col] = block
        row = next_row
        col = next_col
    return out


def trace(x):
    """Trace of a square matrix, with scalar passthrough."""
    x = np.asarray(x)
    if x.ndim == 0 or x.size == 1:
        return x.reshape(-1)[0]
    if x.ndim != 2 or x.shape[0] != x.shape[1]:
        raise ValueError("x must be square")
    return np.trace(x)


def is_singular(x, tol=None):
    """Return whether a matrix is computationally singular."""
    x = np.asarray(x, dtype=float)
    tol = np.finfo(float).eps * 1e3 if tol is None else tol
    if x.ndim != 2 or x.shape[0] != x.shape[1]:
        raise ValueError("x must be square")
    return bool(1.0 / np.linalg.cond(x) < tol)


def dM(d, matrix):
    """Compute diag(d) @ matrix without forming diag(d)."""
    return np.asarray(d)[:, None] * np.asarray(matrix)


def Md(matrix, d):
    """Compute matrix @ diag(d) without forming diag(d)."""
    return np.asarray(matrix) * np.asarray(d)[None, :]


def dMd(d, matrix):
    """Compute diag(d) @ matrix @ diag(d) without forming diag(d)."""
    d = np.asarray(d)
    matrix = np.asarray(matrix)
    return d[:, None] * matrix * d[None, :]


def commutation_matrix(m: int, n: int):
    """Return the commutation matrix K(m, n)."""
    out = np.zeros((m * n, m * n), dtype=float)
    key = np.ravel(np.arange(m * n).reshape((m, n), order="F").T, order="F")
    out[np.arange(m * n), key] = 1.0
    return out


def apply_commutation(m: int, n: int, x, transpose: bool = False):
    """Apply K(m, n) on the left or right, matching R `K.X`."""
    x = np.asarray(x)
    if not transpose:
        if x.shape[0] != m * n:
            raise ValueError("K(m, n) @ x has incompatible dimensions")
        key = np.ravel(np.arange(m * n).reshape((m, n), order="F").T, order="F")
        return x[key, ...]

    if x.shape[1] != m * n:
        raise ValueError("x @ K(m, n) has incompatible dimensions")
    key = np.ravel(np.arange(m * n).reshape((n, m), order="F").T, order="F")
    return x[..., key]


def hessian_approx(gradient, method: str):
    """Approximate a Hessian from a gradient vector."""
    gradient = np.asarray(gradient)
    method = method.lower()
    if method == "outer":
        return -np.diag(np.ravel(gradient) ** 2)
    if method == "identity":
        return -np.eye(gradient.size)
    if method == "skip":
        return None
    raise ValueError("unknown Hessian approximation method")


def ineff(chain):
    """Estimate the MCMC inefficiency factor from autocorrelations."""
    chain = np.asarray(chain, dtype=float).reshape(-1)
    if np.mean(np.isnan(chain)) > 0.2:
        return np.nan
    chain = chain[~np.isnan(chain)]
    centered = chain - np.mean(chain)
    denom = np.dot(centered, centered)
    if denom == 0:
        return np.nan
    corr = np.correlate(centered, centered, mode="full")[len(chain) - 1 :] / denom
    out = 2 * np.sum(corr) - 1
    if np.isnan(out):
        return np.inf
    if out < 0:
        return np.nan
    return out


def rhex(n: int):
    """Return a random hexadecimal string."""
    alphabet = "0123456789abcdef"
    return "".join(secrets.choice(alphabet) for _ in range(n))
