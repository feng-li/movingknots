"""Distribution helpers ported from `flutils`."""

from __future__ import annotations

import math

import numpy as np


def multigammaln(p: int, x: float):
    """Log multivariate gamma function."""
    return p * (p - 1) / 4 * math.log(math.pi) + sum(
        math.lgamma(x + (1 - j) / 2) for j in range(1, p + 1)
    )


def rwishart(df: float, scale, rng=None):
    """Draw from a Wishart(df, scale) distribution."""
    scale = np.asarray(scale, dtype=float)
    rng = np.random.default_rng(rng)
    dim = scale.shape[0]
    if scale.shape != (dim, dim):
        raise ValueError("scale must be square")

    bartlett = np.zeros((dim, dim), dtype=float)
    for i in range(dim):
        bartlett[i, i] = math.sqrt(rng.chisquare(df - i))
        for j in range(i):
            bartlett[i, j] = rng.normal()
    upper = np.linalg.cholesky(scale).T
    root = bartlett.T @ upper
    draw = root.T @ root
    inv_draw = np.linalg.inv(draw)
    return {"W": draw, "IW": inv_draw, "C": root, "CI": np.linalg.inv(root)}


def riwishart(df: float, scale, rng=None):
    """Draw from an inverse-Wishart distribution with R-compatible parameterization."""
    return rwishart(df, np.linalg.inv(np.asarray(scale, dtype=float)), rng=rng)["IW"]


def diwishart(x, df: float, scale, log: bool = True):
    """Density of the inverse-Wishart distribution."""
    x = np.asarray(x, dtype=float)
    scale = np.asarray(scale, dtype=float)
    p = scale.shape[0]
    sign_scale, logdet_scale = np.linalg.slogdet(scale)
    sign_x, logdet_x = np.linalg.slogdet(x)
    if sign_scale <= 0 or sign_x <= 0:
        return -np.inf if log else 0.0
    log_density = (
        -df * p / 2 * math.log(2)
        - multigammaln(p, df / 2)
        + df / 2 * logdet_scale
        - (df + p + 1) / 2 * logdet_x
        - 0.5 * np.trace(np.linalg.solve(x, scale))
    )
    return log_density if log else math.exp(log_density)
