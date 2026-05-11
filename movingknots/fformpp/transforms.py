"""Feature transformations used by fformpp."""

from __future__ import annotations

import numpy as np


def glogit(x, a=None, b=None, epsilon: float = 1e-6):
    """Generalized logit transform on the interval ``[a, b]``."""
    x = np.asarray(x, dtype=float)
    lower = np.nanmin(x) if a is None else float(a)
    upper = np.nanmax(x) if b is None else float(b)
    if not upper > lower:
        raise ValueError("upper bound must be greater than lower bound")
    clipped = np.clip(x, lower + epsilon, upper - epsilon)
    p = (clipped - lower) / (upper - lower)
    return np.log(p / (1.0 - p))


def transform_features(feature, transformation: str):
    """Transform a feature vector with a named fformpp transformation."""
    transformation = str(transformation).lower()
    if transformation == "logit":
        return glogit(feature)
    if transformation == "sqrt":
        return np.sqrt(np.asarray(feature, dtype=float))
    raise ValueError("transformation must be 'logit' or 'sqrt'")
