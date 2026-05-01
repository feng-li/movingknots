"""Data helpers ported from `flutils`."""

from __future__ import annotations

import numpy as np


def std_data(x, method: str):
    """Standardize columns of an array."""
    x = np.asarray(x, dtype=float)
    was_vector = x.ndim == 1
    if was_vector:
        x = x[:, None]

    method_lower = method.lower()
    if method_lower == "norm-0-1":
        mean = np.mean(x, axis=0)
        sd = np.std(x, axis=0, ddof=1)
        out = (x - mean) / sd
        config = {"mean": mean, "sd": sd, "method": method}
    elif method_lower == "-1to1":
        min_value = np.min(x, axis=0)
        max_value = np.max(x, axis=0)
        out = (2 * x - max_value - min_value) / (max_value - min_value)
        config = {"min": min_value, "max": max_value, "method": method}
    else:
        raise ValueError("unknown standardization method")

    if was_vector:
        out = out.reshape(-1)
    return {"data": out, "config": config}


def _systematic_lengths(n_obs: int, n_subsets: int):
    return [len(range(start, n_obs, n_subsets)) for start in range(n_subsets)]


def data_partition(n_obs: int, args: dict, rng=None):
    """Partition zero-based observation indices for cross-validation."""
    method = args.get("partiMethod", "systematic").lower()
    n_subsets = int(args.get("N.subsets", 1))
    test_ratio = args.get("testRatio")
    if n_obs < n_subsets or (n_subsets < 1 and method != "time-series"):
        raise ValueError("number of subsets must be between 1 and n_obs")

    labels = np.arange(n_obs)
    rng = np.random.default_rng(rng)

    if method == "systematic":
        if n_subsets == 1 and test_ratio is not None:
            count = int(np.ceil(n_obs * test_ratio))
            return [np.floor(np.linspace(0, n_obs - 1, count)).astype(int)]
        return [labels[start::n_subsets] for start in range(n_subsets)]

    if method == "random":
        if n_subsets == 1 and test_ratio is not None:
            count = int(np.ceil(n_obs * test_ratio))
            return [rng.choice(labels, size=count, replace=False)]
        remaining = labels.copy()
        out = []
        for size in _systematic_lengths(n_obs, n_subsets):
            subset = rng.choice(remaining, size=size, replace=False)
            out.append(subset)
            remaining = np.setdiff1d(remaining, subset, assume_unique=False)
        return out

    if method == "ordered":
        if n_subsets == 1 and test_ratio is not None:
            count = int(np.ceil(n_obs * test_ratio))
            return [labels[-count:]]
        out = []
        start = 0
        for size in _systematic_lengths(n_obs, n_subsets):
            out.append(labels[start : start + size])
            start += size
        return out

    if method == "time-series":
        if n_subsets != 1 or test_ratio is None:
            raise ValueError("time-series partitioning requires N.subsets=1 and testRatio")
        count = int(np.ceil(n_obs * test_ratio))
        return [labels[-count:]]

    raise ValueError("unknown partitioning method")


def set_crossvalid(n_obs: int, crossvalid_args: dict, rng=None):
    """Return zero-based training/testing indices."""
    n_subsets = int(crossvalid_args.get("N.subsets", 0))
    if n_subsets == 0:
        full = np.arange(n_obs)
        return {"training": [full], "testing": [full]}

    testing = data_partition(n_obs, crossvalid_args, rng=rng)
    all_idx = np.arange(n_obs)
    training = [np.setdiff1d(all_idx, test_idx, assume_unique=False) for test_idx in testing]
    return {"training": training, "testing": testing}


def make_knots(x, method: str, spline_config: dict, rng=None):
    """Initialize moving-knot locations."""
    x = np.asarray(x, dtype=float)
    if x.ndim != 2:
        raise ValueError("x must be a matrix")

    components = set(spline_config.get("comp", ()))
    rng = np.random.default_rng(rng)
    out = {}
    surface_dim = spline_config.get("thinplate.s.dim", (0, x.shape[1]))
    n_surface = int(surface_dim[0])
    additive_counts = tuple(int(v) for v in spline_config.get("thinplate.a.locate", ()))
    method = method.lower()

    if method == "no-knots":
        if "thinplate.s" in components:
            out["thinplate.s"] = np.full((n_surface, x.shape[1]), np.nan)
        if "thinplate.a" in components:
            out["thinplate.a"] = np.full((sum(additive_counts), 1), np.nan)
        return out

    if "thinplate.s" in components:
        if method == "k-means":
            out["thinplate.s"] = _kmeans(x, n_surface, rng)
        elif method == "random":
            out["thinplate.s"] = x[rng.choice(x.shape[0], size=n_surface, replace=False)]
        elif method == "equal-spaced":
            probs = np.linspace(0, 1, n_surface + 2)[1:-1]
            out["thinplate.s"] = np.column_stack([np.quantile(x[:, j], probs) for j in range(x.shape[1])])
        else:
            raise ValueError("unknown knot initialization method")

    if "thinplate.a" in components:
        knots = []
        for covariate_index, count in enumerate(additive_counts):
            if count == 0:
                continue
            values = x[:, covariate_index]
            if method == "k-means":
                centers = _kmeans(values[:, None], count, rng).reshape(-1)
            elif method == "random":
                centers = rng.choice(values, size=count, replace=False)
            elif method == "equal-spaced":
                probs = np.linspace(0, 1, count + 2)[1:-1]
                centers = np.quantile(values, probs)
            else:
                raise ValueError("unknown knot initialization method")
            knots.extend(centers)
        out["thinplate.a"] = np.asarray(knots, dtype=float)[:, None]

    return out


def rmixnorm(n: int, means, sigmas, weights, rng=None):
    """Draw from a finite mixture of multivariate normal distributions."""
    means = np.asarray(means, dtype=float)
    sigmas = np.asarray(sigmas, dtype=float)
    weights = np.asarray(weights, dtype=float)
    if means.ndim != 2 or sigmas.ndim != 3:
        raise ValueError("means must be q-by-k and sigmas must be q-by-q-by-k")
    rng = np.random.default_rng(rng)
    weights = weights / np.sum(weights)
    components = rng.choice(len(weights), size=n, p=weights)
    out = np.empty((n, means.shape[0]), dtype=float)
    for i, component in enumerate(components):
        out[i] = rng.multivariate_normal(means[:, component], sigmas[:, :, component])
    return out


def _kmeans(x, n_centers: int, rng, max_iter: int = 200):
    if n_centers <= 0:
        return np.zeros((0, x.shape[1]), dtype=float)
    if n_centers == 1:
        return np.mean(x, axis=0, keepdims=True)
    if n_centers > x.shape[0]:
        raise ValueError("number of centers cannot exceed number of observations")

    centers = x[rng.choice(x.shape[0], size=n_centers, replace=False)].copy()
    for _ in range(max_iter):
        distances = np.sum((x[:, None, :] - centers[None, :, :]) ** 2, axis=-1)
        labels = np.argmin(distances, axis=1)
        updated = centers.copy()
        for k in range(n_centers):
            assigned = x[labels == k]
            if len(assigned):
                updated[k] = np.mean(assigned, axis=0)
        if np.allclose(updated, centers):
            break
        centers = updated
    return centers
