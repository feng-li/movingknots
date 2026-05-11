"""High-level fformpp API built on the Gaussian moving-knots fitters."""

from __future__ import annotations

from dataclasses import dataclass
from typing import Callable

import jax
import numpy as np

from movingknots.data import make_knots
from movingknots.fit import fit_marginal_gaussian_vi, predict_samples


@dataclass(frozen=True)
class FFormPPFit:
    """Fitted forecast-performance model."""

    movingknots_fit: dict
    spline_config: dict
    feature_names: tuple[str, ...]
    model_names: tuple[str, ...]
    x_standardization: dict
    y_transform: str = "identity"


def fit(
    features,
    errors,
    *,
    model_names=None,
    surface_knots: int = 2,
    additive_knots=2,
    knot_method: str = "k-means",
    p_matrix_types=("identity", "identity", "identity"),
    prior_knots=None,
    y_transform: str = "identity",
    key=None,
    fit_kwargs=None,
):
    """Fit forecast-error prediction surfaces with moving-knot Gaussian VI."""
    x, feature_names = _as_matrix_and_names(features, "features", prefix="feature")
    y, inferred_model_names = _as_matrix_and_names(errors, "errors", prefix="model")
    if y.ndim == 1:
        y = y[:, None]
    if x.shape[0] != y.shape[0]:
        raise ValueError("features and errors must have the same number of rows")

    model_names = _resolve_names(model_names, inferred_model_names, y.shape[1], "model")
    y_transform = _normalize_y_transform(y_transform)
    if y_transform == "log":
        if np.any(y <= 0):
            raise ValueError("errors must be positive when y_transform='log'")
        y_fit = np.log(y)
    else:
        y_fit = y

    standardized = _standardize_features(x)
    x_fit = standardized["data"]
    spline_config = _make_spline_config(
        n_features=x_fit.shape[1],
        surface_knots=surface_knots,
        additive_knots=additive_knots,
    )
    knots = make_knots(
        x_fit,
        method=knot_method,
        spline_config=spline_config,
        rng=_rng_seed_from_key(key),
    )

    kwargs = {} if fit_kwargs is None else dict(fit_kwargs)
    if prior_knots is not None and "knot_prior_variance" not in kwargs:
        kwargs["knot_prior_variance"] = prior_knots

    movingknots_fit = fit_marginal_gaussian_vi(
        x_fit,
        y_fit,
        knots=knots,
        spline_config=spline_config,
        free_additive=True,
        free_surface=True,
        key=jax.random.PRNGKey(0) if key is None else key,
        p_matrix_types=p_matrix_types,
        **kwargs,
    )
    return FFormPPFit(
        movingknots_fit=movingknots_fit,
        spline_config=spline_config,
        feature_names=tuple(feature_names),
        model_names=tuple(model_names),
        x_standardization=standardized["config"],
        y_transform=y_transform,
    )


def predict(
    fit: FFormPPFit,
    features,
    *,
    key=None,
    n_samples: int = 500,
    estimate: str | Callable = "median",
    return_frame: bool | None = None,
):
    """Predict forecast errors for candidate forecasting methods."""
    if n_samples < 1:
        raise ValueError("n_samples must be positive")
    x, _ = _as_matrix_and_names(features, "features", prefix="feature")
    x_new = _apply_standardization(x, fit.x_standardization)
    key = jax.random.PRNGKey(0) if key is None else key
    samples = predict_samples(
        fit.movingknots_fit,
        x_new,
        key=key,
        n_samples=n_samples,
        include_noise=False,
    )
    prediction = _summarize_samples(np.asarray(samples), estimate)
    if fit.y_transform == "log":
        prediction = np.exp(prediction)
    return _maybe_return_frame(
        prediction,
        model_names=fit.model_names,
        index=_input_index(features),
        return_frame=return_frame,
    )


def _make_spline_config(n_features: int, surface_knots: int, additive_knots):
    if np.isscalar(additive_knots):
        additive_counts = (int(additive_knots),) * int(n_features)
    else:
        additive_counts = tuple(int(value) for value in additive_knots)
        if len(additive_counts) != int(n_features):
            raise ValueError("additive_knots must have one entry per feature")
    return {
        "comp": ("intercept", "covariates", "thinplate.s", "thinplate.a"),
        "thinplate.s.dim": (int(surface_knots), int(n_features)),
        "thinplate.a.locate": additive_counts,
    }


def _as_matrix_and_names(value, name: str, prefix: str):
    names = getattr(value, "columns", None)
    if hasattr(value, "to_numpy"):
        array = value.to_numpy(dtype=float)
    else:
        array = np.asarray(value, dtype=float)
    if array.ndim == 1:
        array = array[:, None]
    if array.ndim != 2:
        raise ValueError(f"{name} must be a one- or two-dimensional array")
    if names is None:
        names = tuple(f"{prefix}_{i}" for i in range(array.shape[1]))
    else:
        names = tuple(str(column) for column in names)
    return array, names


def _resolve_names(names, inferred_names, width: int, prefix: str):
    if names is None:
        names = inferred_names
    names = tuple(str(name) for name in names)
    if len(names) != width:
        raise ValueError(f"{prefix}_names must have length {width}")
    return names


def _normalize_y_transform(y_transform: str):
    value = str(y_transform).lower()
    if value in ("identity", "none", "raw"):
        return "identity"
    if value == "log":
        return "log"
    raise ValueError("y_transform must be 'identity' or 'log'")


def _standardize_features(x):
    mean = np.mean(x, axis=0)
    sd = np.std(x, axis=0, ddof=1)
    safe_sd = np.where(np.isfinite(sd) & (sd > 0), sd, 1.0)
    return {
        "data": (x - mean) / safe_sd,
        "config": {
            "mean": mean,
            "sd": safe_sd,
            "constant": ~(np.isfinite(sd) & (sd > 0)),
            "method": "norm-0-1",
        },
    }


def _apply_standardization(x, config):
    method = str(config["method"]).lower()
    if method == "norm-0-1":
        return (x - np.asarray(config["mean"])) / np.asarray(config["sd"])
    if method == "-1to1":
        return (
            2 * x
            - np.asarray(config["max"])
            - np.asarray(config["min"])
        ) / (np.asarray(config["max"]) - np.asarray(config["min"]))
    raise ValueError("unknown standardization method")


def _summarize_samples(samples, estimate):
    if str(estimate).lower() == "median":
        return np.median(samples, axis=0)
    if str(estimate).lower() == "mean":
        return np.mean(samples, axis=0)
    if callable(estimate):
        try:
            return np.asarray(estimate(samples, axis=0))
        except TypeError:
            return np.apply_along_axis(estimate, 0, samples)
    raise ValueError("estimate must be 'median', 'mean', or a callable")


def _rng_seed_from_key(key):
    if key is None:
        return 0
    key_array = np.asarray(key, dtype=np.uint32).reshape(-1)
    return int(np.bitwise_xor.reduce(key_array, initial=np.uint32(0)))


def _input_index(value):
    index = getattr(value, "index", None)
    return None if index is None else index


def _maybe_return_frame(prediction, model_names, index, return_frame):
    prefer_frame = bool(return_frame) if return_frame is not None else index is not None
    if not prefer_frame:
        return prediction
    try:
        import pandas as pd
    except ImportError:
        return prediction
    return pd.DataFrame(prediction, columns=list(model_names), index=index)


fit_fformpp = fit
predict_fformpp = predict
