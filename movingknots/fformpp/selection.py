"""Forecast selection utilities for predicted model-performance matrices."""

from __future__ import annotations

import numpy as np


def individual_forecast(
    predicted,
    *,
    actual_errors=None,
    forecasts=None,
    accuracy_fn=None,
    tslist=None,
    model_names=None,
):
    """Select the forecast method with the smallest predicted error per series."""
    predicted = _as_2d(predicted, "predicted")
    model_names = _model_names(model_names, predicted.shape[1])
    actual = None if actual_errors is None else _as_2d(actual_errors, "actual_errors")
    selected_indices = []
    selected_names = []
    selected_errors = []

    for i, row in enumerate(predicted):
        winners = np.flatnonzero(row == np.nanmin(row))
        selected_indices.append(winners)
        selected_names.append(tuple(model_names[index] for index in winners))
        selected_errors.append(
            _selected_error(
                i,
                winners,
                actual=actual,
                forecasts=forecasts,
                accuracy_fn=accuracy_fn,
                tslist=tslist,
                combine="median",
                model_names=model_names,
            )
        )

    selected_errors = np.asarray(selected_errors, dtype=float)
    return {
        "models": selected_indices,
        "model_names": selected_names,
        "min_errors": selected_errors,
        "summary": _summary(selected_errors, actual, model_names, "our_method"),
    }


def combination_forecast(
    predicted,
    *,
    n_components: int = 2,
    actual_errors=None,
    forecasts=None,
    accuracy_fn=None,
    tslist=None,
    model_names=None,
    weights=None,
    measure: str = "median",
):
    """Combine forecasts from the methods with the lowest predicted errors."""
    if n_components < 1:
        raise ValueError("n_components must be positive")
    predicted = _as_2d(predicted, "predicted")
    model_names = _model_names(model_names, predicted.shape[1])
    actual = None if actual_errors is None else _as_2d(actual_errors, "actual_errors")
    selected_indices = []
    selected_names = []
    selected_errors = []

    for i, row in enumerate(predicted):
        finite = np.sort(np.unique(row[np.isfinite(row)]))
        if finite.size == 0:
            winners = np.asarray([], dtype=int)
        else:
            threshold_values = finite[:n_components]
            winners = np.flatnonzero(np.isin(row, threshold_values))
        selected_indices.append(winners)
        selected_names.append(tuple(model_names[index] for index in winners))
        selected_errors.append(
            _selected_error(
                i,
                winners,
                actual=actual,
                forecasts=forecasts,
                accuracy_fn=accuracy_fn,
                tslist=tslist,
                combine=measure,
                model_names=model_names,
                predicted_row=row,
                weights=weights,
            )
        )

    selected_errors = np.asarray(selected_errors, dtype=float)
    return {
        "models": selected_indices,
        "model_names": selected_names,
        "min_errors": selected_errors,
        "summary": _summary(selected_errors, actual, model_names, "our_method_comb"),
    }


def _selected_error(
    i,
    winners,
    *,
    actual,
    forecasts,
    accuracy_fn,
    tslist,
    combine,
    model_names,
    predicted_row=None,
    weights=None,
):
    if actual is None:
        return np.nan
    if winners.size == 0:
        return np.nan
    if winners.size == 1 and forecasts is None:
        return actual[i, winners[0]]
    if forecasts is None or accuracy_fn is None or tslist is None:
        return np.nanmin(actual[i, winners])

    forecast_matrix = np.column_stack(
        [_forecast_column(forecasts, model_names[index], i) for index in winners]
    )
    forecast = _combine_forecasts(
        forecast_matrix,
        combine=combine,
        predicted_values=None if predicted_row is None else predicted_row[winners],
        weights=weights,
    )
    training, test = _series_train_test(tslist[i])
    try:
        return float(accuracy_fn(training, test, forecast=forecast))
    except TypeError:
        return float(accuracy_fn(training, test, forecast))


def _combine_forecasts(forecast_matrix, *, combine, predicted_values=None, weights=None):
    combine = str(combine).lower()
    if weights in (None, False, "equal"):
        weighted = forecast_matrix
    else:
        if predicted_values is None:
            raise ValueError("predicted values are required for weighted combinations")
        predicted_values = np.asarray(predicted_values, dtype=float)
        if weights == "legacy" or weights is True:
            weight = predicted_values / np.sum(predicted_values)
        elif weights == "inverse_error":
            inverse = 1.0 / np.maximum(predicted_values, np.finfo(float).eps)
            weight = inverse / np.sum(inverse)
        else:
            weight = np.asarray(weights, dtype=float)
            weight = weight / np.sum(weight)
        weighted = forecast_matrix * weight[None, :]
    if combine == "mean":
        return np.nanmean(weighted, axis=1)
    if combine == "median":
        return np.nanmedian(weighted, axis=1)
    raise ValueError("measure must be 'mean' or 'median'")


def _forecast_column(forecasts, model_name, series_index):
    forecast = forecasts[model_name] if isinstance(forecasts, dict) else forecasts[model_name]
    forecast = np.asarray(forecast, dtype=float)
    if forecast.ndim == 1:
        return forecast
    return forecast[:, series_index]


def _series_train_test(series):
    if isinstance(series, dict):
        return series["x"], series["xx"]
    return series[0], series[1]


def _summary(selected_errors, actual, model_names, first_name):
    if actual is None or not np.any(np.isfinite(selected_errors)):
        return None
    summary = np.vstack(
        [
            np.concatenate([[np.nanmean(selected_errors)], np.nanmean(actual, axis=0)]),
            np.concatenate([[np.nanmedian(selected_errors)], np.nanmedian(actual, axis=0)]),
        ]
    )
    return {
        "values": summary,
        "rows": ("mean", "median"),
        "columns": (first_name, *model_names),
    }


def _as_2d(value, name: str):
    if hasattr(value, "to_numpy"):
        array = value.to_numpy(dtype=float)
    else:
        array = np.asarray(value, dtype=float)
    if array.ndim == 1:
        array = array[:, None]
    if array.ndim != 2:
        raise ValueError(f"{name} must be one- or two-dimensional")
    return array


def _model_names(model_names, width: int):
    if model_names is None:
        return tuple(f"model_{i}" for i in range(width))
    model_names = tuple(str(name) for name in model_names)
    if len(model_names) != width:
        raise ValueError(f"model_names must have length {width}")
    return model_names
