"""Bundled fformpp example datasets."""

from __future__ import annotations

import csv
from dataclasses import dataclass
from importlib.resources import files

import numpy as np


@dataclass(frozen=True)
class FFormPPDataset:
    """Feature and forecast-error matrices with column names."""

    features: np.ndarray
    errors: np.ndarray
    feature_names: tuple[str, ...]
    model_names: tuple[str, ...]


def load_m3_example(n_rows: int | None = None):
    """Load the bundled M3-style training feature/error matrices."""
    return _load_example("features_m3.csv", "forecast_error_m3.csv", n_rows=n_rows)


def load_m1_example(n_rows: int | None = None):
    """Load the bundled M1-style evaluation feature/error matrices."""
    return _load_example("features_m1.csv", "forecast_error_m1.csv", n_rows=n_rows)


def _load_example(features_file: str, errors_file: str, n_rows: int | None = None):
    features, feature_names = _read_csv_matrix(features_file, n_rows=n_rows)
    errors, model_names = _read_csv_matrix(errors_file, n_rows=n_rows)
    if features.shape[0] != errors.shape[0]:
        raise ValueError("feature and error files contain different row counts")
    return FFormPPDataset(
        features=features,
        errors=errors,
        feature_names=feature_names,
        model_names=model_names,
    )


def _read_csv_matrix(name: str, n_rows: int | None = None):
    if n_rows is not None and n_rows < 1:
        raise ValueError("n_rows must be positive")
    path = files("movingknots.fformpp").joinpath("data", name)
    with path.open(newline="") as handle:
        reader = csv.reader(handle)
        header = tuple(next(reader))
        rows = []
        for i, row in enumerate(reader):
            if n_rows is not None and i >= n_rows:
                break
            rows.append([float(value) for value in row])
    return np.asarray(rows, dtype=float), header
