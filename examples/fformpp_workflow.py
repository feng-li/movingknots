"""Compact fformpp workflow using bundled feature/error matrices."""

from __future__ import annotations

import jax

jax.config.update("jax_enable_x64", True)

import numpy as np

from movingknots import fformpp


def main(
    n_train: int = 16,
    n_test: int = 5,
    n_features: int = 4,
    n_steps: int = 2,
    n_predictive_samples: int = 2,
    key_seed: int = 123,
    print_results: bool = True,
):
    train = fformpp.load_m3_example(n_rows=n_train)
    test = fformpp.load_m1_example(n_rows=n_test)
    keys = jax.random.split(jax.random.PRNGKey(key_seed), 2)

    fit = fformpp.fit(
        train.features[:, :n_features],
        train.errors,
        model_names=train.model_names,
        surface_knots=1,
        additive_knots=1,
        key=keys[0],
        fit_kwargs={
            "n_steps": n_steps,
            "n_samples": 1,
            "learning_rate": 0.01,
            "init_scale": 0.01,
        },
    )
    predicted = fformpp.predict(
        fit,
        test.features[:, :n_features],
        key=keys[1],
        n_samples=n_predictive_samples,
        estimate="median",
    )
    selected = fformpp.individual_forecast(
        predicted,
        actual_errors=test.errors,
        model_names=train.model_names,
    )

    metrics = {
        "n_train": int(n_train),
        "n_test": int(n_test),
        "n_features": int(n_features),
        "model_names": train.model_names,
        "predicted": predicted,
        "selected_model_names": selected["model_names"],
        "selected_errors": selected["min_errors"],
        "mean_selected_error": float(np.nanmean(selected["min_errors"])),
    }
    if print_results:
        print("models:", ", ".join(metrics["model_names"]))
        print("selected:", metrics["selected_model_names"])
        print("mean selected error:", f"{metrics['mean_selected_error']:.3f}")
    return metrics


if __name__ == "__main__":
    main()
