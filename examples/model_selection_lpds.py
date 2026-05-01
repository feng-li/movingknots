"""LPDS model-selection demo."""

from __future__ import annotations

import jax
import jax.numpy as jnp

from movingknots.basis import design_matrix
from movingknots.fit import cross_validate_gaussian_vi


def make_synthetic_data():
    grid = jnp.linspace(-1.0, 1.0, 5)
    x1, x2 = jnp.meshgrid(grid, grid, indexing="xy")
    x = jnp.column_stack((x1.reshape(-1), x2.reshape(-1)))

    true_knots = {
        "thinplate.a": jnp.array([[-0.55], [0.45]]),
        "thinplate.s": jnp.array([[-0.4, 0.5]]),
    }
    spline_config = {
        "comp": ("intercept", "covariates", "thinplate.a", "thinplate.s"),
        "thinplate.a.locate": (1, 1),
        "thinplate.s.dim": (1, 2),
    }
    beta = jnp.array([[0.2], [0.2], [-0.1], [6.0], [-4.0], [8.0]])
    y_signal = design_matrix(x, true_knots, spline_config) @ beta
    noise = 0.03 * jax.random.normal(jax.random.PRNGKey(5), y_signal.shape)
    return x, y_signal + noise


def score_model(name, x, y, knots, spline_config, free_knots, key, fit_kwargs):
    result = cross_validate_gaussian_vi(
        x=x,
        y=y,
        knots=knots,
        spline_config=spline_config,
        crossvalid_args={"N.subsets": 2, "partiMethod": "systematic"},
        free_knots=free_knots,
        key=key,
        n_predictive_samples=4,
        fit_kwargs=fit_kwargs,
    )
    return {
        "model": name,
        "lpds": result["lpds"],
        "nse_lpds": result["nse_lpds"],
    }


def print_table(rows):
    print(f"{'model':<18} {'lpds':>12} {'nse_lpds':>12}")
    for row in rows:
        print(f"{row['model']:<18} {row['lpds']:>12.3f} {row['nse_lpds']:>12.3f}")


def main():
    x, y = make_synthetic_data()
    keys = jax.random.split(jax.random.PRNGKey(10), 3)

    linear_config = {"comp": ("intercept", "covariates")}
    knot_config = {
        "comp": ("intercept", "covariates", "thinplate.a", "thinplate.s"),
        "thinplate.a.locate": (1, 1),
        "thinplate.s.dim": (1, 2),
    }
    initial_knots = {
        "thinplate.a": jnp.array([[0.85], [-0.85]]),
        "thinplate.s": jnp.array([[0.8, -0.8]]),
    }

    common_fit_kwargs = {
        "n_steps": 60,
        "learning_rate": 0.025,
        "n_samples": 1,
        "init_scale": 0.01,
    }

    rows = [
        score_model(
            "linear_fixed",
            x,
            y,
            knots={},
            spline_config=linear_config,
            free_knots=False,
            key=keys[0],
            fit_kwargs=common_fit_kwargs,
        ),
        score_model(
            "fixed_knots",
            x,
            y,
            knots=initial_knots,
            spline_config=knot_config,
            free_knots=False,
            key=keys[1],
            fit_kwargs=common_fit_kwargs,
        ),
        score_model(
            "moving_marginal",
            x,
            y,
            knots=initial_knots,
            spline_config=knot_config,
            free_knots="marginal",
            key=keys[2],
            fit_kwargs={
                **common_fit_kwargs,
                "p_matrix_types": ("X'X", "identity", "identity"),
                "knot_prior_variance": 30.0,
            },
        ),
    ]
    print_table(rows)


if __name__ == "__main__":
    main()
