"""Free additive-knot demo."""

from __future__ import annotations

import jax
import jax.numpy as jnp

from movingknots.basis import design_matrix
from movingknots.fit import (
    fit_free_additive_knots_gaussian_vi,
    predict_mean,
    summarize_fit,
)


def main():
    key = jax.random.PRNGKey(10)
    x = jnp.linspace(-1.0, 1.0, 50)[:, None]

    spline_config = {
        "comp": ("intercept", "covariates", "thinplate.a"),
        "thinplate.a.locate": (1,),
    }
    true_knots = {"thinplate.a": jnp.array([[-0.5]])}
    initial_knots = {"thinplate.a": jnp.array([[0.8]])}
    beta_true = jnp.array([[0.0], [0.0], [4.0]])
    y = design_matrix(x, true_knots, spline_config) @ beta_true

    fit = fit_free_additive_knots_gaussian_vi(
        x,
        y,
        knots=initial_knots,
        spline_config=spline_config,
        key=key,
        n_steps=80,
        learning_rate=0.03,
        n_samples=1,
        knot_prior_variance=20.0,
        init_scale=0.01,
    )
    summary = summarize_fit(fit)
    mse = jnp.mean((predict_mean(fit, x) - y) ** 2)

    print("learned additive knots:", summary["additive_knots_mean"].reshape(-1))
    print("beta mean:", summary["beta_mean"].reshape(-1))
    print("sigma mean:", summary["sigma_mean"])
    print("final elbo:", summary["final_elbo"])
    print("training mse:", float(mse))


if __name__ == "__main__":
    main()
