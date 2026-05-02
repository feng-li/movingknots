"""Rajan-Zingales data workflow from the moving-knots paper examples."""

from __future__ import annotations

from pathlib import Path

import jax

jax.config.update("jax_enable_x64", True)

import jax.numpy as jnp
import numpy as np
from scipy.io import loadmat

from movingknots.basis import design_matrix
from movingknots.data import make_knots, std_data
from movingknots.fit import (
    fit_marginal_gaussian_vi,
    gaussian_lpds,
    predict_mean,
)


REPO_ROOT = Path(__file__).resolve().parents[1]
DEFAULT_DATA_PATH = REPO_ROOT / "data" / "RajanData.mat"


def load_rajan_data(path=DEFAULT_DATA_PATH):
    """Load and standardize the Rajan-Zingales data used by the paper examples."""
    mat = loadmat(path, squeeze_me=True)
    x_raw = np.asarray(mat["X"], dtype=float)
    y = np.asarray(mat["y"], dtype=float).reshape(-1, 1)
    x_names = [str(name) for name in np.ravel(mat["XName"])]
    y_name = str(np.ravel(mat["yName"])[0])
    description = str(np.ravel(mat["Description"])[0])

    if x_names and x_names[0].lower() == "const":
        x_raw = x_raw[:, 1:]
        x_names = x_names[1:]

    standardized = std_data(x_raw, "norm-0-1")
    return {
        "x": jnp.asarray(standardized["data"]),
        "y": jnp.asarray(y),
        "x_names": tuple(x_names),
        "y_name": y_name,
        "description": description,
        "standardization": standardized["config"],
    }


def make_rajan_spline_config(
    n_covariates: int,
    n_surface_knots: int = 2,
    n_additive_knots_per_covariate: int = 2,
):
    """Build the surface-plus-additive Rajan spline configuration."""
    return {
        "comp": ("intercept", "covariates", "thinplate.s", "thinplate.a"),
        "thinplate.s.dim": (int(n_surface_knots), int(n_covariates)),
        "thinplate.a.locate": (int(n_additive_knots_per_covariate),) * int(n_covariates),
    }


def fixed_knot_fit(x, y, knots, spline_config):
    """Least-squares fixed-knot baseline in the simple fit dictionary format."""
    x_design = design_matrix(x, knots, spline_config)
    beta = np.linalg.lstsq(np.asarray(x_design), np.asarray(y), rcond=None)[0]
    residual = np.asarray(y) - np.asarray(x_design) @ beta
    variance = max(float(np.mean(residual**2)), 1e-6)
    position = jnp.concatenate(
        [jnp.asarray(beta).reshape(-1), jnp.asarray([jnp.log(variance)])]
    )
    return {
        "kind": "fixed",
        "n_coef": int(x_design.shape[1]),
        "beta_mean": jnp.asarray(beta),
        "sigma_mean": jnp.asarray([[variance]]),
        "vi": {
            "mu": position,
            "rho": jnp.full_like(position, -30.0),
            "elbo": jnp.asarray([jnp.nan]),
        },
        "knots": knots,
        "spline_config": spline_config,
    }


def fit_metrics(name, fit, x_train, y_train, x_score, y_score, key, n_predictive_samples):
    residual = predict_mean(fit, x_train) - y_train
    score = gaussian_lpds(
        fit,
        x_score,
        y_score,
        key=key,
        n_samples=n_predictive_samples,
    )
    elbo = fit["vi"]["elbo"][-1]
    out = {
        "model": name,
        "train_mse": float(jnp.mean(residual**2)),
        "lpds": float(score["lpds"]),
        "nse_lpds": float(score["nse_lpds"]),
        "final_elbo": None if bool(jnp.isnan(elbo)) else float(elbo),
        "n_variational_parameters": int(fit["vi"]["mu"].size),
    }
    if "additive_knots_mean" in fit:
        out["additive_knots_mean"] = np.asarray(fit["additive_knots_mean"], dtype=float)
    if "surface_knots_mean" in fit:
        out["surface_knots_mean"] = np.asarray(fit["surface_knots_mean"], dtype=float)
    return out


def print_metrics(metrics):
    print(metrics["description"])
    print("covariates:", ", ".join(metrics["x_names"]))
    print(
        "subset:",
        metrics["n_obs"],
        "rows; surface knots:",
        metrics["n_surface_knots"],
        "additive knots per covariate:",
        metrics["n_additive_knots_per_covariate"],
    )
    print(f"{'model':<12} {'mse':>12} {'lpds':>12} {'elbo':>12} {'npar':>6}")
    for row in metrics["models"].values():
        elbo = row["final_elbo"]
        elbo_text = "NA" if elbo is None else f"{elbo:.3f}"
        print(
            f"{row['model']:<12} {row['train_mse']:>12.4f} "
            f"{row['lpds']:>12.3f} {elbo_text:>12} "
            f"{row['n_variational_parameters']:>6}"
        )


def main(
    n_obs: int = 120,
    n_surface_knots: int = 2,
    n_additive_knots_per_covariate: int = 2,
    n_steps: int = 20,
    n_predictive_samples: int = 4,
    key_seed: int = 123,
    print_results: bool = True,
):
    """Run a compact Rajan workflow using the paper's surface-plus-additive structure."""
    data = load_rajan_data()
    x = data["x"][:n_obs]
    y = data["y"][:n_obs]
    spline_config = make_rajan_spline_config(
        n_covariates=x.shape[1],
        n_surface_knots=n_surface_knots,
        n_additive_knots_per_covariate=n_additive_knots_per_covariate,
    )
    initial_knots = make_knots(
        np.asarray(x),
        method="k-means",
        spline_config=spline_config,
        rng=key_seed,
    )
    keys = jax.random.split(jax.random.PRNGKey(key_seed), 3)

    fixed_fit = fixed_knot_fit(x, y, initial_knots, spline_config)
    marginal_fit = fit_marginal_gaussian_vi(
        x,
        y,
        knots=initial_knots,
        spline_config=spline_config,
        free_additive=True,
        free_surface=True,
        key=keys[0],
        n_steps=n_steps,
        learning_rate=0.02,
        n_samples=1,
        init_scale=0.01,
        knot_prior_variance=30.0,
        p_matrix_types=("X'X", "identity", "identity"),
    )

    score_n = min(20, n_obs)
    metrics = {
        "description": data["description"],
        "x_names": data["x_names"],
        "y_name": data["y_name"],
        "n_obs": int(n_obs),
        "n_surface_knots": int(n_surface_knots),
        "n_additive_knots_per_covariate": int(n_additive_knots_per_covariate),
        "models": {
            "fixed": fit_metrics(
                "fixed",
                fixed_fit,
                x,
                y,
                x[:score_n],
                y[:score_n],
                keys[1],
                n_predictive_samples,
            ),
            "marginal": fit_metrics(
                "marginal",
                marginal_fit,
                x,
                y,
                x[:score_n],
                y[:score_n],
                keys[2],
                n_predictive_samples,
            ),
        },
    }
    if print_results:
        print_metrics(metrics)
    return metrics


if __name__ == "__main__":
    main()
