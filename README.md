# `movingknots`

This repository root is the main entry point for the active Python/JAX implementation of
`movingknots`. The package source is in `movingknots/`, tests are in `tests/`, and runnable
examples are in `examples/`.

The original R code is retained under `legacy/` as legacy/reference material. The Python
tests include R-generated fixtures where parity with the original implementation is
important.

## Setup

From the repository root:

```bash
python -m pip install -e .
python -m unittest discover -s tests
```

Run the marginal Gaussian workflow example:

```bash
python -m examples.gaussian_marginal_workflow
```

Run the compact Rajan paper-data workflow:

```bash
python -m examples.rajan_paper_workflow
```

## Current Scope

- thin-plate surface and additive basis construction
- Gaussian moving-knot regression with JAX automatic differentiation
- mean-field variational inference
- fixed-knot, full-parameter, and marginalized-coefficient Gaussian fitters
- R parity fixtures for likelihood, prior, parameter schema, and posterior calculations
- Rajan-Zingales paper-data example using `data/RajanData.mat` and `data/RajanData.csv`

## Data

The Rajan-Zingales paper example data are available in two formats:

- `data/RajanData.mat`: original MATLAB data file used by the Python example.
- `data/RajanData.csv`: CSV conversion with columns
  `const,tang,mbtr,logsale,profit,debtratio`.

## Gaussian Workflows

Use the fixed-knot path when knot locations are treated as known:

- `fit_fixed_knots_gaussian_vi`
- `fit_gaussian_vi(..., free_knots=False)`

Use the full Gaussian path when coefficients, shrinkage, covariance, and moving knots
should all live in the variational state:

- `fit_full_gaussian_vi`
- `fit_gaussian_vi(..., free_knots="full")`

Use the marginalized Gaussian path when coefficients should be integrated out during VI:

- `fit_marginal_gaussian_vi`
- `fit_gaussian_vi(..., free_knots="marginal")`

The marginalized path usually has fewer variational parameters and lower Monte Carlo
variance for scoring because `B | Y, knots, shrinkage, Sigma` is available in closed form.
The full path remains useful as a direct representation of all model parameters and as a
cross-check against the marginalized implementation.

## Prediction And Scoring

Common helpers:

- `predict_mean(fit, x_new)`: posterior mean prediction at the variational mean.
- `predict_samples(fit, x_new, key, n_samples, include_noise=False)`: posterior latent or
  noisy predictive samples.
- `gaussian_lpds(fit, x_test, y_test, key, n_samples)`: log predictive density summary.
- `cross_validate_gaussian_vi(...)`: K-fold model scoring wrapper.

Marginalized Gaussian helpers:

- `marginal_fit_beta_posterior(fit, z=None)`: exact conditional posterior for the
  coefficient matrix `B`.
- `marginal_fit_predictive_moments(fit, x_new, z=None, include_noise=False)`: exact
  predictive mean and covariance at fixed non-coefficient parameters.
- `marginal_fit_log_predictive_density(fit, x_new, y_new, z=None)`: exact Gaussian log
  predictive density with `B` integrated out.

## Minimal Marginal Gaussian Example

```python
import jax
import jax.numpy as jnp

from movingknots.basis import design_matrix
from movingknots.fit import fit_marginal_gaussian_vi, gaussian_lpds, predict_mean

x = jnp.linspace(-1.0, 1.0, 20)[:, None]
true_knots = {"thinplate.a": jnp.array([[-0.45]])}
initial_knots = {"thinplate.a": jnp.array([[0.75]])}
spline_config = {
    "comp": ("intercept", "covariates", "thinplate.a"),
    "thinplate.a.locate": (1,),
}
beta_true = jnp.array([[0.0], [0.0], [4.0]])
y = design_matrix(x, true_knots, spline_config) @ beta_true

fit = fit_marginal_gaussian_vi(
    x,
    y,
    knots=initial_knots,
    spline_config=spline_config,
    free_additive=True,
    key=jax.random.PRNGKey(0),
    n_steps=20,
    n_samples=1,
    p_matrix_types=("X'X", "identity"),
)

y_hat = predict_mean(fit, x)
score = gaussian_lpds(fit, x, y, key=jax.random.PRNGKey(1), n_samples=4)
```

## fformpp Forecast Performance Prediction

`movingknots.fformpp` is the Python/JAX migration path for the legacy R `fformpp`
application. It fits a multivariate moving-knot regression from time-series feature
matrices to forecast-error matrices, then predicts which forecasting method is expected
to perform best.

Preferred API:

```python
import jax

from movingknots import fformpp

train = fformpp.load_m3_example(n_rows=16)
test = fformpp.load_m1_example(n_rows=5)

fit = fformpp.fit(
    train.features[:, :4],
    train.errors,
    model_names=train.model_names,
    surface_knots=1,
    additive_knots=1,
    key=jax.random.PRNGKey(0),
    fit_kwargs={
        "n_steps": 2,
        "n_samples": 1,
        "learning_rate": 0.01,
        "init_scale": 0.01,
    },
)

predicted_errors = fformpp.predict(
    fit,
    test.features[:, :4],
    key=jax.random.PRNGKey(1),
    n_samples=2,
)
selected = fformpp.individual_forecast(
    predicted_errors,
    actual_errors=test.errors,
    model_names=train.model_names,
)
```

Runnable compact example:

```bash
python -m examples.fformpp_workflow
```

Bundled example data:

- `load_m3_example()`: M3-style training features and forecast errors.
- `load_m1_example()`: M1-style evaluation features and forecast errors.

Differences from legacy R `fformpp`:

- The Python implementation uses JAX mean-field VI through `fit_marginal_gaussian_vi`
  instead of the original R MCMC machinery.
- Test features are standardized with the training-set mean and standard deviation
  saved in the fit object. The R `predict_fformpp` standardized each test batch by its
  own mean and standard deviation.
- `fformpp.fit` and `fformpp.predict` are the preferred names. `fit_fformpp` and
  `predict_fformpp` remain as compatibility aliases.
- The repository-level untracked `fformpp/` directory is treated as legacy source
  reference for this migration unless it is explicitly moved or committed later.

## R Fixtures

Files under `tests/fixtures` include R-generated reference values. They are used to ensure
the Python/JAX port matches the original implementation for selected likelihood, prior,
and posterior calculations. Treat them as parity checks, not as the primary runtime path.
Use `legacy/` only for fixture regeneration, paper comparison, or algorithm archaeology.

Normal development should prioritize the current root-level Python structure:

- package API in `movingknots/`
- tests in `tests/`
- examples in `examples/`
- reference R code in `legacy/`

## Citation

```bibtex
@article{LiF2013EfficientBayesian,
  title = {Efficient Bayesian Multivariate Surface Regression},
  author = {Li, Feng and Villani, Mattias},
  date = {2013-06},
  journaltitle = {Scandinavian Journal of Statistics},
  volume = {40},
  number = {4},
  pages = {706--723},
  issn = {1467-9469},
  doi = {10.1111/sjos.12022},
  url = {https://arxiv.org/abs/1110.3689},
  urldate = {2022-11-11},
  language = {en},
  keywords = {Bayesian inference,free knots,Markov chain Monte Carlo,splines,surface regression},
}
```
