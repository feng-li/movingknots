# Python/JAX Implementation Plan for `movingknots`

## Purpose

Implement a Python version of the `movingknots` R package for Bayesian multivariate surface regression. The Python version should preserve the statistical model from Li and Villani (2013), use JAX for differentiable numerical computation, and replace the current tailored Newton/MCMC inference machinery with variational Bayes.

The original R code uses analytic gradients and tailored Metropolis-Hastings proposals to update moving knot locations, shrinkage parameters, and covariance parameters. In the Python version, JAX automatic differentiation should provide gradients, and variational inference should provide a faster deterministic approximation to the posterior.

## Target Model

The model remains:

```text
Y = X_o B_o + X_a(theta_a) B_a + X_s(theta_s) B_s + E
E_i ~ N_p(0, Sigma)
```

where:

- `Y` is an `n x p` multivariate response matrix.
- `X_o` contains the intercept and original covariates.
- `X_a(theta_a)` contains additive thin-plate spline basis functions.
- `X_s(theta_s)` contains surface/interacting thin-plate spline basis functions.
- `theta_a` are scalar additive knot locations.
- `theta_s` are multivariate surface knot locations.
- `B` contains regression coefficients.
- `Sigma` is the response covariance matrix.
- `lambda` or `K` are shrinkage parameters for coefficient blocks.

The initial Python implementation should prioritize the Gaussian linear model. GLM extensions can be deferred.

## Main Design Shift

The R implementation uses:

```text
analytic marginal posterior gradients
+ K-step Newton approximation
+ tailored Metropolis-Hastings
+ posterior sampling of B after MCMC
```

The Python/JAX implementation should use:

```text
JAX differentiable log joint / ELBO
+ variational posterior family
+ stochastic or full-batch ELBO optimization
+ posterior summaries from variational parameters
```

The coefficient matrix `B` can either be:

1. Integrated out analytically, matching the R marginal posterior logic.
2. Included directly in the variational posterior.

Recommended first implementation: include `B` directly in the variational posterior. This is simpler, easier to validate with JAX, and avoids immediately porting all marginal likelihood algebra. A later optimized version can integrate out `B`.

## Proposed Python Package Layout

```text
movingknots_py/
  __init__.py
  basis.py
  model.py
  priors.py
  variational.py
  inference.py
  prediction.py
  diagnostics.py
  data.py
  utils.py

tests/
  test_basis.py
  test_logprob.py
  test_variational_shapes.py
  test_prediction.py
  test_against_r_fixtures.py

examples/
  rajan.py
  hwang.py
  simulated_surface.py
```

## Phase 1: Core Basis and Data Structures

Implement spline basis construction first.

Tasks:

- Implement `thinplate_surface_basis(x, knots_s)`.
- Implement `thinplate_additive_basis(x, knots_a, additive_counts)`.
- Implement `design_matrix(x, knots, spline_config)`.
- Match R behavior for:
  - intercept inclusion,
  - raw covariates,
  - additive spline columns,
  - surface spline columns,
  - column ordering.
- Implement knot flattening/unflattening utilities equivalent to:
  - `knots_list2mat`
  - `knots_mat2list`

Validation:

- Create small deterministic fixtures.
- Compare Python design matrices against R-generated design matrices for the same `x`, knots, and spline configuration.
- Verify behavior at zero distance in the thin-plate basis. Use a numerically stable convention for `r^2 log(r)` with value `0` at `r = 0`.

## Phase 2: Probabilistic Model in JAX

Implement the full log joint density.

Parameters:

- `B`: coefficient matrix, shape `q x p`.
- `theta_s`: surface knots.
- `theta_a`: additive knots.
- `log_lambda`: transformed shrinkage parameters.
- `L_Sigma`: unconstrained Cholesky parameterization for response covariance.

Priors:

- `B | Sigma, lambda` follows the shrinkage prior from the paper.
- `Sigma` uses an inverse-Wishart equivalent if practical, or a more JAX-friendly LKJ/separation prior if preferred.
- `log_lambda` uses the log-normal prior from the paper.
- knots use normal priors centered at initial k-means/equal-spaced/random knot locations.

Recommended implementation detail:

- Use unconstrained parameters internally.
- Transform to constrained parameters inside the model:
  - `lambda = exp(log_lambda)`,
  - `Sigma = L @ L.T`,
  - optional bounded knot transform if knots should stay inside covariate range.

Deliverables:

- `log_likelihood(params, data, config)`.
- `log_prior(params, data, config)`.
- `log_joint(params, data, config)`.

Validation:

- Check all shapes explicitly.
- Compare `log_likelihood` against R `linear_logpost(..., callParam = "likelihood")`.
- Run JAX `grad(log_joint)` on small examples and verify finite gradients.

## Phase 3: Variational Family

Start with a mean-field Gaussian variational family on unconstrained parameters:

```text
q(z) = Normal(mu, diag(sigma^2))
```

where `z` is the flattened unconstrained parameter vector.

Initial variational variables:

- `mu_z`
- `rho_z`, with `sigma_z = softplus(rho_z) + jitter`

Later improvements:

- block-diagonal covariance by parameter group,
- low-rank plus diagonal Gaussian,
- structured variational posterior for `B`,
- natural-gradient updates for conjugate blocks.

ELBO:

```text
ELBO = E_q[log p(Y, params) - log q(params)]
```

Use the reparameterization trick:

```text
z = mu_z + sigma_z * epsilon
epsilon ~ Normal(0, I)
```

Tasks:

- Implement parameter flattening/unflattening with a stable PyTree schema.
- Implement `sample_q`.
- Implement `log_q`.
- Implement Monte Carlo ELBO estimator.
- Optimize with Optax Adam or AdamW.

Validation:

- ELBO should increase on toy examples.
- Gradients should be finite.
- Posterior means should recover simulated low-dimensional surfaces.

## Phase 4: Inference API

Provide a user-facing API similar to:

```python
fit = MovingKnotsVB(
    spline_config=spline_config,
    prior_config=prior_config,
    vi_config=vi_config,
).fit(x, y)

pred = fit.predict(x_new, n_samples=500)
summary = fit.summary()
```

The fit object should store:

- variational parameters,
- training ELBO history,
- posterior mean parameters,
- posterior samples,
- spline configuration,
- prior configuration,
- optimizer state or metadata,
- random seed.

Important options:

- number of surface knots,
- additive knots per covariate,
- knot initialization method,
- whether knots are fixed or free,
- minibatch size,
- number of ELBO samples per step,
- optimizer learning rate,
- maximum iterations,
- convergence tolerance.

## Phase 5: Prediction and Model Comparison

Implement prediction using posterior samples from the variational distribution.

Prediction outputs:

- posterior mean surface,
- posterior standard deviation surface,
- predictive mean,
- predictive covariance or intervals,
- pointwise log predictive density.

Implement model comparison:

- D-fold cross-validation LPDS, matching the paper.
- Optional WAIC/ELPD-style diagnostics later.

Validation:

- Reproduce simple simulated examples.
- Check whether free knots improve held-out LPDS relative to fixed knots.
- Compare rough behavior against the R examples, not exact posterior samples.

## Phase 6: R-to-Python Parity Checklist

Map the important R functions to Python equivalents:

| R Function | Python Target |
| --- | --- |
| `d.matrix` from `flutils` | `basis.design_matrix` |
| `make.knots` from `flutils` | `data.initialize_knots` |
| `knots_list2mat` | `utils.flatten_knots` |
| `knots_mat2list` | `utils.unflatten_knots` |
| `P.matrix` | `priors.make_p_matrices` |
| `Sigma4betaFun` | `priors.coefficient_prior_covariance` |
| `linear_logpost` | `model.log_joint` / `model.log_likelihood` |
| `linear_gradhess` | JAX `grad`, `jacfwd`, `hessian` if needed |
| `MovingKnots_MCMC` | `inference.MovingKnotsVB.fit` |
| `LogPredScore` | `diagnostics.lpds` |
| `FitDiagnosis` | `diagnostics.surface_loss` |

The Python code should not port the many hand-coded derivative helper functions unless needed for performance. JAX should own differentiation.

## Phase 7: Numerical and Statistical Risks

Key risks:

- Mean-field VB may underestimate posterior uncertainty.
- Knot posteriors can be multimodal because of label switching.
- Free knots can collapse to similar locations without constraints or repulsive priors.
- Thin-plate basis values can produce numerical issues near zero distances.
- Full covariance over all parameters may become too expensive for larger knot counts.
- Optimizing knots and shrinkage jointly may require careful learning-rate schedules.

Mitigations:

- Start with small simulations and fixed knots.
- Then enable free additive knots.
- Then enable free surface knots.
- Use multiple random restarts.
- Sort or softly identify additive knots per covariate when useful.
- Consider weak repulsive penalties between surface knots if collapse is severe.
- Track ELBO, LPDS, and posterior predictive checks.
- Use double precision in JAX for validation runs.

## Phase 8: Implementation Order

Recommended order:

1. Implement basis functions and design matrix.
2. Implement Gaussian likelihood with fixed knots.
3. Fit Bayesian linear regression with VB and fixed knots.
4. Add shrinkage parameters.
5. Add free additive knots.
6. Add free surface knots.
7. Add multivariate response covariance.
8. Add LPDS cross-validation.
9. Compare against R fixtures and paper examples.
10. Optimize performance with JIT, batching, and structured variational families.

## Minimum Viable Version

The first usable version should support:

- Gaussian response.
- One or more response variables.
- intercept and original covariates.
- additive and surface thin-plate spline bases.
- fixed or free knots.
- mean-field Gaussian VB over unconstrained parameters.
- posterior predictive mean and uncertainty.
- cross-validation LPDS.

This version does not need:

- exact reproduction of the R MCMC sampler,
- K-step Newton updates,
- hand-coded analytic derivatives,
- GLM support,
- RJMCMC or variable selection,
- exact inverse-Wishart matching if a better JAX-compatible covariance prior is used.

## Success Criteria

The Python/JAX implementation is successful when:

- design matrices match the R implementation on fixtures,
- JAX gradients are stable and finite,
- ELBO optimization converges on toy data,
- posterior predictive surfaces recover simulated surfaces,
- free-knot models improve held-out LPDS over fixed-knot baselines on selected examples,
- the API can run the Rajan and Hwang-style experiments with concise Python scripts.

