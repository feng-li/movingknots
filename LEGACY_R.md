# Legacy R Code

The original R implementation is retained in this repository as legacy/reference code.
The active implementation target is the Python/JAX package at the repository root.

## Where It Lives

- [legacy/R/](legacy/R/): original R functions.
- [legacy/inst/](legacy/inst/): original examples, tests, and scripts.
- [legacy/man/](legacy/man/): original R documentation.
- [data/](data/): historical datasets used by the original package.

## What It Is Used For

Use the R code for:

- understanding the original algorithm from the paper,
- comparing Python behavior to the historical implementation,
- regenerating R parity fixtures in `tests/fixtures`,
- checking details that have not yet been ported to Python.

## What Not To Do

Do not treat the R package as the primary runtime path for new work. Avoid broad R
refactors, formatting sweeps, or behavior changes unless the task is explicitly about:

- fixture generation,
- reproducing a paper result,
- validating a Python port against the original implementation.

Normal development should prioritize:

- Python package API in `movingknots`,
- Python tests in `tests`,
- runnable examples in `examples`,
- Python documentation in `README.md`.

## Fixture Regeneration

When updating R-generated fixtures, keep the generator script and generated CSV together
under `tests/fixtures`. After regeneration, run:

```bash
python -m unittest discover -s tests
```
