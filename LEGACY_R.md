# Legacy R Code

The original R implementation is retained as legacy/reference code. The active
implementation target is the Python/JAX package at the repository root.

## Where It Lives

- `legacy/R/`: original R functions.
- `legacy/inst/`: original examples, tests, and scripts.
- `legacy/man/`: original R documentation.
- `data/`: historical datasets used by the original package and Python examples.

## What It Is Used For

Use the R code for:

- understanding the original algorithm from the paper,
- comparing Python behavior to the historical implementation,
- regenerating R parity fixtures in `tests/fixtures`,
- checking details that have not yet been ported to Python.

## Development Priority

Normal development should prioritize the current root-level Python structure:

- package API in `movingknots/`,
- tests in `tests/`,
- runnable examples in `examples/`,
- Python documentation in `README.md`.

Avoid broad R refactors, formatting sweeps, or behavior changes unless the task is
explicitly about fixture generation, reproducing a paper result, or validating the Python
port against the original implementation.

## Fixture Regeneration

When updating R-generated fixtures, keep the generator script and generated CSV together
under `tests/fixtures`. After regeneration, run:

```bash
python -m unittest discover -s tests
```
