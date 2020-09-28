# `movingknots`: Efficient Bayesian multivariate surface regression

Efficient Bayesian multivariate surface regression that combines both additive splines and
interactive splines, and a highly efficient Markov chain Monte Carlo algorithm to update
all the knot locations jointly.


The code is written in native R and should be compatible with R version >= 2.12


Copyright & Citation
--------------------

This code is base on our published paper:

* Li, F., & Villani, M. (2013). [Efficient Bayesian multivariate surface
  regression](http://dx.doi.org/10.1111/sjos.12022). _Scandinavian Journal of Statistics_, 40(4), 706-723.

* Please use this BibTeX entry to cite our work:

```bibtex
@article{li2013efficient,
  title={Efficient Bayesian multivariate surface regression},
  author={Li, Feng and Villani, Mattias},
  journal={Scandinavian Journal of Statistics},
  volume={40},
  number={4},
  pages={706--723},
  year={2013},
  publisher={Wiley Online Library}
}
```

Installation
-------------

- This package depends on Feng Li's [`flutils`](https://github.com/feng-li/flutils)
package. Please follow the link to download the latest version and install it.

``` r
devtools::install_github("feng-li/flutils")
```

- The package could be installed as a standard R library, provided that `flutils` and
`devtools` are installed.

```r
devtools::install_github("feng-li/movingknots")
```

Run the code
------------

Please take a look at the `inst/examples` folder which contains a few setup templates for
configuring the model. You should edit the file to point the location of `flutils`.


### Run inside R

* You only need to use the `source` function to source the model template and the
algorithm run automatically and the summary of the results will be printed on the R
console. For example

```R
source(file.path(system.file(package = "movingknots"), "examples","RUN_rajan_s_moving_2_plus_a_moving2.R"))
source(file.path(system.file(package = "movingknots"), "examples","RUN_rajan_s_moving_2_plus_a_moving2_SGLD.R"))
```
### Run with Rscript (with Linux)

* Make sure that `#! /usr/bin/Rscript` or `#! /usr/bin/env Rscript` is on the first line of your R script.

* You need make sure the script is executable in a terminal

```sh
chmod +x movingknots/inst/examples/RUN_rajan_s_moving_2_plus_a_moving2.R
chmod +x movingknots/inst/examples/RUN_rajan_s_moving_2_plus_a_moving2_SGLD.R
```
* And then just execute it like other bash scripts

```sh
./movingknots/inst/examples/RUN_rajan_s_moving_2_plus_a_moving2.R
./movingknots/inst/examples/RUN_rajan_s_moving_2_plus_a_moving2_SGLD.R
```

Help and bug reports
--------------------

Please visit [issues](https://github.com/feng-li/movingknots/issues) for bug reports. For
further assistance, please contact the package author Feng Li <feng.li@cufe.edu.cn>.
