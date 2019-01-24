MovingKnots
===========

Efficient Bayesian multivariate surface regression that combines both additive splines and
interactive splines, and a highly efficient Markov chain Monte Carlo algorithm to update
all the knot locations jointly.


The code is written in native R and should be compatible with R version >= 2.12


Copyright & Citation
--------------------

This code is base on our published paper:

* Li, F., & Villani, M. (2013). Efficient Bayesian multivariate surface
  regression. _Scandinavian Journal of Statistics_, 40(4), 706-723.

* Please use this BibTeX entry to cite our work:

```
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





Speed up R (optional)
---------------------

The moving knots model requires intensive matrix operations and one may compile R and
link with fast BLAS library whenever possible.

Dependences
-----------

This package depends on Feng Li's [`flutils`](https://github.com/feng-li/flutils)
package. Please follow the link to download the latest version and place it in a folder
where the running template (see next session) can find.


Installation
-------------

The package could be installed as a standard R library by following the steps, provided
that `flutils` and `devtools` are installed. Assume the package folder is now cloned into
folder `code/movingknots`. Now within R

    project.flatten <- flutils::package.flatten("code/movingknots")
    devtools::document(project.flatten)
    devtools::install_local(project.flatten)

Run the code
------------

Please take a look at the `example` folder which contains a few setup templates for
configuring the model. You should edit the file to point the location of `flutils`.


### Run inside R

* You only need to use the `source` function to source the model template and the
algorithm run automatically and the summary of the results will be printed on the R
console. For example

        R> source(file.path(path.package("movingknots"),"examples","rajan_s_moving_2_plus_a_moving2.R"))

### Run with Rscript (with Linux)

* Make sure that `#!/usr/bin/Rscript` or `#!/usr/bin/env Rscript` is on the first line of your R script.

* You need make sure the script is executable in a terminal

        chmod +x movingknots/inst/examples/rajan_s_moving_2_plus_a_moving2.R

* And then just execute it like other bash scripts

        ./movingknots/inst/examples/rajan_s_moving_2_plus_a_moving2.R

Help and bug reports
--------------------

Please visit https://github.com/feng-li/movingknots/issues for bug reports. For further
assistance, please contact the package author Feng Li <feng.li@cufe.edu.cn>.
