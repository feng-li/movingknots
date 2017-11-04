MovingKnots
===========

Cite

The code is written in native R and should be compatible with R version >= 2.12


Copyright & Citation
--------------------

Use this entry to cite this work:

Li & Villani, 2013, "Efficient Bayesian Multivariate Surface Regression", Scandinavian
Journal of Statistics, 40(4), pp. 706-723.


Speed up R (optional)
---------------------

The moving knots model requires intensive matrix operations and one may compile R and
link with fast BLAS library whenever possible.

Dependences
-----------

This package depends on Feng Li's `flutils` package
<https://bitbucket.org/fli/flutils/>. Please follow the link to download the latest
version and place it in a folder where the running template (see next session) can find.


Run the code
------------

Please take a look at the `example` folder which contains a few setup templates for
configuring the model. You should edit the file to point the location of `flutils`.

There are two ways to run the code:

### Run inside R

* You only need to use the `source` function to source the setup template and it
will run automatically and the summary of the results will be printed on the R
console. For example


        R> source("movingknots/inst/examples/rajan_s_moving_2_plus_a_moving2.R")


### Run with Rscript (with Linux)

* Make sure that `#!/usr/bin/Rscript` or `#!/usr/bin/env Rscript` is on the first line of your R script.

* You need make sure the script is executable in a terminal

        chmod +x movingknots/inst/examples/rajan_s_moving_2_plus_a_moving2.R

* And then just execute it like other bash scripts

        ./"movingknots/inst/examples/rajan_s_moving_2_plus_a_moving2.R

Help and bug reports
--------------------

For further assistance, please contact the package author Feng Li <feng.li@cufe.edu.cn>.

Sat Nov 04 16:52:18 CST 2017
