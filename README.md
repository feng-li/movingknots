Feng Li's Moving Knots Project
==============================

The code is written in native R and should be compatible with R version >= 2.12


Copyright
---------

See the each individual file.

Speed up R
----------

The moving knots model requires intensive matrix operations and one may compile R and
link with fast BLAS library whenever possible.

Run the code
------------

Please take a look at the `example` folder which contains a few setup templates
for configuring the model. There are two ways to run the code

* Run inside R

  You only need to use the `source` function to source the setup template and it
will run automatically and the summary of the results will be printed on the R
console.

* Run with Rscript

  You need make sure the script is executable

    chmod +x

  And then just execute it like other bash script

    ./rajan_example.R

Help and bug reports
--------------------

For further assistance, please contact the package author Feng Li <feng.li@stat.su.se>.


Mon Aug 29 17:26:33 CEST 2011
