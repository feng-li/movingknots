% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models__linear__etc__delta.K.R
\name{delta.K}
\alias{delta.K}
\title{Give the gradient for a diagnoal matrix (K) w.r.t. its diagonal elements.}
\usage{
delta.K(diag.K, power)
}
\arguments{
\item{diag.K}{NA}

\item{power}{"scalar".  The power for K to obtain the gradient for vec(K^(power))
w.r.t. diag(K)'.}

\item{K}{"matrix".  The diagonal matrix. p-by-p}
}
\value{
"matrix".  The gradient, pp-by-p
}
\description{
Details are available in the paper.
}
\author{
Feng Li, Department of Statistics, Stockholm University, Sweden.
}
