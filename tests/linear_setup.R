## The Main file for the file
rm(list=ls())
gc()
setwd("~/workplace/MovingKnots/R")

## LOAD DEPENDENCES
## The packages are loaded automatically when needed.
## But you have to install them first

## Dependences
require("MCMCpack") 
require("mvtnorm")

## LOAD FUNCTIONS
sourceDir(c("stable", "new"), recursive = TRUE)

## DATA
DATA <- as.matrix(read.table("data/lidar.txt",header = TRUE))
DATA.std <- StdData(DATA,method=2)

## model Name
model.name <- 'linear'

## PARAMETERS
Y <- y <- DATA.std[,2,drop = FALSE]
x <- DATA.std[,1,drop = FALSE]
link <- "identity"

l0 <- 2
l <- 2


## Configure
k <- 20
n <- dim(x)[1]
p <- dim(Y)[2]
q <- 1+l+k*(l-l0+1)

## Initial Values
xi <- Make.Knots(x,k,"es")
Sigma <- diag(p)
B <- matrix(rnorm(q*p),q,p)

## PRIORS
set.seed(123456)
ka <- 1/n
M <- matrix(rnorm(q*p),q,p) # prior mean of B, Set.seed(1,2,3,4,5,6)
n0 <- 1 # Inverse Wishart prameters
S0 <- diag(p)

## GRADIENT AND HESSIAN TEST
## TODO!
## The Hessian may sigular with bad initial vaules.

tmp.grad_xi <- gradient_xi(Y,x,xi,l0,l,n0,S0,B,ka,gradient.prior.xi=0) # The marginal method
tmp.grad_vecB <- gradient_vecB(B,Sigma,x,xi,l0,l,link,gradient.prior.vecB=0)
tmp.grad_xi_condi <- gradient_xi_condi(B,Sigma,x,xi,l0,l,link,gradient.prior.xi=0)
tmp.grad_vech_Sigma <- grad_vech_Sigma(B,Sigma,x,xi,l0,l,link,gradient.prior.Sigma=0)

tmp.hess_xi <- hessian_approx(tmp.grad_xi,"OPG")
tmp.hess_vecB <- hessian_approx(tmp.grad_vecB,"OPG")
tmp.hess_xi_condi <- hessian_approx(tmp.grad_xi_condi,"OPG")
tmp.hess_vech_Sigma<- hessian_approx(tmp.grad_vech_Sigma,"OPG")

