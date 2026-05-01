xi2 <- OUT.Params.mode$xi
x1 <- seq(0, 1, length.out = 20)
x2 <- x1
x.mesh <- mesh.grid(x1, x2)
xx <- d.matrix(x, xi2, splineArgs)
B2 <- matrix(lm(Y~0+xx)$coef)

xx.mesh <- d.matrix(x.mesh, xi2, splineArgs)

Y.mesh <- xx.mesh%*%B2

rgl.surface(x1, x2, Y.mesh)


######################################################################

P3 <- crossprod(xx)
ka3 <- as.vector(OUT.Params.mode$ka)
B3 <- ka3^2/(1+ka3^2)*solve(P3)%*%(crossprod(xx, Y)+1/ka3^2*P3%*%M)
