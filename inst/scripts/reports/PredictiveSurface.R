## The predictive surface and efficiency factor

###----------------------------------------------------------------------------
### INPUTS
###----------------------------------------------------------------------------

FITTED.file <- "~/running/JOB5463.tsfeature_s_moving_2_plus_a_moving_2+20171022@20.44.8744e8.Rdata"
TESTING.file <- "~/code/dgp/data/M3.features.Rdata"

load(FITTED.file)
load(TESTING.file)

period = 12
x.testing <- M3.features[M3.features[, 13] == period, c(1:10, 12, 14, 16:20)]

x <- StdData(x.testing, method = "norm-0-1")[["data"]]
## Which cross validation fold used
iCross <- 1

###----------------------------------------------------------------------------
### The scripts
###----------------------------------------------------------------------------

## OUT.IF.testing <- matrix(NA, nTest, length(RdataFiles))
nTest <- nrow(x.testing)
nDim <- dim(OUT.FITTED[["Params"]][["coefficients"]])[2]
nIter <- dim(OUT.FITTED[["Params"]][["coefficients"]])[3]
nCross <- dim(OUT.FITTED[["Params"]][["coefficients"]])[4]
Y.pred <- array(NA, c(nTest,nDim, nIter))

for(iCross in 1:nCross)
{
    for(i in 1:nIter)
    {
        knots.ilst <- knots.mat2list(OUT.FITTED[["Params"]][["knots"]][, , i, iCross], splineArgs)
        ## knots.s.mat[(1+q.s*(i-1)):(i*q.s), ] <- knots.ilst[["thinplate.s"]]
        ## knots.a.mat[(1+q.a1*(i-1)):(i*q.a1), ] <- matrix(knots.ilst[["thinplate.a"]], q.a1, 2)
        X.i <- d.matrix(x.testing, knots.ilst, splineArgs)
        B.i <- matrix(OUT.FITTED[["Params"]][["coefficients"]][, , i, iCross], , nDim)
        Y.pred[, , i] <- X.i %*% B.i # Transformed scale,  but should be OK here.
    }
}
Y.pred.mean <- apply(Y.pred, c(1, 2), mean)
load("~/code/dgp/data/M3MASEout.Rdata")
M3.fm <- apply(Y.pred.mean, 1, which.min)
M3MASEoutReal <- M3MASEout[M3.features[, 13] == period, ]
M3MASEOpt <- rep(NA, length(M3.fm))
for (i in 1: length(M3.fm))
{
    M3MASEOpt[i] <- M3MASEoutReal[i, M3.fm[i]]
}

out <- rbind(c(mean(M3MASEOpt), apply(M3MASEoutReal, 2, mean)),
             c(median(M3MASEOpt), apply(M3MASEoutReal, 2, median)))
dimnames(out) <- list(c("mean", "median"), c("mixture", colnames(M3MASEoutReal)))
out
