
## Combine all the running results
CombLOSS <- function(RDPath)
  {
    RDS <- file.path(RDPath, dir(RDPath))
    nRDS <- length(RDS)

    HwangLOSS <- NULL
    for(i in 1:nRDS)
      {
        load(RDS[i])
        HwangLOSS <- rbind(HwangLOSS, OUT.LOSS)
      }

    return(HwangLOSS)
  }

HwangRadiaLOSS <- CombLOSS("~/running/hwang/Results/radial")
HwangHarmonicLOSS <- CombLOSS("~/running/hwang/Results/harmonic")



## Make a box plot
HwangFixedCol <- "gray"
HwangFreeCol <- NA

boxplot(HwangRadiaLOSS, col = c(rep(HwangFreeCol, 3), rep(HwangFixedCol, 6)),
#        ylim = c(0, 0.012),
        axes = TRUE, notch = TRUE,
        main = "Radial",
        ylab = "LOSS",
        xlab = "No. of knots",
        names = c(10, 20, 40, 10, 20, 40, 60, 80, 100))
grid2(y.at = seq(0.002, 0.010, 0.001))
legend(6.5, 0.0108,  c("Free knots model",  "Fixed knots model"),
       fill  =  c(HwangFreeCol,  HwangFixedCol),
       bg = "white")

dev.copy2eps(file = "simul_radial.eps")

dev.new()
boxplot(HwangHarmonicLOSS, col = c(rep(HwangFreeCol, 3), rep(HwangFixedCol, 6)),
#        ylim = c(0, 0.012),
        axes = TRUE, notch = TRUE,
        main = "Harmonic",
        ylab = "LOSS",
        xlab = "No. of knots",
        names = c(10, 20, 40, 10, 20, 40, 60, 80, 100))
grid2(y.at = seq(0.05, 0.55, 0.05))
legend(6.5, 0.58,  c("Free knots model",  "Fixed knots model"),
       fill  =  c(HwangFreeCol,  HwangFixedCol),
       bg = "white")
dev.copy2eps(file = "simul_harmonic.eps")
