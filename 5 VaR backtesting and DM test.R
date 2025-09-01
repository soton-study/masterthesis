## Backtesting

## Create  specification for comparison
# Single regime models
eg <- CreateSpec(variance.spec = list(model = "eGARCH"), #Change "sGARCH" for GARCH, "gjrGARCH" for GJR-GARCH
                    distribution.spec = list(distribution = "sstd"),
                    switch.spec = list(K = 1))
# Markov switching regime models
mseg <- CreateSpec(variance.spec = list(model = "eGARCH"), #Change "sGARCH" for GARCH, "gjrGARCH" for GJR-GARCH
                        distribution.spec = list(distribution = "sstd"),
                        switch.spec = list(K = 2))
models <- list(eg, mseg)
v <- as.numeric(vni$ret_ar1) #Entire sample
n.its    <- length(r1)  # fit sample size
n.ots    <- length(v) - n.its  # number of out-of-sample evaluation

alpha    <- 0.01 # risk Level
k.update <- 30  # estimation frequency

## Initialization
VaR   <- matrix(NA, nrow = n.ots, ncol = length(models))
y.ots <- matrix(NA, nrow = n.ots, ncol = 1)
model.fit <- vector(mode = "list", length = length(models))

# iterate over out-of-sample time
for (i in 1:n.ots) {
  set.seed(123)
  cat("Backtest - Iteration: ", i, "\n")
  y.its    <- v[i:(n.its + i - 1)] # in-sample data
  y.ots[i] <- v[n.its + i]         # out-of-sample data
  
  # iterate over models
  for (j in 1:length(models)) {
    
    # do we update the model estimation
    if (k.update == 1 || i %% k.update == 1) {
      cat("Model", j, "is reestimated\n")
      model.fit[[j]] <- FitML(spec = models[[j]], data = y.its,
                              ctr = list(do.se = FALSE))
    }
    
    # calculate VaR 1-step ahead
    VaR[i,j] <- Risk(model.fit[[j]]$spec, par = model.fit[[j]]$par,
                     data = y.its,
                     n.ahead = 1,
                     alpha   = alpha,
                     do.es   = FALSE,
                     do.its  = FALSE)$VaR
  }
}

## Test the VaR
# install.packages("GAS") if needed
library("GAS")
UC.pval <- DQ.pval  <- vector("double", length(models))
QL <- vector("list", length(models))  # store time series of quantile losses
for (j in 1:length(models)) {
  set.seed(123)
  test <- GAS::BacktestVaR(data  = y.ots,
                           VaR   = VaR[,j],
                           alpha = alpha)
  
  QL[[j]] <- test$Loss$LossSeries  # full time series of quantile loss
  UC.pval[j] <- test$LRuc[2]
  DQ.pval[j] <- test$DQ$pvalue
}
names(UC.pval) <- names(DQ.pval) <- names(QL) <- c("SR-sstd", "MS2-sstd")
print(UC.pval)
print(DQ.pval)

library(forecast)
# Compare model 2 (MS2-sstd) against model 1 (SR-sstd)
set.seed(123)
dm.test(QL[[2]], QL[[1]], alternative = "less", h = 1, power = 1)

