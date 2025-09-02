library(MSGARCH)
library(kableExtra)
y <- as.numeric(r1)

# 2) Specify single‑regime EGARCH with skewed Student‑t
spec <- CreateSpec(
  variance.spec     = list(model = "eGARCH"),     # EGARCH(1,1)
  distribution.spec = list(distribution = "sstd"),# skewed Student‑t
  switch.spec       = list(do.mix = FALSE, K = 1)
)

# 3) Fit by ML
fit <- FitML(spec, data = y)

print(summary(fit))  # quick console summary
# This will return unconditional volatility per regime
unc_vol <- simulate(fit, do.unc.vol = TRUE)

print(unc_vol)
# Output is in daily units (σ). To annualize, multiply by sqrt(252):
ann_unc_vol1 <- as.numeric(unc_vol$CondVol) * sqrt(252)
names(ann_unc_vol1) <- c("Regime 1")
print(ann_unc_vol1)
# 4, MSEGARCH
spec2 <- CreateSpec(
  variance.spec     = list(model = "eGARCH"),     # EGARCH(1,1)
  distribution.spec = list(distribution = "sstd"),# skewed Student‑t
  switch.spec       = list(do.mix = FALSE, K =2L)
)
fit2 <- FitML(spec2, data = y)
print(summary(fit2))  # quick console summary
# This will return unconditional volatility per regime
unc_vol2 <- simulate(fit2, do.unc.vol = TRUE)
ann_unc_vol <- as.numeric(unc_vol2$CondVol) * sqrt(252)
names(ann_unc_vol) <- c("Regime 1", "Regime 2")
print(ann_unc_vol)
#Ljung box test for ARCH effect of residuals
library(MSGARCH)

# 1) Get conditional volatility σ_t , Change "fit" into "fit2" to for MS-E-GARCH model
sigma_t <- as.numeric(Volatility(fit))

# 2) Standardized residuals
z_t <- y / sigma_t    # where y is your return series used in FitML

# 3) Squared standardized residuals
z2 <- z_t^2

# 4) Ljung-Box test on squared residuals
#    e.g. lags 5, 10, 20 (common choices)
lags <- c(1,5,10,20,30)
lb_results <- sapply(lags, function(L) {
  test <- Box.test(z2, lag = L, type = "Ljung-Box")
  c(stat = unname(test$statistic), p.value = test$p.value)
})
t(lb_results)
# --- Standardized residuals ---------------------------------------------------
sigma_t <- as.numeric(Volatility(fit))   # conditional sd
z <- as.numeric(y) / sigma_t             # standardized residuals
# (Optional) de-mean if your model includes a mean:
z <- z - mean(z, na.rm = TRUE)

# --- Ljung–Box on standardized residuals (autocorrelation) -------------------
lags <- c(1, 5, 10, 20, 30)

lb_res_z <- sapply(lags, function(L) {
  test <- Box.test(z, lag = L, type = "Ljung-Box")
  c(stat = unname(test$statistic), p.value = test$p.value)
})
t(lb_res_z)

mu  <- mean(y)            # daily %
sig <- median(as.numeric(Volatility(fit)))  # typical daily sigma
q99 <- -2.7               # rough 99% quantile (heavier tails => larger magnitude)
rel_err <- abs(mu) / abs(sig*q99)
rel_err
