



# 2) Specify models with different type of distributions
spec101 <- CreateSpec(
  # Change model "sGARCH" for GARCH, "eGARCH" for EGARCH, "gjrGARCH" for GJR-GARCH
  variance.spec     = list(model = "eGARCH"), 
  #Change distribution "norm" for normal, "std" for Student-t, "sstd" for skewed Student-t
  distribution.spec = list(distribution = "sstd"),
  # Change K = 1L for single regime models, K=2L for Markov Switching models 
  switch.spec       = list(do.mix = FALSE, K =1L)
)

  
# 3) Fit by ML
FitML(spec101, data = r1)
