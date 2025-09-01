library(MSGARCH)
library(readr)
library(dplyr)
library(data.table)
set.seed(123)
# --- 1) Read & clean  CSV -------------------------------------------------
csv_path <- "daily VN Index.csv"   

vni_raw <- read_csv(
  file = csv_path,
  col_types = cols(
    Date   = col_character(),
    Close  = col_character(),
    Open   = col_skip(),
    High   = col_skip(),
    Low    = col_skip(),
    Volume = col_skip(),
    `% Change` = col_skip()
  )
)

# Parse numbers safely
parse_num <- function(x) {
  as.numeric(gsub("[^0-9.\\-]", "", x))
}

vni <- vni_raw %>%
  mutate(
    Date  = as.Date(Date, format = "%d/%m/%Y"),
    Close = parse_num(Close)
  ) %>%
  arrange(Date) %>%
  filter(Date >= as.Date("2017-01-01") & Date <= as.Date("2024-12-31")) %>%
  mutate(ret = 100 * c(NA, diff(log(Close)))) %>%
  filter(!is.na(ret))

# De-mean log return make sure ret has no NA first:
# Fit AR(1) on the full return series

fit <- stats::arima(vni$ret, order = c(1,0,0), include.mean = TRUE, method = "ML")

# AR(1)-demeaned (innovations) return
vni$ret_ar1 <- as.numeric(stats::residuals(fit))

head(vni)
#------------------------------------
# Split VNI into 2 sub-samples
# Sub-sample 1: 2017 - 2021
vni_1 <- vni %>%
  filter(Date >= as.Date("2017-01-01") & Date <= as.Date("2021-12-31"))
r1 <- vni_1$ret_ar1
# Sub-sample 2: 2022 - 2024
vni_2 <- vni %>%
  filter(Date >= as.Date("2022-01-01") & Date <= as.Date("2024-12-31"))
r2 <- vni_2$ret_ar1

