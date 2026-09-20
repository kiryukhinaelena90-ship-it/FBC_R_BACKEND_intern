getwd()
source("40_run_deterministic_p1.R")
setwd("/Users/leo/Documents/GitHub/FBC_R_BACKEND")
getwd()
source("40_run_deterministic_p1.R")
library(jsonlite)

cfg <- fromJSON(
  "request_example.json",
  simplifyVector = FALSE
)

cfg$confirmed_bounds <- list(
  owner_price = list(
    lower = cfg$owner$price,
    upper = cfg$owner$price * 1.20,
    source_type = "user_confirmed",
    reason = "Test",
    implementation = list(
      months_to_full = 1,
      mode = "step",
      source_type = "user_confirmed"
    )
  )
)

# То, что обычно добавляет api.R перед запуском P1
cfg$owner_holiday_days <- cfg$owner$holidays
cfg$employee_holiday_days <- cfg$employee$holidays
cfg$employer_addon_rate <- cfg$employee$employer_addon_rate
cfg$owner_pension_month <- 0
cfg$tax_month <- 0

cfg$employee$paid_hours_month <-
  cfg$employee$paid_available_hours_month

cfg$employee$contract_hours_week <-
  cfg$employee$hours_week

res <- fbc_run_deterministic_p1_40(
  cfg = cfg,
  root = getwd()
)
library(jsonlite)

cfg <- fromJSON(
  "request_example.json",
  simplifyVector = FALSE
)
cfg$confirmed_bounds <- list(
  owner_price = list(
    lower = cfg$owner$price,
    upper = cfg$owner$price * 1.20,
    source_type = "user_confirmed",
    reason = "Test",
    implementation = list(
      months_to_full = 1,
      mode = "step",
      source_type = "user_confirmed"
    )
  )
)

cfg$owner_holiday_days <- cfg$owner$holidays
cfg$employee_holiday_days <- cfg$employee$holidays
cfg$employer_addon_rate <- cfg$employee$employer_addon_rate
cfg$owner_pension_month <- 0
cfg$tax_month <- 0

cfg$employee$paid_hours_month <-
  cfg$employee$paid_available_hours_month

cfg$employee$contract_hours_week <-
  cfg$employee$hours_week
res <- fbc_run_deterministic_p1_40(
  cfg = cfg,
  root = getwd()
)
cfg$confirmed_bounds
cfg$owner$price
cfg$confirmed_bounds <- list(
  owner_price = list(
    lower = cfg$owner$price,
    upper = cfg$owner$price * 1.20,
    source_type = "user_confirmed",
    reason = "Test",
    implementation = list(
      months_to_full = 1,
      mode = "step",
      source_type = "user_confirmed"
    )
  )
)
cfg$confirmed_bounds
res <- fbc_run_deterministic_p1_40(
  cfg = cfg,
  root = getwd()
)
str(res, max.level = 3)
