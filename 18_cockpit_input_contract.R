# ==========================================
# 18_cockpit_input_contract.R
# FUTURE Business Cockpit
# Cockpit -> R canonical input/state contract
# Scope: Inhaber/in + 1 Mitarbeiter/in only
# ==========================================
#
# Purpose:
# - mirror the real fields/formulas of CLEAN V16
# - keep every operating cost line separate
# - prepare a stable state for sensitivity + constrained optimization
# - do NOT optimize yet
#
# Existing modules to be sourced by production pipeline:
# 08_personal_inputs_costs_V2.R
# 09_finanzierung_inputs_costs_FINAL.R
# 10_gesamtkosten_kapitaldienst.R
# 11_business_break_even.R
# 07b_monte_carlo_business.R / current MC layer
# ==========================================

assert_num18 <- function(x, name, lower = -Inf, upper = Inf) {
  if (length(x) != 1 || is.na(x) || !is.finite(x)) {
    stop(name, " muss ein endlicher numerischer Einzelwert sein.")
  }
  if (x < lower || x > upper) {
    stop(name, " liegt außerhalb [", lower, ", ", upper, "].")
  }
  invisible(TRUE)
}

FBC_EMP_COST_KEYS <- c(
  office = "empCostOffice",
  energy = "empCostEnergy",
  vehicle = "empCostVehicle",
  fuel = "empCostFuel",
  software = "empCostSoftware",
  accounting = "empCostAccounting",
  business_insurance = "empCostInsurance",
  material = "empCostMaterial",
  marketing = "empCostMarketing",
  other = "empCostOther"
)

FBC_COCKPIT_FIELD_MAP <- list(
  owner = c(
    price = "empOwnerPrice",
    hours_week = "empOwnerHours",
    days_week = "empOwnerDays",
    vacation_days = "empOwnerVacation",
    monthly_target = "empOwnerGoal",
    insurance_month = "empOwnerInsurance",
    pension_mode = "empPensionMode",
    pension_fixed_month = "empPensionFixed"
  ),
  employee = c(
    form = "empForm",
    wage_hour = "empWage",
    hours_week = "empHours",
    days_week = "empDays",
    vacation_days = "empVacation",
    direct_billing = "empBillable",
    customer_price = "empCustomerPrice"
  ),
  financing = c(
    active = "fbcFinActive",
    type = "fbcFinType",
    amount = "fbcFinAmount",
    rate_pa = "fbcFinRate",
    binding = "fbcFinBinding",
    months = "fbcFinMonths",
    fees_month = "fbcFinFees"
  ),
  costs = FBC_EMP_COST_KEYS
)

calc_monthly_capacity_v16 <- function(
    weekly_hours,
    days_week,
    vacation_days,
    holiday_days
) {
  assert_num18(weekly_hours, "weekly_hours", 0)
  assert_num18(days_week, "days_week", 1, 7)
  assert_num18(vacation_days, "vacation_days", 0)
  assert_num18(holiday_days, "holiday_days", 0)

  annual <- max(
    0,
    weekly_hours * 52 -
      (vacation_days + holiday_days) * (weekly_hours / days_week)
  )
  annual / 12
}

calc_employee_economics_v16 <- function(
    wage_hour,
    hours_week,
    days_week,
    vacation_days,
    holiday_days,
    employer_addon_rate,
    direct_billing = FALSE,
    customer_price = 0,
    extra_cost_month = 0
) {
  assert_num18(wage_hour, "wage_hour", 0)
  assert_num18(hours_week, "hours_week", 0)
  assert_num18(days_week, "days_week", 1, 7)
  assert_num18(vacation_days, "vacation_days", 0)
  assert_num18(holiday_days, "holiday_days", 0)
  assert_num18(employer_addon_rate, "employer_addon_rate", 0)
  assert_num18(customer_price, "customer_price", 0)
  assert_num18(extra_cost_month, "extra_cost_month", 0)

  contract_hours_month <- hours_week * 52 / 12
  available_hours_month <- calc_monthly_capacity_v16(
    hours_week,
    days_week,
    vacation_days,
    holiday_days
  )

  gross_month <- wage_hour * contract_hours_month
  employer_addons_month <- gross_month * employer_addon_rate
  personnel_cost_month <-
    gross_month + employer_addons_month + extra_cost_month

  revenue_month <- if (isTRUE(direct_billing)) {
    customer_price * available_hours_month
  } else {
    0
  }

  list(
    contract_hours_month = contract_hours_month,
    available_hours_month = available_hours_month,
    gross_month = gross_month,
    employer_addons_month = employer_addons_month,
    personnel_cost_month = personnel_cost_month,
    revenue_month = revenue_month,
    contribution_month = revenue_month - personnel_cost_month,
    full_cost_per_available_hour = if (available_hours_month > 0) {
      personnel_cost_month / available_hours_month
    } else {
      NA_real_
    }
  )
}

calc_financing_split_v16 <- function(
    active = FALSE,
    type = c("annuity", "amortizing"),
    amount = 0,
    rate_pa = 0,
    months = 36,
    fees_month = 0,
    binding = c("fixed", "variable")
) {
  type <- match.arg(type)
  binding <- match.arg(binding)

  if (!isTRUE(active)) {
    return(list(
      active = FALSE,
      interest_plus_fees_month = 0,
      principal_month = 0,
      debt_service_month = 0,
      rate_pa = 0,
      amount = 0,
      binding = binding
    ))
  }

  assert_num18(amount, "amount", 0)
  assert_num18(rate_pa, "rate_pa", 0)
  assert_num18(months, "months", 1)
  assert_num18(fees_month, "fees_month", 0)

  rm <- rate_pa / 1200
  interest_month <- amount * rm

  if (type == "annuity") {
    payment_month <- if (rm > 0) {
      amount * rm / (1 - (1 + rm)^(-months))
    } else {
      amount / months
    }
    principal_month <- max(0, payment_month - interest_month)
  } else {
    principal_month <- amount / months
    payment_month <- principal_month + interest_month
  }

  list(
    active = TRUE,
    interest_plus_fees_month = interest_month + fees_month,
    principal_month = principal_month,
    debt_service_month = payment_month + fees_month,
    rate_pa = rate_pa,
    amount = amount,
    binding = binding
  )
}

build_cockpit_decision_state18 <- function(
    owner,
    operating_costs,
    employee,
    financing,
    owner_holiday_days,
    employee_holiday_days,
    employer_addon_rate
) {
  missing_costs <- setdiff(names(FBC_EMP_COST_KEYS), names(operating_costs))
  if (length(missing_costs) > 0) {
    stop("Fehlende Betriebskostenfelder: ", paste(missing_costs, collapse = ", "))
  }

  operating_costs <- as.numeric(operating_costs[names(FBC_EMP_COST_KEYS)])
  names(operating_costs) <- names(FBC_EMP_COST_KEYS)
  if (any(!is.finite(operating_costs)) || any(operating_costs < 0)) {
    stop("Betriebskosten müssen vollständig, endlich und >= 0 sein.")
  }

  owner_capacity <- calc_monthly_capacity_v16(
    owner$hours_week,
    owner$days_week,
    owner$vacation_days,
    owner_holiday_days
  )

  emp_econ <- calc_employee_economics_v16(
    wage_hour = employee$wage_hour,
    hours_week = employee$hours_week,
    days_week = employee$days_week,
    vacation_days = employee$vacation_days,
    holiday_days = employee_holiday_days,
    employer_addon_rate = employer_addon_rate,
    direct_billing = employee$direct_billing,
    customer_price = employee$customer_price,
    extra_cost_month = employee$extra_cost_month %||% 0
  )

  fin <- do.call(calc_financing_split_v16, financing)

  list(
    schema_version = "18.0",
    source = "FBC_INHABER_MITARBEITER_CLEAN_V16.html",
    owner = list(
      price = owner$price,
      hours_week = owner$hours_week,
      days_week = owner$days_week,
      vacation_days = owner$vacation_days,
      available_hours_month = owner_capacity,
      monthly_target = owner$monthly_target,
      insurance_month = owner$insurance_month,
      pension_mode = owner$pension_mode,
      pension_fixed_month = owner$pension_fixed_month
    ),
    operating_costs = operating_costs,
    employee = c(employee, emp_econ),
    financing = fin,
    optimization_eligibility = list(
      price = TRUE,
      capacity = TRUE,
      costs = setNames(rep(FALSE, length(FBC_EMP_COST_KEYS)), names(FBC_EMP_COST_KEYS)),
      personnel = FALSE,
      financing_alternative = FALSE
    )
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x

cat(
  "\n18 Cockpit Input Contract geladen.\n",
  "Keine Optimierung automatisch ausgeführt.\n",
  "Kosten bleiben positionsweise getrennt.\n",
  sep = ""
)
