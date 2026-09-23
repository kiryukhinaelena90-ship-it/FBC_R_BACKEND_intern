# ==========================================
# 19_reality_constraints.R
# FUTURE Business Cockpit
# Reality / feasibility checks BEFORE optimization
# Scope: Inhaber/in + 1 Mitarbeiter/in
# ==========================================
#
# Purpose:
# - reject unrealistic decision space before any optimizer runs
# - preserve physical capacity, break-even, liquidity and debt-service safety
# - keep financing as an alternative, not an automatic shortcut
# - require explicit controllability bounds for cost reductions
#
# Expected prior sources:
# 18_cockpit_input_contract.R
# 09_finanzierung_inputs_costs_FINAL.R
# 10_gesamtkosten_kapitaldienst.R
# 11_business_break_even.R
# ==========================================

assert_flag19 <- function(x, name) {
  if (length(x) != 1 || is.na(x) || !is.logical(x)) {
    stop(name, " muss TRUE oder FALSE sein.")
  }
  invisible(TRUE)
}

assert_nonneg19 <- function(x, name) {
  if (length(x) != 1 || is.na(x) || !is.finite(x) || x < 0) {
    stop(name, " muss endlich und >= 0 sein.")
  }
  invisible(TRUE)
}

# Commercial billable hours are independent from physical/paid capacity.
fbc_owner_billable_hours19 <- function(state) {
  x <- state$owner$billable_hours_month
  if (is.null(x) || !is.finite(as.numeric(x)))
    stop("state$owner$billable_hours_month fehlt oder ist ungültig.")
  as.numeric(x)
}

fbc_employee_billable_hours19 <- function(state) {
  if (!isTRUE(state$employee$direct_billing)) return(0)
  x <- state$employee$billable_hours_month
  if (is.null(x) || !is.finite(as.numeric(x)))
    stop("state$employee$billable_hours_month fehlt oder ist ungültig.")
  as.numeric(x)
}

# Expected/realized billable hours can differ from planned billable hours
# after a price change. Planned hours remain the optimization lever.
fbc_owner_expected_hours19 <- function(state) {
  x <- state$owner$expected_billable_hours_month
  if(!is.null(x) && is.finite(as.numeric(x))) return(max(0,as.numeric(x)))
  fbc_owner_billable_hours19(state)
}

fbc_employee_expected_hours19 <- function(state) {
  if(!isTRUE(state$employee$direct_billing)) return(0)
  x <- state$employee$expected_billable_hours_month
  if(!is.null(x) && is.finite(as.numeric(x))) return(max(0,as.numeric(x)))
  fbc_employee_billable_hours19(state)
}


# ----------------------------------------------------------
# Employee utilization model for sustainable cost coverage
# ----------------------------------------------------------
#
# This is NOT a physical cap and does NOT alter commercial revenue.
# It is used only for employee cost-coverage / break-even diagnostics.
#
# Marginal hour weights by billable utilization:
#   0–70%    -> 0.95
#   70–85%   -> 1.00
#   85–100%  -> 0.85
#   >100%    -> 0.65
#
# Hours above paid capacity are NOT forbidden. They stay in the
# >100% segment and therefore receive the lower marginal weight.
# ----------------------------------------------------------

FBC_EMPLOYEE_UTIL_THRESHOLDS19 <- c(0.70, 0.85, 1.00)
FBC_EMPLOYEE_UTIL_WEIGHTS19 <- c(0.95, 1.00, 0.85, 0.65)

fbc_employee_capacity19 <- function(state) {
  candidates <- suppressWarnings(as.numeric(c(
    state$employee$paid_available_hours_month,
    state$employee$available_hours_month
  )))

  candidates <- candidates[
    is.finite(candidates) &
    candidates > 0
  ]

  if(length(candidates)) candidates[1] else NA_real_
}

fbc_employee_weighted_hours19 <- function(
    billable_hours,
    available_hours
) {
  h <- suppressWarnings(as.numeric(billable_hours)[1])
  a <- suppressWarnings(as.numeric(available_hours)[1])

  if(!is.finite(h) || h < 0)
    stop("billable_hours muss endlich und >= 0 sein.")

  if(!is.finite(a) || a <= 0) {
    return(list(
      billable_hours = h,
      available_hours = NA_real_,
      utilization_rate = NA_real_,
      effective_hours = FBC_EMPLOYEE_UTIL_WEIGHTS19[1] * h,
      zone = "capacity_unknown"
    ))
  }

  cuts <- c(
    FBC_EMPLOYEE_UTIL_THRESHOLDS19 * a,
    Inf
  )

  weights <- FBC_EMPLOYEE_UTIL_WEIGHTS19

  weighted <- 0
  lower <- 0

  for(i in seq_along(weights)) {
    upper <- cuts[i]
    segment_end <- min(h, upper)
    segment_hours <- max(0, segment_end - lower)

    weighted <- weighted + weights[i] * segment_hours

    if(h <= upper) break
    lower <- upper
  }

  utilization <- h / a

  zone <-
    if(utilization < FBC_EMPLOYEE_UTIL_THRESHOLDS19[1]) {
      "low"
    } else if(utilization <= FBC_EMPLOYEE_UTIL_THRESHOLDS19[2]) {
      "normal"
    } else if(utilization <= FBC_EMPLOYEE_UTIL_THRESHOLDS19[3]) {
      "high"
    } else {
      "overload"
    }

  list(
    billable_hours = h,
    available_hours = a,
    utilization_rate = utilization,
    effective_hours = weighted,
    zone = zone
  )
}

fbc_employee_utilization19 <- function(
    state,
    billable_hours = NULL
) {
  if(!isTRUE(state$employee$direct_billing)) {
    return(list(
      billable_hours = 0,
      available_hours = NA_real_,
      utilization_rate = NA_real_,
      effective_hours = 0,
      zone = "not_applicable"
    ))
  }

  h <- if(is.null(billable_hours)) {
    fbc_employee_expected_hours19(state)
  } else {
    suppressWarnings(as.numeric(billable_hours)[1])
  }

  fbc_employee_weighted_hours19(
    billable_hours = h,
    available_hours = fbc_employee_capacity19(state)
  )
}


fbc_employee_arithmetic_break_even_hours19 <- function(
    state,
    customer_price = state$employee$customer_price,
    variable_cost_per_hour = 0
) {
  if(!isTRUE(state$employee$direct_billing))
    return(NA_real_)

  price <- suppressWarnings(as.numeric(customer_price)[1])
  variable_cost <- suppressWarnings(as.numeric(variable_cost_per_hour)[1])
  personnel_cost <- suppressWarnings(
    as.numeric(state$employee$personnel_cost_month)[1]
  )

  if(
    !is.finite(price) ||
    !is.finite(variable_cost) ||
    !is.finite(personnel_cost)
  ) {
    return(NA_real_)
  }

  db <- price - variable_cost

  if(db <= 0)
    return(Inf)

  personnel_cost / db
}

fbc_employee_break_even_hours19 <- function(
    state,
    customer_price = state$employee$customer_price,
    variable_cost_per_hour = 0
) {
  if(!isTRUE(state$employee$direct_billing))
    return(NA_real_)

  price <- suppressWarnings(as.numeric(customer_price)[1])
  variable_cost <- suppressWarnings(as.numeric(variable_cost_per_hour)[1])
  personnel_cost <- suppressWarnings(
    as.numeric(state$employee$personnel_cost_month)[1]
  )

  if(
    !is.finite(price) ||
    !is.finite(variable_cost) ||
    !is.finite(personnel_cost) ||
    price < 0 ||
    variable_cost < 0 ||
    personnel_cost < 0
  ) {
    stop("Ungültige Mitarbeiterwerte für Break-even.")
  }

  if(personnel_cost <= 0)
    return(0)

  a <- fbc_employee_capacity19(state)

  if(!is.finite(a) || a <= 0) {
    slope <-
      price * FBC_EMPLOYEE_UTIL_WEIGHTS19[1] -
      variable_cost

    return(
      if(slope > 0)
        personnel_cost / slope
      else
        Inf
    )
  }

  cuts <- c(
    FBC_EMPLOYEE_UTIL_THRESHOLDS19 * a,
    Inf
  )

  weights <- FBC_EMPLOYEE_UTIL_WEIGHTS19

  deficit <- personnel_cost
  lower <- 0

  for(i in seq_along(weights)) {
    upper <- cuts[i]
    slope <- price * weights[i] - variable_cost

    if(is.infinite(upper)) {
      if(slope <= 0)
        return(Inf)

      return(
        lower + deficit / slope
      )
    }

    span <- upper - lower

    if(slope > 0 && deficit <= slope * span) {
      return(
        lower + deficit / slope
      )
    }

    deficit <- deficit - slope * span
    lower <- upper
  }

  Inf
}

fbc_owner_revenue19 <- function(state) {
  x <- state$owner$revenue_month
  if(!is.null(x) && is.finite(as.numeric(x))) return(as.numeric(x))
  state$owner$price * fbc_owner_expected_hours19(state)
}

fbc_employee_revenue19 <- function(state) {
  if(!isTRUE(state$employee$direct_billing)) return(0)
  x <- state$employee$revenue_month
  if(!is.null(x) && is.finite(as.numeric(x))) return(as.numeric(x))
  state$employee$customer_price * fbc_employee_expected_hours19(state)
}

# Explicit decision bounds. Nothing is guessed.
# For each cost line, controllable=TRUE is required before optimization may change it.
# min_value/max_value are absolute monthly euro bounds.
build_cost_control_bounds <- function(
    state,
    controls = NULL
) {
  current <- state$operating_costs
  keys <- names(current)

  out <- data.frame(
    key = keys,
    current = as.numeric(current),
    controllable = FALSE,
    min_value = as.numeric(current),
    max_value = as.numeric(current),
    reason = "Keine belastbare Änderungsgrenze hinterlegt",
    stringsAsFactors = FALSE
  )

  if (is.null(controls)) return(out)

  required <- c("key", "controllable", "min_value", "max_value")
  missing <- setdiff(required, names(controls))
  if (length(missing) > 0) {
    stop("controls fehlen Spalten: ", paste(missing, collapse = ", "))
  }

  for (i in seq_len(nrow(controls))) {
    k <- controls$key[i]
    if (!k %in% keys) stop("Unbekannte Kostenposition in controls: ", k)

    j <- match(k, out$key)
    ctrl <- isTRUE(controls$controllable[i])
    lo <- controls$min_value[i]
    hi <- controls$max_value[i]

    assert_nonneg19(lo, paste0(k, "$min_value"))
    assert_nonneg19(hi, paste0(k, "$max_value"))
    if (lo > hi) stop(k, ": min_value > max_value")
    if (current[k] < lo || current[k] > hi) {
      stop(k, ": aktueller Wert liegt außerhalb der angegebenen Grenzen.")
    }

    out$controllable[j] <- ctrl
    out$min_value[j] <- if (ctrl) lo else current[k]
    out$max_value[j] <- if (ctrl) hi else current[k]
    if ("reason" %in% names(controls) && !is.na(controls$reason[i])) {
      out$reason[j] <- as.character(controls$reason[i])
    } else if (ctrl) {
      out$reason[j] <- "Explizite Änderungsgrenze vorhanden"
    }
  }

  out
}

calc_current_business_result19 <- function(
    state,
    owner_pension_month = 0,
    tax_month = 0
) {
  assert_nonneg19(owner_pension_month, "owner_pension_month")
  assert_nonneg19(tax_month, "tax_month")

  owner_revenue <- fbc_owner_revenue19(state)
  employee_revenue <- fbc_employee_revenue19(state)
  operating <- sum(state$operating_costs)
  personnel <- state$employee$personnel_cost_month
  financing_result <- state$financing$interest_plus_fees_month
  owner_protection <-
    state$owner$insurance_month + owner_pension_month

  result_before_owner_protection_tax <-
    owner_revenue + employee_revenue -
    operating - personnel - financing_result

  net_available <-
    result_before_owner_protection_tax -
    owner_protection - tax_month

  goal <- state$owner$monthly_target
  gap_eur <- max(0, goal - net_available)
  gap_pct <- if (goal > 0) gap_eur / goal else NA_real_

  list(
    owner_revenue = owner_revenue,
    employee_revenue = employee_revenue,
    operating_costs = operating,
    personnel_costs = personnel,
    financing_result_cost = financing_result,
    owner_protection = owner_protection,
    tax_month = tax_month,
    result_before_owner_protection_tax = result_before_owner_protection_tax,
    net_available = net_available,
    monthly_target = goal,
    gap_eur = gap_eur,
    gap_pct = gap_pct
  )
}

# Generic break-even check grounded in the existing 11_business_break_even.R module.
# We do not invent a variable-cost rate here. Caller must pass one when defensible.
calc_reality_break_even19 <- function(
    state,
    business_price_per_unit,
    business_variable_cost_per_unit = 0,
    business_available_units_month,
    employee_variable_cost_per_hour = 0
) {
  if (!exists("calc_business_break_even", mode = "function")) {
    stop("calc_business_break_even() fehlt. 11_business_break_even.R zuerst laden.")
  }

  assert_nonneg19(business_price_per_unit, "business_price_per_unit")
  assert_nonneg19(business_variable_cost_per_unit, "business_variable_cost_per_unit")
  assert_nonneg19(business_available_units_month, "business_available_units_month")
  assert_nonneg19(employee_variable_cost_per_hour, "employee_variable_cost_per_hour")

  # Existing validated financing object can be passed separately in the production adapter.
  business_be <- calc_business_break_even(
    betriebskosten = state$operating_costs,
    personal = state$employee$personnel_cost_month,
    finanzierung = NULL,
    preis_pro_einheit = business_price_per_unit,
    variable_kosten_pro_einheit = business_variable_cost_per_unit,
    verfuegbare_einheiten_monat = business_available_units_month
  )

  emp_hours <- fbc_employee_expected_hours19(state)
  emp_price <- state$employee$customer_price
  emp_cost <- state$employee$personnel_cost_month

  emp_util <- fbc_employee_utilization19(
    state,
    billable_hours = emp_hours
  )

  employee_arithmetic_be_hours <-
    fbc_employee_arithmetic_break_even_hours19(
      state,
      customer_price = emp_price,
      variable_cost_per_hour = employee_variable_cost_per_hour
    )

  employee_be_hours <- fbc_employee_break_even_hours19(
    state,
    customer_price = emp_price,
    variable_cost_per_hour = employee_variable_cost_per_hour
  )

  sustainable_revenue <-
    if(isTRUE(state$employee$direct_billing))
      emp_price * emp_util$effective_hours
    else
      0

  sustainable_contribution <-
    sustainable_revenue -
    emp_cost -
    employee_variable_cost_per_hour * emp_hours

  list(
    business = business_be,
    employee = list(
      direct_billing = isTRUE(state$employee$direct_billing),
      billable_hours_month = emp_hours,
      effective_billable_hours = emp_util$effective_hours,
      available_hours_month = emp_util$available_hours,
      utilization_rate = emp_util$utilization_rate,
      utilization_zone = emp_util$zone,
      utilization_model = "piecewise_sustainable_billable_hours",
      utilization_thresholds = FBC_EMPLOYEE_UTIL_THRESHOLDS19,
      utilization_weights = FBC_EMPLOYEE_UTIL_WEIGHTS19,
      sustainable_revenue_month = sustainable_revenue,
      sustainable_result_contribution_month = sustainable_contribution,
      arithmetic_break_even_hours = employee_arithmetic_be_hours,
      break_even_hours = employee_be_hours,
      break_even_reachable = if (is.na(employee_be_hours)) {
        NA
      } else {
        employee_be_hours <= emp_hours
      }
    )
  )
}

# Current debt-service check based on validated 10_gesamtkosten_kapitaldienst.R semantics.
calc_current_debt_capacity19 <- function(
    state,
    monthly_revenue
) {
  assert_nonneg19(monthly_revenue, "monthly_revenue")

  debt_service <- state$financing$debt_service_month
  free_before_debt <-
    monthly_revenue -
    sum(state$operating_costs) -
    state$employee$personnel_cost_month

  if (debt_service <= 0) {
    ratio <- Inf
    covered <- TRUE
  } else {
    ratio <- free_before_debt / debt_service
    covered <- ratio >= 1
  }

  list(
    free_funds_before_debt_service = free_before_debt,
    debt_service_month = debt_service,
    debt_service_ratio = ratio,
    debt_service_covered = covered,
    liquidity_after_debt_service = free_before_debt - debt_service
  )
}

# Financing/refinancing alternative must be evaluated against the no-new-debt path.
# It is NOT automatically eligible merely because it injects cash.
evaluate_financing_alternative19 <- function(
    current_state,
    alternative_financing,
    projected_monthly_revenue,
    projected_operating_costs = current_state$operating_costs,
    projected_personnel_cost = current_state$employee$personnel_cost_month,
    min_debt_service_ratio = 1,
    require_positive_liquidity = TRUE
) {
  assert_nonneg19(projected_monthly_revenue, "projected_monthly_revenue")
  assert_nonneg19(projected_personnel_cost, "projected_personnel_cost")
  assert_nonneg19(min_debt_service_ratio, "min_debt_service_ratio")
  assert_flag19(require_positive_liquidity, "require_positive_liquidity")

  if (is.null(alternative_financing)) {
    return(list(
      eligible = FALSE,
      reason = "Keine Finanzierungsalternative angegeben"
    ))
  }

  if (!exists("calc_finanzierung_split", mode = "function")) {
    stop("calc_finanzierung_split() fehlt. 10_gesamtkosten_kapitaldienst.R zuerst laden.")
  }

  split <- calc_finanzierung_split(alternative_financing)
  op <- sum(projected_operating_costs)
  free_before_debt <-
    projected_monthly_revenue - op - projected_personnel_cost
  ratio <- if (split$kapitaldienst_monat > 0) {
    free_before_debt / split$kapitaldienst_monat
  } else {
    Inf
  }
  liquidity_after <- free_before_debt - split$kapitaldienst_monat

  ratio_ok <- ratio >= min_debt_service_ratio
  liquidity_ok <- !require_positive_liquidity || liquidity_after >= 0

  list(
    eligible = ratio_ok && liquidity_ok,
    debt_service_ratio = ratio,
    liquidity_after_debt_service = liquidity_after,
    financing_result_cost_month = split$finanzierungsaufwand_monat,
    debt_service_month = split$kapitaldienst_monat,
    ratio_ok = ratio_ok,
    liquidity_ok = liquidity_ok,
    reason = if (ratio_ok && liquidity_ok) {
      "Finanzierungsalternative erfüllt die vorgegebenen Tragfähigkeitsbedingungen."
    } else {
      paste(
        if (!ratio_ok) "Kapitaldienstquote unter Mindestgrenze." else "",
        if (!liquidity_ok) "Liquidität nach Kapitaldienst negativ." else ""
      )
    }
  )
}

build_reality_constraints19 <- function(
    state,
    cost_controls = NULL,
    owner_pension_month = 0,
    tax_month = 0,
    max_owner_hours_month = fbc_owner_billable_hours19(state),
    financing_alternative = NULL,
    financing_projection_revenue = NULL,
    min_debt_service_ratio = 1
) {
  assert_nonneg19(max_owner_hours_month, "max_owner_hours_month")

  current <- calc_current_business_result19(
    state,
    owner_pension_month = owner_pension_month,
    tax_month = tax_month
  )

  cost_bounds <- build_cost_control_bounds(state, cost_controls)

  monthly_revenue <- current$owner_revenue + current$employee_revenue
  debt <- calc_current_debt_capacity19(state, monthly_revenue)

  fin_alt <- if (!is.null(financing_alternative)) {
    if (is.null(financing_projection_revenue)) {
      stop("Für Finanzierungsalternative fehlt financing_projection_revenue.")
    }
    evaluate_financing_alternative19(
      current_state = state,
      alternative_financing = financing_alternative,
      projected_monthly_revenue = financing_projection_revenue,
      min_debt_service_ratio = min_debt_service_ratio
    )
  } else {
    list(eligible = FALSE, reason = "Keine Alternative geprüft")
  }

  list(
    schema_version = "19.1",
    stage = "pre_optimization_reality_check",
    current = current,
    bounds = list(
      owner_price_min = 0,
      owner_hours_month_current = fbc_owner_billable_hours19(state),
      owner_hours_month_max = max_owner_hours_month,
      operating_costs = cost_bounds
    ),
    debt_service = debt,
    optimization_allowed = list(
      price = TRUE,
      capacity = max_owner_hours_month > fbc_owner_billable_hours19(state),
      costs = setNames(cost_bounds$controllable, cost_bounds$key),
      personnel = FALSE
    ),
    financing_alternative = fin_alt
  )
}

cat(
  "\n19.1 Reality Constraints geladen.\n",
  "Dieser Schritt läuft VOR der Optimierung.\n",
  "Keine Kostenreduktion und kein Kredit werden automatisch angenommen.\n",
  sep = ""
)
