# ==========================================
# 21_differential_influence.R
# FUTURE Business Cockpit
# Differential / marginal influence BEFORE optimization
# Scope: Inhaber/in + 1 Mitarbeiter/in
# ==========================================
#
# Purpose:
# - quantify local influence of controllable variables before optimization
# - preserve module 19 reality/feasibility restrictions
# - keep financing OUTSIDE the main decision vector
# - provide derivatives / cross-effects for the later Lagrange/KKT layer
#
# IMPORTANT:
# This is NOT the final optimizer.
# Taxes/pension can be supplied as monthly values; until the full tax adapter
# is connected, they are treated as fixed monthly amounts.
# ==========================================

assert_scalar21 <- function(x, name, lower = -Inf, upper = Inf) {
  if (length(x) != 1 || is.na(x) || !is.finite(x)) {
    stop(name, " muss ein endlicher numerischer Einzelwert sein.")
  }
  if (x < lower || x > upper) {
    stop(name, " liegt außerhalb [", lower, ", ", upper, "].")
  }
  invisible(TRUE)
}

# Generic deterministic evaluation point.
# Employee wage/personnel cost stays fixed unless a later personnel model
# explicitly unlocks and recalculates it.
evaluate_point21 <- function(
    state,
    owner_price = state$owner$price,
    owner_hours = state$owner$available_hours_month,
    operating_costs = state$operating_costs,
    employee_customer_price = state$employee$customer_price,
    employee_billable_hours = state$employee$available_hours_month,
    owner_pension_month = 0,
    tax_month = 0
) {
  assert_scalar21(owner_price, "owner_price", 0)
  assert_scalar21(owner_hours, "owner_hours", 0)
  assert_scalar21(employee_customer_price, "employee_customer_price", 0)
  assert_scalar21(employee_billable_hours, "employee_billable_hours", 0)
  assert_scalar21(owner_pension_month, "owner_pension_month", 0)
  assert_scalar21(tax_month, "tax_month", 0)

  if (any(!is.finite(operating_costs)) || any(operating_costs < 0)) {
    stop("operating_costs müssen vollständig, endlich und >= 0 sein.")
  }

  owner_revenue <- owner_price * owner_hours

  employee_revenue <- if (isTRUE(state$employee$direct_billing)) {
    employee_customer_price * employee_billable_hours
  } else {
    0
  }

  operating_total <- sum(operating_costs)
  personnel_cost <- state$employee$personnel_cost_month
  financing_result_cost <- state$financing$interest_plus_fees_month

  result_before_owner_protection_tax <-
    owner_revenue + employee_revenue -
    operating_total -
    personnel_cost -
    financing_result_cost

  net_available <-
    result_before_owner_protection_tax -
    state$owner$insurance_month -
    owner_pension_month -
    tax_month

  list(
    owner_revenue = owner_revenue,
    employee_revenue = employee_revenue,
    operating_total = operating_total,
    personnel_cost = personnel_cost,
    financing_result_cost = financing_result_cost,
    result_before_owner_protection_tax = result_before_owner_protection_tax,
    net_available = net_available
  )
}

finite_diff21 <- function(f, x0, h = NULL, lower = -Inf, upper = Inf) {
  assert_scalar21(x0, "x0")
  if (is.null(h)) h <- max(abs(x0) * 1e-5, 1e-5)
  assert_scalar21(h, "h", 0)

  x_lo <- max(lower, x0 - h)
  x_hi <- min(upper, x0 + h)

  if (x_hi == x_lo) return(NA_real_)
  if (x_lo == x0) return((f(x_hi) - f(x0)) / (x_hi - x0))
  if (x_hi == x0) return((f(x0) - f(x_lo)) / (x0 - x_lo))

  (f(x_hi) - f(x_lo)) / (x_hi - x_lo)
}

cross_partial21 <- function(f, x0, y0, hx = NULL, hy = NULL) {
  if (is.null(hx)) hx <- max(abs(x0) * 1e-5, 1e-5)
  if (is.null(hy)) hy <- max(abs(y0) * 1e-5, 1e-5)

  (f(x0 + hx, y0 + hy) -
   f(x0 + hx, y0 - hy) -
   f(x0 - hx, y0 + hy) +
   f(x0 - hx, y0 - hy)) / (4 * hx * hy)
}

build_differential_influence21 <- function(
    state,
    reality,
    owner_pension_month = 0,
    tax_month = 0
) {
  if (is.null(reality$optimization_allowed)) {
    stop("reality$optimization_allowed fehlt. Modul 19 V2 zuerst ausführen.")
  }

  base <- evaluate_point21(
    state = state,
    owner_pension_month = owner_pension_month,
    tax_month = tax_month
  )

  price0 <- state$owner$price
  hours0 <- state$owner$available_hours_month

  # Module 19 V2 exposes price as owner_price_min (scalar), not as a min/max list.
  # Until a defensible upper price bound is supplied, local differentiation is
  # allowed upward without inventing a production cap.
  price_min <- reality$bounds$owner_price_min
  price_max <- Inf
  hours_max <- reality$bounds$owner_hours_month_max

  d_price <- finite_diff21(
    function(p) evaluate_point21(
      state, owner_price = p, owner_hours = hours0,
      owner_pension_month = owner_pension_month, tax_month = tax_month
    )$net_available,
    x0 = price0,
    lower = price_min,
    upper = price_max
  )

  d_hours <- finite_diff21(
    function(h) evaluate_point21(
      state, owner_price = price0, owner_hours = h,
      owner_pension_month = owner_pension_month, tax_month = tax_month
    )$net_available,
    x0 = hours0,
    lower = 0,
    upper = hours_max
  )

  # Exact interaction for revenue = price * hours.
  d2_price_hours <- cross_partial21(
    function(p, h) evaluate_point21(
      state, owner_price = p, owner_hours = h,
      owner_pension_month = owner_pension_month, tax_month = tax_month
    )$net_available,
    price0, hours0
  )

  cost_rows <- reality$bounds$operating_costs
  cost_derivatives <- lapply(seq_len(nrow(cost_rows)), function(i) {
    key <- cost_rows$key[i]
    current <- cost_rows$current[i]
    controllable <- isTRUE(cost_rows$controllable[i])
    lo <- cost_rows$min_value[i]
    hi <- cost_rows$max_value[i]

    d <- finite_diff21(
      function(v) {
        costs <- state$operating_costs
        costs[[key]] <- v
        evaluate_point21(
          state, operating_costs = costs,
          owner_pension_month = owner_pension_month, tax_month = tax_month
        )$net_available
      },
      x0 = current,
      lower = lo,
      upper = hi
    )

    data.frame(
      variable = paste0("cost_", key),
      current = current,
      derivative_net_per_eur = d,
      controllable = controllable,
      min_value = lo,
      max_value = hi,
      stringsAsFactors = FALSE
    )
  })
  cost_derivatives <- do.call(rbind, cost_derivatives)

  main <- data.frame(
    variable = c("owner_price", "owner_hours"),
    current = c(price0, hours0),
    derivative_net = c(d_price, d_hours),
    controllable = c(
      isTRUE(reality$optimization_allowed$price),
      # hours can always be reduced in less-work mode, but increasing is
      # only possible up to the physical upper bound.
      TRUE
    ),
    stringsAsFactors = FALSE
  )

  # Employee variables are diagnostic only unless explicitly unlocked.
  employee_price_derivative <- if (isTRUE(state$employee$direct_billing)) {
    state$employee$available_hours_month
  } else {
    0
  }

  employee_hours_derivative <- if (isTRUE(state$employee$direct_billing)) {
    state$employee$customer_price
  } else {
    0
  }

  employee <- data.frame(
    variable = c("employee_customer_price", "employee_billable_hours", "employee_wage"),
    derivative_net = c(employee_price_derivative, employee_hours_derivative, NA_real_),
    controllable = c(FALSE, FALSE, FALSE),
    reason = c(
      "Diagnostisch; standardmäßig nicht freigegeben",
      "Diagnostisch; standardmäßig nicht freigegeben",
      "Ohne belastbare Kausalbeziehung kein Optimierungshebel"
    ),
    stringsAsFactors = FALSE
  )

  list(
    schema_version = "21.0",
    base_net = base$net_available,
    main = main,
    operating_costs = cost_derivatives,
    employee = employee,
    interactions = data.frame(
      variable_1 = "owner_price",
      variable_2 = "owner_hours",
      cross_partial_net = d2_price_hours,
      interpretation = "Preis und Stunden wirken gemeinsam multiplikativ auf den Inhaber-Umsatz.",
      stringsAsFactors = FALSE
    ),
    financing_role = "alternative_only",
    note = paste(
      "Lokale Ableitungen beschreiben die marginale Wirkung am aktuellen Punkt.",
      "Globale Unsicherheit/Sobol folgt erst nach Anbindung des Statistik- und Unsicherheitslayers."
    )
  )
}

cat(
  "\n21 Differential Influence geladen.\n",
  "Ableitungen werden VOR der Optimierung berechnet.\n",
  "Finanzierung bleibt außerhalb des Haupt-Optimierungsvektors.\n",
  sep = ""
)
