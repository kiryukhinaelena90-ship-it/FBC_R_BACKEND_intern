# ==========================================
# 22_statistical_mc_bridge.R
# FUTURE Business Cockpit
# Statistical / Monte-Carlo bridge for Decision Engine
# Scope: Inhaber/in + 1 Mitarbeiter/in
# ==========================================
#
# Purpose:
# - connect real Cockpit state (module 18) to existing Monte-Carlo draws
# - apply external factors ONLY to exposed components
# - keep financing separate from the main optimizer
# - prepare draw-level outputs for Sobol/global sensitivity and later optimization
#
# Expected MC columns:
#   energie_aenderung_prozent
#   kraftstoff_aenderung_prozent
#   arbeitskosten_aenderung_prozent
#   kreditzins                     [only needed for variable-rate financing]
#
# Semantics:
# - Energie -> state$operating_costs["energy"] only
# - Kraftstoff -> state$operating_costs["fuel"] only
# - Arbeitskosten -> personnel_cost_month only
# - fixed credit -> unchanged by market-rate draws
# - variable credit -> user's own current rate remains the anchor;
#   market movement is applied as a percentage-point delta relative to an
#   explicitly supplied current market reference rate.
# - principal repayment is NOT an expense/result cost.
# ==========================================

`%||%` <- function(a,b) if(is.null(a) || length(a)==0L) b else a

assert_scalar22 <- function(x, name, lower = -Inf, upper = Inf) {
  if (length(x) != 1 || is.na(x) || !is.finite(x)) {
    stop(name, " muss ein endlicher numerischer Einzelwert sein.")
  }
  if (x < lower || x > upper) {
    stop(name, " liegt außerhalb [", lower, ", ", upper, "].")
  }
  invisible(TRUE)
}

validate_mc22 <- function(mc, require_interest = FALSE) {
  if (!is.data.frame(mc) || nrow(mc) < 2) {
    stop("mc muss ein data.frame mit mindestens 2 Simulationen sein.")
  }

  required <- c(
    "energie_aenderung_prozent",
    "kraftstoff_aenderung_prozent",
    "arbeitskosten_aenderung_prozent"
  )
  if (isTRUE(require_interest)) required <- c(required, "kreditzins")

  missing <- setdiff(required, names(mc))
  if (length(missing) > 0) {
    stop("MC-Daten fehlen Spalten: ", paste(missing, collapse = ", "))
  }

  for (nm in required) {
    if (any(!is.finite(mc[[nm]]))) {
      stop("MC-Spalte ", nm, " enthält NA/Inf.")
    }
  }
  invisible(TRUE)
}

infer_financing_fees22 <- function(state) {
  if (!isTRUE(state$financing$active)) return(0)
  base_interest <- state$financing$amount * state$financing$rate_pa / 1200
  max(0, state$financing$interest_plus_fees_month - base_interest)
}

variable_financing_result_cost22 <- function(
    state,
    market_rate_draws,
    market_reference_rate
) {
  assert_scalar22(market_reference_rate, "market_reference_rate", 0)

  if (!isTRUE(state$financing$active)) {
    return(rep(0, length(market_rate_draws)))
  }

  if (!identical(state$financing$binding, "variable")) {
    return(rep(state$financing$interest_plus_fees_month, length(market_rate_draws)))
  }

  own_rate <- state$financing$rate_pa
  fees_month <- infer_financing_fees22(state)

  # Own contractual rate is the anchor.
  # We transfer ONLY the modeled market movement in percentage points.
  rate_draw <- own_rate + (market_rate_draws - market_reference_rate)
  rate_draw <- pmax(0, rate_draw)

  state$financing$amount * rate_draw / 1200 + fees_month
}

build_mc_business_draws22 <- function(
    state,
    mc,
    owner_pension_month = 0,
    tax_month = 0,
    market_reference_rate = NULL
) {
  assert_scalar22(owner_pension_month, "owner_pension_month", 0)
  assert_scalar22(tax_month, "tax_month", 0)

  variable_finance <-
    isTRUE(state$financing$active) &&
    identical(state$financing$binding, "variable")

  validate_mc22(mc, require_interest = variable_finance)

  if (variable_finance && is.null(market_reference_rate)) {
    stop(
      "Bei variablem Kredit muss market_reference_rate explizit angegeben werden. ",
      "Der Nutzerzins wird nicht durch einen Marktzinssatz ersetzt."
    )
  }

  n <- nrow(mc)
  op_base <- state$operating_costs
  energy_base <- unname(op_base[["energy"]])
  fuel_base <- unname(op_base[["fuel"]])
  other_operating <-
    sum(op_base) - energy_base - fuel_base

  energy_draw <-
    energy_base * (1 + mc$energie_aenderung_prozent / 100)
  fuel_draw <-
    fuel_base * (1 + mc$kraftstoff_aenderung_prozent / 100)

  personnel_draw <-
    state$employee$personnel_cost_month *
    (1 + mc$arbeitskosten_aenderung_prozent / 100)

  if (any(energy_draw < 0) || any(fuel_draw < 0) || any(personnel_draw < 0)) {
    stop("MC-Änderungen erzeugen negative Kosten. Draws prüfen.")
  }

  financing_result_draw <- if (variable_finance) {
    variable_financing_result_cost22(
      state = state,
      market_rate_draws = mc$kreditzins,
      market_reference_rate = market_reference_rate
    )
  } else {
    rep(state$financing$interest_plus_fees_month, n)
  }

  operating_draw <- other_operating + energy_draw + fuel_draw

  # Revenue must follow the commercial demand state, never physical capacity.
  owner_revenue <- if(exists("fbc_state_owner_revenue25", mode="function")){
    fbc_state_owner_revenue25(state)
  } else {
    h <- state$owner$expected_billable_hours_month %||% state$owner$billable_hours_month
    state$owner$price * as.numeric(h)
  }

  employee_revenue <- if (isTRUE(state$employee$direct_billing)) {
    if(exists("fbc_state_employee_revenue25", mode="function")){
      fbc_state_employee_revenue25(state)
    } else {
      h <- state$employee$expected_billable_hours_month %||% state$employee$billable_hours_month
      state$employee$customer_price * as.numeric(h)
    }
  } else {
    0
  }

  result_before_owner_protection_tax <-
    owner_revenue +
    employee_revenue -
    operating_draw -
    personnel_draw -
    financing_result_draw

  net_available <-
    result_before_owner_protection_tax -
    state$owner$insurance_month -
    owner_pension_month -
    tax_month

  target <- state$owner$monthly_target

  data.frame(
    simulation =
      if ("simulation" %in% names(mc)) mc$simulation else seq_len(n),

    energie_aenderung_prozent = mc$energie_aenderung_prozent,
    kraftstoff_aenderung_prozent = mc$kraftstoff_aenderung_prozent,
    arbeitskosten_aenderung_prozent = mc$arbeitskosten_aenderung_prozent,
    kreditzins =
      if ("kreditzins" %in% names(mc)) mc$kreditzins else NA_real_,

    energy_cost_month = energy_draw,
    fuel_cost_month = fuel_draw,
    personnel_cost_month = personnel_draw,
    financing_result_cost_month = financing_result_draw,
    operating_costs_month = operating_draw,

    owner_revenue_month = rep(owner_revenue, n),
    employee_revenue_month = rep(employee_revenue, n),

    result_before_owner_protection_tax =
      result_before_owner_protection_tax,
    net_available = net_available,

    target_reached = net_available >= target,
    target_gap_eur = pmax(0, target - net_available),

    stringsAsFactors = FALSE
  )
}

summarise_mc_business22 <- function(draws, target) {
  assert_scalar22(target, "target")

  q <- function(x, p) as.numeric(quantile(x, p, na.rm = TRUE))

  list(
    n = nrow(draws),
    expected_net = mean(draws$net_available),
    median_net = median(draws$net_available),
    p10_net = q(draws$net_available, 0.10),
    p90_net = q(draws$net_available, 0.90),
    probability_target = mean(draws$net_available >= target),
    expected_gap_eur = mean(draws$target_gap_eur),
    p90_gap_eur = q(draws$target_gap_eur, 0.90),
    financing_varies =
      length(unique(round(draws$financing_result_cost_month, 10))) > 1
  )
}

build_statistical_bridge22 <- function(
    state,
    mc,
    owner_pension_month = 0,
    tax_month = 0,
    market_reference_rate = NULL
) {
  draws <- build_mc_business_draws22(
    state = state,
    mc = mc,
    owner_pension_month = owner_pension_month,
    tax_month = tax_month,
    market_reference_rate = market_reference_rate
  )

  list(
    schema_version = "22.1",
    draws = draws,
    summary = summarise_mc_business22(
      draws,
      target = state$owner$monthly_target
    ),
    exposure = list(
      energy = state$operating_costs[["energy"]] > 0,
      fuel = state$operating_costs[["fuel"]] > 0,
      labor = state$employee$personnel_cost_month > 0,
      interest =
        isTRUE(state$financing$active) &&
        identical(state$financing$binding, "variable")
    ),
    financing_role = "alternative_or_external_uncertainty_only",
    notes = c(
      "Keine Günstig/Erwartet/Ungünstig-Karten erforderlich.",
      "Draw-Level-Verteilung dient der internen Unsicherheitsanalyse.",
      "Sobol/global sensitivity folgt nach diesem Bridge-Test."
    )
  )
}

cat(
  "\n22 Statistical MC Bridge geladen.\n",
  "Externe Faktoren wirken nur auf tatsächlich exponierte Cockpit-Komponenten.\n",
  "Fester Kredit bleibt unverändert; variabler Nutzerzins bleibt Anker.\n",
  sep = ""
)
