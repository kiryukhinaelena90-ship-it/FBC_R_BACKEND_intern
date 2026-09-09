# ============================================================
# 41_post_p1_monte_carlo.R
# FUTURE Business Cockpit
# Post-P1 Monte Carlo robustness layer.
#
# IMPORTANT:
# - does NOT rerun or change the optimizer decision;
# - applies recommendation.changes$proposed to the factual cfg state;
# - evaluates the selected/current state under the existing MC data layer;
# - if MC data or an auxiliary module fails, the deterministic P1 payload
#   is returned unchanged except for explicit audit/availability metadata.
# ============================================================

`%||%` <- function(a,b) if(is.null(a) || length(a)==0L) b else a

fbc_source41 <- function(file, envir){
  if(!file.exists(file)) stop("Required FBC module missing: ", file)
  source(file, local=envir)
}

fbc_load_modules41 <- function(root=getwd(), envir){
  mods <- c(
    "09_finanzierung_inputs_costs_FINAL.R",
    "10_gesamtkosten_kapitaldienst.R",
    "11_business_break_even.R",
    "18_cockpit_input_contract.R",
    "22_statistical_mc_bridge.R",
    "29_production_statistical_cost_layer.R"
  )
  invisible(lapply(file.path(root, mods), fbc_source41, envir=envir))
}

fbc_state_from_cfg41 <- function(cfg){
  state <- build_cockpit_decision_state18(
    owner = cfg$owner,
    operating_costs = cfg$operating_costs,
    employee = cfg$employee,
    financing = cfg$financing,
    owner_holiday_days = cfg$owner_holiday_days,
    employee_holiday_days = cfg$employee_holiday_days,
    employer_addon_rate = cfg$employer_addon_rate
  )

  state$legal_form <- cfg$legal_form %||% "freelance"
  state$trade_tax_rate <- cfg$trade_tax_rate %||% 0

  # Same factual-hours contract as deterministic runner 40.
  state$owner$physical_available_hours_month <- state$owner$available_hours_month
  state$owner$available_hours_month <- max(
    0,
    as.numeric(cfg$owner$billable_hours_month %||% state$owner$available_hours_month)
  )

  state$employee$paid_available_hours_month <- as.numeric(
    cfg$employee$paid_hours_month %||% state$employee$available_hours_month
  )

  state$employee$available_hours_month <- if(isTRUE(state$employee$direct_billing)) {
    max(
      0,
      as.numeric(cfg$employee$billable_hours_month %||% state$employee$available_hours_month)
    )
  } else 0

  if(is.finite(as.numeric(cfg$employee$personnel_cost_month %||% NA_real_))){
    state$employee$personnel_cost_month <- as.numeric(cfg$employee$personnel_cost_month)
  }

  state$employee$revenue_month <- if(isTRUE(state$employee$direct_billing)) {
    state$employee$customer_price * state$employee$available_hours_month
  } else 0

  state
}

fbc_apply_payload_changes41 <- function(state, payload){
  changes <- payload$recommendation$changes %||% list()
  if(!length(changes)) return(state)

  for(ch in changes){
    lever <- as.character(ch$lever %||% "")
    proposed <- suppressWarnings(as.numeric(ch$proposed %||% NA_real_))
    if(!nzchar(lever) || !is.finite(proposed)) next

    if(identical(lever, "owner_price")){
      state$owner$price <- proposed
    } else if(identical(lever, "owner_hours")){
      state$owner$available_hours_month <- proposed
    } else if(identical(lever, "employee_customer_price")){
      state$employee$customer_price <- proposed
    } else if(identical(lever, "employee_billable_hours")){
      state$employee$available_hours_month <- proposed
    } else if(grepl("^cost_", lever)){
      key <- sub("^cost_", "", lever)
      if(key %in% names(state$operating_costs)) state$operating_costs[key] <- proposed
    }
  }

  state$employee$revenue_month <- if(isTRUE(state$employee$direct_billing)) {
    state$employee$customer_price * state$employee$available_hours_month
  } else 0

  state
}

fbc_empty_robustness41 <- function(reason=NULL){
  list(
    available = FALSE,
    expected = NULL,
    target_probability = NULL,
    probability_not_target = NULL,
    p10 = NULL,
    p50 = NULL,
    p90 = NULL,
    n = NULL,
    reason = reason
  )
}

fbc_mc_risk_drivers41 <- function(layer, max_n=5L){
  rows <- list()

  for(k in FBC_COST_KEYS29){
    exposed <- isTRUE(layer$exposure$statistical_exposure[layer$exposure$key==k])
    if(exposed){
      x <- layer$draws[[paste0("cost_", k)]]
      rows[[length(rows)+1L]] <- data.frame(
        factor = k,
        variance = var(x),
        source = layer$exposure$source_label[layer$exposure$key==k][1],
        stringsAsFactors = FALSE
      )
    }
  }

  if(isTRUE(layer$labor_exposure)){
    rows[[length(rows)+1L]] <- data.frame(
      factor = "labor",
      variance = var(layer$draws$personnel_cost),
      source = "Arbeitskosten",
      stringsAsFactors = FALSE
    )
  }

  if(isTRUE(layer$interest_exposure)){
    rows[[length(rows)+1L]] <- data.frame(
      factor = "interest",
      variance = var(layer$draws$financing_result_cost),
      source = "Kreditzins",
      stringsAsFactors = FALSE
    )
  }

  if(!length(rows)) return(list())
  tab <- do.call(rbind, rows)
  tab <- tab[order(-tab$variance),,drop=FALSE]
  tab <- head(tab, max_n)

  lapply(seq_len(nrow(tab)), function(i){
    list(
      factor = as.character(tab$factor[i]),
      variance = as.numeric(tab$variance[i]),
      source = as.character(tab$source[i])
    )
  })
}

fbc_run_post_p1_mc_41 <- function(
    payload,
    cfg,
    root=getwd(),
    mc_file=file.path(root,"data/processed/fbc_monte_carlo_draws.csv"),
    cost_factor_map=NULL,
    labor_pct_column="arbeitskosten_aenderung_prozent",
    market_reference_rate=NULL
){
  out <- payload
  if(is.null(out) || !is.list(out)) stop("payload must be a P1 payload list.")

  # MC is an enrichment layer. Never make a valid deterministic payload fatal.
  fail_soft <- function(message){
    out$robustness <<- fbc_empty_robustness41(message)
    out$post_decision$mc_target_probability <<- NULL
    out$post_decision$mc_policy_ok <<- NULL
    out$audit$mc_executed <<- FALSE
    out$audit$mc_available <<- FALSE
    out$audit$mc_error <<- message
    out
  }

  tryCatch({
    fbc_load_modules41(root, environment())

    if(!file.exists(mc_file)){
      return(fail_soft(paste0("Monte Carlo data file not found: ", mc_file)))
    }

    mc <- read.csv(mc_file, stringsAsFactors=FALSE, check.names=FALSE)
    if(!nrow(mc)) return(fail_soft("Monte Carlo data file is empty."))

    state <- fbc_state_from_cfg41(cfg)
    state <- fbc_apply_payload_changes41(state, out)

    target <- suppressWarnings(as.numeric(out$current$monthly_target %||% state$owner$monthly_target))
    if(is.finite(target)) state$owner$monthly_target <- target

    map <- if(is.null(cost_factor_map)) default_cost_factor_map29(mc) else cost_factor_map

    layer <- build_all_cost_mc29(
      state = state,
      mc = mc,
      cost_factor_map = map,
      labor_pct_column = labor_pct_column,
      owner_pension_month = cfg$owner_pension_month %||% 0,
      tax_month = cfg$tax_month %||% 0,
      market_reference_rate = market_reference_rate
    )

    d <- layer$draws
    net <- d$net_available
    reached <- d$target_reached

    if(!length(net) || any(!is.finite(net)))
      return(fail_soft("Monte Carlo produced non-finite net results."))

    rob <- list(
      available = TRUE,
      expected = as.numeric(mean(net)),
      target_probability = as.numeric(mean(reached)),
      probability_not_target = as.numeric(mean(!reached)),
      p10 = as.numeric(quantile(net, .10, names=FALSE)),
      p50 = as.numeric(median(net)),
      p90 = as.numeric(quantile(net, .90, names=FALSE)),
      n = as.integer(length(net)),
      risk_drivers = fbc_mc_risk_drivers41(layer),
      method = "post-P1 Monte Carlo on the already selected deterministic state",
      decision_reoptimized = FALSE
    )

    out$robustness <- rob
    out$post_decision$mc_target_probability <- rob$target_probability
    # No invented policy threshold: probability is reported, not converted to pass/fail.
    out$post_decision$mc_policy_ok <- NULL

    if(is.null(out$production_meta)) out$production_meta <- list()
    methods <- out$production_meta$methods_used %||% character()
    out$production_meta$methods_used <- unique(c(methods, "post-P1 Monte Carlo robustness"))

    if(is.null(out$audit)) out$audit <- list()
    out$audit$mc_executed <- TRUE
    out$audit$mc_available <- TRUE
    out$audit$mc_error <- NULL
    out$audit$mc_data_file <- mc_file
    out$audit$mc_decision_reoptimized <- FALSE

    out
  }, error=function(e){
    fail_soft(conditionMessage(e))
  })
}

cat("\n41 Post-P1 Monte Carlo robustness layer loaded.\n")
