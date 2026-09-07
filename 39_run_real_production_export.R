# ============================================================
# 39_run_real_production_export.R
# FUTURE Business Cockpit
# REAL production export runner
#
# This is the bridge from real Cockpit inputs + real MC data
# to the production decision JSON consumed by the HTML.
#
# It DOES NOT invent:
# - price bounds
# - controllable cost ranges
# - implementation times
# - variable-cost classification
# - robustness threshold
# - debt-service threshold
#
# Financing is exported as a separate alternative branch.
# ============================================================

`%||%` <- function(a,b) if(is.null(a) || length(a)==0L) b else a

fbc_source39 <- function(file){
  if(!file.exists(file)) stop("Required FBC module missing: ",file)
  source(file,local=.GlobalEnv)
}

fbc_load_modules39 <- function(){
  mods <- c(
    "09_finanzierung_inputs_costs_FINAL.R",
    "10_gesamtkosten_kapitaldienst.R",
    "11_business_break_even.R",
    "18_cockpit_input_contract.R",
    "19_reality_constraints_V2.R",
    "21_differential_influence_V2.R",
    "22_statistical_mc_bridge.R",
    "26_constrained_optimizer_kkt.R",
    "27C_shapley_exact_bilinear_fix.R",
    "29_production_statistical_cost_layer.R",
    "FBC_ENGINE_CORE_FAST_FIXED.R",
    "34_economic_ranking_PRODUCTION.R",
    "35_time_post_validation.R",
    "36_financing_alternative.R",
    "37_production_rules_data_layer_V2_1.R",
    "38_production_decision_pipeline.R"
  )
  invisible(lapply(mods,fbc_source39))
}

fbc_required_config39 <- function(cfg){
  req <- c(
    "owner","operating_costs","employee","financing",
    "owner_holiday_days","employee_holiday_days","employer_addon_rate",
    "owner_pension_month","tax_month",
    "confirmed_bounds","cost_evidence",
    "mc_file","horizon_months","post_target_months",
    "variable_cost_keys"
  )
  miss <- setdiff(req,names(cfg))
  if(length(miss)) stop("Production config missing: ",paste(miss,collapse=", "))

  if(is.null(cfg$confirmed_bounds) || !length(cfg$confirmed_bounds))
    stop("confirmed_bounds is empty. Production optimizer may not invent decision ranges.")

  if(is.null(cfg$variable_cost_keys))
    stop("variable_cost_keys must be explicitly provided (use character(0) only if that is a confirmed classification).")

  allowed_costs <- c(
    "office","energy","vehicle","fuel","software",
    "accounting","business_insurance","material","marketing","other"
  )
  bad <- setdiff(cfg$variable_cost_keys,allowed_costs)
  if(length(bad)) stop("Unknown variable_cost_keys: ",paste(bad,collapse=", "))

  if(!file.exists(cfg$mc_file))
    stop("MC file not found: ",cfg$mc_file)

  invisible(TRUE)
}

# Apply one optimizer solution to a canonical expected state.
fbc_state_from_solution39 <- function(base_state,problem,solution){
  x <- as.numeric(solution)
  names(x) <- names(solution)
  if(is.null(names(x)) || any(!problem$registry$name %in% names(x))){
    names(x) <- problem$registry$name
  }

  s <- base_state

  if("owner_price" %in% names(x))
    s$owner$price <- as.numeric(x["owner_price"])

  if("owner_hours" %in% names(x))
    s$owner$available_hours_month <- as.numeric(x["owner_hours"])

  for(k in names(s$operating_costs)){
    nm <- paste0("cost_",k)
    if(nm %in% names(x)) s$operating_costs[k] <- as.numeric(x[nm])
  }

  if("employee_customer_price" %in% names(x))
    s$employee$customer_price <- as.numeric(x["employee_customer_price"])

  if("employee_billable_hours" %in% names(x))
    s$employee$available_hours_month <- as.numeric(x["employee_billable_hours"])

  # Keep employee revenue internally consistent with candidate solution.
  s$employee$revenue_month <- if(isTRUE(s$employee$direct_billing)){
    s$employee$customer_price * s$employee$available_hours_month
  } else 0

  s
}

fbc_business_break_even39 <- function(
    state,
    variable_cost_keys,
    employee_variable_cost_per_hour=0
){
  total_owner_h <- as.numeric(state$owner$available_hours_month)
  total_emp_h <- if(isTRUE(state$employee$direct_billing))
    as.numeric(state$employee$available_hours_month) else 0
  units <- total_owner_h + total_emp_h

  revenue <- state$owner$price*total_owner_h +
    if(isTRUE(state$employee$direct_billing))
      state$employee$customer_price*total_emp_h else 0

  if(!is.finite(units) || units<=0 || !is.finite(revenue) || revenue<0)
    stop("Cannot calculate weighted Business Break-even: invalid billable capacity/revenue.")

  weighted_price <- revenue/units

  variable_month <- if(length(variable_cost_keys))
    sum(state$operating_costs[variable_cost_keys]) else 0

  variable_per_unit <- variable_month/units

  fixed_operating <- sum(state$operating_costs) - variable_month

  # Financing result cost (interest + fees) is a fixed result cost.
  fixed_result_costs <-
    fixed_operating +
    state$employee$personnel_cost_month +
    state$financing$interest_plus_fees_month

  db_per_unit <- weighted_price - variable_per_unit
  reachable <- is.finite(db_per_unit) && db_per_unit>0

  if(reachable){
    be_units <- fixed_result_costs/db_per_unit
    be_revenue <- be_units*weighted_price
  } else {
    be_units <- Inf
    be_revenue <- Inf
  }

  safety_margin <- revenue-be_revenue

  emp_be_hours <- NA_real_
  emp_ok <- TRUE
  if(isTRUE(state$employee$direct_billing)){
    emp_db <- state$employee$customer_price-employee_variable_cost_per_hour
    emp_be_hours <- if(emp_db>0)
      state$employee$personnel_cost_month/emp_db else Inf
    emp_ok <- is.finite(emp_be_hours) &&
      emp_be_hours<=state$employee$available_hours_month+1e-8
  }

  list(
    margin_eur=safety_margin,
    ok=isTRUE(reachable) && safety_margin>=-1e-8,
    employee_ok=emp_ok,
    business=list(
      weighted_price_per_billable_hour=weighted_price,
      variable_cost_per_billable_hour=variable_per_unit,
      fixed_result_costs_month=fixed_result_costs,
      break_even_hours=be_units,
      break_even_revenue=be_revenue,
      projected_revenue=revenue,
      safety_margin=safety_margin,
      reachable=reachable
    ),
    employee=list(
      applicable=isTRUE(state$employee$direct_billing),
      customer_price=state$employee$customer_price,
      variable_cost_per_hour=employee_variable_cost_per_hour,
      personnel_cost_month=state$employee$personnel_cost_month,
      billable_hours=state$employee$available_hours_month,
      break_even_hours=emp_be_hours,
      hours_above_break_even=if(isTRUE(state$employee$direct_billing))
        state$employee$available_hours_month-emp_be_hours else NA_real_,
      ok=emp_ok
    )
  )
}

fbc_run_real_export39 <- function(cfg,output_json="data/processed/fbc_decision_payload_production.json"){
  fbc_required_config39(cfg)
  fbc_load_modules39()

  mc <- read.csv(cfg$mc_file,check.names=FALSE,stringsAsFactors=FALSE)

  state <- build_cockpit_decision_state18(
    owner=cfg$owner,
    operating_costs=cfg$operating_costs,
    employee=cfg$employee,
    financing=cfg$financing,
    owner_holiday_days=cfg$owner_holiday_days,
    employee_holiday_days=cfg$employee_holiday_days,
    employer_addon_rate=cfg$employer_addon_rate
  )

  # Production employment/cost evidence layer BEFORE optimizer.
  state_rules <- state

  # V2.1 employment resolver expects paid/contract information.
  # These values must come from real Cockpit/contract data when applicable.
  state_rules$employee$contract_hours_week <-
    cfg$employee$contract_hours_week %||% cfg$employee$hours_week
  state_rules$employee$paid_hours_month <-
    cfg$employee$paid_hours_month %||% state$employee$available_hours_month
  state_rules$employee$hours_day <-
    cfg$employee$hours_day %||%
    if(cfg$employee$days_week>0) cfg$employee$hours_week/cfg$employee$days_week else NA_real_

  for(nm in c(
    "minimum_wage_exception_confirmed","werkstudent_exception_confirmed",
    "working_time_exception_confirmed","arbzg_compensation_confirmed",
    "employment_months_calendar_year","employment_workdays_calendar_year",
    "agriculture_short_term","berufsmaessigkeit_checked","regular_monthly_pay"
  )){
    if(!is.null(cfg$employee[[nm]])) state_rules$employee[[nm]] <- cfg$employee[[nm]]
  }

  prod_rules <- fbc_build_production_rules37v2(
    state=state_rules,
    confirmed=cfg$confirmed_bounds,
    cost_evidence=cfg$cost_evidence
  )
  fbc_validate_production_rules37v2(prod_rules)

  # Statistical layer on real MC draws.
  stat_layer <- build_all_cost_mc29(
    state=state,
    mc=mc,
    owner_pension_month=cfg$owner_pension_month,
    tax_month=cfg$tax_month,
    market_reference_rate=cfg$market_reference_rate %||% NULL
  )
  expected_state <- build_expected_state29(
    state=state,
    layer=stat_layer,
    use=cfg$expected_state_basis %||% "expected"
  )

  # Rebase analysis-only cost bounds to expected statistical state.
  cc <- fbc_rebase_cost_controls37v2(
    prod_rules$reality_cost_controls,
    expected_state
  )

  owner_hours_max <- prod_rules$registry$upper[
    prod_rules$registry$name=="owner_hours"
  ]
  if(!length(owner_hours_max))
    owner_hours_max <- expected_state$owner$available_hours_month

  reality <- build_reality_constraints19(
    state=expected_state,
    cost_controls=cc,
    owner_pension_month=cfg$owner_pension_month,
    tax_month=cfg$tax_month,
    max_owner_hours_month=owner_hours_max,
    financing_alternative=NULL,
    min_debt_service_ratio=cfg$min_debt_service_ratio %||% 1
  )

  influence <- build_differential_influence21(
    state=expected_state,
    reality=reality,
    owner_pension_month=cfg$owner_pension_month,
    tax_month=cfg$tax_month
  )

  get_upper <- function(nm){
    v <- prod_rules$registry$upper[prod_rules$registry$name==nm]
    if(length(v)) as.numeric(v[1]) else NULL
  }

  problem <- build_optimizer_problem26(
    state=expected_state,
    reality=reality,
    influence=influence,
    objective_mode=cfg$objective_mode %||% "reach_income_target",
    desired_net=cfg$desired_net %||% expected_state$owner$monthly_target,
    confirmed_bounds=list(
      owner_price_max=get_upper("owner_price")
    ),
    employee_bounds=list(
      customer_price_max=get_upper("employee_customer_price"),
      billable_hours_max=get_upper("employee_billable_hours")
    ),
    owner_pension_month=cfg$owner_pension_month,
    tax_month=cfg$tax_month
  )

  fast <- run_fbc_fast_candidate_search(
    problem=problem,
    max_actions=cfg$max_actions %||% nrow(problem$registry),
    verbose=isTRUE(cfg$verbose)
  )
  if(!isTRUE(fast$feasible) || !length(fast$candidate_pool))
    stop("No feasible KKT candidate pool: ",fast$reason %||% "unknown reason")

  # Implementation plan only for levers actually present in optimizer registry.
  ip <- prod_rules$implementation_plan
  ip <- ip[ip$lever %in% problem$registry$name,,drop=FALSE]
  missing_ip <- setdiff(problem$registry$name,ip$lever)
  if(length(missing_ip))
    stop("Missing evidenced implementation timing for optimizer levers: ",
         paste(missing_ip,collapse=", "))

  # Real callbacks used by post-decision validation.
  mc_eval <- function(solution){
    s <- fbc_state_from_solution39(expected_state,problem,solution)
    lay <- build_all_cost_mc29(
      state=s,
      mc=mc,
      owner_pension_month=cfg$owner_pension_month,
      tax_month=cfg$tax_month,
      market_reference_rate=cfg$market_reference_rate %||% NULL
    )
    lay$draws$net_available
  }

  be_eval <- function(solution){
    s <- fbc_state_from_solution39(expected_state,problem,solution)
    fbc_business_break_even39(
      state=s,
      variable_cost_keys=cfg$variable_cost_keys,
      employee_variable_cost_per_hour=
        cfg$employee_variable_cost_per_hour %||% 0
    )
  }

  capital_eval <- function(solution){
    s <- fbc_state_from_solution39(expected_state,problem,solution)
    cur <- calc_current_business_result19(
      s,
      owner_pension_month=cfg$owner_pension_month,
      tax_month=cfg$tax_month
    )
    revenue <- cur$owner_revenue+cur$employee_revenue
    ds <- calc_current_debt_capacity19(s,revenue)
    list(
      ratio=ds$debt_service_ratio,
      ok=ds$debt_service_covered,
      liquidity_after_debt_service=ds$liquidity_after_debt_service
    )
  }

  # Time path = implementation path on expected statistical state.
  # No extra demand ramp is invented. If a future monthly forecast evaluator
  # is provided by the product, it can replace this callback later.
  monthly_eval <- function(solution,month){
    problem$evaluate(solution)
  }

  # Financing branch: current contract + optional explicit refinancing/loan alternative.
  current_fin_contract <- list(
    active=isTRUE(cfg$financing$active),
    type=cfg$financing$type,
    amount=cfg$financing$amount,
    rate_pa=cfg$financing$rate_pa,
    months=cfg$financing$months,
    fees_month=cfg$financing$fees_month %||% 0,
    one_time_fee=cfg$financing$one_time_fee %||% 0,
    binding=cfg$financing$binding %||% NA_character_
  )

  ranking_policy <- cfg$ranking_policy %||% fbc_production_ranking_policy()

  result <- fbc_run_production_decision38(
    problem=problem,
    candidate_pool=fast$candidate_pool,
    implementation_plan=ip,
    horizon_months=cfg$horizon_months,
    post_target_months=cfg$post_target_months,
    ranking_policy=ranking_policy,
    monthly_evaluator=monthly_eval,
    mc_evaluator=mc_eval,
    break_even_evaluator=be_eval,
    capital_service_evaluator=capital_eval,
    min_target_probability=cfg$min_target_probability %||% NULL,
    min_debt_service_ratio=cfg$min_debt_service_ratio %||% NULL,
    shapley_fun=shapley_exact27C,
    current_financing=current_fin_contract,
    financing_alternative=cfg$financing_alternative %||% NULL,
    financing_comparison_horizon_months=
      cfg$financing_comparison_horizon_months %||% cfg$financing$months,
    free_cash_before_debt_service_month=
      cfg$free_cash_before_debt_service_month %||% NULL,
    financing_market_context=cfg$financing_market_context %||% NULL
  )

  if(!isTRUE(result$feasible))
    stop("Economic ranking produced no production recommendation: ",result$reason %||% "unknown")

  # Attach transparent production metadata useful to frontend/debugging.
  result$payload$production_meta <- list(
    input_schema=state$schema_version,
    statistical_schema=stat_layer$schema_version,
    reality_schema=reality$schema_version,
    candidate_count=length(fast$candidate_pool),
    optimizer_levers=problem$registry$name,
    employment_form=prod_rules$employment$form,
    financing_separate=TRUE,
    variable_cost_keys=cfg$variable_cost_keys,
    time_path_note="Implementation path on expected statistical state; no invented demand ramp."
  )

  dir.create(dirname(output_json),recursive=TRUE,showWarnings=FALSE)
  fbc_export_decision_json38(result,output_json)

  cat("\n=====================================\n")
  cat("FBC REAL PRODUCTION EXPORT CREATED\n")
  cat("File:",output_json,"\n")
  cat("Candidate count:",length(fast$candidate_pool),"\n")
  cat("Selected candidate:",result$selected_id,"\n")
  cat("Selected levers:",paste(result$selected$active_levers,collapse=" + "),"\n")
  cat("Time to target:",result$payload$target_path$time_to_target_months,"month(s)\n")
  cat("Financing branch:",result$payload$financing$status,"\n")
  cat("=====================================\n")

  invisible(result)
}

cat("\n39 Real Production Export Runner loaded.\n")
