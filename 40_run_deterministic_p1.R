# ============================================================
# 40_run_deterministic_p1.R
# FUTURE Business Cockpit
# Deterministic P1:
# confirmed real-world bounds -> optimizer
# WITHOUT MC / Sobol / robustness thresholds
# ============================================================

`%||%` <- function(a,b) if(is.null(a) || length(a)==0L) b else a

fbc_source40 <- function(file){
  if(!file.exists(file)) stop("Required FBC module missing: ", file)
  source(file, local=.GlobalEnv)
}

fbc_load_modules40 <- function(root=getwd()){
  mods <- c(
    "09_finanzierung_inputs_costs_FINAL.R",
    "10_gesamtkosten_kapitaldienst.R",
    "11_business_break_even.R",
    "18_cockpit_input_contract.R",
    "19_reality_constraints_V2.R",
    "21_differential_influence_V2.R",
    "26_constrained_optimizer_kkt.R",
    "27C_shapley_exact_bilinear_fix.R",
    "FBC_ENGINE_CORE_FAST_FIXED.R",
    "34_economic_ranking_PRODUCTION.R",
    "35_time_post_validation.R",
    "36_financing_alternative.R",
    "37_production_rules_data_layer_V2_1.R",
    "38_production_decision_pipeline.R"
  )
  
  invisible(lapply(file.path(root, mods), fbc_source40))
}

fbc_state_from_solution40 <- function(base_state, problem, solution){
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
    nm <- paste0("cost_", k)
    if(nm %in% names(x))
      s$operating_costs[k] <- as.numeric(x[nm])
  }
  
  if("employee_customer_price" %in% names(x))
    s$employee$customer_price <- as.numeric(x["employee_customer_price"])
  
  if("employee_billable_hours" %in% names(x))
    s$employee$available_hours_month <- as.numeric(x["employee_billable_hours"])
  
  s$employee$revenue_month <-
    if(isTRUE(s$employee$direct_billing))
      s$employee$customer_price * s$employee$available_hours_month
  else 0
  
  s
}

fbc_business_break_even40 <- function(
    state,
    variable_cost_keys,
    employee_variable_cost_per_hour=0
){
  owner_h <- as.numeric(state$owner$available_hours_month)
  
  emp_h <- if(isTRUE(state$employee$direct_billing))
    as.numeric(state$employee$available_hours_month)
  else 0
  
  units <- owner_h + emp_h
  
  revenue <-
    state$owner$price * owner_h +
    if(isTRUE(state$employee$direct_billing))
      state$employee$customer_price * emp_h
  else 0
  
  if(!is.finite(units) || units <= 0 || !is.finite(revenue) || revenue < 0)
    stop("Cannot calculate Business Break-even: invalid billable capacity/revenue.")
  
  weighted_price <- revenue / units
  
  variable_month <- if(length(variable_cost_keys))
    sum(state$operating_costs[variable_cost_keys])
  else 0
  
  variable_per_unit <- variable_month / units
  fixed_operating <- sum(state$operating_costs) - variable_month
  
  fixed_result_costs <-
    fixed_operating +
    state$employee$personnel_cost_month +
    state$financing$interest_plus_fees_month
  
  db_per_unit <- weighted_price - variable_per_unit
  reachable <- is.finite(db_per_unit) && db_per_unit > 0
  
  be_units <- if(reachable) fixed_result_costs / db_per_unit else Inf
  be_revenue <- if(reachable) be_units * weighted_price else Inf
  safety_margin <- revenue - be_revenue
  
  emp_be_hours <- NA_real_
  emp_ok <- TRUE
  
  if(isTRUE(state$employee$direct_billing)){
    emp_db <- state$employee$customer_price - employee_variable_cost_per_hour
    
    emp_be_hours <- if(emp_db > 0)
      state$employee$personnel_cost_month / emp_db
    else Inf
    
    emp_ok <-
      is.finite(emp_be_hours) &&
      emp_be_hours <= state$employee$available_hours_month + 1e-8
  }
  
  list(
    margin_eur = safety_margin,
    ok = isTRUE(reachable) && safety_margin >= -1e-8,
    employee_ok = emp_ok,
    
    business = list(
      weighted_price_per_billable_hour = weighted_price,
      variable_cost_per_billable_hour = variable_per_unit,
      fixed_result_costs_month = fixed_result_costs,
      break_even_hours = be_units,
      break_even_revenue = be_revenue,
      projected_revenue = revenue,
      safety_margin = safety_margin,
      reachable = reachable
    ),
    
    employee = list(
      applicable = isTRUE(state$employee$direct_billing),
      customer_price = state$employee$customer_price,
      variable_cost_per_hour = employee_variable_cost_per_hour,
      personnel_cost_month = state$employee$personnel_cost_month,
      billable_hours = state$employee$available_hours_month,
      break_even_hours = emp_be_hours,
      hours_above_break_even =
        if(isTRUE(state$employee$direct_billing))
          state$employee$available_hours_month - emp_be_hours
      else NA_real_,
      ok = emp_ok
    )
  )
}

fbc_run_deterministic_p1_40 <- function(cfg, root=getwd()){
  
  fbc_load_modules40(root)
  
  state <- build_cockpit_decision_state18(
    owner = cfg$owner,
    operating_costs = cfg$operating_costs,
    employee = cfg$employee,
    financing = cfg$financing,
    owner_holiday_days = cfg$owner_holiday_days,
    employee_holiday_days = cfg$employee_holiday_days,
    employer_addon_rate = cfg$employer_addon_rate
  )
  
  # factual billable hours from Cockpit
  state$owner$physical_available_hours_month <- state$owner$available_hours_month
  state$owner$available_hours_month <-
    max(0, as.numeric(cfg$owner$billable_hours_month %||%
                        state$owner$available_hours_month))
  
  state$employee$paid_available_hours_month <-
    as.numeric(cfg$employee$paid_hours_month %||%
                 state$employee$available_hours_month)
  
  state$employee$available_hours_month <-
    if(isTRUE(state$employee$direct_billing))
      max(0, as.numeric(cfg$employee$billable_hours_month %||%
                          state$employee$available_hours_month))
  else 0
  
  if(is.finite(as.numeric(cfg$employee$personnel_cost_month %||% NA_real_))){
    state$employee$personnel_cost_month <-
      as.numeric(cfg$employee$personnel_cost_month)
  }
  
  state$employee$revenue_month <-
    if(isTRUE(state$employee$direct_billing))
      state$employee$customer_price * state$employee$available_hours_month
  else 0
  
  state_rules <- state
  
  state_rules$employee$contract_hours_week <-
    cfg$employee$contract_hours_week %||% cfg$employee$hours_week
  
  state_rules$employee$paid_hours_month <-
    cfg$employee$paid_hours_month %||%
    state$employee$paid_available_hours_month
  
  state_rules$employee$hours_day <-
    cfg$employee$hours_day %||%
    if(cfg$employee$days_week > 0)
      cfg$employee$hours_week / cfg$employee$days_week
  else NA_real_
  
  prod_rules <- fbc_build_production_rules37v2(
    state = state_rules,
    confirmed = cfg$confirmed_bounds,
    cost_evidence = cfg$cost_evidence
  )
  
  fbc_validate_production_rules37v2(prod_rules)
  
  cc <- fbc_rebase_cost_controls37v2(
    prod_rules$reality_cost_controls,
    state
  )
  
  owner_hours_max <-
    prod_rules$registry$upper[
      prod_rules$registry$name == "owner_hours"
    ]
  
  if(!length(owner_hours_max))
    owner_hours_max <- state$owner$available_hours_month
  
  reality <- build_reality_constraints19(
    state = state,
    cost_controls = cc,
    owner_pension_month = cfg$owner_pension_month %||% 0,
    tax_month = cfg$tax_month %||% 0,
    max_owner_hours_month = owner_hours_max,
    financing_alternative = NULL,
    min_debt_service_ratio = NULL
  )
  
  influence <- build_differential_influence21(
    state = state,
    reality = reality,
    owner_pension_month = cfg$owner_pension_month %||% 0,
    tax_month = cfg$tax_month %||% 0
  )
  
  get_upper <- function(nm){
    v <- prod_rules$registry$upper[
      prod_rules$registry$name == nm
    ]
    if(length(v)) as.numeric(v[1]) else NULL
  }
  
  problem <- build_optimizer_problem26(
    state = state,
    reality = reality,
    influence = influence,
    objective_mode = cfg$objective_mode %||% "reach_income_target",
    desired_net = cfg$desired_net %||% state$owner$monthly_target,
    
    confirmed_bounds = list(
      owner_price_max = get_upper("owner_price")
    ),
    
    employee_bounds = list(
      customer_price_max = get_upper("employee_customer_price"),
      billable_hours_max = get_upper("employee_billable_hours")
    ),
    
    owner_pension_month = cfg$owner_pension_month %||% 0,
    tax_month = cfg$tax_month %||% 0
  )
  
  fast <- run_fbc_fast_candidate_search(
    problem = problem,
    max_actions = cfg$max_actions %||% nrow(problem$registry),
    verbose = isTRUE(cfg$verbose)
  )
  
  if(!isTRUE(fast$feasible) || !length(fast$candidate_pool))
    stop(
      "No feasible KKT candidate pool: ",
      fast$reason %||% "unknown reason"
    )
  
  ip <- prod_rules$implementation_plan
  ip <- ip[ip$lever %in% problem$registry$name,,drop=FALSE]
  
  missing_ip <- setdiff(problem$registry$name, ip$lever)
  
  if(length(missing_ip))
    stop(
      "Missing evidenced implementation timing for optimizer levers: ",
      paste(missing_ip, collapse=", ")
    )
  
  monthly_eval <- function(solution, month){
    problem$evaluate(solution)
  }
  
  be_eval <- function(solution){
    s <- fbc_state_from_solution40(
      state,
      problem,
      solution
    )
    
    fbc_business_break_even40(
      s,
      cfg$variable_cost_keys %||% character(),
      cfg$employee_variable_cost_per_hour %||% 0
    )
  }
  
  capital_eval <- function(solution){
    s <- fbc_state_from_solution40(
      state,
      problem,
      solution
    )
    
    cur <- calc_current_business_result19(
      s,
      owner_pension_month = cfg$owner_pension_month %||% 0,
      tax_month = cfg$tax_month %||% 0
    )
    
    revenue <- cur$owner_revenue + cur$employee_revenue
    ds <- calc_current_debt_capacity19(s, revenue)
    
    list(
      ratio = ds$debt_service_ratio,
      ok = ds$debt_service_covered,
      liquidity_after_debt_service =
        ds$liquidity_after_debt_service
    )
  }
  
  current_fin_contract <- list(
    active = isTRUE(cfg$financing$active),
    type = cfg$financing$type,
    amount = cfg$financing$amount,
    rate_pa = cfg$financing$rate_pa,
    months = cfg$financing$months,
    fees_month = cfg$financing$fees_month %||% 0,
    one_time_fee = cfg$financing$one_time_fee %||% 0,
    binding = cfg$financing$binding %||% NA_character_
  )
  
  result <- fbc_run_production_decision38(
    problem = problem,
    candidate_pool = fast$candidate_pool,
    implementation_plan = ip,
    horizon_months = cfg$horizon_months %||% 12,
    post_target_months = cfg$post_target_months %||% 6,
    ranking_policy =
      cfg$ranking_policy %||%
      fbc_production_ranking_policy(),
    
    monthly_evaluator = monthly_eval,
    mc_evaluator = NULL,
    break_even_evaluator = be_eval,
    capital_service_evaluator = capital_eval,
    
    min_target_probability = NULL,
    min_debt_service_ratio = NULL,
    
    shapley_fun = shapley_exact27C,
    
    current_financing = current_fin_contract,
    financing_alternative = NULL,
    financing_comparison_horizon_months =
      cfg$financing$months,
    free_cash_before_debt_service_month = NULL,
    financing_market_context = NULL
  )
  
  if(!isTRUE(result$feasible))
    stop(
      "Economic ranking produced no P1 recommendation: ",
      result$reason %||% "unknown"
    )
  
  payload <- result$payload
  
  payload$robustness <- list(
    target_probability = NULL,
    p10 = NULL,
    p50 = NULL,
    p90 = NULL
  )
  
  payload$production_meta <- list(
    input_schema = cfg$schema_version,
    backend = "FBC_R_BACKEND_P1_DETERMINISTIC_1.0",
    candidate_count = length(fast$candidate_pool),
    optimizer_levers = as.list(problem$registry$name),
    financing_separate = TRUE,
    methods_used = c(
      "R factual state",
      "confirmed production bounds",
      "constrained KKT candidate pool",
      "deterministic post-decision validation",
      "economic ranking",
      "Shapley after selection",
      "Business Break-even",
      "Employee Break-even"
    )
  )
  
  payload$audit <- list(
    path = "P1_DETERMINISTIC",
    mc_executed = FALSE,
    optimizer_executed = TRUE,
    demand_guard =
      "free capacity is not treated as demand"
  )
  
  payload
}

