# ============================================================
# 40_run_deterministic_p1.R
# FUTURE Business Cockpit
# Deterministic P1:
# confirmed real-world bounds -> optimizer
# WITHOUT MC / Sobol / robustness thresholds
# ============================================================

`%||%` <- function(a,b) if(is.null(a) || length(a)==0L) b else a

fbc_source40 <- function(file, envir){
  if(!file.exists(file))
    stop("Required FBC module missing: ", file)

  source(file, local=envir)
}

fbc_load_modules40 <- function(root=getwd(), envir){
  mods <- c(
    "09_finanzierung_inputs_costs_FINAL.R",
    "10_gesamtkosten_kapitaldienst.R",
    "11_business_break_even.R",
    "18_cockpit_input_contract.R",
    "19_reality_constraints_V2.R",
    "25_price_elasticity.R",
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

  invisible(
  lapply(
    file.path(root, mods),
    fbc_source40,
    envir = envir
  )
)
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
    s$owner$billable_hours_month <- as.numeric(x["owner_hours"])

  for(k in names(s$operating_costs)){
    nm <- paste0("cost_", k)
    if(nm %in% names(x))
      s$operating_costs[k] <- as.numeric(x[nm])
  }

  if("employee_customer_price" %in% names(x))
    s$employee$customer_price <- as.numeric(x["employee_customer_price"])

  if("employee_billable_hours" %in% names(x))
    s$employee$billable_hours_month <- as.numeric(x["employee_billable_hours"])

  if(!exists("fbc_apply_demand_to_state25", mode="function"))
    stop("fbc_apply_demand_to_state25() fehlt. 25_price_elasticity.R zuerst laden.")

  fbc_apply_demand_to_state25(
    state=s,
    base_state=base_state
  )
}

fbc_business_break_even40 <- function(
    state,
    variable_cost_keys,
    employee_variable_cost_per_hour=0
){
  owner_h <- if(exists("fbc_state_expected_owner_hours25", mode="function"))
    fbc_state_expected_owner_hours25(state)
  else
    as.numeric(state$owner$expected_billable_hours_month %||% state$owner$billable_hours_month)

  emp_h <- if(isTRUE(state$employee$direct_billing)){
    if(exists("fbc_state_expected_employee_hours25", mode="function"))
      fbc_state_expected_employee_hours25(state)
    else
      as.numeric(state$employee$expected_billable_hours_month %||% state$employee$billable_hours_month)
  } else 0

  units <- owner_h + emp_h

  owner_revenue <- if(exists("fbc_state_owner_revenue25", mode="function"))
    fbc_state_owner_revenue25(state)
  else
    state$owner$price * owner_h

  employee_revenue <- if(isTRUE(state$employee$direct_billing)){
    if(exists("fbc_state_employee_revenue25", mode="function"))
      fbc_state_employee_revenue25(state)
    else
      state$employee$customer_price * emp_h
  } else 0

  revenue <- owner_revenue + employee_revenue

  if(!is.finite(units) || units <= 0 || !is.finite(revenue) || revenue < 0)
    stop("Cannot calculate Business Break-even: invalid expected billable hours/revenue.")

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
  emp_util <- NULL

  if(isTRUE(state$employee$direct_billing)){
    if(
      !exists("fbc_employee_utilization19", mode="function") ||
      !exists("fbc_employee_break_even_hours19", mode="function")
    ){
      stop(
        "Employee utilization/break-even model fehlt. ",
        "19_reality_constraints_V2.R zuerst laden."
      )
    }

    emp_util <- fbc_employee_utilization19(
      state,
      billable_hours = emp_h
    )

    emp_be_hours <- fbc_employee_break_even_hours19(
      state,
      customer_price = state$employee$customer_price,
      variable_cost_per_hour = employee_variable_cost_per_hour
    )

    emp_ok <-
      is.finite(emp_be_hours) &&
      emp_be_hours <= emp_h + 1e-8
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
      expected_billable_hours = units,
      safety_margin = safety_margin,
      reachable = reachable
    ),

    employee = list(
      applicable = isTRUE(state$employee$direct_billing),
      customer_price = state$employee$customer_price,
      variable_cost_per_hour = employee_variable_cost_per_hour,
      personnel_cost_month = state$employee$personnel_cost_month,
      planned_billable_hours = state$employee$billable_hours_month,
      billable_hours = emp_h,
      expected_billable_hours = emp_h,

      # Commercial revenue stays factual / elasticity-aware.
      revenue_month = employee_revenue,
      variable_cost_month = employee_variable_cost_per_hour * emp_h,
      result_contribution_month =
        employee_revenue -
        state$employee$personnel_cost_month -
        employee_variable_cost_per_hour * emp_h,

      # Sustainable employee cost-coverage diagnostics.
      effective_billable_hours =
        if(!is.null(emp_util))
          emp_util$effective_hours
        else
          0,

      available_hours_month =
        if(!is.null(emp_util))
          emp_util$available_hours
        else
          NA_real_,

      utilization_rate =
        if(!is.null(emp_util))
          emp_util$utilization_rate
        else
          NA_real_,

      utilization_zone =
        if(!is.null(emp_util))
          emp_util$zone
        else
          "not_applicable",

      utilization_model =
        "piecewise_sustainable_billable_hours",

      utilization_thresholds =
        if(exists("FBC_EMPLOYEE_UTIL_THRESHOLDS19"))
          FBC_EMPLOYEE_UTIL_THRESHOLDS19
        else
          c(0.70,0.80,0.85),

      utilization_weights =
        if(exists("FBC_EMPLOYEE_UTIL_WEIGHTS19"))
          FBC_EMPLOYEE_UTIL_WEIGHTS19
        else
          c(0.90,1.00,0.75,0.50),

      sustainable_revenue_month =
        if(!is.null(emp_util))
          state$employee$customer_price *
          emp_util$effective_hours
        else
          0,

      sustainable_result_contribution_month =
        if(!is.null(emp_util))
          state$employee$customer_price *
          emp_util$effective_hours -
          state$employee$personnel_cost_month -
          employee_variable_cost_per_hour * emp_h
        else
          0,

      break_even_hours = emp_be_hours,
      hours_above_break_even =
        if(isTRUE(state$employee$direct_billing))
          emp_h - emp_be_hours
        else NA_real_,
      ok = emp_ok
    )
  )
}


# Build the financing result against the OPERATING state selected by P1.
# Borrowed principal is never treated as income. DSCR uses the existing
# calc_current_debt_capacity19() semantics (free funds before debt service).
fbc_financing_payload40 <- function(
    base_state,
    problem,
    solution,
    cfg
){
  if(!isTRUE(cfg$financing$active)){
    return(list(
      active = FALSE,
      status = "no_financing",
      current = NULL,
      alternative = NULL,
      comparison = NULL,
      note = "Finanzierung ist kein operativer KKT-Hebel."
    ))
  }

  contract <- list(
    type = cfg$financing$type,
    amount = cfg$financing$amount,
    rate_pa = cfg$financing$rate_pa,
    months = cfg$financing$months,
    fees_month = cfg$financing$fees_month %||% 0,
    one_time_fee = cfg$financing$one_time_fee %||% 0,
    binding = cfg$financing$binding %||% NA_character_
  )

  selected_state <- fbc_state_from_solution40(
    base_state,
    problem,
    solution
  )

  cur <- calc_current_business_result19(
    selected_state,
    owner_pension_month = cfg$owner_pension_month %||% 0,
    tax_month = cfg$tax_month %||% 0
  )

  revenue <- cur$owner_revenue + cur$employee_revenue

  debt <- calc_current_debt_capacity19(
    selected_state,
    revenue
  )

  summary <- fbc_summarize_financing(
    contract,
    comparison_horizon_months = cfg$financing$months,
    free_cash_before_debt_service_month =
      debt$free_funds_before_debt_service,
    min_debt_service_ratio = NULL
  )

  ratio <- summary$min_capital_service_ratio

  list(
    active = TRUE,
    status = "current_financing_only",
    current = list(
      type = summary$contract$type,
      amount = summary$contract$amount,
      rate_pa = summary$contract$rate_pa,
      months = summary$contract$months %||% NA,
      binding = summary$contract$binding %||% NA_character_,
      total_interest = summary$total_interest,
      total_fees = summary$total_fees,
      total_financing_expense = summary$total_financing_expense,
      total_cash_service = summary$total_cash_service,
      first_month_cash_service = summary$first_month_cash_service,
      last_month_cash_service = summary$last_month_cash_service,
      max_month_cash_service = summary$max_month_cash_service,
      free_cash_before_debt_service = debt$free_funds_before_debt_service,
      restschuld_end = summary$restschuld_end,
      min_capital_service_ratio = ratio,
      capital_service_policy_ok =
        if(is.na(ratio)) NA else ratio >= 1
    ),
    alternative = NULL,
    comparison = NULL,
    note = paste(
      "Finanzierung bleibt getrennt vom operativen Ergebnis.",
      "Kreditbetrag ist kein Betriebsergebnis; Tilgung ist Liquiditätsabfluss, aber kein Aufwand."
    )
  )
}

fbc_run_deterministic_p1_40 <- function(cfg, root=getwd()){

  stage <- "load_modules"

  tryCatch({

    fbc_load_modules40(
  root,
  envir = environment()
)
  stage <- "18_build_state"
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

  # Separate physical/paid capacity from commercial billable hours.
  # available_hours_month stays physical capacity; optimization uses billable_hours_month.
  state$owner$physical_available_hours_month <- state$owner$available_hours_month
  state$owner$billable_hours_month <-
    max(0, as.numeric(cfg$owner$billable_hours_month %||% 0))

  state$employee$paid_available_hours_month <-
    as.numeric(cfg$employee$paid_hours_month %||%
                 state$employee$available_hours_month)

  state$employee$billable_hours_month <-
    if(isTRUE(state$employee$direct_billing))
      max(0, as.numeric(cfg$employee$billable_hours_month %||% 0))
    else 0

  if(is.finite(as.numeric(cfg$employee$personnel_cost_month %||% NA_real_))){
    state$employee$personnel_cost_month <-
      as.numeric(cfg$employee$personnel_cost_month)
  }

  if(!exists("fbc_apply_demand_to_state25", mode="function"))
    stop("fbc_apply_demand_to_state25() fehlt. 25_price_elasticity.R zuerst laden.")

  # Factual state: price is unchanged, therefore expected billable hours equal
  # the explicitly supplied commercial billable hours.
  state <- fbc_apply_demand_to_state25(
    state=state,
    base_state=state
  )

  state_rules <- state
  state_rules$owner$billable_hours_month <-
  state$owner$billable_hours_month

state_rules$employee$billable_hours_month <-
  state$employee$billable_hours_month
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
  stage <- "37_production_rules"
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
    owner_hours_max <- state$owner$billable_hours_month
  stage <- "19_reality"
  reality <- build_reality_constraints19(
    state = state,
    cost_controls = cc,
    owner_pension_month = cfg$owner_pension_month %||% 0,
    tax_month = cfg$tax_month %||% 0,
    max_owner_hours_month = owner_hours_max,
    financing_alternative = NULL,
    min_debt_service_ratio = NULL
  )
  stage <- "21_influence"
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
  stage <- "26_optimizer_problem"
  problem <- build_optimizer_problem26(
    state = state,
    reality = reality,
    influence = influence,
    objective_mode =
  if(identical(cfg$objective_mode %||% "", "reduce_owner_work"))
    "reduce_owner_work"
  else
    "reach_income_target",
    desired_net = cfg$desired_net %||% state$owner$monthly_target,

    confirmed_bounds = list(
  owner_price_max = get_upper("owner_price"),
  owner_hours_max = get_upper("owner_hours")
),

    employee_bounds = list(
      customer_price_max = get_upper("employee_customer_price"),
      billable_hours_max = get_upper("employee_billable_hours")
    ),

    owner_pension_month = cfg$owner_pension_month %||% 0,
    tax_month = cfg$tax_month %||% 0
  )
  stage <- "fast_candidate_search"
  fast <- run_fbc_fast_candidate_search(
    problem = problem,
    max_actions = cfg$max_actions %||% nrow(problem$registry),
    verbose = isTRUE(cfg$verbose)
  )

 if(!isTRUE(fast$feasible) || !length(fast$candidate_pool)){

  best <- fbc_fast_best_corner(problem)

  if(!is.finite(best$net) || is.null(best$x))
    stop(
      "No feasible target solution and no valid best-attainable solution: ",
      fast$reason %||% "unknown reason"
    )

  best_solution <- best$x
  names(best_solution) <- problem$registry$name

   metrics_error <- NULL

best_metrics <- tryCatch(
  fbc_fast_metrics(problem, best_solution),
  error = function(e){
    metrics_error <<- conditionMessage(e)
    list()
  }
)
  best_candidate <- list(
    active_levers = problem$registry$name[
      abs(best_solution - problem$registry$current) > 1e-6
    ],
    solution = best_solution,
    projected_net = as.numeric(best$net),
    metrics = best_metrics,
    cardinality = sum(
      abs(best_solution - problem$registry$current) > 1e-6
    )
  )

  ip <- prod_rules$implementation_plan
  ip <- ip[ip$lever %in% problem$registry$name,,drop=FALSE]

missing_ip <- setdiff(
  best_candidate$active_levers,
  ip$lever
)

implementation_timing_error <-
  if(length(missing_ip))
    paste(
      "Missing evidenced implementation timing for:",
      paste(missing_ip, collapse=", ")
    )
  else NULL

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

stage <- "target_not_reachable_post_validation"

validation_error <- NULL

validation <- tryCatch(
  fbc_validate_post_decision(
    problem = problem,
    candidate = best_candidate,
    implementation_plan = ip,
    horizon_months = cfg$horizon_months %||% 12,
    post_target_months = cfg$post_target_months %||% 6,
    monthly_evaluator = monthly_eval,
    mc_evaluator = NULL,
    break_even_evaluator = be_eval,
    capital_service_evaluator = capital_eval,
    min_target_probability = NULL,
    min_debt_service_ratio = NULL
  ),
  error = function(e){
    validation_error <<- conditionMessage(e)

list(
  time_to_target_months = NA_real_,
  implementation_months_max = NA_real_,
  liquidity_bridge_need_eur = 0,
  time_path = NULL,
  post_target_stable = NA,
  max_post_target_gap_eur = NA_real_,
  business_break_even_margin_eur = NA_real_,
  business_break_even_ok = NA,
  employee_break_even_ok = NA,
  capital_service_ratio = NA_real_,
  capital_service_ok = NA
)
  }
)

changes_error <- NULL

changes <- tryCatch(
  fbc_build_change_payload38(
    problem,
    best_candidate
  ),
  error = function(e){
    changes_error <<- conditionMessage(e)
    list()
  }
)

stage <- "target_not_reachable_shapley"

shapley_error <- NULL

shapley <- tryCatch(
  fbc_build_shapley_explanation38(
    problem,
    best_candidate,
    shapley_exact27C
  ),
  error = function(e){
    shapley_error <<- conditionMessage(e)

    list(
      available = FALSE,
      sentence = NULL,
      contributions = list()
    )
  }
)

  stage <- "target_not_reachable_break_even"

break_even_error <- NULL

be_detail <- tryCatch(
  be_eval(best_solution),
  error = function(e){
    break_even_error <<- conditionMessage(e)
    NULL
  }
)

  current_net <- as.numeric(
    problem$evaluate(problem$registry$current)
  )

  target <- as.numeric(problem$desired_net)

  remaining_gap <- max(
    0,
    target - as.numeric(best$net)
  )

  remaining_gap_pct <-
    if(is.finite(target) && target > 0)
      100 * remaining_gap / target
    else NA_real_

  # The fast KKT pool can be empty even when the validated best corner
  # reaches the target. Do not mislabel such a fallback as unreachable.
  fallback_target_reached <-
    is.finite(best$net) &&
    is.finite(target) &&
    as.numeric(best$net) >= target - 1e-6

  payload <- list(
    schema_version = "fbc_decision_payload_v1",

    status =
      if(isTRUE(fallback_target_reached))
        "recommendation_selected"
      else
        "target_not_reachable",

    current = list(
      expected_net = current_net,
      monthly_target = target,
      target_gap_eur = max(0, target - current_net),
      target_gap_percent =
        if(is.finite(target) && target > 0)
          100 * max(0, target - current_net) / target
        else NA_real_
    ),

    recommendation = list(
      candidate_id =
        if(isTRUE(fallback_target_reached))
          "best_corner_fallback"
        else
          NULL,
      active_levers = best_candidate$active_levers,
      changes = changes,
      projected_net = as.numeric(best$net),
      target_reached = isTRUE(fallback_target_reached),
      remaining_gap_eur = remaining_gap,
      remaining_gap_percent = remaining_gap_pct,
      demand = if(is.function(problem$demand_details)) problem$demand_details(best_solution) else NULL,
      explanation = shapley
    ),

target_path = list(
  time_to_target_months =
    validation$time_to_target_months,
  target_reached_within_horizon =
    isTRUE(validation$target_reached_within_horizon),
  implementation_months_max =
    validation$implementation_months_max,
  liquidity_bridge_need_eur =
    validation$liquidity_bridge_need_eur %||% 0,
  path = validation$time_path
),

    post_decision = list(
      post_target_stable =
        validation$post_target_stable,
      max_post_target_gap_eur =
        validation$max_post_target_gap_eur,
      mc_target_probability = NULL,
      mc_policy_ok = NULL,
      business_break_even_margin_eur =
        validation$business_break_even_margin_eur,
      business_break_even_ok =
        validation$business_break_even_ok,
      employee_break_even_ok =
        validation$employee_break_even_ok,
      capital_service_ratio =
        validation$capital_service_ratio,
      capital_service_ok =
        validation$capital_service_ok
    ),

    break_even = be_detail,

    financing = fbc_financing_payload40(
      base_state = state,
      problem = problem,
      solution = best_solution,
      cfg = cfg
    ),

    robustness = list(
      target_probability = NULL,
      p10 = NULL,
      p50 = NULL,
      p90 = NULL
    ),

    production_meta = list(
      input_schema = cfg$schema_version,
      backend = "FBC_R_BACKEND_P1_DETERMINISTIC_1.0",
      candidate_count = 0,
      optimizer_levers =
        as.list(problem$registry$name),
      financing_separate = TRUE,
      target_feasible = isTRUE(fallback_target_reached),
      best_attainable_used = !isTRUE(fallback_target_reached),
      fallback_corner_used = TRUE,
      demand_model = problem$demand_model
    ),

    audit = list(
      path =
        if(isTRUE(fallback_target_reached))
          "P1_DETERMINISTIC_REACHABLE_FALLBACK"
        else
          "P1_DETERMINISTIC_TARGET_NOT_REACHABLE",
      mc_executed = FALSE,
      optimizer_executed = TRUE,
      demand_guard =
        "free capacity is not treated as demand",
      demand_model = problem$demand_model,

      auxiliary_analysis = list(
        implementation_timing_ok =
          is.null(implementation_timing_error),
        implementation_timing_error =
          implementation_timing_error,

        metrics_ok =
          is.null(metrics_error),
        metrics_error =
          metrics_error,

        post_validation_ok =
          is.null(validation_error),
        post_validation_error =
          validation_error,

        changes_ok =
          is.null(changes_error),
        changes_error =
          changes_error,

        shapley_ok =
          is.null(shapley_error),
        shapley_error =
          shapley_error,

        break_even_ok =
          is.null(break_even_error),
        break_even_error =
          break_even_error
      )
    )
  )

  return(payload)
}

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
  stage <- "38_production_decision"
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

  payload$financing <- fbc_financing_payload40(
    base_state = state,
    problem = problem,
    solution = result$selected$solution,
    cfg = cfg
  )

  if(
    isTRUE(payload$financing$active) &&
    is.list(payload$financing$current)
  ){
    fin_ratio <- suppressWarnings(
      as.numeric(
        payload$financing$current$min_capital_service_ratio %||% NA_real_
      )[1]
    )

    payload$post_decision$capital_service_ratio <-
      if(is.na(fin_ratio)) NA_real_ else fin_ratio

    payload$post_decision$capital_service_ok <-
      if(is.na(fin_ratio)) NA else fin_ratio >= 1
  }

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
      "bounded nonlinear candidate pool",
      "deterministic post-decision validation",
      "economic ranking",
      "Shapley after selection",
      "Business Break-even",
      "Employee Break-even"
    ),
    demand_model = problem$demand_model
  )

  payload$audit <- list(
    path = "P1_DETERMINISTIC",
    mc_executed = FALSE,
    optimizer_executed = TRUE,
    demand_guard =
      "free capacity is not treated as demand",
    demand_model = problem$demand_model
  )

     payload

  }, error=function(e){
    stop(
      sprintf(
        "[P1 stage=%s] %s",
        stage,
        conditionMessage(e)
      ),
      call. = FALSE
    )
  })
}
