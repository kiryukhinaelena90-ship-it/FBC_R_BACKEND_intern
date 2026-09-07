# ============================================================
# FUTURE Business Cockpit — R Backend API
# Plumber service for FBC_R_INTEGRATION_STAGE1 frontend.
#
# Routes:
#   GET  /health
#   POST /analyze
#
# Default behavior:
# - factual/P0 response is enabled;
# - no evidence => no optimizer lever;
# - P1 runner is present but disabled unless FBC_ENABLE_P1_RUNNER=true.
# ============================================================

suppressPackageStartupMessages({
  library(plumber)
  library(jsonlite)
})

ROOT <- normalizePath(getwd(), mustWork=TRUE)
source(file.path(ROOT,"24_financial_net_adapter.R"), local=.GlobalEnv)

# Source factual-state modules once.
source(file.path(ROOT,"18_cockpit_input_contract.R"), local=.GlobalEnv)
source(file.path(ROOT,"10_gesamtkosten_kapitaldienst.R"), local=.GlobalEnv)
source(file.path(ROOT,"11_business_break_even.R"), local=.GlobalEnv)
source(file.path(ROOT,"19_reality_constraints_V2.R"), local=.GlobalEnv)

`%||%` <- function(a,b) if(is.null(a) || length(a)==0L) b else a
num1 <- function(x, default=0){
  z <- suppressWarnings(as.numeric(x %||% default)[1])
  if(!is.finite(z)) default else z
}
flag1 <- function(x) isTRUE(x)

fbc_authorized <- function(req){
  expected <- Sys.getenv("FBC_R_API_KEY", unset="")
  if(!nzchar(expected)) return(TRUE)
  got <- req$HTTP_AUTHORIZATION %||% ""
  identical(got, paste0("Bearer ", expected))
}

fbc_validate_request <- function(x){
  errors <- character()
  if(is.null(x) || !is.list(x)) return("body_missing")
  if(!identical(x$schema_version,"FBC_P1_COCKPIT_REQUEST_1.0")) errors <- c(errors,"schema_version")
  if(!identical(x$mode,"owner_employee")) errors <- c(errors,"mode")
  for(k in c("owner","employee","operating_costs","financing")){
    if(is.null(x[[k]]) || !is.list(x[[k]])) errors <- c(errors,k)
  }
  unique(errors)
}

fbc_map_costs <- function(x){
  # Frontend calls this field "insurance"; canonical R state calls it "business_insurance".
  c(
    office=num1(x$office),
    energy=num1(x$energy),
    vehicle=num1(x$vehicle),
    fuel=num1(x$fuel),
    software=num1(x$software),
    accounting=num1(x$accounting),
    business_insurance=num1(x$business_insurance %||% x$insurance),
    material=num1(x$material),
    marketing=num1(x$marketing),
    other=num1(x$other)
  )
}

fbc_map_financing <- function(x){
  type <- as.character(x$type %||% "annuity")
  if(identical(type,"amortizing")) type <- "amortizing"
  if(!type %in% c("annuity","amortizing")) type <- "annuity"
  list(
    active=flag1(x$active),
    type=type,
    amount=num1(x$amount),
    rate_pa=num1(x$rate_pa),
    months=max(1,round(num1(x$months,1))),
    fees_month=num1(x$fees_month),
    binding=as.character(x$binding %||% "fixed")
  )
}

fbc_build_factual_state <- function(cfg){
  owner <- cfg$owner
  employee <- cfg$employee

  state <- build_cockpit_decision_state18(
    owner=list(
      price=num1(owner$price),
      hours_week=num1(owner$hours_week),
      days_week=max(1,num1(owner$days_week,5)),
      vacation_days=num1(owner$vacation_days),
      monthly_target=num1(owner$monthly_target),
      insurance_month=num1(owner$insurance_month),
      pension_mode=as.character(owner$pension_mode %||% "none"),
      pension_fixed_month=num1(owner$pension_fixed_month)
    ),
    operating_costs=fbc_map_costs(cfg$operating_costs),
    employee=list(
      form=as.character(employee$form %||% "part"),
      wage_hour=num1(employee$wage_hour),
      hours_week=num1(employee$hours_week),
      days_week=max(1,num1(employee$days_week,5)),
      vacation_days=num1(employee$vacation_days),
      direct_billing=flag1(employee$direct_billing),
      customer_price=num1(employee$customer_price),
      extra_cost_month=num1(employee$extra_cost_month)
    ),
    financing=fbc_map_financing(cfg$financing),
    owner_holiday_days=num1(owner$holidays),
    employee_holiday_days=num1(employee$holidays),
    employer_addon_rate=num1(employee$employer_addon_rate)
  )

  # Critical factual separation:
  # contract/paid capacity is not the same thing as customer-billable hours.
  owner_bill <- max(0,num1(owner$billable_hours_month))
  emp_bill <- if(flag1(employee$direct_billing)) max(0,num1(employee$billable_hours_month)) else 0

  state$owner$physical_available_hours_month <- state$owner$available_hours_month
  state$owner$available_hours_month <- owner_bill

  state$employee$paid_available_hours_month <-
    num1(employee$paid_available_hours_month, state$employee$available_hours_month)
  state$employee$billable_hours_month <- emp_bill
  state$employee$available_hours_month <- emp_bill

  # Use factual personnel cost from the Cockpit when supplied. This preserves
  # employment-form logic already calculated in the current frontend.
  factual_pc <- num1(employee$personnel_cost_month, NA_real_)
  if(is.finite(factual_pc) && factual_pc >= 0){
    state$employee$personnel_cost_month <- factual_pc
  }

  state$employee$revenue_month <-
    if(flag1(employee$direct_billing)) num1(employee$customer_price)*emp_bill else 0

  state
}

fbc_current_financing_payload <- function(state){
  if(!isTRUE(state$financing$active)){
    return(list(status="no_financing",current=NULL,alternative=NULL,comparison=NULL))
  }
  list(
    status="current_financing_only",
    current=list(
      rate_pa=state$financing$rate_pa,
      interest_plus_fees_month=state$financing$interest_plus_fees_month,
      principal_month=state$financing$principal_month,
      debt_service_month=state$financing$debt_service_month
    ),
    alternative=NULL,
    comparison=NULL
  )
}

fbc_p0_payload <- function(cfg){
  state <- fbc_build_factual_state(cfg)

  owner_rev <- state$owner$price * state$owner$available_hours_month
  emp_rev <- state$employee$revenue_month
  op <- sum(state$operating_costs)
  pc <- state$employee$personnel_cost_month
  fin_result <- state$financing$interest_plus_fees_month
  before <- owner_rev + emp_rev - op - pc - fin_result

  financial <- fbc_financial_point24(
    before,
    state$owner,
    legal_form=as.character(cfg$legal_form %||% "freelance"),
    trade_tax_rate=num1(cfg$trade_tax_rate)
  )

  target <- num1(state$owner$monthly_target)
  gap <- max(0,target-financial$net_available)
  gap_pct <- if(target>0) 100*gap/target else NA_real_

  total_h <- state$owner$available_hours_month +
    if(isTRUE(state$employee$direct_billing)) state$employee$available_hours_month else 0
  revenue <- owner_rev+emp_rev
  weighted_price <- if(total_h>0) revenue/total_h else NA_real_

  variable_keys <- as.character(unlist(cfg$variable_cost_keys %||% character()))
  variable_keys <- intersect(variable_keys,names(state$operating_costs))
  variable_month <- if(length(variable_keys)) sum(state$operating_costs[variable_keys]) else 0
  variable_per_h <- if(total_h>0) variable_month/total_h else 0
  fixed_costs <- op-variable_month+pc+fin_result
  db_h <- weighted_price-variable_per_h
  be_hours <- if(is.finite(db_h) && db_h>0) fixed_costs/db_h else Inf
  be_revenue <- if(is.finite(be_hours) && is.finite(weighted_price)) be_hours*weighted_price else Inf
  be_margin <- revenue-be_revenue

  emp_be <- NA_real_
  if(isTRUE(state$employee$direct_billing)){
    ep <- state$employee$customer_price
    emp_be <- if(ep>0) pc/ep else Inf
  }

  ds <- state$financing$debt_service_month
  free_before_ds <- before
  dscr <- if(is.finite(ds) && ds>0) free_before_ds/ds else NULL

  list(
    schema_version="fbc_decision_payload_v1",
    status="no_evidenced_lever",
    current=list(
      expected_net=financial$net_available,
      monthly_target=target,
      target_gap_eur=gap,
      target_gap_percent=gap_pct
    ),
    recommendation=list(
      candidate_id="no_change_without_evidence",
      active_levers=list(),
      changes=list(),
      projected_net=financial$net_available,
      explanation=list(
        available=FALSE,
        sentence="Keine belastbare Entscheidungsgrenze bestätigt; daher keine operative Maßnahme aus dem Optimizer."
      )
    ),
    target_path=list(
      time_to_target_months=NULL,
      target_reached_within_horizon=(gap<=1e-9),
      implementation_months_max=NULL,
      path=list()
    ),
    post_decision=list(
      post_target_stable=NULL,
      max_post_target_gap_eur=NULL,
      mc_target_probability=NULL,
      mc_policy_ok=NULL,
      business_break_even_margin_eur=be_margin,
      business_break_even_ok=is.finite(be_margin) && be_margin>=0,
      employee_break_even_ok=if(isTRUE(state$employee$direct_billing)) is.finite(emp_be) && state$employee$available_hours_month>=emp_be else TRUE,
      capital_service_ratio=dscr,
      capital_service_ok=if(is.null(dscr)) TRUE else dscr>=1
    ),
    break_even=list(
      business=list(
        weighted_price_per_billable_hour=weighted_price,
        variable_cost_per_billable_hour=variable_per_h,
        fixed_result_costs_month=fixed_costs,
        break_even_hours=be_hours,
        break_even_revenue=be_revenue,
        projected_revenue=revenue,
        safety_margin=be_margin
      ),
      employee=list(
        applicable=isTRUE(state$employee$direct_billing),
        customer_price=state$employee$customer_price,
        personnel_cost_month=pc,
        billable_hours=state$employee$available_hours_month,
        break_even_hours=emp_be,
        hours_above_break_even=if(isTRUE(state$employee$direct_billing)) state$employee$available_hours_month-emp_be else NULL
      )
    ),
    financing=fbc_current_financing_payload(state),
    robustness=list(
      target_probability=NULL,
      p10=NULL,p50=NULL,p90=NULL
    ),
    production_meta=list(
      input_schema=cfg$schema_version,
      backend="FBC_R_BACKEND_P0_1.0",
      candidate_count=0,
      optimizer_levers=list(),
      employment_form=as.character(cfg$employee$form %||% ""),
      financing_separate=TRUE,
      methods_used=c(
        "R factual state",
        "2026 tax orientation",
        "Business Break-even",
        "Employee Break-even",
        if(isTRUE(state$financing$active)) "Kapitaldienst current-state" else NULL
      )
    ),
    audit=list(
      path="P0",
      evidence_gate="no confirmed bounds -> no lever",
      demand_guard="free capacity is not treated as demand",
      tax_disclosure_de=financial$disclosure_de,
      tax_disclosure_ru=financial$disclosure_ru,
      mc_executed=FALSE,
      optimizer_executed=FALSE
    )
  )
}

fbc_has_evidence <- function(cfg){
  b <- cfg$confirmed_bounds
  e <- cfg$cost_evidence
  (is.list(b) && length(b)>0L) || (is.list(e) && length(e)>0L)
}

fbc_runner39_cfg <- function(cfg){
  out <- cfg
  out$operating_costs <- fbc_map_costs(cfg$operating_costs)
  out$owner_holiday_days <- num1(cfg$owner$holidays)
  out$employee_holiday_days <- num1(cfg$employee$holidays)
  out$employer_addon_rate <- num1(cfg$employee$employer_addon_rate)
  out$owner_pension_month <- 0
  out$tax_month <- 0
  out$employee$paid_hours_month <- num1(cfg$employee$paid_available_hours_month)
  out$employee$contract_hours_week <- num1(cfg$employee$hours_week)
  out$mc_file <- Sys.getenv(
    "FBC_MC_FILE",
    unset=file.path(ROOT,"data","processed","fbc_monte_carlo_draws.csv")
  )
  out
}

health_handler <- function(req, res) {
  list(
    status = "ok",
    service = "FUTURE Business Cockpit R backend",
    backend_version = "FBC_R_BACKEND_P0_1.0",
    p0 = TRUE,
    p1_runner_enabled =
      identical(
        tolower(Sys.getenv("FBC_ENABLE_P1_RUNNER", "false")),
        "true"
      ),
    mc_file_present =
      file.exists(
        Sys.getenv(
          "FBC_MC_FILE",
          unset = file.path(
            ROOT,
            "data",
            "processed",
            "fbc_monte_carlo_draws.csv"
          )
        )
      )
  )
}

analyze_handler <- function(req, res) {

  if (!fbc_authorized(req)) {
    res$status <- 401
    return(list(error = "unauthorized"))
  }

  cfg <- req$body

  errors <- fbc_validate_request(cfg)

  if (length(errors)) {
    res$status <- 422
    return(
      list(
        error = "invalid_fbc_request",
        fields = as.list(errors)
      )
    )
  }

  if (!fbc_has_evidence(cfg)) {
    return(fbc_p0_payload(cfg))
  }

  runner_cfg <- fbc_runner39_cfg(cfg)

  source(
    file.path(ROOT, "40_run_deterministic_p1.R"),
    local = .GlobalEnv
  )

  tryCatch(
    {
      payload <- fbc_run_deterministic_p1_40(
        runner_cfg,
        root = ROOT
      )

      return(payload)
    },
    error = function(e){
      res$status <- 500

      return(
        list(
          error = "p1_deterministic_failed",
          message = conditionMessage(e)
        )
      )
    }
  )
}
