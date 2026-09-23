# ============================================================
# 38_production_decision_pipeline.R
# FUTURE Business Cockpit
# End-to-end production decision assembly AFTER candidate generation
#
# Connects:
#   nonlinear bounded candidate pool
#   -> post-decision validation
#   -> economic ranking
#   -> Shapley explanation
#   -> time-to-target
#   -> Break-even recheck
#   -> financing as a SEPARATE alternative branch
#   -> production payload / JSON export
#
# IMPORTANT:
# - no arbitrary global weights
# - no financing in operating decision vector
# - no invented thresholds, bounds or implementation durations
# ============================================================

source("34_economic_ranking_PRODUCTION.R")
source("35_time_post_validation.R")
source("36_financing_alternative.R")

`%||%` <- function(a,b) if(is.null(a) || length(a)==0L) b else a

fbc_label_lever38 <- function(x){
  map <- c(
    owner_price="Inhaberpreis",
    owner_hours="Abrechenbare Stunden Inhaber/in",
    employee_customer_price="Kundenpreis Mitarbeiter/in",
    employee_billable_hours="Abrechenbare Stunden Mitarbeiter/in",
    cost_office="Raum / Büro",
    cost_energy="Energie / Strom",
    cost_vehicle="Fahrzeug",
    cost_fuel="Kraftstoff",
    cost_software="Programme / Software",
    cost_accounting="Steuerberater / Buchhaltung",
    cost_business_insurance="Betriebsversicherungen",
    cost_material="Material / direkte Kosten",
    cost_marketing="Werbung / Akquise",
    cost_other="Sonstiges"
  )
  ifelse(x %in% names(map),unname(map[x]),x)
}

fbc_unit_lever38 <- function(x){
  if(x %in% c("owner_price","employee_customer_price")) return("EUR/h")
  if(x %in% c("owner_hours","employee_billable_hours")) return("h/month")
  if(grepl("^cost_",x)) return("EUR/month")
  ""
}

fbc_build_change_payload38 <- function(problem,selected){
  ch <- fbc_candidate_changes(problem,selected$solution)
  if(!nrow(ch)) return(list())
  lapply(seq_len(nrow(ch)),function(i){
    list(
      lever=as.character(ch$lever[i]),
      label=fbc_label_lever38(as.character(ch$lever[i])),
      current=as.numeric(ch$current[i]),
      proposed=as.numeric(ch$proposed[i]),
      delta=as.numeric(ch$delta[i]),
      unit=fbc_unit_lever38(as.character(ch$lever[i]))
    )
  })
}

fbc_build_shapley_explanation38 <- function(problem,selected,shapley_fun=NULL){
  if(is.null(shapley_fun)){
    return(list(
      available=FALSE,
      contributions=list(),
      dominant_lever=NULL,
      sentence=NULL
    ))
  }

  sh <- shapley_fun(problem,selected$solution)
  if(is.null(sh) || !is.data.frame(sh) || !nrow(sh)){
    return(list(
      available=FALSE,
      contributions=list(),
      dominant_lever=NULL,
      sentence=NULL
    ))
  }

  if(!all(c("variable","contribution") %in% names(sh)))
    stop("Shapley output must contain variable and contribution.")

  ord <- order(abs(sh$contribution),decreasing=TRUE)
  sh <- sh[ord,,drop=FALSE]
  dom <- as.character(sh$variable[1])
  dom_label <- fbc_label_lever38(dom)

  contrib <- lapply(seq_len(nrow(sh)),function(i){
    list(
      lever=as.character(sh$variable[i]),
      label=fbc_label_lever38(as.character(sh$variable[i])),
      contribution_eur=as.numeric(sh$contribution[i])
    )
  })

  sentence <- if(nrow(sh)==1){
    sprintf("%s trägt die empfohlene Ergebnisverbesserung.",dom_label)
  } else {
    sprintf("%s liefert den größten Beitrag; die weiteren Anpassungen ergänzen die Wirkung.",dom_label)
  }

  list(
    available=TRUE,
    contributions=contrib,
    dominant_lever=dom,
    dominant_label=dom_label,
    sentence=sentence,
    total_improvement_eur=as.numeric(attr(sh,"total_improvement") %||% sum(sh$contribution))
  )
}

fbc_build_financing_branch38 <- function(
    current_financing=NULL,
    financing_alternative=NULL,
    comparison_horizon_months=NULL,
    free_cash_before_debt_service_month=NULL,
    min_debt_service_ratio=NULL,
    market_context=NULL
){
  if(is.null(current_financing) || !isTRUE(current_financing$active %||% TRUE)){
    return(list(
      active=FALSE,
      status="no_financing",
      current=NULL,
      alternative=NULL,
      comparison=NULL,
      market_context=market_context,
      note="Finanzierung ist kein operativer KKT-Hebel."
    ))
  }

  # Remove non-contract fields that the financing engine does not need.
  cur_contract <- current_financing
  cur_contract$active <- NULL
  cur <- fbc_summarize_financing(
    cur_contract,
    comparison_horizon_months=comparison_horizon_months,
    free_cash_before_debt_service_month=free_cash_before_debt_service_month,
    min_debt_service_ratio=min_debt_service_ratio
  )

  current_payload <- list(
    type=cur$contract$type,
    amount=cur$contract$amount,
    rate_pa=cur$contract$rate_pa,
    months=cur$contract$months %||% NA,
    binding=cur$contract$binding %||% NA,
    total_interest=cur$total_interest,
    total_fees=cur$total_fees,
    total_financing_expense=cur$total_financing_expense,
    total_cash_service=cur$total_cash_service,
    first_month_cash_service=cur$first_month_cash_service,
    max_month_cash_service=cur$max_month_cash_service,
    restschuld_end=cur$restschuld_end,
    min_capital_service_ratio=cur$min_capital_service_ratio,
    capital_service_policy_ok=cur$capital_service_policy_ok
  )

  if(is.null(financing_alternative)){
    return(list(
      active=TRUE,
      status="current_financing_only",
      current=current_payload,
      alternative=NULL,
      comparison=NULL,
      market_context=market_context,
      note=paste(
        "Finanzierung bleibt eine getrennte Alternative.",
        "Kreditbetrag ist kein Betriebsergebnis; Tilgung ist Liquiditätsabfluss, aber kein Aufwand."
      )
    ))
  }

  alt_contract <- financing_alternative
  alt_contract$active <- NULL

  cmp <- fbc_compare_financing_alternatives(
    current=cur_contract,
    alternative=alt_contract,
    free_cash_before_debt_service_month=free_cash_before_debt_service_month,
    min_debt_service_ratio=min_debt_service_ratio
  )

  alt <- cmp$alternative
  alt_payload <- list(
    type=alt$contract$type,
    amount=alt$contract$amount,
    rate_pa=alt$contract$rate_pa,
    months=alt$contract$months %||% NA,
    binding=alt$contract$binding %||% NA,
    total_interest=alt$total_interest,
    total_fees=alt$total_fees,
    total_financing_expense=alt$total_financing_expense,
    total_cash_service=alt$total_cash_service,
    first_month_cash_service=alt$first_month_cash_service,
    max_month_cash_service=alt$max_month_cash_service,
    restschuld_end=alt$restschuld_end,
    min_capital_service_ratio=alt$min_capital_service_ratio,
    capital_service_policy_ok=alt$capital_service_policy_ok
  )

  comparison_payload <- list(
    comparable=cmp$comparable,
    reason=cmp$reason,
    financing_expense_saving=cmp$financing_expense_saving,
    first_month_cash_service_delta=cmp$first_month_cash_service_delta,
    alternative_economically_better=cmp$alternative_economically_better,
    note=cmp$note
  )

  list(
    active=TRUE,
    status="alternative_evaluated",
    current=current_payload,
    alternative=alt_payload,
    comparison=comparison_payload,
    market_context=market_context,
    note="Finanzierung wurde separat vom operativen Optimizer bewertet."
  )
}

fbc_make_decision_payload38 <- function(
    problem,
    ranking,
    validations,
    implementation_plan,
    shapley_fun=NULL,
    break_even_evaluator=NULL,
    current_financing=NULL,
    financing_alternative=NULL,
    financing_comparison_horizon_months=NULL,
    free_cash_before_debt_service_month=NULL,
    min_debt_service_ratio=NULL,
    financing_market_context=NULL,
    schema_version="fbc_decision_payload_v1"
){
  if(is.null(ranking$selected))
    stop("No selected recommendation available; economic ranking has no winner.")

  id <- ranking$selected_id
  selected <- ranking$selected
  val <- validations[[id]]

  current_net <- as.numeric(problem$evaluate(problem$registry$current))
  target <- as.numeric(problem$desired_net)
  gap <- max(0,target-current_net)
  gap_pct <- if(is.finite(target) && target>0) 100*gap/target else NA_real_

  changes <- fbc_build_change_payload38(problem,selected)
  sh <- fbc_build_shapley_explanation38(problem,selected,shapley_fun)

  be_detail <- NULL
  if(!is.null(break_even_evaluator)){
    be_detail <- break_even_evaluator(selected$solution)
  }

  fin <- fbc_build_financing_branch38(
    current_financing=current_financing,
    financing_alternative=financing_alternative,
    comparison_horizon_months=financing_comparison_horizon_months,
    free_cash_before_debt_service_month=free_cash_before_debt_service_month,
    min_debt_service_ratio=min_debt_service_ratio,
    market_context=financing_market_context
  )

  demand_detail <- if(is.function(problem$demand_details)){
    problem$demand_details(selected$solution)
  } else {
    NULL
  }

  list(
    schema_version=schema_version,
    status =
  if(current_net >= target && !length(changes))
    "target_already_reached"
  else
    "recommendation_selected",
    current=list(
      expected_net=current_net,
      monthly_target=target,
      target_gap_eur=gap,
      target_gap_percent=gap_pct
    ),
    recommendation=list(
      candidate_id=id,
      active_levers=selected$active_levers,
      changes=changes,
      projected_net=as.numeric(selected$projected_net),
      demand=demand_detail,
      explanation=sh
    ),
target_path=list(
  time_to_target_months=val$time_to_target_months,
  target_reached_within_horizon=val$target_reached_within_horizon,
  implementation_months_max=val$implementation_months_max,
  liquidity_bridge_need_eur=val$liquidity_bridge_need_eur,
 
  path=val$time_path
),
    post_decision=list(
      post_target_stable=val$post_target_stable,
      max_post_target_gap_eur=val$max_post_target_gap_eur,
      mc_target_probability=val$mc_target_probability,
      mc_policy_ok=val$mc_policy_ok,
      business_break_even_margin_eur=val$business_break_even_margin_eur,
      business_break_even_ok=val$business_break_even_ok,
      employee_break_even_ok=val$employee_break_even_ok,
      capital_service_ratio=val$capital_service_ratio,
      capital_service_ok=val$capital_service_ok
    ),
    break_even=be_detail,
    financing=fin,
    method=list(
      candidate_generation="bounded nonlinear candidate pool",
      selection="hard business policy + Pareto + safety-first lexicographic ranking",
      explanation="exact Shapley on nonlinear problem evaluation",
      demand=problem$demand_model,
      financing="separate alternative branch; never an operating KKT lever"
    )
  )
}

fbc_run_production_decision38 <- function(
    problem,
    candidate_pool,
    implementation_plan,
    horizon_months,
    post_target_months,
    ranking_policy=list(),
    monthly_evaluator=NULL,
    mc_evaluator=NULL,
    break_even_evaluator=NULL,
    capital_service_evaluator=NULL,
    min_target_probability=NULL,
    min_debt_service_ratio=NULL,
    shapley_fun=NULL,
    current_financing=NULL,
    financing_alternative=NULL,
    financing_comparison_horizon_months=NULL,
    free_cash_before_debt_service_month=NULL,
    financing_market_context=NULL
){
  if(!length(candidate_pool))
    stop("candidate_pool is empty.")

  validations <- fbc_validate_candidate_pool(
    problem=problem,
    candidate_pool=candidate_pool,
    implementation_plan=implementation_plan,
    horizon_months=horizon_months,
    post_target_months=post_target_months,
    monthly_evaluator=monthly_evaluator,
    mc_evaluator=mc_evaluator,
    break_even_evaluator=break_even_evaluator,
    capital_service_evaluator=capital_service_evaluator,
    min_target_probability=min_target_probability,
    min_debt_service_ratio=min_debt_service_ratio
  )

  policy <- ranking_policy
  if(!is.null(min_target_probability))
    policy$min_target_probability <- min_target_probability
  if(!is.null(min_debt_service_ratio))
    policy$min_debt_service_ratio <- min_debt_service_ratio

  ranking <- fbc_rank_economic_candidates(
    problem=problem,
    candidate_pool=candidate_pool,
    validations=validations,
    policy=policy
  )

  if(is.null(ranking$selected)){
    return(list(
      feasible=FALSE,
      ranking=ranking,
      validations=validations,
      payload=NULL,
      reason=ranking$reason
    ))
  }

  payload <- fbc_make_decision_payload38(
    problem=problem,
    ranking=ranking,
    validations=validations,
    implementation_plan=implementation_plan,
    shapley_fun=shapley_fun,
    break_even_evaluator=break_even_evaluator,
    current_financing=current_financing,
    financing_alternative=financing_alternative,
    financing_comparison_horizon_months=financing_comparison_horizon_months,
    free_cash_before_debt_service_month=free_cash_before_debt_service_month,
    min_debt_service_ratio=min_debt_service_ratio,
    financing_market_context=financing_market_context
  )

  list(
    feasible=TRUE,
    selected=ranking$selected,
    selected_id=ranking$selected_id,
    ranking=ranking,
    validations=validations,
    payload=payload
  )
}

fbc_export_decision_json38 <- function(x,file,pretty=TRUE){
  payload <- if(!is.null(x$payload)) x$payload else x
  if(!requireNamespace("jsonlite",quietly=TRUE))
    stop("Package 'jsonlite' is required for JSON export.")
  jsonlite::write_json(
    payload,
    path=file,
    pretty=pretty,
    auto_unbox=TRUE,
    na="null",
    null="null",
    digits=NA
  )
  invisible(normalizePath(file,mustWork=FALSE))
}

cat("\n38 Production Decision Pipeline loaded.\n")
