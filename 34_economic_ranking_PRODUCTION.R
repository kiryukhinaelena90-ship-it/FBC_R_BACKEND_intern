# ============================================================
# 34_economic_ranking.R
# FUTURE Business Cockpit
# Economic ranking — PRODUCTION SAFETY-FIRST, no arbitrary global weights
# ============================================================
# Purpose:
# - rank already feasible KKT candidates economically
# - no hard "minimum number of levers wins" rule
# - no fantasy weights such as Preis=0.7 / Stunden=1.3
# - hard business requirements are checked first
# - remaining trade-offs are handled by Pareto filtering and an explicit
#   safety-first lexicographic product policy (order, not numeric weights)
# - cardinality is a late complexity tie-break only
#
# IMPORTANT:
# This module does NOT invent robustness thresholds, implementation times,
# market price bounds, cost reduction limits or debt-service thresholds.
# They must come from Reality/Constraints or an explicit product policy.
# ============================================================

fbc_num_or_na <- function(x){
  if(is.null(x) || length(x)==0 || !is.finite(as.numeric(x)[1])) return(NA_real_)
  as.numeric(x)[1]
}

fbc_bool_or_na <- function(x){
  if(is.null(x) || length(x)==0 || is.na(x[1])) return(NA)
  isTRUE(x[1])
}

fbc_candidate_changes <- function(problem, solution, tol=1e-6){
  r <- problem$registry
  x <- as.numeric(solution)
  names(x) <- names(solution)
  if(is.null(names(x)) || any(!r$name %in% names(x))) {
    x <- as.numeric(solution)
    names(x) <- r$name
  }
  out <- data.frame(
    lever=r$name,
    current=r$current,
    proposed=as.numeric(x[r$name]),
    delta=as.numeric(x[r$name])-r$current,
    stringsAsFactors=FALSE
  )
  out$changed <- abs(out$delta) > tol
  out[out$changed,,drop=FALSE]
}

# Validation object expected per candidate (all fields optional unless policy
# makes them mandatory):
#   time_to_target_months
#   target_reached_within_horizon
#   post_target_stable
#   mc_target_probability
#   business_break_even_margin_eur
#   business_break_even_ok
#   employee_break_even_ok
#   capital_service_ratio
#   capital_service_ok
#   max_post_target_gap_eur
#   implementation_months_max
#
# `candidate_id` should match the list name or position in candidate_pool.

fbc_build_economic_table <- function(problem, candidate_pool, validations=NULL){
  if(!length(candidate_pool)) return(data.frame())

  if(is.null(validations)) validations <- vector("list",length(candidate_pool))
  if(length(validations) != length(candidate_pool))
    stop("validations muss dieselbe Länge wie candidate_pool haben.")

  rows <- vector("list",length(candidate_pool))
  for(i in seq_along(candidate_pool)){
    cnd <- candidate_pool[[i]]
    v <- validations[[i]]
    if(is.null(v)) v <- list()

    sol <- cnd$solution
    changes <- fbc_candidate_changes(problem,sol)
    rows[[i]] <- data.frame(
      candidate_id=i,
      cardinality=if(!is.null(cnd$cardinality)) cnd$cardinality else nrow(changes),
      levers=paste(cnd$active_levers,collapse=" + "),
      projected_net=fbc_num_or_na(cnd$projected_net),
      target_gap_eur=max(0,problem$desired_net-fbc_num_or_na(cnd$projected_net)),
      owner_extra_hours=fbc_num_or_na(cnd$metrics$owner_extra_hours),
      normalized_movement=fbc_num_or_na(cnd$metrics$normalized_movement),
      time_to_target_months=fbc_num_or_na(v$time_to_target_months),
      target_reached_within_horizon=fbc_bool_or_na(v$target_reached_within_horizon),
      post_target_stable=fbc_bool_or_na(v$post_target_stable),
      mc_target_probability=fbc_num_or_na(v$mc_target_probability),
      business_break_even_margin_eur=fbc_num_or_na(v$business_break_even_margin_eur),
      business_break_even_ok=fbc_bool_or_na(v$business_break_even_ok),
      employee_break_even_ok=fbc_bool_or_na(v$employee_break_even_ok),
      capital_service_ratio=fbc_num_or_na(v$capital_service_ratio),
      capital_service_ok=fbc_bool_or_na(v$capital_service_ok),
      max_post_target_gap_eur=fbc_num_or_na(v$max_post_target_gap_eur),
      implementation_months_max=fbc_num_or_na(v$implementation_months_max),
      stringsAsFactors=FALSE
    )
  }
  do.call(rbind,rows)
}

# Hard eligibility. Numeric thresholds are NEVER defaulted here.
fbc_apply_hard_business_policy <- function(tab, policy=list()){
  if(!nrow(tab)) return(tab)
  ok <- rep(TRUE,nrow(tab))
  reasons <- vector("list",nrow(tab))
  add_reason <- function(idx,msg){
    for(j in idx) reasons[[j]] <<- c(reasons[[j]],msg)
  }

  # Every KKT candidate must at least reach deterministic target.
  bad <- which(tab$target_gap_eur > 1e-4 | !is.finite(tab$projected_net))
  if(length(bad)){ok[bad] <- FALSE; add_reason(bad,"deterministic_target_not_reached")}

  if(isTRUE(policy$require_time_to_target)){
    bad <- which(is.na(tab$target_reached_within_horizon) | !tab$target_reached_within_horizon)
    if(length(bad)){ok[bad] <- FALSE; add_reason(bad,"target_not_reached_in_time_horizon")}
  }

  if(isTRUE(policy$require_post_target_stability)){
    bad <- which(is.na(tab$post_target_stable) | !tab$post_target_stable)
    if(length(bad)){ok[bad] <- FALSE; add_reason(bad,"post_target_not_stable")}
  }

  if(!is.null(policy$min_target_probability)){
    p <- as.numeric(policy$min_target_probability)
    if(!is.finite(p) || p<0 || p>1) stop("min_target_probability muss explizit zwischen 0 und 1 liegen.")
    bad <- which(is.na(tab$mc_target_probability) | tab$mc_target_probability < p)
    if(length(bad)){ok[bad] <- FALSE; add_reason(bad,"mc_target_probability_below_policy")}
  }

  if(isTRUE(policy$require_business_break_even)){
    bad <- which(is.na(tab$business_break_even_ok) | !tab$business_break_even_ok)
    if(length(bad)){ok[bad] <- FALSE; add_reason(bad,"business_break_even_not_covered")}
  }

  if(isTRUE(policy$require_employee_break_even)){
    # NA is allowed when employee is not directly billed; caller should set
    # policy FALSE in that situation or pass TRUE for non-applicable case.
    bad <- which(is.na(tab$employee_break_even_ok) | !tab$employee_break_even_ok)
    if(length(bad)){ok[bad] <- FALSE; add_reason(bad,"employee_break_even_not_covered")}
  }

  if(!is.null(policy$min_debt_service_ratio)){
    q <- as.numeric(policy$min_debt_service_ratio)
    if(!is.finite(q) || q<=0) stop("min_debt_service_ratio muss explizit > 0 sein.")
    bad <- which(is.na(tab$capital_service_ratio) | tab$capital_service_ratio < q)
    if(length(bad)){ok[bad] <- FALSE; add_reason(bad,"capital_service_ratio_below_policy")}
  }

  tab$eligible <- ok
  tab$exclusion_reason <- vapply(reasons,function(x)paste(unique(x),collapse=";"),character(1))
  tab
}

# Pareto dominance on explicit business metrics. No weighted sum.
# directions: named vector with "min" or "max".
fbc_pareto_front <- function(tab, directions){
  if(!nrow(tab)) return(logical(0))
  vars <- names(directions)
  missing <- setdiff(vars,names(tab))
  if(length(missing)) stop(paste("Pareto-Metriken fehlen:",paste(missing,collapse=", ")))

  # A metric with NA cannot establish dominance; compare only candidates with
  # complete values for the requested Pareto set.
  complete <- complete.cases(tab[,vars,drop=FALSE])
  keep <- rep(FALSE,nrow(tab))
  idx <- which(complete)
  if(!length(idx)) return(keep)

  M <- as.matrix(tab[idx,vars,drop=FALSE])
  for(j in seq_along(vars)) if(directions[[j]]=="max") M[,j] <- -M[,j]

  nondom <- rep(TRUE,nrow(M))
  for(i in seq_len(nrow(M))){
    for(k in seq_len(nrow(M))){
      if(i==k) next
      # k dominates i if no worse in all and strictly better in >=1.
      if(all(M[k,] <= M[i,]) && any(M[k,] < M[i,])){
        nondom[i] <- FALSE
        break
      }
    }
  }
  keep[idx[nondom]] <- TRUE
  keep
}

# Final selection is lexicographic ONLY after hard policy and Pareto filtering.
# This is deliberately an order of interpretable business metrics, not weights.
# action_count/cardinality should be placed late if used at all.
fbc_rank_economic_candidates <- function(
    problem,
    candidate_pool,
    validations,
    policy=list(),
    ranking_order=c(
      "max_post_target_gap_eur",
      "business_break_even_margin_eur",
      "mc_target_probability",
      "capital_service_ratio",
      "owner_extra_hours",
      "time_to_target_months",
      "implementation_months_max",
      "cardinality",
      "normalized_movement"
    ),
    ranking_direction=c(
      max_post_target_gap_eur="min",
      business_break_even_margin_eur="max",
      mc_target_probability="max",
      capital_service_ratio="max",
      owner_extra_hours="min",
      time_to_target_months="min",
      implementation_months_max="min",
      cardinality="min",
      normalized_movement="min"
    ),
    pareto_metrics=c(
      max_post_target_gap_eur="min",
      business_break_even_margin_eur="max",
      mc_target_probability="max",
      capital_service_ratio="max",
      owner_extra_hours="min",
      time_to_target_months="min"
    )
){
  tab <- fbc_build_economic_table(problem,candidate_pool,validations)
  tab <- fbc_apply_hard_business_policy(tab,policy)
  elig <- tab[tab$eligible,,drop=FALSE]
  if(!nrow(elig)){
    return(list(
      selected=NULL,
      selected_id=NA_integer_,
      table=tab,
      pareto_table=data.frame(),
      reason="Kein Kandidat erfüllt die explizite Business-Policy."
    ))
  }

  # Use only Pareto metrics that are actually available for every eligible row.
  pm <- pareto_metrics[names(pareto_metrics) %in% names(elig)]
  if(length(pm)){
    usable <- names(pm)[vapply(names(pm),function(nm)all(is.finite(elig[[nm]])),logical(1))]
    pm <- pm[usable]
  }
  if(length(pm)>=2){
    pf <- fbc_pareto_front(elig,pm)
    pareto <- elig[pf,,drop=FALSE]
  } else {
    pareto <- elig
  }

  # Lexicographic ordering among the non-dominated / eligible set.
  ord_vars <- ranking_order[ranking_order %in% names(pareto)]
  ord_vars <- ord_vars[vapply(ord_vars,function(nm)all(is.finite(pareto[[nm]])),logical(1))]
  if(!length(ord_vars)){
    # No fabricated fallback criterion: return shortlist but no winner.
    return(list(
      selected=NULL,
      selected_id=NA_integer_,
      table=tab,
      pareto_table=pareto,
      reason="Economic ranking pending: keine vollständigen Ranking-Metriken verfügbar."
    ))
  }

  keys <- lapply(ord_vars,function(nm){
    x <- pareto[[nm]]
    dir <- ranking_direction[[nm]]
    if(is.null(dir)) stop(paste("Ranking-Richtung fehlt für",nm))
    if(dir=="max") -x else x
  })
  o <- do.call(order,c(keys,list(na.last=TRUE)))
  winner <- pareto[o[1],,drop=FALSE]
  id <- winner$candidate_id[[1]]

  list(
    selected=candidate_pool[[id]],
    selected_id=id,
    selected_metrics=winner,
    table=tab,
    pareto_table=pareto,
    policy=policy,
    ranking_order=ord_vars,
    reason="selected_from_eligible_pareto_set"
  )
}

# Explicit production policy descriptor. This is an ordered business policy,
# not a weighted score. Safety and post-target viability precede speed and
# complexity. Missing metrics are not fabricated; they are simply unavailable
# for ranking until their validators are connected.
fbc_production_ranking_policy <- function(){
  list(
    name="safety_first_v1",
    order=c(
      "max_post_target_gap_eur",
      "business_break_even_margin_eur",
      "mc_target_probability",
      "capital_service_ratio",
      "owner_extra_hours",
      "time_to_target_months",
      "implementation_months_max",
      "cardinality",
      "normalized_movement"
    ),
    meaning=c(
      max_post_target_gap_eur="Nach Zielerreichung keine erneute Ziellücke bevorzugen",
      business_break_even_margin_eur="größere Sicherheitsmarge über Break-even bevorzugen",
      mc_target_probability="höhere Zielerreichungswahrscheinlichkeit bevorzugen",
      capital_service_ratio="größere Kapitaldienstreserve bevorzugen",
      owner_extra_hours="zusätzliche Inhaberbelastung minimieren",
      time_to_target_months="bei sonst vergleichbarer Sicherheit schnelleres Ziel bevorzugen",
      implementation_months_max="kürzere Umsetzung bevorzugen",
      cardinality="Anzahl der Hebel nur als später Komplexitäts-Tie-Break",
      normalized_movement="rein mathematischer letzter Tie-Break"
    )
  )
}

cat("\n34 Economic Ranking PRODUCTION loaded.\n")
