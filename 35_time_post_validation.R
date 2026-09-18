# ============================================================
# 35_time_post_validation.R
# FUTURE Business Cockpit
# Time-to-target + post-decision validation
# ============================================================
# No invented implementation duration:
# every changed lever must have an explicit implementation plan.
# ============================================================

fbc_validate_implementation_plan <- function(problem,candidate,implementation_plan){
  if(is.null(implementation_plan) || !is.data.frame(implementation_plan))
    stop("implementation_plan muss ein data.frame sein.")
  req <- c("lever","months_to_full","mode")
  miss <- setdiff(req,names(implementation_plan))
  if(length(miss)) stop(paste("implementation_plan fehlt:",paste(miss,collapse=", ")))

  changed <- fbc_candidate_changes(problem,candidate$solution)$lever
  p <- implementation_plan[implementation_plan$lever %in% changed,,drop=FALSE]
  miss_changed <- setdiff(changed,p$lever)
  if(length(miss_changed))
    stop(paste("Keine Umsetzungsdauer für:",paste(miss_changed,collapse=", ")))
  if(anyDuplicated(p$lever)) stop("Jeder Hebel darf im implementation_plan nur einmal vorkommen.")
  if(any(!is.finite(p$months_to_full) | p$months_to_full<0))
    stop("months_to_full muss für alle geänderten Hebel >= 0 sein.")
  if(any(!p$mode %in% c("step","linear")))
    stop("mode muss 'step' oder 'linear' sein.")
  p
}

fbc_progress_at_month <- function(month,months_to_full,mode){
  if(months_to_full==0) return(1)
  if(mode=="step") return(if(month>=months_to_full) 1 else 0)
  min(1,max(0,month/months_to_full))
}

fbc_solution_at_month <- function(problem,candidate,implementation_plan,month){
  r <- problem$registry
  x0 <- r$current; names(x0) <- r$name
  x1 <- as.numeric(candidate$solution); names(x1) <- names(candidate$solution)
  if(any(!r$name %in% names(x1))){names(x1)<-r$name}
  x <- x0
  p <- fbc_validate_implementation_plan(problem,candidate,implementation_plan)
  for(i in seq_len(nrow(p))){
    nm <- p$lever[i]
    prog <- fbc_progress_at_month(month,p$months_to_full[i],p$mode[i])
    x[nm] <- x0[nm] + prog*(x1[nm]-x0[nm])
  }
  x
}

# monthly_evaluator optional signature: function(solution, month) -> net result.
# If absent, deterministic problem$evaluate is used.
fbc_build_time_path <- function(
    problem,candidate,implementation_plan,horizon_months,
    monthly_evaluator=NULL
){
  if(length(horizon_months)!=1 || !is.finite(horizon_months) || horizon_months<1)
    stop("horizon_months muss explizit >= 1 gesetzt werden.")
  horizon_months <- as.integer(horizon_months)
  p <- fbc_validate_implementation_plan(problem,candidate,implementation_plan)
  eval_fun <- if(is.null(monthly_evaluator)){
    function(solution,month) problem$evaluate(solution)
  } else monthly_evaluator

  rows <- vector("list",horizon_months+1L)
  for(t in 0:horizon_months){
    x <- fbc_solution_at_month(problem,candidate,p,t)
    val <- as.numeric(eval_fun(x,t))[1]
    rows[[t+1L]] <- data.frame(
      month=t,
      result=val,
      target=problem$desired_net,
      gap=max(0,problem$desired_net-val),
      target_reached=is.finite(val) && val>=problem$desired_net-1e-6,
      stringsAsFactors=FALSE
    )
  }
  path <- do.call(rbind,rows)
  hit <- which(path$target_reached)
  time_to_target <- if(length(hit)) path$month[hit[1]] else NA_integer_
  list(
    path=path,
    time_to_target_months=time_to_target,
    target_reached_within_horizon=!is.na(time_to_target),
    implementation_months_max=max(p$months_to_full,0)
  )
}

# External validators are callbacks to avoid inventing business formulas:
# mc_evaluator(solution) -> numeric vector of MC net results
# break_even_evaluator(solution) -> list(margin_eur=..., ok=..., employee_ok=...)
# capital_service_evaluator(solution) -> list(ratio=..., ok=...) OR numeric ratio
#
# min_target_probability and min_debt_service_ratio are optional explicit policy
# parameters. If absent, probabilities/ratios are reported but not classified.
fbc_validate_post_decision <- function(
    problem,candidate,implementation_plan,horizon_months,
    post_target_months,
    monthly_evaluator=NULL,
    mc_evaluator=NULL,
    break_even_evaluator=NULL,
    capital_service_evaluator=NULL,
    min_target_probability=NULL,
    min_debt_service_ratio=NULL
){
  tp <- fbc_build_time_path(
    problem,candidate,implementation_plan,horizon_months,monthly_evaluator
  )
liquidity_bridge_need_eur <- 0

if (
  is.data.frame(tp$path) &&
  nrow(tp$path) > 0 &&
  "gap" %in% names(tp$path)
) {
  gaps <- as.numeric(tp$path$gap)
  gaps <- gaps[is.finite(gaps) & gaps > 0]

  if (length(gaps)) {
    liquidity_bridge_need_eur <- sum(gaps)
  }
}
  if(length(post_target_months)!=1 || !is.finite(post_target_months) || post_target_months<0)
    stop("post_target_months muss explizit >= 0 gesetzt werden.")
  post_target_months <- as.integer(post_target_months)

  post_stable <- NA
  max_post_gap <- NA_real_
  if(!is.na(tp$time_to_target_months)){
    start <- tp$time_to_target_months
    end <- min(horizon_months,start+post_target_months)
    post <- tp$path[tp$path$month>=start & tp$path$month<=end,,drop=FALSE]
    if(nrow(post)){
      post_stable <- all(post$result>=problem$desired_net-1e-6)
      max_post_gap <- max(pmax(0,problem$desired_net-post$result))
    }
  }

  mc_prob <- NA_real_
  mc_ok <- NA
  if(!is.null(mc_evaluator)){
    draws <- as.numeric(mc_evaluator(candidate$solution))
    draws <- draws[is.finite(draws)]
    if(!length(draws)) stop("mc_evaluator liefert keine gültigen Ziehungen.")
    mc_prob <- mean(draws>=problem$desired_net)
    if(!is.null(min_target_probability)){
      p <- as.numeric(min_target_probability)
      if(!is.finite(p)||p<0||p>1) stop("min_target_probability muss zwischen 0 und 1 liegen.")
      mc_ok <- mc_prob>=p
    }
  }

  be_margin <- NA_real_; be_ok <- NA; emp_be_ok <- NA
  if(!is.null(break_even_evaluator)){
    be <- break_even_evaluator(candidate$solution)
    if(!is.null(be$margin_eur)) be_margin <- as.numeric(be$margin_eur)[1]
    if(!is.null(be$ok)) be_ok <- isTRUE(be$ok)
    if(!is.null(be$employee_ok)) emp_be_ok <- if(is.na(be$employee_ok)) NA else isTRUE(be$employee_ok)
  }

  ds_ratio <- NA_real_; ds_ok <- NA
  if(!is.null(capital_service_evaluator)){
    ds <- capital_service_evaluator(candidate$solution)
    ds_ratio <- if(is.list(ds)) as.numeric(ds$ratio)[1] else as.numeric(ds)[1]
    if(!is.null(min_debt_service_ratio)){
      q <- as.numeric(min_debt_service_ratio)
      if(!is.finite(q)||q<=0) stop("min_debt_service_ratio muss > 0 sein.")
      ds_ok <- is.finite(ds_ratio) && ds_ratio>=q
    } else if(is.list(ds) && !is.null(ds$ok)) {
      ds_ok <- if(is.na(ds$ok)) NA else isTRUE(ds$ok)
    }
  }

  list(
    time_path=tp$path,
    time_to_target_months=tp$time_to_target_months,
    target_reached_within_horizon=tp$target_reached_within_horizon,
    implementation_months_max=tp$implementation_months_max,
    post_target_stable=post_stable,
    max_post_target_gap_eur=max_post_gap,
    mc_target_probability=mc_prob,
    mc_policy_ok=mc_ok,
    business_break_even_margin_eur=be_margin,
    business_break_even_ok=be_ok,
    employee_break_even_ok=emp_be_ok,
    capital_service_ratio=ds_ratio,
    capital_service_ok=ds_ok
    liquidity_bridge_need_eur=liquidity_bridge_need_eur,
  )
}

# Batch validation for a candidate pool.
fbc_validate_candidate_pool <- function(
    problem,candidate_pool,implementation_plan,horizon_months,post_target_months,
    monthly_evaluator=NULL,mc_evaluator=NULL,break_even_evaluator=NULL,
    capital_service_evaluator=NULL,min_target_probability=NULL,
    min_debt_service_ratio=NULL
){
  lapply(candidate_pool,function(cnd){
    fbc_validate_post_decision(
      problem=problem,candidate=cnd,implementation_plan=implementation_plan,
      horizon_months=horizon_months,post_target_months=post_target_months,
      monthly_evaluator=monthly_evaluator,mc_evaluator=mc_evaluator,
      break_even_evaluator=break_even_evaluator,
      capital_service_evaluator=capital_service_evaluator,
      min_target_probability=min_target_probability,
      min_debt_service_ratio=min_debt_service_ratio
    )
  })
}

cat("\n35 Time/Post-Decision Validation loaded.\n")
