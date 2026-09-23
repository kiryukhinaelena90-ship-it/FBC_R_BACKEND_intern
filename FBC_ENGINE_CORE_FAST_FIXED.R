# ============================================================
# FBC_ENGINE_CORE_FAST.R
# FUTURE Business Cockpit
# Bounded nonlinear candidate core
#
# Compatibility:
# - public function names are retained;
# - candidate generation still enumerates admissible lever sets;
# - final economic ranking remains outside this module;
# - financing is never introduced as a decision variable.
#
# The previous closed-form bilinear KKT step assumed revenue = price * hours.
# That assumption is no longer valid after the constant price-elasticity layer.
# Candidate solutions are therefore generated numerically against
# problem$evaluate(), which is the canonical nonlinear financial evaluator.
# ============================================================

fbc_fast_objective <- function(problem,x){
  r<-problem$registry
  0.5*sum(((x-r$current)/r$scale)^2)
}

fbc_fast_grad_net <- function(problem,x){
  n<-length(x)
  g<-numeric(n)
  for(i in seq_len(n)){
    h<-max(abs(x[i])*1e-6,1e-6)
    xp<-xm<-x
    xp[i]<-min(problem$registry$upper[i],x[i]+h)
    xm[i]<-max(problem$registry$lower[i],x[i]-h)
    if(abs(xp[i]-xm[i])<1e-12){
      g[i]<-0
    } else {
      g[i]<-(problem$evaluate(xp)-problem$evaluate(xm))/(xp[i]-xm[i])
    }
  }
  g
}

fbc_fast_hessian <- function(problem,x=problem$registry$current){
  # Numerical Hessian of the canonical nonlinear net evaluator.
  n<-length(x)
  H<-matrix(0,n,n,dimnames=list(problem$registry$name,problem$registry$name))
  g0<-fbc_fast_grad_net(problem,x)
  for(j in seq_len(n)){
    h<-max(abs(x[j])*1e-5,1e-5)
    xp<-xm<-x
    xp[j]<-min(problem$registry$upper[j],x[j]+h)
    xm[j]<-max(problem$registry$lower[j],x[j]-h)
    if(abs(xp[j]-xm[j])<1e-12) next
    gp<-fbc_fast_grad_net(problem,xp)
    gm<-fbc_fast_grad_net(problem,xm)
    H[,j]<-(gp-gm)/(xp[j]-xm[j])
  }
  (H+t(H))/2
}

fbc_fast_status <- function(problem,x,tol=1e-7){
  r<-problem$registry
  st<-rep(0L,nrow(r))
  st[abs(x-r$lower)<=tol]<--1L
  st[abs(x-r$upper)<=tol]<-1L
  setNames(st,r$name)
}

fbc_fast_kkt_check <- function(problem,x,lambda=NA_real_,status=NULL,tol=1e-7){
  r<-problem$registry
  if(is.null(status)) status<-fbc_fast_status(problem,x,tol)
  net<-as.numeric(problem$evaluate(x))
  list(
    valid=is.finite(net) &&
      net>=problem$desired_net-1e-4 &&
      all(x>=r$lower-tol) &&
      all(x<=r$upper+tol),
    method="numeric nonlinear box-constrained search",
    lambda_target=lambda,
    mu_lower=setNames(rep(NA_real_,nrow(r)),r$name),
    mu_upper=setNames(rep(NA_real_,nrow(r)),r$name),
    stationarity_residual=NA_real_,
    complementarity_residual=NA_real_,
    target_residual=problem$desired_net-net
  )
}

fbc_fast_best_corner <- function(problem){
  r<-problem$registry
  n<-nrow(r)
  corners<-as.matrix(expand.grid(rep(list(c(0L,1L)),n)))
  best_net<--Inf
  best_x<-r$current

  for(i in seq_len(nrow(corners))){
    x<-ifelse(corners[i,]==0L,r$lower,r$upper)
    val<-tryCatch(problem$evaluate(x),error=function(e) -Inf)
    if(is.finite(val) && val>best_net){
      best_net<-val
      best_x<-x
    }
  }
  list(net=best_net,x=setNames(best_x,r$name))
}

fbc_fast_project_current_to_target <- function(problem,x_feasible,tol=1e-7){
  r<-problem$registry
  x0<-r$current
  if(problem$evaluate(x0)>=problem$desired_net-tol)
    return(setNames(x0,r$name))

  if(problem$evaluate(x_feasible)<problem$desired_net-tol)
    return(NULL)

  lo<-0
  hi<-1
  for(i in seq_len(80)){
    mid<-(lo+hi)/2
    x<-x0+mid*(x_feasible-x0)
    val<-problem$evaluate(x)
    if(is.finite(val) && val>=problem$desired_net){
      hi<-mid
    } else {
      lo<-mid
    }
  }

  x<-x0+hi*(x_feasible-x0)
  names(x)<-r$name
  x
}

fbc_fast_repair_feasible <- function(problem,x,corner,tol=1e-7){
  val<-tryCatch(problem$evaluate(x),error=function(e) -Inf)

  if(is.finite(val) && val>=problem$desired_net-tol){
    return(fbc_fast_project_current_to_target(problem,x,tol))
  }

  corner_val<-tryCatch(problem$evaluate(corner),error=function(e) -Inf)
  if(!is.finite(corner_val) || corner_val<problem$desired_net-tol)
    return(NULL)

  # Move from the numerical candidate towards the best feasible corner until
  # the target is crossed, then project once more from the factual current
  # state to keep unnecessary movement out of the candidate.
  lo<-0
  hi<-1
  for(i in seq_len(80)){
    mid<-(lo+hi)/2
    z<-x+mid*(corner-x)
    v<-tryCatch(problem$evaluate(z),error=function(e) -Inf)
    if(is.finite(v) && v>=problem$desired_net){
      hi<-mid
    } else {
      lo<-mid
    }
  }
  repaired<-x+hi*(corner-x)
  fbc_fast_project_current_to_target(problem,repaired,tol)
}

fbc_fast_y_from_x <- function(problem,x){
  r<-problem$registry
  span<-r$upper-r$lower
  y<-rep(0,length(span))
  ok<-span>1e-12
  y[ok]<-(x[ok]-r$lower[ok])/span[ok]
  pmin(1,pmax(0,y))
}

fbc_fast_x_from_y <- function(problem,y){
  r<-problem$registry
  x<-r$lower+pmin(1,pmax(0,y))*(r$upper-r$lower)
  names(x)<-r$name
  x
}

fbc_fast_numeric_search <- function(problem,start_y,rho){
  net_scale<-max(abs(problem$desired_net),1)

  fn<-function(y){
    x<-fbc_fast_x_from_y(problem,y)
    net<-tryCatch(problem$evaluate(x),error=function(e) NA_real_)
    if(!is.finite(net)) return(.Machine$double.xmax/100)
    gap<-max(0,problem$desired_net-net)/net_scale
    fbc_fast_objective(problem,x)+rho*gap^2
  }

  tryCatch(
    optim(
      par=pmin(1,pmax(0,start_y)),
      fn=fn,
      method="L-BFGS-B",
      lower=rep(0,length(start_y)),
      upper=rep(1,length(start_y)),
      control=list(maxit=800,factr=1e7,pgtol=1e-9)
    ),
    error=function(e) NULL
  )
}

fbc_fast_solve_reduced_kkt <- function(problem){
  if(problem$objective_mode!="reach_income_target")
    stop("FAST nonlinear core ist aktuell für reach_income_target validiert.")

  r<-problem$registry
  n<-nrow(r)

  current_net<-as.numeric(problem$evaluate(r$current))
  if(current_net>=problem$desired_net){
    st<-fbc_fast_status(problem,r$current)
    chk<-fbc_fast_kkt_check(problem,r$current,status=st)
    return(list(
      feasible=TRUE,
      solution=setNames(r$current,r$name),
      projected_net=current_net,
      objective_value=0,
      kkt=chk,
      active_status=st,
      candidates=1,
      solver="numeric_nonlinear_box"
    ))
  }

  corner<-fbc_fast_best_corner(problem)
  if(!is.finite(corner$net) || corner$net<problem$desired_net-1e-7){
    return(list(
      feasible=FALSE,
      solution=NULL,
      candidates=0,
      solver="numeric_nonlinear_box"
    ))
  }

  current_y<-fbc_fast_y_from_x(problem,r$current)
  corner_y<-fbc_fast_y_from_x(problem,corner$x)

  starts<-list(
    current_y,
    (current_y+corner_y)/2,
    corner_y
  )

  # Add deterministic one-lever-biased starts. This helps when different
  # nonlinear levers have very different scales.
  if(n>1){
    for(i in seq_len(n)){
      y<-(current_y+corner_y)/2
      y[i]<-corner_y[i]
      starts[[length(starts)+1]]<-y
    }
  }

  feasible<-list()

  # The exact projected best-corner path is always a safe fallback.
  z0<-fbc_fast_project_current_to_target(problem,corner$x)
  if(!is.null(z0)) feasible[[length(feasible)+1]]<-z0

  for(start in starts){
    y<-start
    for(rho in c(1e2,1e4,1e6,1e8)){
      fit<-fbc_fast_numeric_search(problem,y,rho)
      if(is.null(fit) || any(!is.finite(fit$par))) next
      y<-fit$par
      x<-fbc_fast_x_from_y(problem,y)
      z<-fbc_fast_repair_feasible(problem,x,corner$x)
      if(!is.null(z)) feasible[[length(feasible)+1]]<-z
    }
  }

  if(!length(feasible)){
    return(list(
      feasible=FALSE,
      solution=NULL,
      candidates=0,
      solver="numeric_nonlinear_box"
    ))
  }

  objs<-vapply(feasible,function(x) fbc_fast_objective(problem,x),numeric(1))
  k<-which.min(objs)
  x<-feasible[[k]]
  names(x)<-r$name
  st<-fbc_fast_status(problem,x)
  chk<-fbc_fast_kkt_check(problem,x,status=st)

  list(
    feasible=TRUE,
    solution=x,
    projected_net=as.numeric(problem$evaluate(x)),
    objective_value=objs[k],
    kkt=chk,
    active_status=st,
    candidates=length(feasible),
    solver="numeric_nonlinear_box"
  )
}

# Retained only for compatibility with old diagnostics. The nonlinear solver
# no longer parameterizes a solution by a bilinear KKT lambda.
fbc_fast_x_lambda <- function(problem,status,lambda){
  stop("fbc_fast_x_lambda() is not used by the nonlinear elasticity-aware solver.")
}

fbc_fast_lambda_root <- function(problem,status){
  NA_real_
}

fbc_fast_reduced_problem <- function(full_problem,active_names){
  fr<-full_problem$registry
  idx<-match(active_names,fr$name)
  if(anyNA(idx)) stop("Unbekannter Hebel im reduced problem.")

  rr<-fr[idx,,drop=FALSE]
  full_current<-fr$current
  names(full_current)<-fr$name

  eval_reduced<-function(z){
    names(z)<-rr$name
    full<-full_current
    full[rr$name]<-z
    full_problem$evaluate(full)
  }

  demand_reduced<-if(is.function(full_problem$demand_details)){
    function(z){
      names(z)<-rr$name
      full<-full_current
      full[rr$name]<-z
      full_problem$demand_details(full)
    }
  } else NULL

  list(
    registry=rr,
    evaluate=eval_reduced,
    desired_net=full_problem$desired_net,
    objective_mode=full_problem$objective_mode,
    demand_model=full_problem$demand_model,
    demand_details=demand_reduced
  )
}

fbc_fast_expand_solution <- function(full_problem,active_solution){
  x<-full_problem$registry$current
  names(x)<-full_problem$registry$name
  x[names(active_solution)]<-active_solution
  x
}

fbc_fast_metrics <- function(full_problem,full_solution){
  r<-full_problem$registry
  names(full_solution)<-r$name
  changed<-r$name[abs(full_solution-r$current)>1e-6]

  owner_extra<-if("owner_hours"%in%r$name)
    max(0,as.numeric(full_solution["owner_hours"]-r$current[r$name=="owner_hours"])) else 0

  list(
    action_count=length(changed),
    changed=changed,
    owner_extra_hours=owner_extra,
    surplus_eur=max(0,full_problem$evaluate(full_solution)-full_problem$desired_net),
    normalized_movement=sum(abs((full_solution-r$current)/r$scale))
  )
}

fbc_fast_choose_same_cardinality <- function(candidates){
  owner<-vapply(candidates,function(z)z$metrics$owner_extra_hours,numeric(1))
  surplus<-vapply(candidates,function(z)z$metrics$surplus_eur,numeric(1))
  move<-vapply(candidates,function(z)z$metrics$normalized_movement,numeric(1))
  candidates[[order(owner,surplus,move)[1]]]
}

run_fbc_fast_candidate_search <- function(
    problem,
    max_actions=NULL,
    forbidden_levers=character(0),
    verbose=TRUE
){
  all_names<-setdiff(problem$registry$name,forbidden_levers)
  n<-length(all_names)
  if(is.null(max_actions)) max_actions<-n
  max_actions<-min(max_actions,n)

  tested<-0L
  pruned<-0L
  started<-proc.time()[["elapsed"]]
  pool<-list()
  feasible_by_k<-vector("list",max_actions)

  for(k in seq_len(max_actions)){
    sets<-combn(all_names,k,simplify=FALSE)
    feasible_this_k<-list()

    if(verbose){
      cat(sprintf("\nNONLINEAR SEARCH: %d lever(s), %d combination(s)\n",k,length(sets)))
      flush.console()
    }

    for(j in seq_along(sets)){
      s<-sets[[j]]
      reduced<-fbc_fast_reduced_problem(problem,s)

      corner<-fbc_fast_best_corner(reduced)
      if(corner$net + 1e-6 < problem$desired_net){
        pruned<-pruned+1L
        if(verbose){
          cat(sprintf("  [%d/%d] %-55s PRUNED (max %.2f < target %.2f)\n",
                      j,length(sets),paste(s,collapse=" + "),
                      corner$net,problem$desired_net))
          flush.console()
        }
        next
      }

      tested<-tested+1L
      if(verbose){
        cat(sprintf("  [%d/%d] %-55s NUMERIC...\n",
                    j,length(sets),paste(s,collapse=" + ")))
        flush.console()
      }

      sol<-tryCatch(fbc_fast_solve_reduced_kkt(reduced),error=function(e) NULL)
      if(is.null(sol) || !isTRUE(sol$feasible) || is.null(sol$solution)) next

      full_sol<-fbc_fast_expand_solution(problem,sol$solution)
      if(problem$evaluate(full_sol)+1e-5 < problem$desired_net) next

      metrics<-fbc_fast_metrics(problem,full_sol)

      cand<-list(
        active_levers=metrics$changed,
        reduced_solution=sol,
        solution=full_sol,
        projected_net=problem$evaluate(full_sol),
        metrics=metrics,
        cardinality=length(metrics$changed)
      )

      feasible_this_k[[length(feasible_this_k)+1]]<-cand
      pool[[length(pool)+1]]<-cand
    }

    feasible_by_k[[k]]<-feasible_this_k
    if(verbose && length(feasible_this_k)){
      cat(sprintf(
        "  -> %d feasible candidate(s) with %d lever(s); search continues for economic comparison.\n",
        length(feasible_this_k),k
      ))
      flush.console()
    }
  }

  elapsed<-proc.time()[["elapsed"]]-started
  if(!length(pool)){
    return(list(
      feasible=FALSE,
      selected=NULL,
      selection_pending=FALSE,
      candidate_pool=list(),
      feasible_by_cardinality=feasible_by_k,
      kkt_solved=tested,
      pruned=pruned,
      elapsed_seconds=elapsed,
      reason="Kein zulässiger Hebelsatz erreicht das Ziel.",
      solver="numeric_nonlinear_box"
    ))
  }

  min_k<-min(vapply(pool,function(z)z$cardinality,integer(1)))
  list(
    feasible=TRUE,
    selected=NULL,
    selection_pending=TRUE,
    candidate_pool=pool,
    feasible_by_cardinality=feasible_by_k,
    minimal_action_count=min_k,
    kkt_solved=tested,
    pruned=pruned,
    elapsed_seconds=elapsed,
    solver="numeric_nonlinear_box",
    policy=c(
      "candidate_generation_only",
      "target_feasible",
      "nonlinear_problem_evaluation",
      "no_hard_min_action_selection",
      "economic_ranking_pending"
    )
  )
}

run_fbc_fast_lexicographic <- function(
    problem,
    max_actions=NULL,
    forbidden_levers=character(0),
    verbose=TRUE
){
  warning("run_fbc_fast_lexicographic() returns a nonlinear candidate pool for economic ranking.")
  run_fbc_fast_candidate_search(
    problem=problem,
    max_actions=max_actions,
    forbidden_levers=forbidden_levers,
    verbose=verbose
  )
}

cat("\nFBC nonlinear candidate core loaded.\n")
