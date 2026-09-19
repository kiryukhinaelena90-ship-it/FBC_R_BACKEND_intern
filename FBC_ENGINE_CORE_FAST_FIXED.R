# ============================================================
# FBC_ENGINE_CORE_FAST.R
# FUTURE Business Cockpit
# FAST KKT candidate core — fixed bracketing / no economic early-stop
#
# Purpose:
# - no embedded test suite
# - search admissible lever sets up to max_actions; DO NOT stop at first feasible cardinality
# - KKT runs only on the REDUCED active-variable problem
# - lambda bracketing may evaluate outside bounds; feasibility is checked AFTER root finding
# - this core generates feasible candidates; final economic ranking belongs to post-decision validation
# - financing is never introduced as a decision variable here
# ============================================================

fbc_fast_objective <- function(problem,x){
  r<-problem$registry
  0.5*sum(((x-r$current)/r$scale)^2)
}

fbc_fast_hessian <- function(problem){
  r<-problem$registry
  H<-matrix(0,nrow(r),nrow(r),dimnames=list(r$name,r$name))
  add_cross<-function(a,b){
    if(a%in%r$name && b%in%r$name){
      H[a,b]<<-1
      H[b,a]<<-1
    }
  }
  add_cross("owner_price","owner_hours")
  add_cross("employee_customer_price","employee_billable_hours")
  H
}

fbc_fast_grad_net <- function(problem,x){
  n<-length(x)
  g<-numeric(n)
  for(i in seq_len(n)){
    h<-max(abs(x[i])*1e-6,1e-6)
    xp<-xm<-x
    xp[i]<-xp[i]+h
    xm[i]<-xm[i]-h
    g[i]<-(problem$evaluate(xp)-problem$evaluate(xm))/(2*h)
  }
  g
}

fbc_fast_kkt_check <- function(problem,x,lambda,status,tol=1e-7){
  r<-problem$registry
  Q<-diag(1/(r$scale^2),nrow(r))
  gf<-as.numeric(Q%*%(x-r$current))
  gn<-fbc_fast_grad_net(problem,x)
  base<-gf-lambda*gn

  muL<-muU<-rep(0,nrow(r))
  lo<-which(status==-1L)
  up<-which(status==1L)
  fr<-which(status==0L)

  if(length(lo)) muL[lo]<-base[lo]
  if(length(up)) muU[up]<--base[up]

  station_free<-if(length(fr)) max(abs(base[fr])) else 0
  primal<-abs(problem$desired_net-problem$evaluate(x))<=1e-4 &&
          all(x>=r$lower-1e-7) && all(x<=r$upper+1e-7)
  dual<-lambda>=-tol && all(muL>=-tol) && all(muU>=-tol)
  comp<-max(c(abs(muL*(r$lower-x)),abs(muU*(x-r$upper)),0))

  list(
    valid=primal && dual && station_free<=1e-5 && comp<=1e-5,
    lambda_target=lambda,
    mu_lower=setNames(muL,r$name),
    mu_upper=setNames(muU,r$name),
    stationarity_residual=station_free,
    complementarity_residual=comp,
    target_residual=problem$desired_net-problem$evaluate(x)
  )
}

fbc_fast_x_lambda <- function(problem,status,lambda){
  r<-problem$registry
  n<-nrow(r)
  x<-r$current
  lo<-which(status==-1L)
  up<-which(status==1L)
  fr<-which(status==0L)

  if(length(lo)) x[lo]<-r$lower[lo]
  if(length(up)) x[up]<-r$upper[up]
  if(!length(fr)) return(x)

  H<-fbc_fast_hessian(problem)
  grad0<-fbc_fast_grad_net(problem,r$current)
  b<-grad0-as.numeric(H%*%r$current)
  Qd<-1/(r$scale^2)

  A<-diag(Qd[fr],length(fr))-lambda*H[fr,fr,drop=FALSE]
  rhs<-Qd[fr]*r$current[fr] + lambda*b[fr]
  act<-setdiff(seq_len(n),fr)
  if(length(act)){
    rhs<-rhs + lambda*as.numeric(H[fr,act,drop=FALSE]%*%x[act])
  }

  z<-tryCatch(solve(A,rhs),error=function(e) rep(NA_real_,length(fr)))
  x[fr]<-z
  x
}

fbc_fast_lambda_root <- function(problem,status){
  # IMPORTANT:
  # Root bracketing must not discard a bracket merely because one grid point
  # produces x outside the variable bounds. The true target root can lie
  # BEFORE that bound crossing (this was the owner_price one-lever bug).
  # Bounds are therefore enforced only after a root has been found.

  residual_raw<-function(lam){
    x<-fbc_fast_x_lambda(problem,status,lam)
    if(any(!is.finite(x))) return(NA_real_)
    y<-tryCatch(problem$desired_net-problem$evaluate(x),error=function(e) NA_real_)
    if(is.finite(y)) y else NA_real_
  }

  # Fast logarithmic scan. No 800-point brute-force grid.
  grid<-unique(c(0,10^seq(-12,2,by=.25)))
  vals<-vapply(grid,residual_raw,numeric(1))

  hit<-which(is.finite(vals) & abs(vals)<1e-8)
  if(length(hit)) return(grid[hit[1]])

  for(i in seq_len(length(grid)-1)){
    if(!is.finite(vals[i]) || !is.finite(vals[i+1])) next
    if(vals[i]*vals[i+1] <= 0){
      z<-tryCatch(
        uniroot(residual_raw,c(grid[i],grid[i+1]),tol=1e-10)$root,
        error=function(e) NA_real_
      )
      if(is.finite(z)) return(z)
    }
  }
  NA_real_
}

fbc_fast_solve_reduced_kkt <- function(problem){
  if(problem$objective_mode!="reach_income_target")
    stop("FAST KKT core ist aktuell für reach_income_target validiert.")

  r<-problem$registry
  n<-nrow(r)

  if(problem$evaluate(r$current)>=problem$desired_net){
    st<-rep(0L,n)
    chk<-fbc_fast_kkt_check(problem,r$current,0,st)
    return(list(
      feasible=TRUE,
      solution=setNames(r$current,r$name),
      projected_net=problem$evaluate(r$current),
      objective_value=0,
      kkt=chk,
      active_status=setNames(st,r$name),
      candidates=1
    ))
  }

  # IMPORTANT: n is only the number of ACTIVE levers, not all global levers.
  S<-as.matrix(expand.grid(rep(list(c(-1L,0L,1L)),n)))
  candidates<-list()

  for(j in seq_len(nrow(S))){
    st<-S[j,]
    lam<-fbc_fast_lambda_root(problem,st)
    if(!is.finite(lam) || lam<0) next

    x<-fbc_fast_x_lambda(problem,st,lam)
    if(any(!is.finite(x))) next

    fr<-which(st==0L)
    if(length(fr) &&
       (any(x[fr]<r$lower[fr]-1e-7) || any(x[fr]>r$upper[fr]+1e-7))) next

    chk<-fbc_fast_kkt_check(problem,x,lam,st)
    if(!isTRUE(chk$valid)) next

    candidates[[length(candidates)+1]]<-list(
      x=x,
      lambda=lam,
      status=st,
      objective=fbc_fast_objective(problem,x),
      kkt=chk
    )
  }

  if(!length(candidates)){
    return(list(feasible=FALSE,solution=NULL,candidates=0))
  }

  k<-which.min(vapply(candidates,function(z)z$objective,numeric(1)))
  z<-candidates[[k]]
  names(z$x)<-r$name

  list(
    feasible=TRUE,
    solution=z$x,
    projected_net=problem$evaluate(z$x),
    objective_value=z$objective,
    kkt=z$kkt,
    active_status=setNames(z$status,r$name),
    candidates=length(candidates)
  )
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

  list(
    registry=rr,
    evaluate=eval_reduced,
    desired_net=full_problem$desired_net,
    objective_mode=full_problem$objective_mode
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
  # Same number of levers by construction.
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
      cat(sprintf("\nFAST SEARCH: %d lever(s), %d combination(s)\n",k,length(sets)))
      flush.console()
    }

    for(j in seq_along(sets)){
      s<-sets[[j]]
      reduced<-fbc_fast_reduced_problem(problem,s)

      # Cheap feasibility pruning before KKT.
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
        cat(sprintf("  [%d/%d] %-55s KKT...\n",
                    j,length(sets),paste(s,collapse=" + ")))
        flush.console()
      }

      sol<-tryCatch(fbc_fast_solve_reduced_kkt(reduced),error=function(e) NULL)
      if(is.null(sol) || !isTRUE(sol$feasible) || is.null(sol$solution)) next

      full_sol<-fbc_fast_expand_solution(problem,sol$solution)
      if(problem$evaluate(full_sol)+1e-5 < problem$desired_net) next

metrics <- fbc_fast_metrics(problem, full_sol)

cand <- list(
  active_levers = metrics$changed,
  reduced_solution = sol,
  solution = full_sol,
  projected_net = problem$evaluate(full_sol),
  metrics = metrics,
  cardinality = length(metrics$changed)
)
      feasible_this_k[[length(feasible_this_k)+1]]<-cand
      pool[[length(pool)+1]]<-cand
    }

    feasible_by_k[[k]]<-feasible_this_k
    if(verbose && length(feasible_this_k)){
      cat(sprintf("  -> %d feasible candidate(s) with %d lever(s); search continues for economic comparison.\n",
                  length(feasible_this_k),k))
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
      reason="Kein zulässiger Hebelsatz erreicht das Ziel."
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
    policy=c(
      "candidate_generation_only",
      "target_feasible",
      "no_hard_min_action_selection",
      "economic_ranking_pending"
    )
  )
}

# Compatibility name retained so existing callers fail visibly rather than
# silently applying the old economically incorrect early-stop policy.
run_fbc_fast_lexicographic <- function(
    problem,
    max_actions=NULL,
    forbidden_levers=character(0),
    verbose=TRUE
){
  warning("run_fbc_fast_lexicographic() no longer performs a final lexicographic selection; returning candidate pool for economic ranking.")
  run_fbc_fast_candidate_search(
    problem=problem,
    max_actions=max_actions,
    forbidden_levers=forbidden_levers,
    verbose=verbose
  )
}

cat("\nFBC FAST KKT Candidate Core loaded.\n")
