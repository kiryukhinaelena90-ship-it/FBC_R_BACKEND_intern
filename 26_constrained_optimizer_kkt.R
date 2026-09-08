# ==========================================
# 26_constrained_optimizer_kkt.R
# FUTURE Business Cockpit
# Constrained optimizer + Lagrange/KKT diagnostics
# ==========================================
#
# Main optimizer uses ONLY confirmed numeric bounds.
# Financing remains separate.
# Employee wage is never a free variable.
# ==========================================

numeric_grad26 <- function(f, x, eps=1e-5) {
  g <- numeric(length(x))
  for (i in seq_along(x)) {
    h <- max(abs(x[i])*eps, eps)
    xp<-xm<-x
    xp[i]<-xp[i]+h; xm[i]<-xm[i]-h
    g[i] <- (f(xp)-f(xm))/(2*h)
  }
  g
}

build_optimizer_problem26 <- function(
    state,
    reality,
    influence,
    objective_mode=c("reach_income_target","reduce_owner_work"),
    desired_net=state$owner$monthly_target,
    confirmed_bounds=list(),
    employee_bounds=NULL,
    owner_pension_month=0,
    tax_month=0
) {
  objective_mode <- match.arg(objective_mode)

  vars <- list()
  add_var <- function(name,current,lower,upper,scale=max(abs(current),1)) {
    vars[[length(vars)+1]] <<- data.frame(
      name=name,current=current,lower=lower,upper=upper,scale=scale,
      stringsAsFactors=FALSE
    )
  }

  # Owner price only if explicit upper bound exists.
  if (!is.null(confirmed_bounds$owner_price_max) &&
      is.finite(confirmed_bounds$owner_price_max) &&
      confirmed_bounds$owner_price_max >= state$owner$price) {
    add_var("owner_price", state$owner$price, state$owner$price,
            confirmed_bounds$owner_price_max)
  }

 # Owner hours: confirmed sellable-hours bound must never exceed
# the factual physical capacity preserved by runner 40.
cur_h <- state$owner$available_hours_month

physical_h <- as.numeric(
  state$owner$physical_available_hours_month %||%
    reality$bounds$owner_hours_month_max
)

owner_h_max <- min(
  as.numeric(reality$bounds$owner_hours_month_max),
  physical_h
)

if (objective_mode=="reduce_owner_work") {
  add_var("owner_hours", cur_h, 0, cur_h)
} else if (is.finite(owner_h_max) && owner_h_max > cur_h) {
  add_var("owner_hours", cur_h, cur_h, owner_h_max)
}

  # Confirmed operating-cost bounds only.
  cb <- reality$bounds$operating_costs
  for (i in seq_len(nrow(cb))) {
    if (isTRUE(cb$controllable[i]) &&
        is.finite(cb$min_value[i]) && is.finite(cb$max_value[i]) &&
        cb$max_value[i] >= cb$min_value[i]) {
      add_var(paste0("cost_",cb$key[i]), cb$current[i], cb$min_value[i], cb$max_value[i])
    }
  }

  # Employee economic levers: ONLY if explicit bounds supplied.
  # Wage is intentionally absent.
  if (!is.null(employee_bounds) && isTRUE(state$employee$direct_billing)) {
    if (!is.null(employee_bounds$customer_price_max) &&
        is.finite(employee_bounds$customer_price_max) &&
        employee_bounds$customer_price_max >= state$employee$customer_price) {
      add_var("employee_customer_price",
              state$employee$customer_price,
              state$employee$customer_price,
              employee_bounds$customer_price_max)
    }
    if (!is.null(employee_bounds$billable_hours_max) &&
        is.finite(employee_bounds$billable_hours_max) &&
        employee_bounds$billable_hours_max >= state$employee$available_hours_month) {
      add_var("employee_billable_hours",
              state$employee$available_hours_month,
              state$employee$available_hours_month,
              employee_bounds$billable_hours_max)
    }
  }

  if (!length(vars)) stop("Keine bestätigten Optimierungshebel mit numerischen Grenzen.")
  reg <- do.call(rbind,vars); rownames(reg)<-NULL

  eval_x <- function(x) {
    names(x)<-reg$name
    price <- if ("owner_price"%in%names(x)) x["owner_price"] else state$owner$price
    hours <- if ("owner_hours"%in%names(x)) x["owner_hours"] else state$owner$available_hours_month
    costs <- state$operating_costs
    for (k in names(costs)) {
      nm<-paste0("cost_",k)
      if (nm%in%names(x)) costs[k]<-x[nm]
    }
    ep <- if ("employee_customer_price"%in%names(x)) x["employee_customer_price"] else state$employee$customer_price
    eh <- if ("employee_billable_hours"%in%names(x)) x["employee_billable_hours"] else state$employee$available_hours_month

    pt <- evaluate_point21(
  state=state,
  owner_price=price,
  owner_hours=hours,
  operating_costs=costs,
  employee_customer_price=ep,
  employee_billable_hours=eh,
  owner_pension_month=0,
  tax_month=0
)

if(exists("fbc_financial_point24", mode="function")){

  gross_before_owner_protection_tax <-
    pt$net_available + state$owner$insurance_month

  fin <- fbc_financial_point24(
    gross_before_owner_protection_tax,
    state$owner,
    legal_form =
      state$legal_form %||% "freelance",
    trade_tax_rate =
      state$trade_tax_rate %||% 0
  )

  return(fin$net_available)
}

pt$net_available
  }

  # Scale-free movement from current state.
  distance <- function(x) {
    z <- (x-reg$current)/reg$scale
    sqrt(sum(z^2))
  }

  if (objective_mode=="reach_income_target") {
    obj <- function(x) {
      gap <- max(0, desired_net-eval_x(x))
      distance(x) + 1e4*(gap/max(abs(desired_net),1))^2
    }
  } else {
    # Primary goal: minimize owner work, secondary: avoid needless movement.
    obj <- function(x) {
      names(x)<-reg$name
      h <- if ("owner_hours"%in%names(x)) x["owner_hours"] else state$owner$available_hours_month
      gap <- max(0, desired_net-eval_x(x))
      h/max(state$owner$available_hours_month,1) +
        0.05*distance(x) +
        1e4*(gap/max(abs(desired_net),1))^2
    }
  }

  list(registry=reg, evaluate=eval_x, objective=obj,
       desired_net=desired_net, objective_mode=objective_mode)
}

solve_optimizer26 <- function(problem) {
  reg<-problem$registry
  fit<-optim(
    par=reg$current,
    fn=problem$objective,
    method="L-BFGS-B",
    lower=reg$lower,
    upper=reg$upper,
    control=list(maxit=3000,factr=1e7)
  )
  x<-fit$par; names(x)<-reg$name
  net<-problem$evaluate(x)
  feasible<-is.finite(net) && net+1e-5>=problem$desired_net

  # KKT/Lagrange diagnostic around target constraint g(x)=desired-net <= 0.
  f_plain <- function(z) {
    sqrt(sum(((z-reg$current)/reg$scale)^2))
  }
  g_con <- function(z) problem$desired_net-problem$evaluate(z)

  grad_f <- numeric_grad26(f_plain,x)
  grad_g <- numeric_grad26(g_con,x)

  active_target <- abs(g_con(x)) < max(1e-3,abs(problem$desired_net)*1e-5)
  lambda <- NA_real_
  stationarity_residual <- NA_real_
  if (active_target && sum(grad_g^2)>0) {
    lambda <- max(0, -sum(grad_f*grad_g)/sum(grad_g^2))
    stationarity_residual <- sqrt(sum((grad_f+lambda*grad_g)^2))
  }

  list(
    solution=x,
    projected_net=net,
    feasible=feasible,
    target_gap_eur=max(0,problem$desired_net-net),
    convergence=fit$convergence,
    objective_value=fit$value,
    kkt=list(
      target_constraint_active=active_target,
      lambda_target=lambda,
      stationarity_residual=stationarity_residual,
      constraint_value=g_con(x)
    )
  )
}

cat("\n26 Constrained Optimizer + KKT geladen.\n")
