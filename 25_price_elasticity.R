# ============================================================
# 25_price_elasticity.R
# FUTURE Business Cockpit
# Constant price-elasticity demand adapter
#
# Model:
#   expected_hours = planned_hours * (new_price / base_price)^epsilon
#   epsilon = -0.60
#
# Semantics:
# - planned_hours are the billable hours the user confirms as realistic
#   at the factual/base price;
# - expected_hours are the hours expected after the price response;
# - if price is unchanged, expected_hours == planned_hours;
# - no physical-capacity cap is introduced here;
# - if the factual/base price is <= 0, elasticity is not applied because
#   a price ratio is undefined. The planned hours are preserved and the
#   audit metadata explicitly reports that fallback.
# ============================================================

FBC_PRICE_ELASTICITY25 <- -0.60
`%||%` <- function(a,b) if(is.null(a) || length(a)==0L) b else a

fbc_scalar25 <- function(x, name, lower=-Inf){
  z <- suppressWarnings(as.numeric(x)[1])
  if(!is.finite(z) || z < lower)
    stop(name, " muss endlich und >= ", lower, " sein.")
  z
}

fbc_price_response25 <- function(
    base_price,
    new_price,
    planned_hours,
    epsilon=FBC_PRICE_ELASTICITY25
){
  p0 <- fbc_scalar25(base_price, "base_price", 0)
  p1 <- fbc_scalar25(new_price, "new_price", 0)
  h  <- fbc_scalar25(planned_hours, "planned_hours", 0)
  e  <- suppressWarnings(as.numeric(epsilon)[1])

  if(!is.finite(e))
    stop("epsilon muss endlich sein.")

  price_changed <- abs(p1-p0) > 1e-10

  if(h <= 0){
    factor <- if(p0 > 0 && p1 > 0) (p1/p0)^e else 1
    return(list(
      base_price=p0,
      new_price=p1,
      planned_hours=h,
      expected_hours=0,
      response_factor=as.numeric(factor),
      epsilon=e,
      elasticity_applied=FALSE,
      reason=if(price_changed && (p0 <= 0 || p1 <= 0))
        "nonpositive_price_ratio"
      else
        "zero_planned_hours"
    ))
  }

  if(!price_changed){
    return(list(
      base_price=p0,
      new_price=p1,
      planned_hours=h,
      expected_hours=h,
      response_factor=1,
      epsilon=e,
      elasticity_applied=FALSE,
      reason="price_unchanged"
    ))
  }

  if(p0 <= 0 || p1 <= 0){
    return(list(
      base_price=p0,
      new_price=p1,
      planned_hours=h,
      expected_hours=h,
      response_factor=1,
      epsilon=e,
      elasticity_applied=FALSE,
      reason="nonpositive_price_ratio"
    ))
  }

  factor <- (p1/p0)^e
  expected <- max(0, h*factor)

  list(
    base_price=p0,
    new_price=p1,
    planned_hours=h,
    expected_hours=as.numeric(expected),
    response_factor=as.numeric(factor),
    epsilon=e,
    elasticity_applied=TRUE,
    reason=NULL
  )
}

fbc_demand_snapshot25 <- function(
    base_state,
    owner_price=base_state$owner$price,
    owner_planned_hours=base_state$owner$billable_hours_month,
    employee_price=base_state$employee$customer_price,
    employee_planned_hours=base_state$employee$billable_hours_month,
    epsilon=FBC_PRICE_ELASTICITY25
){
  owner <- fbc_price_response25(
    base_price=base_state$owner$price,
    new_price=owner_price,
    planned_hours=owner_planned_hours,
    epsilon=epsilon
  )
  owner$revenue_month <- owner$new_price * owner$expected_hours

  if(isTRUE(base_state$employee$direct_billing)){
    employee <- fbc_price_response25(
      base_price=base_state$employee$customer_price,
      new_price=employee_price,
      planned_hours=employee_planned_hours,
      epsilon=epsilon
    )
    employee$revenue_month <- employee$new_price * employee$expected_hours
  } else {
    employee <- list(
      base_price=as.numeric(base_state$employee$customer_price %||% 0),
      new_price=as.numeric(employee_price %||% 0),
      planned_hours=0,
      expected_hours=0,
      response_factor=1,
      epsilon=as.numeric(epsilon),
      elasticity_applied=FALSE,
      reason="direct_billing_false",
      revenue_month=0
    )
  }

  list(
    model="constant_price_elasticity",
    epsilon=as.numeric(epsilon),
    owner=owner,
    employee=employee
  )
}

fbc_apply_demand_to_state25 <- function(
    state,
    base_state=state,
    epsilon=FBC_PRICE_ELASTICITY25
){
  snap <- fbc_demand_snapshot25(
    base_state=base_state,
    owner_price=state$owner$price,
    owner_planned_hours=state$owner$billable_hours_month,
    employee_price=state$employee$customer_price,
    employee_planned_hours=state$employee$billable_hours_month,
    epsilon=epsilon
  )

  state$owner$planned_billable_hours_month <-
    as.numeric(state$owner$billable_hours_month)
  state$owner$expected_billable_hours_month <-
    as.numeric(snap$owner$expected_hours)
  state$owner$revenue_month <-
    as.numeric(snap$owner$revenue_month)

  state$employee$planned_billable_hours_month <-
    if(isTRUE(state$employee$direct_billing))
      as.numeric(state$employee$billable_hours_month)
    else 0
  state$employee$expected_billable_hours_month <-
    as.numeric(snap$employee$expected_hours)
  state$employee$revenue_month <-
    as.numeric(snap$employee$revenue_month)

  state$demand_model <- list(
    model=snap$model,
    epsilon=snap$epsilon,
    owner=snap$owner,
    employee=snap$employee
  )

  state
}

fbc_value25 <- function(x, default=NA_real_){
  z <- suppressWarnings(as.numeric(x))
  if(!length(z) || !is.finite(z[1])) return(default)
  z[1]
}

fbc_state_expected_owner_hours25 <- function(state){
  x <- fbc_value25(state$owner$expected_billable_hours_month)
  if(is.finite(x)) return(max(0,x))
  max(0, fbc_value25(state$owner$billable_hours_month,0))
}

fbc_state_expected_employee_hours25 <- function(state){
  if(!isTRUE(state$employee$direct_billing)) return(0)
  x <- fbc_value25(state$employee$expected_billable_hours_month)
  if(is.finite(x)) return(max(0,x))
  max(0, fbc_value25(state$employee$billable_hours_month,0))
}

fbc_state_owner_revenue25 <- function(state){
  x <- fbc_value25(state$owner$revenue_month)
  if(is.finite(x)) return(x)
  fbc_value25(state$owner$price,0) * fbc_state_expected_owner_hours25(state)
}

fbc_state_employee_revenue25 <- function(state){
  if(!isTRUE(state$employee$direct_billing)) return(0)
  x <- fbc_value25(state$employee$revenue_month)
  if(is.finite(x)) return(x)
  fbc_value25(state$employee$customer_price,0) * fbc_state_expected_employee_hours25(state)
}

cat("\n25 Price Elasticity loaded: epsilon = -0.60.\n")
