# ============================================================
# 45_team_p1_optimizer.R
# FUTURE Business Cockpit
# Team P1: confirmed bounds -> bounded Team optimization
# ============================================================
#
# Design:
# - max. 3 user-confirmed operating levers per run;
# - every economic point is evaluated through fbc_team_eval_point44();
# - price changes therefore reuse the same demand elasticity as Team P0;
# - role hierarchy is a hard monotone price-structure constraint for
#   comparable standard roles; Custom remains outside this hierarchy;
# - financing is NOT an operating optimization lever. It is evaluated only
#   after the operating Team decision, using the same separate financing
#   contract logic as the other Cockpit modes;
# - no arbitrary role score and no fixed percentage gap between roles.
# ============================================================

`%||%` <- function(a,b) if(is.null(a) || length(a)==0L) b else a

fbc_num45 <- function(x, default=NA_real_){
  z <- suppressWarnings(as.numeric(x %||% default)[1])
  if(!is.finite(z)) default else z
}

fbc_bound45 <- function(x){
  if(is.null(x)) return(NULL)
  if(is.data.frame(x)) x <- as.list(x[1,,drop=FALSE])
  if(!is.list(x)) return(NULL)
  x
}

fbc_impl_months45 <- function(x){
  x <- fbc_bound45(x)
  if(is.null(x)) return(1L)
  imp <- x$implementation
  if(is.data.frame(imp)) imp <- as.list(imp[1,,drop=FALSE])
  m <- fbc_num45(imp$months_to_full %||% 1, 1)
  as.integer(max(0, round(m)))
}

fbc_cost_key45 <- function(key){
  key <- as.character(key %||% "")[1]
  if(identical(key,"insurance")) "business_insurance" else key
}

fbc_team_member_by_id45 <- function(state, id){
  id <- as.character(id %||% "")[1]
  hit <- Filter(function(x) identical(as.character(x$id), id), state$employees)
  if(length(hit)) hit[[1]] else NULL
}

fbc_team_label45 <- function(name, state){
  if(identical(name,"owner_price"))
    return("Kundenpreis Inhaber/in")
  if(identical(name,"owner_hours"))
    return("Abrechenbare Stunden Inhaber/in")

  if(grepl("^cost_",name)){
    key <- sub("^cost_","",name)
    labs <- c(
      office="Raum / Büro",
      energy="Energie / Strom",
      vehicle="Fahrzeug",
      fuel="Kraftstoff",
      software="Programme / Software",
      accounting="Steuerberater / Buchhaltung",
      business_insurance="Betriebsversicherung",
      material="Material",
      marketing="Werbung / Akquise",
      other="Sonstiges"
    )
    return(if(key %in% names(labs)) unname(labs[[key]]) else key)
  }

  for(member in state$employees){
    if(identical(name,paste0(member$id,"_customer_price")))
      return(paste0("Kundenpreis · ", member$label))
    if(identical(name,paste0(member$id,"_billable_hours")))
      return(paste0("Abrechenbare Stunden · ", member$label))
  }

  name
}

fbc_team_unit45 <- function(kind){
  if(kind %in% c("owner_price","employee_price")) return("EUR/h")
  if(kind %in% c("owner_hours","employee_hours")) return("h/month")
  if(identical(kind,"cost")) return("EUR/month")
  ""
}

fbc_team_registry45 <- function(cfg, state){
  confirmed <- cfg$confirmed_bounds %||% list()
  costs <- cfg$cost_evidence %||% list()

  if(is.data.frame(confirmed)) confirmed <- as.list(confirmed)
  if(is.data.frame(costs)) costs <- as.list(costs)
  if(!is.list(confirmed)) stop("confirmed_bounds muss eine Liste sein.")
  if(!is.list(costs)) stop("cost_evidence muss eine Liste sein.")

  rows <- list()

  add_row <- function(name, kind, current, lower, upper,
                      actor_id="", cost_key="", months=1L){
    current <- fbc_num45(current)
    lower <- fbc_num45(lower)
    upper <- fbc_num45(upper)

    if(!all(is.finite(c(current,lower,upper))))
      stop("Ungültige numerische Grenze für Team-Hebel: ", name)
    if(lower > upper + 1e-9)
      stop("Untere Grenze liegt über der oberen Grenze: ", name)
    if(current < lower - 1e-8 || current > upper + 1e-8)
      stop("Aktueller Wert liegt außerhalb der bestätigten Grenze: ", name)

    rows[[length(rows)+1L]] <<- data.frame(
      name=as.character(name),
      kind=as.character(kind),
      actor_id=as.character(actor_id),
      cost_key=as.character(cost_key),
      current=as.numeric(current),
      lower=as.numeric(lower),
      upper=as.numeric(upper),
      months=as.integer(months),
      stringsAsFactors=FALSE
    )
  }

  if(length(confirmed)){
    for(name in names(confirmed)){
      b <- fbc_bound45(confirmed[[name]])
      if(is.null(b)) stop("Ungültige bestätigte Grenze: ", name)

      if(identical(name,"owner_price")){
        cur <- as.numeric(state$owner$price)
        up <- fbc_num45(b$upper)
        if(!is.finite(up) || up <= cur + 1e-9)
          stop("Inhaberpreis: Obergrenze muss über dem aktuellen Wert liegen.")
        add_row(name,"owner_price",cur,cur,up,months=fbc_impl_months45(b))
        next
      }

      if(identical(name,"owner_hours")){
        cur <- as.numeric(state$owner$billable_hours_month)
        up <- fbc_num45(b$upper)
        if(!is.finite(up) || up <= cur + 1e-9)
          stop("Inhaberstunden: Obergrenze muss über dem aktuellen Wert liegen.")

        cap <- fbc_num45(state$owner$physical_available_hours_month, NA_real_)
        if(is.finite(cap) && up > cap + 1e-8)
          stop("Inhaberstunden: bestätigte Obergrenze überschreitet die verfügbare Monatskapazität.")

        add_row(name,"owner_hours",cur,cur,up,months=fbc_impl_months45(b))
        next
      }

      matched <- FALSE
      for(member in state$employees){
        price_key <- paste0(member$id,"_customer_price")
        hours_key <- paste0(member$id,"_billable_hours")

        if(identical(name,price_key)){
          if(!isTRUE(member$direct_billing))
            stop("Kundenpreis kann nur für direkt abrechenbare Teammitglieder optimiert werden: ", member$id)
          cur <- as.numeric(member$customer_price)
          up <- fbc_num45(b$upper)
          if(!is.finite(up) || up <= cur + 1e-9)
            stop(member$label, ": Preisobergrenze muss über dem aktuellen Wert liegen.")
          add_row(name,"employee_price",cur,cur,up,actor_id=member$id,months=fbc_impl_months45(b))
          matched <- TRUE
          break
        }

        if(identical(name,hours_key)){
          if(!isTRUE(member$direct_billing))
            stop("Abrechenbare Stunden können nur für direkt abrechenbare Teammitglieder optimiert werden: ", member$id)
          cur <- as.numeric(member$billable_hours_month)
          up <- fbc_num45(b$upper)
          if(!is.finite(up) || up <= cur + 1e-9)
            stop(member$label, ": Stundenobergrenze muss über dem aktuellen Wert liegen.")

          cap <- fbc_num45(member$paid_available_hours_month, NA_real_)
          if(is.finite(cap) && up > cap + 1e-8)
            stop(member$label, ": bestätigte Stundenobergrenze überschreitet die bezahlte Monatskapazität.")

          add_row(name,"employee_hours",cur,cur,up,actor_id=member$id,months=fbc_impl_months45(b))
          matched <- TRUE
          break
        }
      }

      if(!matched)
        stop("Unbekannter Team-Hebel in confirmed_bounds: ", name)
    }
  }

  if(length(costs)){
    for(raw_key in names(costs)){
      b <- fbc_bound45(costs[[raw_key]])
      if(is.null(b)) stop("Ungültige Kostengrenze: ", raw_key)

      key <- fbc_cost_key45(raw_key)
      if(!key %in% names(state$operating_costs))
        stop("Unbekannte Kostenposition: ", raw_key)

      cur <- as.numeric(state$operating_costs[[key]])
      lo <- fbc_num45(b$lower)
      if(!is.finite(lo) || lo < 0 || lo >= cur - 1e-9)
        stop("Kostensenkung ", raw_key, ": bestätigter Betrag muss >= 0 und unter dem aktuellen Wert liegen.")

      add_row(
        paste0("cost_",key),
        "cost",
        cur,
        lo,
        cur,
        cost_key=key,
        months=fbc_impl_months45(b)
      )
    }
  }

  if(!length(rows))
    stop("Mindestens ein bestätigter Team-Hebel ist erforderlich.")
  if(length(rows) > 3L)
    stop("Team P1 unterstützt pro Lauf maximal drei bestätigte Hebel.")

  reg <- do.call(rbind,rows)
  rownames(reg) <- NULL

  if(anyDuplicated(reg$name))
    stop("Ein Team-Hebel wurde mehrfach bestätigt.")

  reg$scale <- pmax(abs(reg$upper-reg$lower), 1e-9)
  reg
}

fbc_team_x_named45 <- function(reg, x){
  x <- as.numeric(x)
  if(length(x) != nrow(reg)) stop("Team-P1 Lösungsvektor hat falsche Länge.")
  names(x) <- reg$name
  x
}

fbc_team_eval_x45 <- function(state, cfg, reg, x){
  x <- fbc_team_x_named45(reg,x)

  owner_price <- as.numeric(state$owner$price)
  owner_hours <- as.numeric(state$owner$billable_hours_month)
  employee_prices <- numeric(0)
  employee_hours <- numeric(0)
  operating_costs <- state$operating_costs

  for(i in seq_len(nrow(reg))){
    value <- as.numeric(x[i])
    kind <- reg$kind[i]

    if(identical(kind,"owner_price")){
      owner_price <- value
    } else if(identical(kind,"owner_hours")){
      owner_hours <- value
    } else if(identical(kind,"employee_price")){
      employee_prices[reg$actor_id[i]] <- value
    } else if(identical(kind,"employee_hours")){
      employee_hours[reg$actor_id[i]] <- value
    } else if(identical(kind,"cost")){
      operating_costs[reg$cost_key[i]] <- value
    }
  }

  point <- fbc_team_eval_point44(
    state=state,
    cfg=cfg,
    owner_price=owner_price,
    owner_hours=owner_hours,
    employee_prices=if(length(employee_prices)) employee_prices else NULL,
    employee_hours=if(length(employee_hours)) employee_hours else NULL,
    operating_costs=operating_costs
  )

  structure <- fbc_team_price_structure44(
    state,
    employee_prices=if(length(employee_prices)) employee_prices else NULL
  )

  point$operating_costs_vector <- operating_costs
  point$role_price_structure <- structure
  point$decision_vector <- x
  point
}

fbc_team_feasible45 <- function(point){
  is.list(point) &&
    is.finite(fbc_num45(point$net_available)) &&
    isTRUE(point$role_price_structure$ok)
}

fbc_team_distance45 <- function(reg, x){
  x <- fbc_team_x_named45(reg,x)
  sqrt(sum(((x-reg$current)/reg$scale)^2))
}

# Coordinate-wise best attainable point. Because all admitted Team-P1 levers
# move only in the user-confirmed improving direction, this gives a robust
# reachability check without enumerating a large combinatorial search space.
fbc_team_best_attainable45 <- function(state, cfg, reg){
  x <- reg$current
  names(x) <- reg$name

  value <- function(z){
    p <- fbc_team_eval_x45(state,cfg,reg,z)
    if(!fbc_team_feasible45(p)) return(-Inf)
    as.numeric(p$net_available)
  }

  best_net <- value(x)
  best_point <- if(is.finite(best_net)) fbc_team_eval_x45(state,cfg,reg,x) else NULL

  # Several passes are sufficient for the only coupled constraint here:
  # monotone prices across comparable role levels. Raising a higher role in
  # one pass can release room for a lower role in the next pass.
  for(pass in seq_len(nrow(reg)+3L)){
    moved <- FALSE

    for(i in seq_len(nrow(reg))){
      target <- if(identical(reg$kind[i],"cost")) reg$lower[i] else reg$upper[i]
      if(abs(target-x[i]) <= 1e-10) next

      trial <- x
      trial[i] <- target
      p <- fbc_team_eval_x45(state,cfg,reg,trial)

      if(fbc_team_feasible45(p)){
        net <- as.numeric(p$net_available)
        if(!is.finite(best_net) || net >= best_net - 1e-8){
          x <- trial
          best_net <- net
          best_point <- p
          moved <- TRUE
        }
        next
      }

      # If a role-price boundary is hit, find the furthest feasible value
      # along this single coordinate by bisection.
      lo <- x[i]
      hi <- target
      if(hi < lo){
        tmp <- lo; lo <- hi; hi <- tmp
      }

      feasible_value <- x[i]
      for(k in seq_len(50L)){
        mid <- (lo+hi)/2
        z <- x
        z[i] <- mid
        pm <- fbc_team_eval_x45(state,cfg,reg,z)

        if(fbc_team_feasible45(pm)){
          feasible_value <- mid
          if(target >= x[i]) lo <- mid else hi <- mid
        } else {
          if(target >= x[i]) hi <- mid else lo <- mid
        }
      }

      trial <- x
      trial[i] <- feasible_value
      p <- fbc_team_eval_x45(state,cfg,reg,trial)
      if(fbc_team_feasible45(p)){
        net <- as.numeric(p$net_available)
        if(!is.finite(best_net) || net >= best_net - 1e-8){
          if(abs(trial[i]-x[i]) > 1e-8) moved <- TRUE
          x <- trial
          best_net <- net
          best_point <- p
        }
      }
    }

    if(!moved) break
  }

  if(is.null(best_point))
    stop("In den bestätigten Grenzen existiert kein Team-Punkt mit zulässiger Rollen-Preisstruktur.")

  list(x=fbc_team_x_named45(reg,x), point=best_point, net=as.numeric(best_net))
}

fbc_team_role_violation45 <- function(point, reg){
  v <- point$role_price_structure$violations %||% list()
  if(!length(v)) return(0)

  price_scale <- max(
    1,
    c(
      reg$current[reg$kind %in% c("owner_price","employee_price")],
      reg$upper[reg$kind %in% c("owner_price","employee_price")]
    ),
    na.rm=TRUE
  )

  sq <- vapply(v,function(z){
    d <- max(0, fbc_num45(z$lower_price,0)-fbc_num45(z$higher_price,0))
    (d/price_scale)^2
  },numeric(1))
  sum(sq)
}

# Stage 2: once reachability is known independently, minimize normalized
# movement while enforcing target and role constraints. The penalty parameter
# is escalated automatically until the already-proven feasible target is met;
# it is not used to decide whether the target is reachable.
fbc_team_minimal_target45 <- function(state, cfg, reg, target, start){
  start <- fbc_team_x_named45(reg,start)
  lower <- reg$lower
  upper <- reg$upper
  scale_net <- max(abs(target),1)

  best <- start
  best_point <- fbc_team_eval_x45(state,cfg,reg,best)

  for(lambda in c(1e2,1e3,1e4,1e5,1e6,1e7)){
    objective <- function(z){
      p <- fbc_team_eval_x45(state,cfg,reg,z)
      net <- fbc_num45(p$net_available,-Inf)
      if(!is.finite(net)) return(.Machine$double.xmax/100)

      gap <- max(0,target-net)/scale_net
      role <- fbc_team_role_violation45(p,reg)
      fbc_team_distance45(reg,z) + lambda*(gap^2 + role)
    }

    fit <- optim(
      par=best,
      fn=objective,
      method="L-BFGS-B",
      lower=lower,
      upper=upper,
      control=list(maxit=2500,factr=1e7)
    )

    z <- fbc_team_x_named45(reg,fit$par)
    p <- fbc_team_eval_x45(state,cfg,reg,z)

    if(fbc_team_feasible45(p)){
      best <- z
      best_point <- p
      if(as.numeric(p$net_available) >= target-0.01) break
    }
  }

  if(!fbc_team_feasible45(best_point) || best_point$net_available < target-0.01){
    # Guaranteed fallback after reachability was proven: search the straight
    # path from the factual point to the feasible best-attainable point.
    # With the admitted monotone levers this avoids turning a numerical
    # optimizer issue into a false "target not reachable" result.
    current <- reg$current
    names(current) <- reg$name
    lo <- 0
    hi <- 1
    fallback_x <- start
    fallback_point <- fbc_team_eval_x45(state,cfg,reg,start)

    for(i in seq_len(70L)){
      a <- (lo+hi)/2
      z <- current + a*(start-current)
      p <- fbc_team_eval_x45(state,cfg,reg,z)
      ok <- fbc_team_feasible45(p) && p$net_available >= target-0.01
      if(ok){
        hi <- a
        fallback_x <- fbc_team_x_named45(reg,z)
        fallback_point <- p
      } else {
        lo <- a
      }
    }

    if(!fbc_team_feasible45(fallback_point) || fallback_point$net_available < target-0.01)
      return(NULL)

    best <- fallback_x
    best_point <- fallback_point
  }

  # Remove avoidable overshoot on the ray from current -> solution.
  # This keeps the selected mix but lands close to the target boundary.
  current <- reg$current
  names(current) <- reg$name
  lo <- 0
  hi <- 1
  ray_best <- best
  ray_point <- best_point

  for(i in seq_len(60L)){
    a <- (lo+hi)/2
    z <- current + a*(best-current)
    p <- fbc_team_eval_x45(state,cfg,reg,z)
    ok <- fbc_team_feasible45(p) && p$net_available >= target-0.01
    if(ok){
      hi <- a
      ray_best <- fbc_team_x_named45(reg,z)
      ray_point <- p
    } else {
      lo <- a
    }
  }

  list(x=ray_best, point=ray_point, net=as.numeric(ray_point$net_available))
}

fbc_team_employee_economics45 <- function(state, point){
  out <- vector("list",length(state$employees))
  point_by_id <- setNames(point$employees, vapply(point$employees,function(x) as.character(x$id),character(1)))

  for(i in seq_along(state$employees)){
    member <- state$employees[[i]]
    pp <- point_by_id[[member$id]]

    if(!isTRUE(member$direct_billing)){
      out[[i]] <- list(
        id=member$id,label=member$label,role=member$role,role_label=member$role_label,
        role_level=if(is.finite(member$role_level)) member$role_level else NULL,
        direct_billing=FALSE,
        personnel_cost_month=member$personnel_cost_month,
        customer_price=NULL,
        planned_billable_hours=0,
        expected_billable_hours=0,
        revenue_month=0,
        variable_cost_month=0,
        result_contribution_month=-member$personnel_cost_month,
        economic_factor=NULL,
        break_even_hours=NULL,
        hours_above_break_even=NULL,
        cost_coverage_ok=NULL,
        utilization_rate=NULL,
        utilization_zone="not_applicable",
        status="internal_non_billable"
      )
      next
    }

    price <- fbc_num45(pp$price,member$customer_price)
    planned <- fbc_num45(pp$planned_hours,member$billable_hours_month)
    expected <- fbc_num45(pp$expected_hours,planned)
    revenue <- fbc_num45(pp$revenue_month,price*expected)
    variable_cost <- fbc_num45(pp$variable_cost_month,member$variable_cost_per_hour*expected)

    projected_member <- member
    projected_member$customer_price <- price
    projected_member$billable_hours_month <- planned
    member_state <- fbc_team_member_state44(state,projected_member)

    util <- fbc_employee_utilization19(member_state,billable_hours=planned)
    be <- fbc_employee_break_even_hours19(
      member_state,
      customer_price=price,
      variable_cost_per_hour=member$variable_cost_per_hour
    )

    contribution <- revenue-variable_cost-member$personnel_cost_month
    factor <- if(member$personnel_cost_month>0)
      (revenue-variable_cost)/member$personnel_cost_month else NA_real_
    covered <- is.finite(be) && expected+1e-8 >= be

    out[[i]] <- list(
      id=member$id,
      label=member$label,
      role=member$role,
      role_label=member$role_label,
      role_level=if(is.finite(member$role_level)) member$role_level else NULL,
      direct_billing=TRUE,
      personnel_cost_month=as.numeric(member$personnel_cost_month),
      customer_price=as.numeric(price),
      planned_billable_hours=as.numeric(planned),
      expected_billable_hours=as.numeric(expected),
      revenue_month=as.numeric(revenue),
      variable_cost_month=as.numeric(variable_cost),
      result_contribution_month=as.numeric(contribution),
      economic_factor=if(is.finite(factor)) as.numeric(factor) else NULL,
      break_even_hours=if(is.finite(be)) as.numeric(be) else NULL,
      hours_above_break_even=if(is.finite(be)) as.numeric(expected-be) else NULL,
      cost_coverage_ok=isTRUE(covered),
      utilization_rate=if(is.finite(util$utilization_rate)) as.numeric(util$utilization_rate) else NULL,
      utilization_zone=as.character(util$zone),
      effective_billable_hours=as.numeric(util$effective_hours),
      available_hours_month=if(is.finite(util$available_hours)) as.numeric(util$available_hours) else NULL,
      status=if(isTRUE(covered)) "covered" else "coverage_open"
    )
  }

  out
}

fbc_team_business_break_even45 <- function(state, cfg, point){
  expected_hours <- fbc_num45(point$owner_demand$expected_hours,0)
  if(length(point$employees)){
    expected_hours <- expected_hours + sum(vapply(point$employees,function(x) fbc_num45(x$expected_hours,0),numeric(1)))
  }

  revenue <- fbc_num45(point$revenue_total,0)
  weighted_price <- if(expected_hours>0) revenue/expected_hours else NA_real_

  costs <- point$operating_costs_vector %||% state$operating_costs
  variable_keys <- as.character(unlist(cfg$variable_cost_keys %||% character()))
  variable_keys <- vapply(variable_keys,fbc_cost_key45,character(1))
  variable_keys <- intersect(variable_keys,names(costs))
  op_variable <- if(length(variable_keys)) sum(costs[variable_keys]) else 0

  variable_total <- op_variable + fbc_num45(point$member_variable_costs,0)
  variable_per_hour <- if(expected_hours>0) variable_total/expected_hours else 0
  fixed_result_costs <- sum(costs)-op_variable+fbc_num45(point$personnel_costs,0)
  db <- weighted_price-variable_per_hour
  reachable <- is.finite(db) && db>0
  be_hours <- if(reachable) fixed_result_costs/db else Inf
  be_revenue <- if(reachable && is.finite(weighted_price)) be_hours*weighted_price else Inf
  margin <- revenue-be_revenue

  list(
    weighted_price_per_billable_hour=if(is.finite(weighted_price)) as.numeric(weighted_price) else NULL,
    variable_cost_per_billable_hour=as.numeric(variable_per_hour),
    fixed_result_costs_month=as.numeric(fixed_result_costs),
    break_even_hours=if(is.finite(be_hours)) as.numeric(be_hours) else NULL,
    break_even_revenue=if(is.finite(be_revenue)) as.numeric(be_revenue) else NULL,
    projected_revenue=as.numeric(revenue),
    safety_margin=if(is.finite(margin)) as.numeric(margin) else NULL,
    ok=isTRUE(reachable) && is.finite(margin) && margin>=-1e-8,
    note="Team P1 Business Break-even uses the projected operating state; financing remains separate."
  )
}

fbc_team_changes45 <- function(reg, solution, state){
  x <- fbc_team_x_named45(reg,solution)
  out <- list()

  for(i in seq_len(nrow(reg))){
    cur <- as.numeric(reg$current[i])
    prop <- as.numeric(x[i])
    tol <- max(1e-7,1e-7*max(abs(cur),1))
    if(abs(prop-cur) <= tol) next

    out[[length(out)+1L]] <- list(
      lever=as.character(reg$name[i]),
      label=fbc_team_label45(as.character(reg$name[i]),state),
      current=cur,
      proposed=prop,
      delta=prop-cur,
      unit=fbc_team_unit45(as.character(reg$kind[i])),
      implementation_months=as.integer(reg$months[i])
    )
  }

  out
}

fbc_team_exact_shapley45 <- function(state, cfg, reg, solution, changes){
  if(!length(changes)){
    return(list(available=FALSE,contributions=list(),dominant_lever=NULL,dominant_label=NULL,sentence=NULL))
  }

  active_names <- vapply(changes,function(x) as.character(x$lever),character(1))
  idx <- match(active_names,reg$name)
  idx <- idx[!is.na(idx)]
  n <- length(idx)
  if(n<1L || n>3L)
    return(list(available=FALSE,contributions=list(),dominant_lever=NULL,dominant_label=NULL,sentence=NULL))

  cur <- reg$current; names(cur) <- reg$name
  sol <- fbc_team_x_named45(reg,solution)

  value_subset <- function(mask){
    z <- cur
    if(length(mask)) z[idx[mask]] <- sol[idx[mask]]
    as.numeric(fbc_team_eval_x45(state,cfg,reg,z)$net_available)
  }

  phi <- numeric(n)
  all_idx <- seq_len(n)
  denom <- factorial(n)

  for(i in all_idx){
    others <- setdiff(all_idx,i)
    for(mask_int in 0:(2^length(others)-1L)){
      S <- integer(0)
      if(length(others)){
        bits <- as.logical(intToBits(mask_int)[seq_along(others)])
        S <- others[bits]
      }
      w <- factorial(length(S))*factorial(n-length(S)-1L)/denom
      phi[i] <- phi[i] + w*(value_subset(c(S,i))-value_subset(S))
    }
  }

  rows <- lapply(seq_len(n),function(i){
    nm <- reg$name[idx[i]]
    list(
      lever=as.character(nm),
      label=fbc_team_label45(as.character(nm),state),
      contribution_eur=as.numeric(phi[i])
    )
  })
  ord <- order(abs(phi),decreasing=TRUE)
  rows <- rows[ord]
  dom <- rows[[1]]

  list(
    available=TRUE,
    contributions=rows,
    dominant_lever=dom$lever,
    dominant_label=dom$label,
    total_improvement_eur=as.numeric(sum(phi)),
    sentence=if(n==1L)
      paste0(dom$label," trägt die berechnete Ergebnisverbesserung.")
    else
      paste0(dom$label," liefert den größten rechnerischen Beitrag; die weiteren bestätigten Änderungen ergänzen die Wirkung.")
  )
}

fbc_team_financing_payload45 <- function(cfg, point){
  fin <- fbc_map_financing(cfg$financing %||% list())

  if(!isTRUE(fin$active)){
    return(list(
      active=FALSE,
      status="no_financing",
      current=NULL,
      alternative=NULL,
      comparison=NULL,
      note="Finanzierung ist kein operativer Team-P1-Hebel."
    ))
  }

  if(!exists("fbc_summarize_financing",mode="function"))
    stop("fbc_summarize_financing() fehlt. 36_financing_alternative.R zuerst laden.")

  contract <- list(
    type=fin$type,
    amount=fin$amount,
    rate_pa=fin$rate_pa,
    months=fin$months,
    fees_month=fin$fees_month %||% 0,
    one_time_fee=0,
    binding=fin$binding %||% NA_character_
  )

  costs <- point$operating_costs_vector
  if(is.null(costs)) stop("Team financing requires projected operating-cost vector.")

  free_before_debt <-
    fbc_num45(point$revenue_total,0) -
    sum(costs) -
    fbc_num45(point$personnel_costs,0) -
    fbc_num45(point$member_variable_costs,0)

  summary <- fbc_summarize_financing(
    contract,
    comparison_horizon_months=fin$months,
    free_cash_before_debt_service_month=free_before_debt,
    min_debt_service_ratio=NULL
  )

  ratio <- fbc_num45(summary$min_capital_service_ratio,NA_real_)

  list(
    active=TRUE,
    status="current_financing_only",
    current=list(
      type=summary$contract$type,
      amount=summary$contract$amount,
      rate_pa=summary$contract$rate_pa,
      months=summary$contract$months %||% NA,
      binding=summary$contract$binding %||% NA_character_,
      total_interest=summary$total_interest,
      total_fees=summary$total_fees,
      total_financing_expense=summary$total_financing_expense,
      total_cash_service=summary$total_cash_service,
      first_month_cash_service=summary$first_month_cash_service,
      last_month_cash_service=summary$last_month_cash_service,
      max_month_cash_service=summary$max_month_cash_service,
      free_cash_before_debt_service=free_before_debt,
      restschuld_end=summary$restschuld_end,
      min_capital_service_ratio=if(is.finite(ratio)) ratio else NULL,
      capital_service_policy_ok=if(is.finite(ratio)) ratio>=1 else NULL
    ),
    alternative=NULL,
    comparison=NULL,
    note=paste(
      "Finanzierung wird nach der operativen Team-Entscheidung separat bewertet.",
      "Kreditbetrag ist kein Betriebsergebnis; Tilgung ist Liquiditätsabfluss, aber kein Aufwand."
    )
  )
}

fbc_team_p1_payload45 <- function(cfg){
  if(!identical(as.character(cfg$mode %||% ""),"owner_team"))
    stop("45_team_p1_optimizer.R ist nur für mode='owner_team'.")

  # The operating decision is deliberately financing-free. If the user starts
  # the separate financing calculation, the contract is evaluated only after
  # the same operating Team decision has been reconstructed.
  cfg_operating <- cfg
  cfg_operating$financing <- cfg$financing %||% list()
  cfg_operating$financing$active <- FALSE
  cfg_operating$financing$amount <- 0
  cfg_operating$financing$rate_pa <- 0
  cfg_operating$financing$fees_month <- 0

  state <- fbc_build_team_factual_state44(cfg_operating)
  reg <- fbc_team_registry45(cfg_operating,state)

  current_x <- reg$current
  names(current_x) <- reg$name
  current_point <- fbc_team_eval_x45(state,cfg_operating,reg,current_x)
  current_net <- as.numeric(current_point$net_available)
  target <- fbc_num45(state$owner$monthly_target,0)

  max_result <- fbc_team_best_attainable45(state,cfg_operating,reg)
  target_reachable <- is.finite(max_result$net) && max_result$net >= target-0.01

  if(current_net >= target-0.01 && isTRUE(current_point$role_price_structure$ok)){
    selected <- list(x=current_x,point=current_point,net=current_net)
  } else if(target_reachable){
    selected <- fbc_team_minimal_target45(
      state=state,
      cfg=cfg_operating,
      reg=reg,
      target=target,
      start=max_result$x
    )
    if(is.null(selected)) selected <- max_result
  } else {
    selected <- max_result
  }

  solution <- fbc_team_x_named45(reg,selected$x)
  point <- selected$point
  projected_net <- as.numeric(point$net_available)
  remaining_gap <- max(0,target-projected_net)
  target_reached <- projected_net >= target-0.01

  economics <- fbc_team_employee_economics45(state,point)
  billable <- Filter(function(x) isTRUE(x$direct_billing),economics)
  employee_coverage_ok <- if(length(billable))
    all(vapply(billable,function(x) isTRUE(x$cost_coverage_ok),logical(1))) else TRUE

  business_be <- fbc_team_business_break_even45(state,cfg_operating,point)
  changes <- fbc_team_changes45(reg,solution,state)
  shapley <- fbc_team_exact_shapley45(state,cfg_operating,reg,solution,changes)

  # P0 layers remain useful in the P1 response for consistent frontend display.
  current_economics <- fbc_team_employee_economics44(state)
  sensitivity <- fbc_team_sensitivity_payload44(state,cfg_operating)
  guidance <- fbc_team_guidance_payload44(state,cfg_operating,current_economics)

  financing <- fbc_team_financing_payload45(cfg,point)
  fin_ratio <- if(isTRUE(financing$active) && is.list(financing$current))
    fbc_num45(financing$current$min_capital_service_ratio,NA_real_) else NA_real_

  implementation_max <- if(length(changes))
    max(vapply(changes,function(x) as.integer(x$implementation_months %||% 0L),integer(1))) else 0L

  list(
    schema_version="fbc_team_decision_payload_v1",
    status=if(target_reached) "team_p1_target_reached" else "target_not_reachable",

    current=list(
      expected_net=as.numeric(current_net),
      monthly_target=as.numeric(target),
      target_gap_eur=as.numeric(max(0,target-current_net)),
      target_gap_percent=if(target>0) as.numeric(100*max(0,target-current_net)/target) else NULL,
      revenue_total=as.numeric(current_point$revenue_total),
      owner_revenue=as.numeric(current_point$owner_revenue),
      employee_revenue=as.numeric(current_point$employee_revenue),
      operating_costs=as.numeric(current_point$operating_costs),
      personnel_costs=as.numeric(current_point$personnel_costs),
      result_before_owner_protection_tax=as.numeric(current_point$result_before_owner_protection_tax)
    ),

    sensitivity=sensitivity,
    decision_guidance=guidance,

    team_economics=list(
      employees=economics,
      current_employees=current_economics,
      uncovered_count=sum(vapply(economics,function(x) identical(x$status,"coverage_open"),logical(1))),
      role_price_structure=point$role_price_structure
    ),

    recommendation=list(
      candidate_id=if(target_reached) "team_p1_minimal_confirmed_change" else "team_p1_best_attainable",
      active_levers=as.list(vapply(changes,function(x) as.character(x$lever),character(1))),
      changes=changes,
      projected_net=as.numeric(projected_net),
      expected_net_after=as.numeric(projected_net),
      remaining_gap_eur=as.numeric(remaining_gap),
      target_reached=isTRUE(target_reached),
      explanation=shapley
    ),

    target_path=list(
      time_to_target_months=if(target_reached) as.integer(implementation_max) else NULL,
      target_reached_within_horizon=isTRUE(target_reached),
      implementation_months_max=as.integer(implementation_max),
      liquidity_bridge_need_eur=0,
      path=list()
    ),

    post_decision=list(
      post_target_stable=NULL,
      max_post_target_gap_eur=NULL,
      mc_target_probability=NULL,
      mc_policy_ok=NULL,
      business_break_even_margin_eur=business_be$safety_margin,
      business_break_even_ok=isTRUE(business_be$ok),
      employee_break_even_ok=isTRUE(employee_coverage_ok),
      capital_service_ratio=if(is.finite(fin_ratio)) as.numeric(fin_ratio) else NULL,
      capital_service_ok=if(is.finite(fin_ratio)) fin_ratio>=1 else NULL
    ),

    break_even=list(
      business=business_be,
      employees=economics
    ),

    financing=financing,

    robustness=list(
      available=FALSE,
      target_probability=NULL,
      p10=NULL,
      p50=NULL,
      p90=NULL,
      reason="Team Monte Carlo is not executed by deterministic module 45."
    ),

    production_meta=list(
      input_schema=cfg$schema_version,
      backend="FBC_R_BACKEND_TEAM_P1_1.0",
      team_member_count=length(state$employees),
      candidate_count=1L,
      optimizer_levers=as.list(reg$name),
      optimizer_dimension=nrow(reg),
      max_optimizer_dimension=3L,
      optimizer_executed=TRUE,
      target_feasible=isTRUE(target_reachable),
      financing_separate=TRUE,
      role_priority_model="ordinal_price_constraint_no_role_score",
      demand_model=list(model="constant_price_elasticity",epsilon=as.numeric(FBC_PRICE_ELASTICITY25)),
      methods_used=c(
        "Team factual state",
        "confirmed Team bounds (max 3)",
        "two-stage reachability + minimal normalized movement",
        "constant price elasticity",
        "hard ordinal role price-structure constraint",
        "Team employee cost coverage",
        "Team Business Break-even",
        "exact Shapley for selected Team levers",
        if(isTRUE(financing$active)) "separate financing / capital-service check" else NULL
      )
    ),

    audit=list(
      path=if(target_reached) "TEAM_P1_TARGET_REACHED" else "TEAM_P1_TARGET_NOT_REACHABLE",
      evidence_gate="1 to 3 user-confirmed Team levers",
      reachability_checked_before_minimal_change=TRUE,
      every_point_via="fbc_team_eval_point44",
      demand_model=list(model="constant_price_elasticity",epsilon=as.numeric(FBC_PRICE_ELASTICITY25)),
      role_priority_guard="role order is never multiplied into economic effect",
      fixed_role_gap=FALSE,
      financing_in_operating_vector=FALSE,
      mc_executed=FALSE,
      optimizer_executed=TRUE
    )
  )
}

cat("\n45 Team P1 optimizer loaded.\n")
