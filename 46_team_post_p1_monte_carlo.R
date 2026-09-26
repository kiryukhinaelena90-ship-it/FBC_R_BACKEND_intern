# ============================================================
# 46_team_post_p1_monte_carlo.R
# FUTURE Business Cockpit
# Team post-P1 Monte Carlo robustness layer
# ============================================================
#
# Принцип:
# - решение Team-P1 НЕ переоптимизируется;
# - используем уже найденную комбинацию recommendation.changes;
# - неопределённость накладывается на статистически доступные затраты;
# - выручка остаётся на выбранном P1-состоянии с уже учтённой эластичностью;
# - кредитный principal никогда не считается доходом;
# - operating robustness и устойчивость обслуживания кредита разделены.
# ============================================================

`%||%` <- function(a,b) if(is.null(a) || length(a)==0L) b else a

fbc_num46 <- function(x, default=NA_real_){
  z <- suppressWarnings(as.numeric(x %||% default)[1])
  if(!is.finite(z)) default else z
}

fbc_team_mc_empty46 <- function(reason=NULL){
  list(
    available=FALSE,
    expected=NULL,
    target_probability=NULL,
    probability_not_target=NULL,
    p10=NULL,
    p50=NULL,
    p90=NULL,
    n=NULL,
    business_break_even_probability=NULL,
    employee_coverage_probability=NULL,
    employees=list(),
    financing=list(
      available=FALSE,
      probability_capital_service_covered=NULL,
      ratio_p10=NULL,
      ratio_p50=NULL,
      ratio_p90=NULL
    ),
    risk_drivers=list(),
    reason=reason
  )
}

fbc_team_mc_cost_key46 <- function(key){
  key <- as.character(key %||% "")[1]
  if(identical(key,"insurance")) "business_insurance" else key
}

# Восстанавливаем именно выбранное Team-P1 operating-состояние.
# Финансирование здесь намеренно выключено: оно проверяется отдельно.
fbc_team_selected_operating46 <- function(payload, cfg){
  cfg_operating <- cfg
  cfg_operating$financing <- cfg$financing %||% list()
  cfg_operating$financing$active <- FALSE
  cfg_operating$financing$amount <- 0
  cfg_operating$financing$rate_pa <- 0
  cfg_operating$financing$fees_month <- 0

  state <- fbc_build_team_factual_state44(cfg_operating)

  owner_price <- as.numeric(state$owner$price)
  owner_hours <- as.numeric(state$owner$billable_hours_month)
  employee_prices <- numeric(0)
  employee_hours <- numeric(0)
  operating_costs <- state$operating_costs

  changes <- payload$recommendation$changes %||% list()

  for(ch in changes){
    lever <- as.character(ch$lever %||% "")[1]
    proposed <- fbc_num46(ch$proposed, NA_real_)
    if(!nzchar(lever) || !is.finite(proposed)) next

    if(identical(lever,"owner_price")){
      owner_price <- proposed
      next
    }

    if(identical(lever,"owner_hours")){
      owner_hours <- proposed
      next
    }

    if(grepl("^cost_",lever)){
      key <- fbc_team_mc_cost_key46(sub("^cost_","",lever))
      if(key %in% names(operating_costs)) operating_costs[[key]] <- proposed
      next
    }

    for(member in state$employees){
      if(identical(lever,paste0(member$id,"_customer_price"))){
        employee_prices[member$id] <- proposed
        break
      }

      if(identical(lever,paste0(member$id,"_billable_hours"))){
        employee_hours[member$id] <- proposed
        break
      }
    }
  }

  point <- fbc_team_eval_point44(
    state=state,
    cfg=cfg_operating,
    owner_price=owner_price,
    owner_hours=owner_hours,
    employee_prices=if(length(employee_prices)) employee_prices else NULL,
    employee_hours=if(length(employee_hours)) employee_hours else NULL,
    operating_costs=operating_costs
  )

  point$operating_costs_vector <- operating_costs

  list(
    cfg=cfg_operating,
    state=state,
    point=point
  )
}

fbc_team_mc_risk_drivers46 <- function(cost_draws, exposure, personnel_draw){
  rows <- list()

  if(nrow(exposure)){
    for(i in seq_len(nrow(exposure))){
      if(!isTRUE(exposure$statistical_exposure[i])) next
      key <- as.character(exposure$key[i])
      x <- cost_draws[[paste0("cost_",key)]]
      if(is.null(x)) next

      rows[[length(rows)+1L]] <- data.frame(
        factor=key,
        variance=var(x),
        source=as.character(exposure$source_label[i]),
        stringsAsFactors=FALSE
      )
    }
  }

  if(length(personnel_draw) && var(personnel_draw) > 0){
    rows[[length(rows)+1L]] <- data.frame(
      factor="labor",
      variance=var(personnel_draw),
      source="Arbeitskosten Team",
      stringsAsFactors=FALSE
    )
  }

  if(!length(rows)) return(list())

  tab <- do.call(rbind,rows)
  tab <- tab[order(-tab$variance),,drop=FALSE]
  tab <- head(tab,5L)

  lapply(seq_len(nrow(tab)),function(i){
    list(
      factor=as.character(tab$factor[i]),
      variance=as.numeric(tab$variance[i]),
      source=as.character(tab$source[i])
    )
  })
}

fbc_team_mc_employee_coverage46 <- function(state, point, labor_factor){
  if(!length(state$employees)){
    return(list(
      probability_all_billable_covered=1,
      employees=list()
    ))
  }

  point_by_id <- setNames(
    point$employees,
    vapply(point$employees,function(x) as.character(x$id),character(1))
  )

  n <- length(labor_factor)
  all_covered <- rep(TRUE,n)
  rows <- list()

  for(member in state$employees){
    pp <- point_by_id[[member$id]]

    if(!isTRUE(member$direct_billing)){
      rows[[length(rows)+1L]] <- list(
        id=member$id,
        label=member$label,
        applicable=FALSE,
        probability_cost_covered=NULL,
        contribution_p10=NULL,
        contribution_p50=NULL,
        contribution_p90=NULL
      )
      next
    }

    personnel_draw <- as.numeric(member$personnel_cost_month) * labor_factor
    revenue <- fbc_num46(pp$revenue_month,0)
    variable <- fbc_num46(pp$variable_cost_month,0)
    contribution <- revenue-variable-personnel_draw
    covered <- contribution >= -1e-8

    all_covered <- all_covered & covered

    rows[[length(rows)+1L]] <- list(
      id=member$id,
      label=member$label,
      applicable=TRUE,
      probability_cost_covered=as.numeric(mean(covered)),
      contribution_p10=as.numeric(quantile(contribution,.10,names=FALSE)),
      contribution_p50=as.numeric(median(contribution)),
      contribution_p90=as.numeric(quantile(contribution,.90,names=FALSE))
    )
  }

  list(
    probability_all_billable_covered=as.numeric(mean(all_covered)),
    employees=rows
  )
}

fbc_team_mc_business_break_even46 <- function(point, cfg, operating_draws, personnel_draw){
  owner_hours <- fbc_num46(point$owner_demand$expected_hours,0)
  employee_hours <- if(length(point$employees))
    sum(vapply(point$employees,function(x) fbc_num46(x$expected_hours,0),numeric(1)))
  else 0

  total_hours <- owner_hours+employee_hours
  revenue <- fbc_num46(point$revenue_total,0)

  if(!is.finite(total_hours) || total_hours <= 0 || !is.finite(revenue)){
    return(list(
      probability=0,
      margin_p10=NULL,
      margin_p50=NULL,
      margin_p90=NULL
    ))
  }

  weighted_price <- revenue/total_hours

  variable_keys <- as.character(unlist(cfg$variable_cost_keys %||% character()))
  variable_keys <- vapply(variable_keys,fbc_team_mc_cost_key46,character(1))
  variable_keys <- intersect(variable_keys,names(point$operating_costs_vector))

  if(length(variable_keys)){
    variable_cols <- paste0("cost_",variable_keys)
    variable_cols <- intersect(variable_cols,names(operating_draws))
    op_variable <- if(length(variable_cols))
      rowSums(operating_draws[,variable_cols,drop=FALSE])
    else
      rep(0,nrow(operating_draws))
  } else {
    op_variable <- rep(0,nrow(operating_draws))
  }

  op_total <- operating_draws$operating_costs_total
  member_variable <- fbc_num46(point$member_variable_costs,0)

  variable_per_hour <- (op_variable+member_variable)/total_hours
  fixed_result_costs <- op_total-op_variable+personnel_draw
  db_per_hour <- weighted_price-variable_per_hour

  reachable <- is.finite(db_per_hour) & db_per_hour>0
  be_revenue <- rep(Inf,length(db_per_hour))
  be_revenue[reachable] <-
    fixed_result_costs[reachable]/db_per_hour[reachable]*weighted_price

  margin <- revenue-be_revenue
  ok <- reachable & is.finite(margin) & margin>=-1e-8

  finite_margin <- margin[is.finite(margin)]

  list(
    probability=as.numeric(mean(ok)),
    margin_p10=if(length(finite_margin)) as.numeric(quantile(finite_margin,.10,names=FALSE)) else NULL,
    margin_p50=if(length(finite_margin)) as.numeric(median(finite_margin)) else NULL,
    margin_p90=if(length(finite_margin)) as.numeric(quantile(finite_margin,.90,names=FALSE)) else NULL
  )
}

fbc_team_mc_financing46 <- function(payload, free_before_debt_draw){
  fin <- payload$financing %||% list()
  cur <- fin$current

  if(!isTRUE(fin$active) || is.null(cur) || !is.list(cur)){
    return(list(
      available=FALSE,
      probability_capital_service_covered=NULL,
      ratio_p10=NULL,
      ratio_p50=NULL,
      ratio_p90=NULL,
      binding=NULL,
      scope="no_financing"
    ))
  }

  service <- fbc_num46(cur$max_month_cash_service,NA_real_)
  if(!is.finite(service) || service <= 0){
    return(list(
      available=FALSE,
      probability_capital_service_covered=NULL,
      ratio_p10=NULL,
      ratio_p50=NULL,
      ratio_p90=NULL,
      binding=as.character(cur$binding %||% ""),
      scope="cash_service_missing"
    ))
  }

  ratio <- free_before_debt_draw/service

  list(
    available=TRUE,
    probability_capital_service_covered=as.numeric(mean(ratio>=1)),
    ratio_p10=as.numeric(quantile(ratio,.10,names=FALSE)),
    ratio_p50=as.numeric(median(ratio)),
    ratio_p90=as.numeric(quantile(ratio,.90,names=FALSE)),
    binding=as.character(cur$binding %||% ""),
    max_month_cash_service=as.numeric(service),
    scope=paste(
      "Operating-cost and labor shocks against the current financing contract.",
      "The loan principal is not income; no financing lever is reoptimized."
    )
  )
}

fbc_run_team_post_p1_mc46 <- function(
    payload,
    cfg,
    root=getwd(),
    mc_file=file.path(root,"data/processed/fbc_monte_carlo_draws.csv"),
    cost_factor_map=NULL,
    labor_pct_column="arbeitskosten_aenderung_prozent"
){
  out <- payload
  if(is.null(out) || !is.list(out)) stop("payload must be a Team P1 payload list.")

  fail_soft <- function(message){
    out$robustness <<- fbc_team_mc_empty46(message)
    if(is.null(out$post_decision)) out$post_decision <<- list()
    out$post_decision$mc_target_probability <<- NULL
    out$post_decision$mc_business_break_even_probability <<- NULL
    out$post_decision$mc_employee_coverage_probability <<- NULL
    out$post_decision$mc_capital_service_probability <<- NULL
    if(is.null(out$audit)) out$audit <<- list()
    out$audit$mc_executed <<- FALSE
    out$audit$mc_available <<- FALSE
    out$audit$mc_error <<- message
    out
  }

  tryCatch({
    if(!identical(as.character(cfg$mode %||% ""),"owner_team"))
      stop("Team Monte Carlo requires mode='owner_team'.")

    if(!exists("default_cost_factor_map29",mode="function")){
      source(file.path(root,"29_production_statistical_cost_layer.R"),local=.GlobalEnv)
    }

    if(!file.exists(mc_file))
      return(fail_soft(paste0("Monte Carlo data file not found: ",mc_file)))

    mc <- read.csv(mc_file,stringsAsFactors=FALSE,check.names=FALSE)
    if(!nrow(mc)) return(fail_soft("Monte Carlo data file is empty."))

    selected <- fbc_team_selected_operating46(out,cfg)
    state <- selected$state
    point <- selected$point
    cfg_operating <- selected$cfg

    base_costs <- point$operating_costs_vector
    keys <- FBC_COST_KEYS29
    missing_keys <- setdiff(keys,names(base_costs))
    if(length(missing_keys))
      return(fail_soft(paste("Team operating costs missing:",paste(missing_keys,collapse=", "))))

    map <- if(is.null(cost_factor_map)) default_cost_factor_map29(mc) else cost_factor_map
    validate_cost_factor_map29(
      list(operating_costs=base_costs),
      mc,
      map
    )

    n <- nrow(mc)
    draws <- data.frame(
      simulation=if("simulation" %in% names(mc)) mc$simulation else seq_len(n)
    )

    exposure <- data.frame(
      key=keys,
      current_eur=as.numeric(base_costs[keys]),
      statistical_exposure=FALSE,
      pct_column=NA_character_,
      source_label=NA_character_,
      stringsAsFactors=FALSE
    )

    for(key in keys){
      base <- as.numeric(base_costs[[key]])
      maprow <- map[map$key==key,,drop=FALSE]

      if(nrow(maprow)==1L){
        pct <- mc[[maprow$pct_column]]
        if(any(!is.finite(pct))) stop("Nicht-endliche Statistikwerte für ",key)
        value <- base*(1+pct/100)
        exposure$statistical_exposure[exposure$key==key] <- TRUE
        exposure$pct_column[exposure$key==key] <- maprow$pct_column
        exposure$source_label[exposure$key==key] <- maprow$source_label
      } else {
        pct <- rep(0,n)
        value <- rep(base,n)
      }

      if(any(value<0)) stop("Statistik erzeugt negative Kosten für ",key)
      draws[[paste0("pct_",key)]] <- pct
      draws[[paste0("cost_",key)]] <- value
    }

    cost_cols <- paste0("cost_",keys)
    draws$operating_costs_total <- rowSums(draws[,cost_cols,drop=FALSE])

    labor_pct <- if(labor_pct_column %in% names(mc))
      mc[[labor_pct_column]]
    else
      rep(0,n)

    if(any(!is.finite(labor_pct)))
      stop("Nicht-endliche Statistikwerte für Team-Arbeitskosten.")

    labor_factor <- 1+labor_pct/100
    if(any(labor_factor<0))
      stop("Statistik erzeugt negative Personalkosten im Team.")

    personnel_base <- sum(vapply(
      state$employees,
      function(x) fbc_num46(x$personnel_cost_month,0),
      numeric(1)
    ))

    draws$labor_pct <- labor_pct
    draws$personnel_cost <- personnel_base*labor_factor

    owner_revenue <- fbc_num46(point$owner_revenue,0)
    employee_revenue <- fbc_num46(point$employee_revenue,0)
    revenue_total <- fbc_num46(point$revenue_total,owner_revenue+employee_revenue)
    member_variable <- fbc_num46(point$member_variable_costs,0)

    draws$owner_revenue <- owner_revenue
    draws$employee_revenue <- employee_revenue
    draws$revenue_total <- revenue_total
    draws$member_variable_costs <- member_variable

    # Operating result only. Finanzierung remains separate from Team-P1 net.
    gross <-
      revenue_total -
      draws$operating_costs_total -
      draws$personnel_cost -
      member_variable

    fin_points <- lapply(
      gross,
      function(g){
        fbc_financial_point24(
          result_before_owner_protection_tax=g,
          owner=state$owner,
          legal_form=state$legal_form %||% "freelance",
          trade_tax_rate=state$trade_tax_rate %||% 0
        )
      }
    )

    draws$result_before_owner_protection_tax <- gross
    draws$owner_insurance_month <- vapply(fin_points,function(z) as.numeric(z$insurance_month),numeric(1))
    draws$owner_pension_month <- vapply(fin_points,function(z) as.numeric(z$pension_month),numeric(1))
    draws$tax_month <- vapply(fin_points,function(z) as.numeric(z$tax_month),numeric(1))
    draws$net_available <- vapply(fin_points,function(z) as.numeric(z$net_available),numeric(1))

    target <- fbc_num46(out$current$monthly_target,state$owner$monthly_target)
    if(!is.finite(target)) stop("Team target is not finite.")

    draws$target_gap_eur <- pmax(0,target-draws$net_available)
    draws$target_reached <- draws$net_available>=target

    if(any(!is.finite(draws$net_available)))
      return(fail_soft("Team Monte Carlo produced non-finite net results."))

    be <- fbc_team_mc_business_break_even46(
      point=point,
      cfg=cfg_operating,
      operating_draws=draws,
      personnel_draw=draws$personnel_cost
    )

    coverage <- fbc_team_mc_employee_coverage46(
      state=state,
      point=point,
      labor_factor=labor_factor
    )

    free_before_debt <-
      revenue_total -
      draws$operating_costs_total -
      draws$personnel_cost -
      member_variable

    financing_robustness <- fbc_team_mc_financing46(
      out,
      free_before_debt
    )

    net <- draws$net_available
    reached <- draws$target_reached

    rob <- list(
      available=TRUE,
      expected=as.numeric(mean(net)),
      target_probability=as.numeric(mean(reached)),
      probability_not_target=as.numeric(mean(!reached)),
      p10=as.numeric(quantile(net,.10,names=FALSE)),
      p50=as.numeric(median(net)),
      p90=as.numeric(quantile(net,.90,names=FALSE)),
      n=as.integer(length(net)),
      business_break_even_probability=be$probability,
      business_break_even_margin=list(
        p10=be$margin_p10,
        p50=be$margin_p50,
        p90=be$margin_p90
      ),
      employee_coverage_probability=coverage$probability_all_billable_covered,
      employees=coverage$employees,
      financing=financing_robustness,
      risk_drivers=fbc_team_mc_risk_drivers46(
        cost_draws=draws,
        exposure=exposure,
        personnel_draw=draws$personnel_cost
      ),
      uncertainty_scope=list(
        operating_costs=as.list(exposure$key[exposure$statistical_exposure]),
        labor=isTRUE(any(abs(labor_pct)>0)),
        revenue=FALSE,
        demand=FALSE,
        financing_rate=FALSE
      ),
      method=paste(
        "Team post-P1 Monte Carlo on the fixed elasticity-adjusted selected operating state.",
        "Statistical shocks affect only mapped operating costs and Team personnel costs.",
        "Financing principal is not income and the operating decision is not reoptimized."
      ),
      decision_reoptimized=FALSE
    )

    out$robustness <- rob

    if(is.null(out$post_decision)) out$post_decision <- list()
    out$post_decision$mc_target_probability <- rob$target_probability
    out$post_decision$mc_business_break_even_probability <- rob$business_break_even_probability
    out$post_decision$mc_employee_coverage_probability <- rob$employee_coverage_probability
    out$post_decision$mc_capital_service_probability <-
      if(isTRUE(rob$financing$available))
        rob$financing$probability_capital_service_covered
      else
        NULL

    if(is.null(out$production_meta)) out$production_meta <- list()
    methods <- out$production_meta$methods_used %||% character()
    out$production_meta$methods_used <- unique(c(methods,"Team post-P1 Monte Carlo robustness"))

    if(is.null(out$audit)) out$audit <- list()
    out$audit$mc_executed <- TRUE
    out$audit$mc_available <- TRUE
    out$audit$mc_error <- NULL
    out$audit$mc_data_file <- mc_file
    out$audit$mc_decision_reoptimized <- FALSE
    out$audit$mc_net_basis <- "same Team operating net basis as deterministic P1; financing separate"

    out
  },error=function(e){
    fail_soft(conditionMessage(e))
  })
}

cat("\n46 Team post-P1 Monte Carlo robustness layer loaded.\n")
