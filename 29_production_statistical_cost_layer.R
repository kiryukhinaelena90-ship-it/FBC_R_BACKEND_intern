# ==========================================
# 29_production_statistical_cost_layer.R
# FUTURE Business Cockpit
# All 10 operating-cost positions + external statistical exposures
# ==========================================
#
# Principle:
# - all 10 cost positions remain separate in every MC draw;
# - no statistical proxy is invented;
# - known mappings are explicit (energy, fuel);
# - additional official/statistical series can be attached later by column name;
# - unmapped costs remain statistically unchanged, but remain present in the
#   deterministic/optimization layer.
# ==========================================

FBC_COST_KEYS29 <- c(
  "office","energy","vehicle","fuel","software",
  "accounting","business_insurance","material","marketing","other"
)

default_cost_factor_map29 <- function(mc) {
  rows <- list()

  add <- function(key, col, source_label) {
    if (col %in% names(mc)) {
      rows[[length(rows)+1]] <<- data.frame(
        key=key,
        pct_column=col,
        source_label=source_label,
        stringsAsFactors=FALSE
      )
    }
  }

  add("energy","energie_aenderung_prozent","Energie")
  add("fuel","kraftstoff_aenderung_prozent","Kraftstoff")

  # Generic future-proof convention. These are used ONLY if the MC table
  # actually contains the corresponding statistical series.
  for (k in FBC_COST_KEYS29) {
    add(k,paste0("cost_",k,"_aenderung_prozent"),
        paste0("Statistische Reihe für ",k))
  }

  if (!length(rows)) {
    return(data.frame(
      key=character(),pct_column=character(),source_label=character(),
      stringsAsFactors=FALSE
    ))
  }

  out <- do.call(rbind,rows)
  out <- out[!duplicated(out$key),,drop=FALSE]
  rownames(out)<-NULL
  out
}

validate_cost_factor_map29 <- function(state, mc, cost_factor_map) {
  if (!all(FBC_COST_KEYS29 %in% names(state$operating_costs))) {
    stop("State enthält nicht alle 10 Betriebskostenpositionen.")
  }
  if (nrow(cost_factor_map)) {
    bad_keys <- setdiff(cost_factor_map$key,FBC_COST_KEYS29)
    if (length(bad_keys)) stop("Unbekannte cost keys: ",paste(bad_keys,collapse=", "))
    bad_cols <- setdiff(cost_factor_map$pct_column,names(mc))
    if (length(bad_cols)) stop("MC-Spalten fehlen: ",paste(bad_cols,collapse=", "))
    if (anyDuplicated(cost_factor_map$key)) stop("Jede Kostenposition darf nur eine statistische Mapping-Reihe haben.")
  }
  invisible(TRUE)
}

build_all_cost_mc29 <- function(
    state,
    mc,
    cost_factor_map=default_cost_factor_map29(mc),
    labor_pct_column="arbeitskosten_aenderung_prozent",
    owner_pension_month=0,
    tax_month=0,
    market_reference_rate=NULL
) {
  validate_cost_factor_map29(state,mc,cost_factor_map)
  n <- nrow(mc)
  if (n < 1) stop("MC-Tabelle ist leer.")

  draws <- data.frame(
    simulation=if("simulation"%in%names(mc)) mc$simulation else seq_len(n)
  )

  exposure <- data.frame(
    key=FBC_COST_KEYS29,
    current_eur=as.numeric(state$operating_costs[FBC_COST_KEYS29]),
    statistical_exposure=FALSE,
    pct_column=NA_character_,
    source_label=NA_character_,
    stringsAsFactors=FALSE
  )

  for (k in FBC_COST_KEYS29) {
    base <- unname(state$operating_costs[[k]])
    maprow <- cost_factor_map[cost_factor_map$key==k,,drop=FALSE]

    if (nrow(maprow)==1) {
      pct <- mc[[maprow$pct_column]]
      if (any(!is.finite(pct))) stop("Nicht-endliche Statistikwerte für ",k)
      vals <- base*(1+pct/100)
      exposure$statistical_exposure[exposure$key==k] <- TRUE
      exposure$pct_column[exposure$key==k] <- maprow$pct_column
      exposure$source_label[exposure$key==k] <- maprow$source_label
      draws[[paste0("pct_",k)]] <- pct
    } else {
      vals <- rep(base,n)
      draws[[paste0("pct_",k)]] <- 0
    }

    if (any(vals<0)) stop("Statistik erzeugt negative Kosten für ",k)
    draws[[paste0("cost_",k)]] <- vals
  }

  if (labor_pct_column %in% names(mc)) {
    labor_pct <- mc[[labor_pct_column]]
    personnel <- state$employee$personnel_cost_month*(1+labor_pct/100)
    labor_exposed <- TRUE
  } else {
    labor_pct <- rep(0,n)
    personnel <- rep(state$employee$personnel_cost_month,n)
    labor_exposed <- FALSE
  }
  draws$labor_pct <- labor_pct
  draws$personnel_cost <- personnel

  variable_finance <- isTRUE(state$financing$active) &&
    identical(state$financing$binding,"variable")

  if (variable_finance) {
    if (!"kreditzins" %in% names(mc))
      stop("Variable Finanzierung aktiv, aber kreditzins fehlt in MC.")
    if (is.null(market_reference_rate))
      stop("market_reference_rate für variable Finanzierung explizit angeben.")
    draws$financing_result_cost <- variable_financing_result_cost22(
      state,mc$kreditzins,market_reference_rate
    )
  } else {
    draws$financing_result_cost <- rep(state$financing$interest_plus_fees_month,n)
  }

  owner_billable <- as.numeric(state$owner$billable_hours_month)
  if(!is.finite(owner_billable)) stop("state$owner$billable_hours_month fehlt oder ist ungültig.")

  employee_billable <- if(isTRUE(state$employee$direct_billing)) {
    x <- as.numeric(state$employee$billable_hours_month)
    if(!is.finite(x)) stop("state$employee$billable_hours_month fehlt oder ist ungültig.")
    x
  } else 0

  owner_revenue <- state$owner$price * owner_billable
  employee_revenue <- if(isTRUE(state$employee$direct_billing)) {
    state$employee$customer_price * employee_billable
  } else 0

  cost_cols <- paste0("cost_",FBC_COST_KEYS29)
  draws$operating_costs_total <- rowSums(draws[,cost_cols,drop=FALSE])
  draws$owner_revenue <- owner_revenue
  draws$employee_revenue <- employee_revenue

  draws$net_available <-
    owner_revenue + employee_revenue -
    draws$operating_costs_total -
    draws$personnel_cost -
    draws$financing_result_cost -
    state$owner$insurance_month -
    owner_pension_month -
    tax_month

  draws$target_gap_eur <- pmax(0,state$owner$monthly_target-draws$net_available)
  draws$target_reached <- draws$net_available>=state$owner$monthly_target

  list(
    schema_version="29.0",
    draws=draws,
    exposure=exposure,
    labor_exposure=labor_exposed,
    interest_exposure=variable_finance,
    note="Alle 10 Kostenpositionen bleiben separat; nur vorhandene Statistikreihen bewegen sie."
  )
}

summarise_cost_layer29 <- function(layer) {
  d<-layer$draws
  rows<-lapply(FBC_COST_KEYS29,function(k){
    x<-d[[paste0("cost_",k)]]
    data.frame(
      key=k,
      expected=mean(x),
      median=median(x),
      p10=as.numeric(quantile(x,.10)),
      p90=as.numeric(quantile(x,.90)),
      statistical_exposure=layer$exposure$statistical_exposure[layer$exposure$key==k],
      stringsAsFactors=FALSE
    )
  })
  costs<-do.call(rbind,rows)
  list(
    costs=costs,
    personnel=data.frame(
      expected=mean(d$personnel_cost),
      median=median(d$personnel_cost),
      p10=as.numeric(quantile(d$personnel_cost,.10)),
      p90=as.numeric(quantile(d$personnel_cost,.90)),
      statistical_exposure=layer$labor_exposure
    ),
    net=list(
      expected=mean(d$net_available),
      median=median(d$net_available),
      p10=as.numeric(quantile(d$net_available,.10)),
      p90=as.numeric(quantile(d$net_available,.90)),
      probability_target=mean(d$target_reached)
    )
  )
}

build_expected_state29 <- function(state, layer, use=c("expected","median")) {
  use<-match.arg(use)
  s<-summarise_cost_layer29(layer)
  out<-state

  vals<-setNames(s$costs[[use]],s$costs$key)
  out$operating_costs[FBC_COST_KEYS29]<-vals[FBC_COST_KEYS29]

  # Personnel cost in the expected state reflects the statistical labor context.
  out$employee$personnel_cost_month <- s$personnel[[use]]

  out$statistical_context <- list(
    basis=use,
    source_schema="29.0",
    operating_costs=s$costs,
    personnel=s$personnel,
    net=s$net
  )
  out
}

cat("\n29 Production Statistical Cost Layer loaded.\n")
