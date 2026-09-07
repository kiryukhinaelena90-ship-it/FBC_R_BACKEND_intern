# ============================================================
# 37_production_rules_data_layer_V2.R
# FUTURE Business Cockpit
# Unified production constraints:
# - employment forms
# - price/capacity bounds
# - 10 operating-cost positions
# - implementation evidence
# - financing stays separate
#
# Production principle:
# NO evidence -> NO optimization lever.
# A change of employment status is NOT silently optimized.
# ============================================================

`%||%` <- function(a,b) if(is.null(a) || length(a)==0L) b else a

FBC_LEGAL_2026_V2 <- list(
  minimum_wage_eur_h = 13.90,
  minijob_regular_monthly_limit_eur = 603.00,
  minijob_annual_limit_eur = 7236.00,
  midijob_lower_eur_month = 603.01,
  midijob_upper_eur_month = 2000.00,
  werkstudent_regular_hours_week = 20.00,
  short_term_months = 3L,
  short_term_workdays = 70L,
  short_term_agriculture_weeks = 15L,
  short_term_agriculture_workdays = 90L,
  arbzg_regular_hours_day = 8.00,
  arbzg_extended_hours_day = 10.00,
  arbzg_compensation_months = 6L,
  arbzg_compensation_weeks = 24L,
  arbzg_rest_hours = 11.00,
  effective_from = as.Date("2026-01-01"),
  sources = c(
    minimum_wage_minijob_midijob =
      "DRV 2026: Mindestlohn 13.90; Minijob 603; Midijob 603.01-2000",
    werkstudent =
      "DRV/TK: Werkstudentenprivileg grundsätzlich bis 20 Std./Woche; Ausnahmen separat prüfen",
    short_term =
      "Minijob-Zentrale 2026: grundsätzlich 3 Monate oder 70 Arbeitstage; Landwirtschaft 15 Wochen/90 Tage",
    working_time =
      "BMAS/ArbZG: grundsätzlich 8 Std./Werktag; bis 10 mit Ausgleich; 11 Std. Ruhezeit"
  )
)

FBC_ALLOWED_EVIDENCE_V2 <- c(
  "user_confirmed",
  "contract",
  "legal",
  "market_evidence",
  "system_physical"
)

fbc_assert_evidence37v2 <- function(source_type,label="bound"){
  if(length(source_type)!=1L || is.na(source_type) ||
     !(source_type %in% FBC_ALLOWED_EVIDENCE_V2)){
    stop(sprintf(
      "%s has no production-grade evidence. Allowed: %s",
      label,paste(FBC_ALLOWED_EVIDENCE_V2,collapse=", ")
    ))
  }
  invisible(TRUE)
}

fbc_validate_implementation37v2 <- function(x,label){
  if(is.null(x)) stop(sprintf("%s: implementation rule missing.",label))
  req <- c("months_to_full","mode","source_type")
  miss <- setdiff(req,names(x))
  if(length(miss))
    stop(sprintf("%s: missing implementation fields: %s",
                 label,paste(miss,collapse=", ")))
  fbc_assert_evidence37v2(x$source_type,paste0(label," implementation"))
  if(!is.numeric(x$months_to_full) || length(x$months_to_full)!=1L ||
     is.na(x$months_to_full) || x$months_to_full<0)
    stop(sprintf("%s: months_to_full must be a non-negative number.",label))
  if(!(x$mode %in% c("step","linear")))
    stop(sprintf("%s: implementation mode must be step or linear.",label))
  invisible(TRUE)
}

fbc_employment_form37v2 <- function(form){
  x <- tolower(trimws(as.character(form %||% "")))
  aliases <- c(
    "mini"="minijob","geringfuegig"="minijob","geringfügig"="minijob",
    "werkstudentin"="werkstudent","working_student"="werkstudent",
    "midi"="midijob",
    "part"="teilzeit","parttime"="teilzeit","part-time"="teilzeit",
    "full"="vollzeit","fulltime"="vollzeit","full-time"="vollzeit",
    "short_term"="kurzfristig","kurzfristige_beschaeftigung"="kurzfristig",
    "kurzfristige beschäftigung"="kurzfristig"
  )
  if(x %in% names(aliases)) x <- unname(aliases[x])
  valid <- c("minijob","midijob","werkstudent","teilzeit","vollzeit","kurzfristig")
  if(!(x %in% valid))
    stop(sprintf("Unbekannte Beschäftigungsform '%s'. Zulässig: %s",
                 x,paste(valid,collapse=", ")))
  x
}

fbc_check_minimum_wage37v2 <- function(employee){
  wage <- as.numeric(employee$wage_hour %||% NA_real_)
  if(!is.finite(wage))
    return("Stundenlohn fehlt; Mindestlohn kann nicht geprüft werden.")
  exc <- isTRUE(employee$minimum_wage_exception_confirmed)
  if(wage < FBC_LEGAL_2026_V2$minimum_wage_eur_h && !exc){
    return(sprintf(
      "Stundenlohn %.2f EUR liegt unter dem gesetzlichen Mindestlohn 2026 von %.2f EUR; keine bestätigte Ausnahme.",
      wage,FBC_LEGAL_2026_V2$minimum_wage_eur_h
    ))
  }
  character(0)
}

fbc_resolve_employment_constraints37v2 <- function(employee){
  form <- fbc_employment_form37v2(employee$form)
  wage <- as.numeric(employee$wage_hour %||% NA_real_)
  hours_week <- as.numeric(employee$hours_week %||% NA_real_)
  paid_hours_month <- as.numeric(employee$paid_hours_month %||% NA_real_)
  regular_monthly_pay <- as.numeric(
    employee$regular_monthly_pay %||%
      if(is.finite(wage) && is.finite(paid_hours_month)) wage*paid_hours_month else NA_real_
  )

  issues <- fbc_check_minimum_wage37v2(employee)
  warnings <- character(0)
  reclassification_required <- FALSE

  max_hours_week <- Inf
  max_paid_hours_month <- Inf
  status_monthly_pay_lower <- 0
  status_monthly_pay_upper <- Inf

  contract_hours_week <- as.numeric(employee$contract_hours_week %||% hours_week)
  if(is.finite(contract_hours_week) && contract_hours_week<0)
    issues <- c(issues,"Vertragliche Wochenstunden dürfen nicht negativ sein.")

  # Contract is a hard cap for ordinary optimization unless a contract change
  # is explicitly modelled as a separate evidenced action.
  if(is.finite(contract_hours_week)) max_hours_week <- contract_hours_week

  if(form=="minijob"){
    status_monthly_pay_upper <- FBC_LEGAL_2026_V2$minijob_regular_monthly_limit_eur
    if(is.finite(wage) && wage>0){
      max_paid_hours_month <- status_monthly_pay_upper/wage
    }
    if(is.finite(regular_monthly_pay) &&
       regular_monthly_pay > status_monthly_pay_upper + 1e-8){
      issues <- c(issues,sprintf(
        "Minijob: regelmäßiges Monatsentgelt %.2f EUR überschreitet die 2026-Grenze %.2f EUR.",
        regular_monthly_pay,status_monthly_pay_upper
      ))
      reclassification_required <- TRUE
    }
  }

  if(form=="midijob"){
    status_monthly_pay_lower <- FBC_LEGAL_2026_V2$midijob_lower_eur_month
    status_monthly_pay_upper <- FBC_LEGAL_2026_V2$midijob_upper_eur_month
    if(is.finite(regular_monthly_pay) &&
       (regular_monthly_pay < status_monthly_pay_lower-1e-8 ||
        regular_monthly_pay > status_monthly_pay_upper+1e-8)){
      issues <- c(issues,sprintf(
        "Midijob: regelmäßiges Monatsentgelt %.2f EUR liegt außerhalb des Übergangsbereichs %.2f-%.2f EUR.",
        regular_monthly_pay,status_monthly_pay_lower,status_monthly_pay_upper
      ))
      reclassification_required <- TRUE
    }
  }

  if(form=="werkstudent"){
    # The 20-hour rule is a status rule, not a wage ceiling.
    # Exceptions are never inferred; they must be explicitly confirmed.
    exception <- isTRUE(employee$werkstudent_exception_confirmed)
    if(!exception){
      max_hours_week <- min(max_hours_week,FBC_LEGAL_2026_V2$werkstudent_regular_hours_week)
      if(is.finite(hours_week) &&
         hours_week > FBC_LEGAL_2026_V2$werkstudent_regular_hours_week+1e-8){
        issues <- c(issues,sprintf(
          "Werkstudent: %.2f Std./Woche überschreiten die reguläre 20-Stunden-Grenze; keine bestätigte Ausnahme.",
          hours_week
        ))
        reclassification_required <- TRUE
      }
    } else {
      warnings <- c(warnings,
        "Werkstudent-Ausnahme ist bestätigt; Dauer/Abend-/Nacht-/Wochenend-/Semesterferienbedingungen müssen separat dokumentiert bleiben.")
    }
  }

  if(form=="kurzfristig"){
    months <- as.numeric(employee$employment_months_calendar_year %||% NA_real_)
    days <- as.numeric(employee$employment_workdays_calendar_year %||% NA_real_)
    agriculture <- isTRUE(employee$agriculture_short_term)

    if(agriculture){
      max_days <- FBC_LEGAL_2026_V2$short_term_agriculture_workdays
      max_months <- NA_real_
    } else {
      max_days <- FBC_LEGAL_2026_V2$short_term_workdays
      max_months <- FBC_LEGAL_2026_V2$short_term_months
    }

    if(!agriculture && is.finite(months) && months>max_months+1e-8){
      issues <- c(issues,sprintf(
        "Kurzfristige Beschäftigung: %.1f Monate überschreiten grundsätzlich %d Monate.",
        months,max_months
      ))
      reclassification_required <- TRUE
    }
    if(is.finite(days) && days>max_days+1e-8){
      issues <- c(issues,sprintf(
        "Kurzfristige Beschäftigung: %.0f Arbeitstage überschreiten die relevante Grenze von %d Tagen.",
        days,max_days
      ))
      reclassification_required <- TRUE
    }
    if(is.finite(regular_monthly_pay) &&
       regular_monthly_pay > FBC_LEGAL_2026_V2$minijob_regular_monthly_limit_eur &&
       !isTRUE(employee$berufsmaessigkeit_checked)){
      warnings <- c(warnings,
        "Kurzfristige Beschäftigung mit Entgelt über der Minijob-Grenze: Berufsmäßigkeit muss außerhalb des Optimizers geprüft/dokumentiert werden.")
    }
  }

  if(form %in% c("teilzeit","vollzeit")){
    if(!is.finite(contract_hours_week)){
      issues <- c(issues,
        sprintf("%s: vertragliche Wochenstunden fehlen; Kapazitätsgrenze nicht production-sicher.",
                form))
    }
  }

  # Generic ArbZG guard. This is deliberately a review gate, not an invented
  # optimization range. Confirmed statutory/tariff exceptions can override it.
  arbzg_exception <- isTRUE(employee$working_time_exception_confirmed)
  hours_day <- as.numeric(employee$hours_day %||% NA_real_)
  if(is.finite(hours_day) &&
     hours_day > FBC_LEGAL_2026_V2$arbzg_extended_hours_day+1e-8 &&
     !arbzg_exception){
    issues <- c(issues,sprintf(
      "Arbeitszeit %.2f Std./Tag überschreitet 10 Std.; keine bestätigte arbeitszeitrechtliche Ausnahme.",
      hours_day
    ))
  } else if(is.finite(hours_day) &&
            hours_day > FBC_LEGAL_2026_V2$arbzg_regular_hours_day+1e-8 &&
            !isTRUE(employee$arbzg_compensation_confirmed)){
    warnings <- c(warnings,
      "Arbeitszeit über 8 Std./Werktag: Ausgleich innerhalb des gesetzlichen Ausgleichszeitraums muss dokumentiert sein.")
  }

  list(
    valid=!length(issues),
    form=form,
    issues=issues,
    warnings=warnings,
    reclassification_required=reclassification_required,
    wage_hour=wage,
    regular_monthly_pay=regular_monthly_pay,
    status_monthly_pay_lower=status_monthly_pay_lower,
    status_monthly_pay_upper=status_monthly_pay_upper,
    max_hours_week=max_hours_week,
    max_paid_hours_month=max_paid_hours_month,
    employee_wage_is_free_lever=FALSE,
    employment_status_is_free_lever=FALSE
  )
}

fbc_default_cost_classification37v2 <- function(){
  data.frame(
    key=c("office","energy","vehicle","fuel","software","accounting",
          "business_insurance","material","marketing","other"),
    label=c("Raum / Büro","Energie / Strom","Fahrzeug","Kraftstoff",
            "Programme / Software","Steuerberater / Buchhaltung",
            "Betriebsversicherungen","Material / direkte Kosten",
            "Werbung / Akquise","Sonstiges"),
    controllable_default=FALSE,
    external_uncertainty=c(FALSE,TRUE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE),
    stringsAsFactors=FALSE
  )
}

fbc_build_cost_controls37v2 <- function(operating_costs,evidence=list()){
  cls <- fbc_default_cost_classification37v2()
  rows <- vector("list",nrow(cls))

  for(i in seq_len(nrow(cls))){
    key <- cls$key[i]
    current <- as.numeric(operating_costs[[key]] %||% NA_real_)
    if(!is.finite(current)) stop(sprintf("Missing operating cost: %s",key))

    ev <- evidence[[key]]
    if(is.null(ev)){
      rows[[i]] <- data.frame(
        key=key,label=cls$label[i],current=current,
        controllable=FALSE,min_value=current,max_value=current,
        source_type=NA_character_,
        reason="Kein bestätigter Änderungsbereich; Analyse ja, Optimierungshebel nein.",
        implementation_months=NA_real_,
        implementation_mode=NA_character_,
        stringsAsFactors=FALSE
      )
      next
    }

    fbc_assert_evidence37v2(ev$source_type,paste0("cost_",key))
    fbc_validate_implementation37v2(ev$implementation,paste0("cost_",key))
    lower <- as.numeric(ev$lower)
    upper <- as.numeric(ev$upper %||% current)

    if(!is.finite(lower) || !is.finite(upper) ||
       lower>current || upper<current || lower>upper){
      stop(sprintf("Invalid confirmed cost range for %s.",key))
    }

    rows[[i]] <- data.frame(
      key=key,label=cls$label[i],current=current,
      controllable=TRUE,min_value=lower,max_value=upper,
      source_type=ev$source_type,
      reason=as.character(ev$reason %||% "Bestätigter Änderungsbereich"),
      implementation_months=ev$implementation$months_to_full,
      implementation_mode=ev$implementation$mode,
      stringsAsFactors=FALSE
    )
  }
  do.call(rbind,rows)
}

fbc_make_bound37v2 <- function(name,current,lower,upper,source_type,reason,implementation){
  fbc_assert_evidence37v2(source_type,name)
  fbc_validate_implementation37v2(implementation,name)
  if(any(!is.finite(c(current,lower,upper))))
    stop(sprintf("%s: non-finite bound.",name))
  if(lower>current || upper<current || lower>upper)
    stop(sprintf("%s: invalid bounds.",name))

  data.frame(
    name=name,current=current,lower=lower,upper=upper,
    source_type=source_type,reason=reason,
    implementation_months=implementation$months_to_full,
    implementation_mode=implementation$mode,
    implementation_source=implementation$source_type,
    stringsAsFactors=FALSE
  )
}

fbc_build_production_rules37v2 <- function(state,confirmed=list(),cost_evidence=list()){
  emp <- fbc_resolve_employment_constraints37v2(state$employee %||% list())
  if(!emp$valid) stop(paste(emp$issues,collapse="\n"))

  rows <- list()

  if(!is.null(confirmed$owner_price)){
    ev <- confirmed$owner_price
    rows[[length(rows)+1L]] <- fbc_make_bound37v2(
      "owner_price",
      as.numeric(state$owner$price),
      as.numeric(ev$lower %||% state$owner$price),
      as.numeric(ev$upper),
      ev$source_type,
      ev$reason %||% "Bestätigter Preisrahmen",
      ev$implementation
    )
  }

  if(!is.null(confirmed$owner_hours)){
    ev <- confirmed$owner_hours
    rows[[length(rows)+1L]] <- fbc_make_bound37v2(
      "owner_hours",
      as.numeric(state$owner$billable_hours_month),
      as.numeric(ev$lower %||% state$owner$billable_hours_month),
      as.numeric(ev$upper),
      ev$source_type,
      ev$reason %||% "Bestätigte Inhaber-Kapazität",
      ev$implementation
    )
  }

  # Employee wage/status is never a continuous optimizer variable.
  if(isTRUE(state$employee$direct_billing)){
    if(!is.null(confirmed$employee_customer_price)){
      ev <- confirmed$employee_customer_price
      rows[[length(rows)+1L]] <- fbc_make_bound37v2(
        "employee_customer_price",
        as.numeric(state$employee$customer_price),
        as.numeric(ev$lower %||% state$employee$customer_price),
        as.numeric(ev$upper),
        ev$source_type,
        ev$reason %||% "Bestätigter Kundenpreisrahmen",
        ev$implementation
      )
    }

    if(!is.null(confirmed$employee_billable_hours)){
      ev <- confirmed$employee_billable_hours
      requested_upper <- as.numeric(ev$upper)

      paid_cap <- as.numeric(state$employee$paid_hours_month %||% Inf)
      if(is.finite(emp$max_paid_hours_month))
        paid_cap <- min(paid_cap,emp$max_paid_hours_month)

      weekly_cap_month <- if(is.finite(emp$max_hours_week))
        emp$max_hours_week*52/12 else Inf

      effective_upper <- min(requested_upper,paid_cap,weekly_cap_month)

      rows[[length(rows)+1L]] <- fbc_make_bound37v2(
        "employee_billable_hours",
        as.numeric(state$employee$billable_hours_month),
        as.numeric(ev$lower %||% state$employee$billable_hours_month),
        effective_upper,
        ev$source_type,
        paste0(ev$reason %||% "Bestätigte abrechenbare Kapazität",
               "; employment cap applied"),
        ev$implementation
      )
    }
  }

  costs <- fbc_build_cost_controls37v2(state$operating_costs,cost_evidence)
  active <- costs[costs$controllable,,drop=FALSE]
  if(nrow(active)){
    for(i in seq_len(nrow(active))){
      z <- active[i,]
      rows[[length(rows)+1L]] <- data.frame(
        name=paste0("cost_",z$key),
        current=z$current,lower=z$min_value,upper=z$max_value,
        source_type=z$source_type,reason=z$reason,
        implementation_months=z$implementation_months,
        implementation_mode=z$implementation_mode,
        implementation_source=z$source_type,
        stringsAsFactors=FALSE
      )
    }
  }

  registry <- if(length(rows)) do.call(rbind,rows) else data.frame()

  implementation_plan <- if(nrow(registry)) data.frame(
    lever=registry$name,
    months_to_full=registry$implementation_months,
    mode=registry$implementation_mode,
    stringsAsFactors=FALSE
  ) else data.frame(
    lever=character(0),months_to_full=numeric(0),mode=character(0)
  )

  list(
    registry=registry,
    reality_cost_controls=costs[,c("key","controllable","min_value","max_value","reason")],
    implementation_plan=implementation_plan,
    employment=emp,
    financing_in_decision_vector=FALSE,
    metadata=list(
      rule="No evidence -> no optimization lever",
      legal_2026=FBC_LEGAL_2026_V2
    )
  )
}

# Rebase cost-control bounds to the state actually entering Reality/KKT.
# This is required because the statistical expected_state may change
# analysis-only external costs (e.g. energy/fuel) relative to raw Cockpit inputs.
#
# For NON-controllable costs:
#   min=max=current expected value, so they remain fixed decision-wise.
# For controllable costs:
#   keep the evidenced min/max bounds, but validate that the current expected
#   value still lies inside them. No silent widening of evidenced bounds.
fbc_rebase_cost_controls37v2 <- function(cost_controls,state){
  cc <- cost_controls

  # Accept named vector/list state$operating_costs.
  oc <- state$operating_costs %||% NULL
  if(is.null(oc))
    stop("Expected state has no operating_costs; cannot rebase cost controls.")

  get_cost <- function(key){
    if(is.list(oc)) val <- oc[[key]]
    else {
      if(is.null(names(oc)) || !(key %in% names(oc))) return(NA_real_)
      val <- oc[[key]]
    }
    as.numeric(val)
  }

  for(i in seq_len(nrow(cc))){
    key <- as.character(cc$key[i])
    cur <- get_cost(key)
    if(!is.finite(cur))
      stop(sprintf("%s: expected-state cost missing/non-finite.",key))

    if(!isTRUE(cc$controllable[i])){
      cc$min_value[i] <- cur
      cc$max_value[i] <- cur
    } else {
      if(cur < cc$min_value[i]-1e-8 || cur > cc$max_value[i]+1e-8){
        stop(sprintf(
          paste0("%s: erwarteter aktueller Wert %.4f liegt außerhalb der ",
                 "bestätigten Produktionsgrenzen [%.4f, %.4f]. ",
                 "Evidence/Vertrag prüfen; Grenzen werden nicht automatisch erweitert."),
          key,cur,cc$min_value[i],cc$max_value[i]
        ))
      }
    }
  }
  cc
}

fbc_validate_production_rules37v2 <- function(x){
  stopifnot(is.list(x),identical(x$financing_in_decision_vector,FALSE))
  r <- x$registry
  if(nrow(r)){
    if(any(grepl("wage|salary|lohn",r$name,ignore.case=TRUE)))
      stop("Employee wage leaked into decision vector.")
    if(any(grepl("financ|credit|loan|zins|darlehen",r$name,ignore.case=TRUE)))
      stop("Financing leaked into decision vector.")
    if(any(!r$source_type %in% FBC_ALLOWED_EVIDENCE_V2))
      stop("Unvalidated evidence source in production registry.")
    if(any(grepl("TECHNICAL TEST|FIXTURE|PLACEHOLDER",r$reason,ignore.case=TRUE)))
      stop("Technical fixture leaked into production registry.")
  }
  invisible(TRUE)
}

cat("FBC Production Rules/Data Layer V2 loaded.\n")
