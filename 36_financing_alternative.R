# ============================================================
# 36_financing_alternative.R
# FUTURE Business Cockpit
# Financing / Darlehen as a SEPARATE alternative branch
# ============================================================
# Financing is NOT a lever of the operating KKT decision vector.
# This module compares explicit financing contracts without treating borrowed
# principal as profit/result.
# ============================================================

fbc_fin_rate <- function(x){
  x<-as.numeric(x)[1]
  if(!is.finite(x)||x<0) stop("Zinssatz muss >= 0 sein.")
  if(x>1) x<-x/100
  x
}

fbc_validate_contract <- function(x,name="Finanzierung"){
  req<-c("type","amount","rate_pa")
  miss<-setdiff(req,names(x))
  if(length(miss)) stop(paste(name,"fehlt:",paste(miss,collapse=", ")))
  if(!x$type %in% c("annuity","tilgung","endfaellig","credit_line","zinsfrei"))
    stop(paste(name,"hat unbekannten type."))
  if(!is.finite(as.numeric(x$amount)) || x$amount<0) stop(paste(name,"amount ungültig."))
  if(x$type!="credit_line"){
    if(is.null(x$months)||!is.finite(as.numeric(x$months))||x$months<=0)
      stop(paste(name,"months muss >0 sein."))
  }
  if(is.null(x$fees_month)) x$fees_month<-0
  if(is.null(x$one_time_fee)) x$one_time_fee<-0
  if(any(c(x$fees_month,x$one_time_fee)<0)) stop("Gebühren dürfen nicht negativ sein.")
  x$rate_pa<-fbc_fin_rate(x$rate_pa)
  x
}

fbc_financing_schedule <- function(contract,horizon_months=NULL){
  c<-fbc_validate_contract(contract)
  r<-c$rate_pa/12
  amount<-as.numeric(c$amount)
  fees<-as.numeric(c$fees_month)
  upfront<-as.numeric(c$one_time_fee)

  if(c$type=="credit_line"){
    if(is.null(horizon_months)||!is.finite(horizon_months)||horizon_months<1)
      stop("Für credit_line muss comparison horizon_months explizit gesetzt werden.")
    n<-as.integer(horizon_months)
  } else {
    n<-as.integer(c$months)
    if(!is.null(horizon_months)) n<-min(n,as.integer(horizon_months))
  }

  rest<-amount
  out<-vector("list",n)
  ann_rate<-NA_real_
  if(c$type=="annuity"){
    m<-as.integer(c$months)
    ann_rate<-if(r==0) amount/m else amount*r/(1-(1+r)^(-m))
  }
  fixed_principal<-if(c$type=="tilgung") amount/as.integer(c$months) else NA_real_

  for(t in seq_len(n)){
    interest<-rest*r
    principal<-0
    if(c$type=="annuity") principal<-min(max(0,ann_rate-interest),rest)
    if(c$type=="tilgung") principal<-min(fixed_principal,rest)
    if(c$type=="zinsfrei") principal<-min(amount/as.integer(c$months),rest)
    if(c$type=="endfaellig" && t==as.integer(c$months)) principal<-rest
    if(c$type=="credit_line") principal<-0
    if(c$type=="zinsfrei") interest<-0

    fee_t<-fees + if(t==1) upfront else 0
    cash_service<-interest+principal+fee_t
    expense<-interest+fee_t  # principal is NOT an expense
    rest2<-max(0,rest-principal)
    out[[t]]<-data.frame(
      month=t,restschuld_start=rest,interest=interest,principal=principal,
      fees=fee_t,financing_expense=expense,cash_service=cash_service,
      restschuld_end=rest2,stringsAsFactors=FALSE
    )
    rest<-rest2
  }
  do.call(rbind,out)
}

fbc_summarize_financing <- function(contract,comparison_horizon_months=NULL,
                                    free_cash_before_debt_service_month=NULL,
                                    min_debt_service_ratio=NULL){
  c<-fbc_validate_contract(contract)
  s<-fbc_financing_schedule(c,comparison_horizon_months)
  ratio<-NA_real_; policy_ok<-NA
  if(!is.null(free_cash_before_debt_service_month)){
    cash<-as.numeric(free_cash_before_debt_service_month)
    if(length(cash)==1) cash<-rep(cash,nrow(s))
    if(length(cash)<nrow(s)) stop("free_cash_before_debt_service_month deckt den Vergleichshorizont nicht ab.")
    ratios<-ifelse(s$cash_service>0,cash[seq_len(nrow(s))]/s$cash_service,Inf)
    ratio<-min(ratios)
    if(!is.null(min_debt_service_ratio)){
      q<-as.numeric(min_debt_service_ratio)
      if(!is.finite(q)||q<=0) stop("min_debt_service_ratio muss explizit >0 sein.")
      policy_ok<-ratio>=q
    }
  }
  list(
    contract=c,
    schedule=s,
    total_interest=sum(s$interest),
    total_fees=sum(s$fees),
    total_financing_expense=sum(s$financing_expense),
    total_principal_paid=sum(s$principal),
    total_cash_service=sum(s$cash_service),
    first_month_cash_service=s$cash_service[1],
    last_month_cash_service=tail(s$cash_service,1),
    max_month_cash_service=max(s$cash_service),
    restschuld_end=tail(s$restschuld_end,1),
    min_capital_service_ratio=ratio,
    capital_service_policy_ok=policy_ok
  )
}

# For a true refinancing comparison, principal amounts should normally match.
# If they do not, the module reports both cases but refuses automatic economic
# superiority because the contracts finance different amounts.
fbc_compare_financing_alternatives <- function(
    current,alternative,
    free_cash_before_debt_service_month=NULL,
    min_debt_service_ratio=NULL
){
  cur<-fbc_summarize_financing(
    current,NULL,free_cash_before_debt_service_month,min_debt_service_ratio
  )
  alt<-fbc_summarize_financing(
    alternative,NULL,free_cash_before_debt_service_month,min_debt_service_ratio
  )

  same_amount<-abs(cur$contract$amount-alt$contract$amount)<1e-8
  comparable<-same_amount
  expense_saving<-cur$total_financing_expense-alt$total_financing_expense
  cash_service_delta_first<-alt$first_month_cash_service-cur$first_month_cash_service

  alt_policy_ok<-if(is.null(min_debt_service_ratio)) NA else isTRUE(alt$capital_service_policy_ok)
  economically_better<-NA
  if(comparable){
    if(is.null(min_debt_service_ratio)){
      # Without an explicit debt-service policy, report cost difference but do
      # not issue a final product recommendation.
      economically_better<-NA
    } else {
      economically_better<-expense_saving>0 && alt_policy_ok
    }
  }

  list(
    comparable=comparable,
    reason=if(comparable) "same_financed_amount" else "different_financed_amounts_no_automatic_ranking",
    current=cur,
    alternative=alt,
    financing_expense_saving=expense_saving,
    first_month_cash_service_delta=cash_service_delta_first,
    alternative_economically_better=economically_better,
    note="Kreditbetrag ist kein Betriebsergebnis; Tilgung ist kein Aufwand, aber Liquiditätsabfluss."
  )
}

cat("\n36 Financing Alternative loaded.\n")
