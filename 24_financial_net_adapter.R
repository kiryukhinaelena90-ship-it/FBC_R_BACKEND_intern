# ============================================================
# 24_financial_net_adapter.R
# FUTURE Business Cockpit — 2026 financial orientation adapter
#
# This deployment copy implements the same 2026 tax orientation
# boundaries/formula used by the current Cockpit and P0 regression.
# It is an orientation model, not an individual tax assessment.
# ============================================================

`%||%` <- function(a,b) if(is.null(a) || length(a)==0L) b else a

fbc_income_tax_2026_24 <- function(annual_profit){
  x <- floor(max(0, as.numeric(annual_profit)))
  tax <- 0
  if(x <= 12348){
    tax <- 0
  } else if(x <= 17799){
    y <- (x - 12348) / 10000
    tax <- (914.51*y + 1400)*y
  } else if(x <= 69878){
    z <- (x - 17799) / 10000
    tax <- (173.10*z + 2397)*z + 1034.87
  } else if(x <= 277825){
    tax <- .42*x - 11135.63
  } else {
    tax <- .45*x - 19470.38
  }
  max(0, floor(tax))
}

fbc_tax_estimate_2026_24 <- function(annual_profit, legal_form="freelance", trade_tax_rate=0){
  profit <- max(0, as.numeric(annual_profit))
  income_before <- fbc_income_tax_2026_24(profit)
  trade <- 0
  credit <- 0
  measure <- 0

  if(identical(legal_form, "sole_trade")){
    rounded <- floor(profit/100)*100
    taxable <- max(0, rounded - 24500)
    measure <- taxable * .035
    trade <- measure * (as.numeric(trade_tax_rate)/100)
    credit <- min(income_before, trade, measure*4)
  }

  income <- max(0, income_before-credit)
  list(
    profit=profit,
    income_before=income_before,
    income=income,
    trade=trade,
    credit=credit,
    total=income+trade
  )
}

fbc_pension_month_24 <- function(result_before_owner_protection_tax, owner){
  mode <- as.character(owner$pension_mode %||% "none")
  if(identical(mode,"statutory")) return(max(0,result_before_owner_protection_tax)*.186)
  if(identical(mode,"standard")) return(735.63)
  if(identical(mode,"fixed")) return(max(0,as.numeric(owner$pension_fixed_month %||% 0)))
  0
}

fbc_financial_point24 <- function(result_before_owner_protection_tax, owner,
                                  legal_form="freelance", trade_tax_rate=0){
  gross <- as.numeric(result_before_owner_protection_tax)
  tax <- fbc_tax_estimate_2026_24(gross*12, legal_form, trade_tax_rate)
  pension <- fbc_pension_month_24(gross, owner)
  insurance <- max(0, as.numeric(owner$insurance_month %||% 0))

  list(
    result_before_owner_protection_tax=gross,
    tax_month=tax$total/12,
    tax=tax,
    pension_month=pension,
    insurance_month=insurance,
    net_available=gross-insurance-pension-tax$total/12,
    disclosure_de="Steuer ist eine Orientierung; tatsächliche Steuer kann durch persönliche Sonderausgaben, weitere Einkünfte und individuelle Abzüge abweichen.",
    disclosure_ru="Налог является ориентировочным; фактический налог может отличаться из-за личных вычетов, других доходов и индивидуальных обстоятельств."
  )
}

fbc_make_financial_adapter24 <- function(legal_form="freelance", trade_tax_rate=0){
  force(legal_form); force(trade_tax_rate)
  function(result_before_owner_protection_tax, state){
    fbc_financial_point24(
      result_before_owner_protection_tax,
      state$owner,
      legal_form=legal_form,
      trade_tax_rate=trade_tax_rate
    )
  }
}
