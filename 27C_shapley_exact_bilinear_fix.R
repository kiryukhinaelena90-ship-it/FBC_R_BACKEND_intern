# ==========================================
# 27C_shapley_exact_bilinear_fix.R
# FUTURE Business Cockpit
# Exact Shapley decomposition by coalition evaluation
#
# Compatibility note:
# The filename/function name are retained so existing production callers do
# not need to change. The previous bilinear-only analytic checks were removed.
# Shapley is now evaluated against problem$evaluate(), so it remains valid for
# nonlinear revenue, including the constant price-elasticity model.
# ==========================================

all_subsets27C <- function(items) {
  n <- length(items)
  out <- vector("list", 2^n)
  for (mask in 0:(2^n - 1)) {
    bits <- as.logical(intToBits(mask)[seq_len(n)])
    out[[mask + 1]] <- items[bits]
  }
  out
}

shapley_exact27C <- function(problem, solution, tol_change=1e-8, max_exact=10) {
  reg <- problem$registry
  base <- reg$current
  names(base) <- reg$name

  sol <- as.numeric(solution)
  if(is.null(names(solution)) || any(!reg$name %in% names(solution))){
    names(sol) <- reg$name
  } else {
    names(sol) <- names(solution)
    sol <- sol[reg$name]
  }

  changed <- reg$name[abs(sol-base) > tol_change]
  if (!length(changed)) {
    out <- data.frame(
      variable=character(),
      contribution=numeric(),
      share_of_improvement=numeric(),
      stringsAsFactors=FALSE
    )
    attr(out,"base_net") <- as.numeric(problem$evaluate(base))
    attr(out,"solution_net") <- as.numeric(problem$evaluate(base))
    attr(out,"total_improvement") <- 0
    return(out)
  }

  if (length(changed) > max_exact)
    stop("Zu viele geänderte Variablen für exaktes Shapley.")

  v <- function(S) {
    x <- base
    if (length(S)) x[S] <- sol[S]
    as.numeric(problem$evaluate(x))
  }

  n <- length(changed)
  phi <- setNames(rep(0,n),changed)

  for (i in changed) {
    others <- setdiff(changed,i)
    for (S in all_subsets27C(others)) {
      s <- length(S)
      w <- factorial(s) * factorial(n-s-1) / factorial(n)
      phi[i] <- phi[i] + w * (v(c(S,i)) - v(S))
    }
  }

  total <- v(changed)-v(character(0))
  share <- if(is.finite(total) && abs(total) > tol_change){
    as.numeric(phi)/total
  } else {
    rep(NA_real_,length(phi))
  }

  out <- data.frame(
    variable=names(phi),
    contribution=as.numeric(phi),
    share_of_improvement=share,
    stringsAsFactors=FALSE
  )
  out <- out[order(-abs(out$contribution)),,drop=FALSE]
  rownames(out)<-NULL

  attr(out,"base_net") <- v(character(0))
  attr(out,"solution_net") <- v(changed)
  attr(out,"total_improvement") <- total
  attr(out,"method") <- "exact coalition evaluation on nonlinear problem$evaluate"
  out
}

# Compatibility stubs retained for any diagnostic code that still calls the old
# bilinear helpers. No analytic P*H check is valid once price elasticity is used.
bilinear_pair_check27C <- function(problem, solution, p_name, h_name) {
  NULL
}

compare_pair_to_exact27C <- function(exact, pair) {
  NULL
}

human_explanation27C <- function(sh) {
  if(!nrow(sh)) return("Keine Anpassung erforderlich.")
  top <- sh[1:min(4,nrow(sh)),,drop=FALSE]

  lab <- function(x){
    map <- c(
      owner_price="die Preisanpassung des Inhabers",
      owner_hours="die Veränderung der geplanten abrechenbaren Inhaberstunden",
      employee_customer_price="die Anpassung des Mitarbeiter-Kundenpreises",
      employee_billable_hours="die Veränderung der geplanten abrechenbaren Mitarbeiterstunden"
    )
    if(x %in% names(map)) return(unname(map[[x]]))
    if(grepl("^cost_",x)) return(paste0("die Anpassung von ",sub("^cost_","",x)))
    x
  }

  paste0(
    "Die Zielerreichung entsteht aus einer Kombination mehrerer Hebel. ",
    "Den größten Einzelbeitrag liefert ", lab(top$variable[1]),
    ". Die Beiträge berücksichtigen Wechselwirkungen über die vollständige ",
    "nichtlineare Bewertungsfunktion."
  )
}

cat("\n27C Exact nonlinear Shapley loaded.\n")
