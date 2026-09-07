# ==========================================
# 27C_shapley_exact_bilinear_fix.R
# FUTURE Business Cockpit
# Correct Shapley decomposition for bilinear revenue terms
# ==========================================
#
# Problem found in previous 27A:
# contribution pairs became exactly equal, which is not generally correct.
#
# For a bilinear term P*H:
#   total change = H0*dP + P0*dH + dP*dH
#
# Exact two-player Shapley:
#   phi_P = H0*dP + 0.5*dP*dH
#   phi_H = P0*dH + 0.5*dP*dH
#
# This module computes exact Shapley by coalition evaluation and additionally
# checks the bilinear pairs analytically.
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
  sol <- solution
  names(sol) <- reg$name

  changed <- reg$name[abs(sol-base) > tol_change]
  if (!length(changed)) {
    return(data.frame(
      variable=character(),
      contribution=numeric(),
      stringsAsFactors=FALSE
    ))
  }
  if (length(changed) > max_exact) stop("Zu viele geänderte Variablen für exaktes Shapley.")

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

  out <- data.frame(
    variable=names(phi),
    contribution=as.numeric(phi),
    share_of_improvement=as.numeric(phi)/total,
    stringsAsFactors=FALSE
  )
  out <- out[order(-abs(out$contribution)),,drop=FALSE]
  rownames(out)<-NULL

  attr(out,"base_net") <- v(character(0))
  attr(out,"solution_net") <- v(changed)
  attr(out,"total_improvement") <- total
  out
}

bilinear_pair_check27C <- function(problem, solution, p_name, h_name) {
  reg <- problem$registry
  base <- reg$current
  names(base)<-reg$name
  sol <- solution
  names(sol)<-reg$name

  if (!(p_name %in% reg$name && h_name %in% reg$name)) {
    return(NULL)
  }

  p0 <- base[p_name]; h0 <- base[h_name]
  dp <- sol[p_name]-p0; dh <- sol[h_name]-h0

  analytic_p <- h0*dp + 0.5*dp*dh
  analytic_h <- p0*dh + 0.5*dp*dh

  data.frame(
    variable=c(p_name,h_name),
    analytic_shapley=c(analytic_p,analytic_h),
    stringsAsFactors=FALSE
  )
}

compare_pair_to_exact27C <- function(exact, pair) {
  if (is.null(pair)) return(NULL)
  m <- merge(pair, exact[,c("variable","contribution")], by="variable", all.x=TRUE)
  m$difference <- m$contribution-m$analytic_shapley
  m
}

human_explanation27C <- function(sh) {
  if(!nrow(sh)) return("Keine Anpassung erforderlich.")
  top <- sh[1:min(4,nrow(sh)),,drop=FALSE]

  lab <- function(x){
    map <- c(
      owner_price="die Preisanpassung des Inhabers",
      owner_hours="die Veränderung der abrechenbaren Inhaberstunden",
      employee_customer_price="die Anpassung des Mitarbeiter-Kundenpreises",
      employee_billable_hours="die Veränderung der abrechenbaren Mitarbeiterstunden"
    )
    if(x %in% names(map)) return(unname(map[[x]]))
    if(grepl("^cost_",x)) return(paste0("die Anpassung von ",sub("^cost_","",x)))
    x
  }

  paste0(
    "Die Zielerreichung entsteht aus einer Kombination mehrerer Hebel. ",
    "Den größten Einzelbeitrag liefert ", lab(top$variable[1]),
    "; danach folgen ", paste(vapply(top$variable[-1],lab,character(1)),collapse=", "), "."
  )
}

cat("\n27C Corrected exact Shapley loaded.\n")
