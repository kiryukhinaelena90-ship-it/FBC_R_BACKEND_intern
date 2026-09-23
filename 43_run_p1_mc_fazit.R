# ============================================================
# 43_run_p1_mc_fazit.R
# FUTURE Business Cockpit
# Orchestration only:
#   40 deterministic P1 -> 41 post-P1 Monte Carlo -> 42 FBC Fazit
# ============================================================

fbc_source43 <- function(file, envir){
  if(!file.exists(file)) stop("Required FBC runner missing: ", file)
  source(file, local=envir)
}

fbc_run_p1_mc_fazit_43 <- function(
    cfg,
    root=getwd(),
    mc_file=file.path(root,"data/processed/fbc_monte_carlo_draws.csv"),
    cost_factor_map=NULL,
    labor_pct_column="arbeitskosten_aenderung_prozent",
    market_reference_rate=NULL
){
  env <- environment()

  fbc_source43(file.path(root,"40_run_deterministic_p1.R"), env)
  fbc_source43(file.path(root,"41_post_p1_monte_carlo.R"), env)
  fbc_source43(file.path(root,"42_fbc_fazit.R"), env)

  # Core result. Technical failure here is still a real backend error.
  payload <- fbc_run_deterministic_p1_40(cfg=cfg, root=root)

  # Optional robustness enrichment. Function 41 is fail-soft by contract.
  payload <- fbc_run_post_p1_mc_41(
    payload=payload,
    cfg=cfg,
    root=root,
    mc_file=mc_file,
    cost_factor_map=cost_factor_map,
    labor_pct_column=labor_pct_column,
    market_reference_rate=market_reference_rate
  )

  # Human-readable conclusion from whatever layers actually executed.
  payload <- fbc_attach_fazit42(payload)

  payload
}

cat("\n43 P1 + Monte Carlo + Fazit runner loaded.\n")
