# ==========================================
# 09_finanzierung_inputs_costs_FINAL.R
# FBC – Finanzierungskosten
# Clean final validation-ready version
# ==========================================
# Deployment package: machine-specific setwd removed; api.R owns working directory.
as_percent_rate <- function(x) {
  if (is.na(x)) return(NA_real_)
  if (x > 1) x <- x / 100
  if (x < 0) stop("Zinssatz darf nicht negativ sein.")
  x
}

validate_financing_inputs <- function(kreditbetrag, zinssatz_pa, laufzeit_monate = NA_real_) {
  if (!is.na(kreditbetrag) && kreditbetrag < 0) stop("Kreditbetrag darf nicht negativ sein.")
  if (!is.na(laufzeit_monate) && laufzeit_monate <= 0) stop("Laufzeit muss > 0 sein.")
  as_percent_rate(zinssatz_pa)
  invisible(TRUE)
}

calc_annuitaet <- function(kreditbetrag, zinssatz_pa, laufzeit_monate) {
  validate_financing_inputs(kreditbetrag, zinssatz_pa, laufzeit_monate)
  r <- as_percent_rate(zinssatz_pa) / 12
  rate <- if (r == 0) kreditbetrag / laufzeit_monate else
    kreditbetrag * r / (1 - (1 + r)^(-laufzeit_monate))
  zins1 <- kreditbetrag * r
  list(
    rate_monat = rate,
    zins_monat_1 = zins1,
    tilgung_monat_1 = rate - zins1,
    kapitaldienst_jahr = rate * 12
  )
}

build_annuitaetenplan <- function(kreditbetrag, zinssatz_pa, laufzeit_monate) {
  ann <- calc_annuitaet(kreditbetrag, zinssatz_pa, laufzeit_monate)
  r <- as_percent_rate(zinssatz_pa) / 12
  rest <- kreditbetrag
  out <- vector("list", laufzeit_monate)

  for (m in seq_len(laufzeit_monate)) {
    zins <- rest * r
    tilgung <- min(ann$rate_monat - zins, rest)
    rate <- zins + tilgung
    rest_neu <- max(0, rest - tilgung)

    out[[m]] <- data.frame(
      monat = m,
      restschuld_start = rest,
      zins = zins,
      tilgung = tilgung,
      rate = rate,
      restschuld_ende = rest_neu
    )
    rest <- rest_neu
  }
  do.call(rbind, out)
}

calc_tilgungsdarlehen <- function(kreditbetrag, zinssatz_pa, laufzeit_monate) {
  validate_financing_inputs(kreditbetrag, zinssatz_pa, laufzeit_monate)
  r <- as_percent_rate(zinssatz_pa) / 12
  tilgung <- kreditbetrag / laufzeit_monate
  zins1 <- kreditbetrag * r
  zins_last <- tilgung * r

  list(
    tilgung_monat = tilgung,
    zins_monat_1 = zins1,
    rate_monat_1 = tilgung + zins1,
    zins_monat_letzte = zins_last,
    rate_monat_letzte = tilgung + zins_last
  )
}

build_tilgungsplan <- function(kreditbetrag, zinssatz_pa, laufzeit_monate) {
  x <- calc_tilgungsdarlehen(kreditbetrag, zinssatz_pa, laufzeit_monate)
  r <- as_percent_rate(zinssatz_pa) / 12
  rest <- kreditbetrag
  out <- vector("list", laufzeit_monate)

  for (m in seq_len(laufzeit_monate)) {
    zins <- rest * r
    tilgung <- min(x$tilgung_monat, rest)
    rate <- zins + tilgung
    rest_neu <- max(0, rest - tilgung)

    out[[m]] <- data.frame(
      monat = m,
      restschuld_start = rest,
      zins = zins,
      tilgung = tilgung,
      rate = rate,
      restschuld_ende = rest_neu
    )
    rest <- rest_neu
  }
  do.call(rbind, out)
}

calc_endfaellig <- function(kreditbetrag, zinssatz_pa, laufzeit_monate) {
  validate_financing_inputs(kreditbetrag, zinssatz_pa, laufzeit_monate)
  zins_monat <- kreditbetrag * as_percent_rate(zinssatz_pa) / 12
  list(
    zins_monat = zins_monat,
    laufender_kapitaldienst_monat = zins_monat,
    rueckzahlung_am_ende = kreditbetrag,
    kapitaldienst_jahr_ohne_endtilgung = zins_monat * 12
  )
}

calc_kreditlinie <- function(kreditlinie_nutzung, zinssatz_pa,
                             sonstige_finanzierungskosten_monat = 0) {
  if (is.na(kreditlinie_nutzung) || kreditlinie_nutzung < 0)
    stop("Genutzter Kreditlinienbetrag muss >= 0 sein.")
  r <- as_percent_rate(zinssatz_pa)
  zins_monat <- kreditlinie_nutzung * r / 12
  gesamt <- zins_monat + sonstige_finanzierungskosten_monat
  list(
    zins_monat = zins_monat,
    sonstige_finanzierungskosten_monat = sonstige_finanzierungskosten_monat,
    finanzierungskosten_monat = gesamt,
    finanzierungskosten_jahr = gesamt * 12
  )
}

calc_finanzierungskosten <- function(
    finanzierungsart,
    kreditbetrag = NA_real_,
    zinssatz_pa = NA_real_,
    laufzeit_monate = NA_real_,
    kreditlinie_nutzung = NA_real_,
    sonstige_finanzierungskosten_monat = 0
) {
  allowed <- c("annuitaet","tilgung","endfaellig","kreditlinie","zinsfrei")
  if (!finanzierungsart %in% allowed)
    stop(paste("Unbekannte Finanzierungsart:", finanzierungsart))

  if (is.na(sonstige_finanzierungskosten_monat)) sonstige_finanzierungskosten_monat <- 0
  if (sonstige_finanzierungskosten_monat < 0) stop("Sonstige Finanzierungskosten dürfen nicht negativ sein.")

  if (finanzierungsart == "annuitaet") {
    x <- calc_annuitaet(kreditbetrag, zinssatz_pa, laufzeit_monate)
    return(list(
      finanzierungsart = finanzierungsart,
      rate_monat = x$rate_monat,
      zinsanteil_monat = x$zins_monat_1,
      tilgungsanteil_monat = x$tilgung_monat_1,
      sonstige_finanzierungskosten_monat = sonstige_finanzierungskosten_monat,
      zahlungsbelastung_monat = x$rate_monat + sonstige_finanzierungskosten_monat,
      kapitaldienst_jahr = x$kapitaldienst_jahr + sonstige_finanzierungskosten_monat * 12
    ))
  }

  if (finanzierungsart == "tilgung") {
    x <- calc_tilgungsdarlehen(kreditbetrag, zinssatz_pa, laufzeit_monate)
    plan <- build_tilgungsplan(kreditbetrag, zinssatz_pa, laufzeit_monate)
    n12 <- min(12, nrow(plan))
    return(list(
      finanzierungsart = finanzierungsart,
      rate_monat = x$rate_monat_1,
      rate_monat_start = x$rate_monat_1,
      rate_monat_ende = x$rate_monat_letzte,
      zinsanteil_monat = x$zins_monat_1,
      tilgungsanteil_monat = x$tilgung_monat,
      sonstige_finanzierungskosten_monat = sonstige_finanzierungskosten_monat,
      zahlungsbelastung_monat = x$rate_monat_1 + sonstige_finanzierungskosten_monat,
      kapitaldienst_jahr = sum(head(plan$rate, n12)) +
        sonstige_finanzierungskosten_monat * n12
    ))
  }

  if (finanzierungsart == "endfaellig") {
    x <- calc_endfaellig(kreditbetrag, zinssatz_pa, laufzeit_monate)
    return(list(
      finanzierungsart = finanzierungsart,
      rate_monat = x$laufender_kapitaldienst_monat,
      zinsanteil_monat = x$zins_monat,
      tilgungsanteil_monat = 0,
      sonstige_finanzierungskosten_monat = sonstige_finanzierungskosten_monat,
      zahlungsbelastung_monat = x$laufender_kapitaldienst_monat +
        sonstige_finanzierungskosten_monat,
      kapitaldienst_jahr = x$kapitaldienst_jahr_ohne_endtilgung +
        sonstige_finanzierungskosten_monat * 12,
      rueckzahlung_am_ende = x$rueckzahlung_am_ende
    ))
  }

  if (finanzierungsart == "kreditlinie") {
    x <- calc_kreditlinie(kreditlinie_nutzung, zinssatz_pa,
                          sonstige_finanzierungskosten_monat)
    return(list(
      finanzierungsart = finanzierungsart,
      rate_monat = x$finanzierungskosten_monat,
      zinsanteil_monat = x$zins_monat,
      tilgungsanteil_monat = NA_real_,
      sonstige_finanzierungskosten_monat = x$sonstige_finanzierungskosten_monat,
      zahlungsbelastung_monat = x$finanzierungskosten_monat,
      kapitaldienst_jahr = x$finanzierungskosten_jahr
    ))
  }

  if (finanzierungsart == "zinsfrei") {
    if (is.na(kreditbetrag) || is.na(laufzeit_monate))
      stop("Für zinsfreie Finanzierung Kreditbetrag und Laufzeit angeben.")
    rate_monat <- kreditbetrag / laufzeit_monate
    return(list(
      finanzierungsart = finanzierungsart,
      rate_monat = rate_monat,
      zinsanteil_monat = 0,
      tilgungsanteil_monat = rate_monat,
      sonstige_finanzierungskosten_monat = sonstige_finanzierungskosten_monat,
      zahlungsbelastung_monat = rate_monat + sonstige_finanzierungskosten_monat,
      kapitaldienst_jahr = (rate_monat + sonstige_finanzierungskosten_monat) * 12
    ))
  }
}

message("09 FINAL geladen. build_tilgungsplan vorhanden: ", exists("build_tilgungsplan"))
