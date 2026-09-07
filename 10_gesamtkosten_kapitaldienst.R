# ==========================================
# 10_gesamtkosten_kapitaldienst.R
# FBC – Gesamtkosten & Kapitaldienstfähigkeit
# PRE-SCENARIO compatible
# ==========================================
#
# Zweck:
# Betriebskosten + Personal + Finanzierung sauber
# zusammenführen, OHNE Aufwand und Liquidität zu vermischen.
#
# Zwei Perspektiven:
#
# A) Ergebnisrechnung
#    Betriebskosten
#  + Personalkosten
#  + Finanzierungsaufwand (Zinsen + Gebühren)
#
# B) Liquidität / Kapitaldienst
#    Betriebskosten
#  + Personalkosten
#  + gesamte Kreditbelastung
#    (Zinsen + Tilgung + Gebühren)
#
# Dieser Block berechnet NOCH NICHT den Business-Break-even.
# Er erzeugt die saubere Kostenbasis dafür.
# ==========================================




# ------------------------------------------
# 1. Sichere Summenfunktion
# ------------------------------------------

sum_required <- function(x, name = "Werte") {

  if (length(x) == 0) {
    return(0)
  }

  if (any(is.na(x))) {
    stop(
      paste0(
        name,
        " enthält NA. Fehlende Werte zuerst klären."
      )
    )
  }

  if (any(x < 0)) {
    stop(
      paste0(
        name,
        " enthält negative Werte."
      )
    )
  }

  sum(x)
}


# ------------------------------------------
# 2. Betriebskosten übernehmen
# ------------------------------------------
#
# Erwartet entweder:
# - einen numerischen Vektor der 8 Cockpit-Kategorien
# oder
# - bereits eine Gesamtsumme.
#
# Die 8 Kategorien bleiben:
# Raum / Büro
# Fahrzeug
# Programme / Software
# Steuerberater / Buchhaltung
# Betriebsversicherungen
# Material / direkte Kosten
# Werbung / Akquise
# Sonstiges

calc_betriebskosten_summe <- function(
    betriebskosten
) {

  if (length(betriebskosten) == 1) {

    if (
      is.na(betriebskosten) ||
      betriebskosten < 0
    ) {
      stop(
        "Betriebskosten müssen >= 0 und vollständig sein."
      )
    }

    return(as.numeric(betriebskosten))
  }

  sum_required(
    betriebskosten,
    "Betriebskosten"
  )
}


# ------------------------------------------
# 3. Personalkosten übernehmen
# ------------------------------------------
#
# Unterstützt:
# - 0 Mitarbeiter
# - einen numerischen Gesamtbetrag
# - mehrere Mitarbeiterbeträge
# - Listen aus calc_personalkosten_v2()

calc_personalkosten_summe <- function(
    personal = NULL
) {

  if (is.null(personal)) {
    return(0)
  }

  if (is.numeric(personal)) {
    return(
      sum_required(
        personal,
        "Personalkosten"
      )
    )
  }

  if (is.list(personal)) {

    # einzelnes calc_personalkosten_v2()-Resultat
    if (
      !is.null(
        personal$personalkosten_gesamt
      )
    ) {
      return(
        as.numeric(
          personal$personalkosten_gesamt
        )
      )
    }

    # Liste mehrerer Mitarbeiter-Resultate
    vals <- vapply(
      personal,
      function(x) {

        if (
          is.null(x$personalkosten_gesamt)
        ) {
          stop(
            "Mindestens ein Personal-Resultat enthält keine personalkosten_gesamt."
          )
        }

        as.numeric(
          x$personalkosten_gesamt
        )
      },
      numeric(1)
    )

    return(
      sum_required(
        vals,
        "Personalkosten"
      )
    )
  }

  stop(
    "personal muss NULL, numerisch oder ein Ergebnis aus calc_personalkosten_v2() sein."
  )
}


# ------------------------------------------
# 4. Finanzierung auf zwei Ebenen trennen
# ------------------------------------------
#
# Ergebnisrechnung:
# Nur ZINSEN + sonstige Finanzierungskosten.
#
# Liquidität:
# gesamte zahlungsbelastung_monat
# = Zinsen + Tilgung + Gebühren
#
# Bei Kreditlinie ist Tilgung nicht automatisch enthalten;
# dort wird die laufende Zins-/Gebührenbelastung übernommen.

calc_finanzierung_split <- function(
    finanzierung = NULL
) {

  if (is.null(finanzierung)) {
    return(
      list(
        finanzierungsaufwand_monat = 0,
        kapitaldienst_monat = 0,
        zins_monat = 0,
        tilgung_monat = 0,
        gebuehren_monat = 0
      )
    )
  }

  required <- c(
    "zinsanteil_monat",
    "sonstige_finanzierungskosten_monat",
    "zahlungsbelastung_monat"
  )

  missing <- required[
    !required %in% names(finanzierung)
  ]

  if (length(missing) > 0) {
    stop(
      paste0(
        "Finanzierungs-Resultat unvollständig. Es fehlen: ",
        paste(missing, collapse = ", ")
      )
    )
  }

  zins_monat <-
    finanzierung$zinsanteil_monat

  gebuehren_monat <-
    finanzierung$sonstige_finanzierungskosten_monat

  kapitaldienst_monat <-
    finanzierung$zahlungsbelastung_monat

  tilgung_monat <- if (
    "tilgungsanteil_monat" %in%
      names(finanzierung) &&
    !is.na(
      finanzierung$tilgungsanteil_monat
    )
  ) {
    finanzierung$tilgungsanteil_monat
  } else {
    0
  }

  vals <- c(
    zins_monat,
    gebuehren_monat,
    kapitaldienst_monat,
    tilgung_monat
  )

  if (any(is.na(vals))) {
    stop(
      "Finanzierungs-Resultat enthält NA."
    )
  }

  if (any(vals < 0)) {
    stop(
      "Finanzierungswerte dürfen nicht negativ sein."
    )
  }

  list(
    finanzierungsaufwand_monat =
      zins_monat +
      gebuehren_monat,

    kapitaldienst_monat =
      kapitaldienst_monat,

    zins_monat =
      zins_monat,

    tilgung_monat =
      tilgung_monat,

    gebuehren_monat =
      gebuehren_monat
  )
}


# ------------------------------------------
# 5. Gesamtkosten-Modell
# ------------------------------------------

calc_gesamtkosten_kapitaldienst <- function(
    betriebskosten,
    personal = NULL,
    finanzierung = NULL
) {

  betriebskosten_monat <-
    calc_betriebskosten_summe(
      betriebskosten
    )

  personalkosten_monat <-
    calc_personalkosten_summe(
      personal
    )

  fin <-
    calc_finanzierung_split(
      finanzierung
    )

  # ----------------------------------------
  # Ergebnisrechnung
  # ----------------------------------------

  kosten_ergebnis_monat <-
    betriebskosten_monat +
    personalkosten_monat +
    fin$finanzierungsaufwand_monat

  # ----------------------------------------
  # Liquidität
  # ----------------------------------------

  liquiditaetsbedarf_monat <-
    betriebskosten_monat +
    personalkosten_monat +
    fin$kapitaldienst_monat

  list(

    # Basis
    betriebskosten_monat =
      betriebskosten_monat,

    personalkosten_monat =
      personalkosten_monat,

    # Finanzierung getrennt
    zinsen_monat =
      fin$zins_monat,

    tilgung_monat =
      fin$tilgung_monat,

    finanzierungsgebuehren_monat =
      fin$gebuehren_monat,

    finanzierungsaufwand_monat =
      fin$finanzierungsaufwand_monat,

    kapitaldienst_monat =
      fin$kapitaldienst_monat,

    # Ergebnisrechnung
    kosten_ergebnis_monat =
      kosten_ergebnis_monat,

    kosten_ergebnis_jahr =
      kosten_ergebnis_monat * 12,

    # Liquidität
    liquiditaetsbedarf_monat =
      liquiditaetsbedarf_monat,

    liquiditaetsbedarf_jahr =
      liquiditaetsbedarf_monat * 12
  )
}


# ------------------------------------------
# 6. Kapitaldienstfähigkeit
# ------------------------------------------
#
# freie_mittel_vor_kapitaldienst:
# Betrag, der nach laufenden betrieblichen Kosten
# und Personalkosten zur Bedienung des Kredits
# verfügbar ist.
#
# DSCR-ähnliche Orientierung:
#
# freie Mittel / Kapitaldienst
#
# > 1  = Kapitaldienst rechnerisch gedeckt
# = 1  = genau gedeckt
# < 1  = nicht vollständig gedeckt
#
# FBC bezeichnet das später als
# "Kapitaldienstfähigkeit" und NICHT als
# bankaufsichtliche Kreditprüfung.

calc_kapitaldienstfaehigkeit <- function(
    umsatz_monat,
    betriebskosten,
    personal = NULL,
    finanzierung = NULL
) {

  if (
    is.na(umsatz_monat) ||
    umsatz_monat < 0
  ) {
    stop(
      "Umsatz muss >= 0 sein."
    )
  }

  basis <-
    calc_gesamtkosten_kapitaldienst(
      betriebskosten =
        betriebskosten,
      personal =
        personal,
      finanzierung =
        finanzierung
    )

  freie_mittel_vor_kapitaldienst <-
    umsatz_monat -
    basis$betriebskosten_monat -
    basis$personalkosten_monat

  if (
    basis$kapitaldienst_monat == 0
  ) {

    kapitaldienstquote <- Inf
    kapitaldienst_gedeckt <- TRUE

  } else {

    kapitaldienstquote <-
      freie_mittel_vor_kapitaldienst /
      basis$kapitaldienst_monat

    kapitaldienst_gedeckt <-
      kapitaldienstquote >= 1
  }

  list(
    umsatz_monat =
      umsatz_monat,

    freie_mittel_vor_kapitaldienst =
      freie_mittel_vor_kapitaldienst,

    kapitaldienst_monat =
      basis$kapitaldienst_monat,

    kapitaldienstquote =
      kapitaldienstquote,

    kapitaldienst_gedeckt =
      kapitaldienst_gedeckt,

    liquiditaet_nach_kapitaldienst =
      freie_mittel_vor_kapitaldienst -
      basis$kapitaldienst_monat
  )
}


# ------------------------------------------
# 7. Kompakte Ausgabe für späteres Cockpit
# ------------------------------------------

summarise_gesamtkosten <- function(
    result
) {

  if (is.null(result)) {
    return(NULL)
  }

  data.frame(
    Kennzahl = c(
      "Betriebskosten / Monat",
      "Personalkosten / Monat",
      "Finanzierungsaufwand / Monat",
      "davon Zinsen",
      "davon Tilgung",
      "Kapitaldienst / Monat",
      "Kosten Ergebnisrechnung / Monat",
      "Liquiditätsbedarf / Monat"
    ),

    Wert = c(
      result$betriebskosten_monat,
      result$personalkosten_monat,
      result$finanzierungsaufwand_monat,
      result$zinsen_monat,
      result$tilgung_monat,
      result$kapitaldienst_monat,
      result$kosten_ergebnis_monat,
      result$liquiditaetsbedarf_monat
    ),

    Einheit = rep(
      "EUR",
      8
    ),

    stringsAsFactors = FALSE
  )
}


# ------------------------------------------
# 8. Architekturregel für nächsten Schritt
# ------------------------------------------
#
# Dieser Block liefert die Basis für:
#
# 10  Gesamtkosten / Kapitaldienst
#        ↓
# 11  Business-Break-even
#        ↓
# 12  Scenario Integration
#        ↓
#     Chance / Basis / Risiko
#        ↓
#     Sensitivität + Monte Carlo
#        ↓
#     Decision Layer
#
# WICHTIG:
#
# Business-Break-even wird später auf der
# Ergebnis-/Deckungsbeitragslogik aufgebaut.
#
# Kapitaldienstfähigkeit bleibt zusätzlich als
# Liquiditätsprüfung bestehen.
#
# So vermeiden wir den Fehler:
# Tilgung als Aufwand in die Gewinnrechnung zu stecken.
#
# ==========================================
