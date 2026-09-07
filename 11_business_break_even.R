# ==========================================
# 11_business_break_even.R
# FBC – Business Break-even
# PRE-SCENARIO compatible
# ==========================================
#
# Zweck:
# Gesamt-Break-even des Betriebs berechnen.
#
# Grundlage:
# - Betriebskosten
# - Personalkosten
# - Finanzierungsaufwand (Zinsen + Gebühren)
# - variable Kosten
# - realisierter Kundenpreis
# - verfügbare / abrechenbare Kapazität
#
# WICHTIG:
# - Tilgung gehört NICHT in den Gewinn-Break-even.
# - Tilgung bleibt separat in der Kapitaldienstfähigkeit.
# - Mitarbeiter-Break-even aus 08c bleibt lokale Diagnostik.
# - Dieser Block ist der zentrale Business-Break-even.
#
# ==========================================

setwd("~/Documents/FBC_Statistics")

source("10_gesamtkosten_kapitaldienst.R")


# ------------------------------------------
# 1. Deckungsbeitrag je Einheit / Stunde
# ------------------------------------------

calc_db_business <- function(
    preis_pro_einheit,
    variable_kosten_pro_einheit = 0
) {

  if (
    is.na(preis_pro_einheit) ||
    is.na(variable_kosten_pro_einheit)
  ) {
    return(NA_real_)
  }

  if (
    preis_pro_einheit < 0 ||
    variable_kosten_pro_einheit < 0
  ) {
    stop(
      "Preis und variable Kosten dürfen nicht negativ sein."
    )
  }

  preis_pro_einheit -
    variable_kosten_pro_einheit
}


# ------------------------------------------
# 2. Fixkostenbasis für Business Break-even
# ------------------------------------------
#
# Ergebnisrechnung:
# Betriebskosten
# + Personalkosten
# + Zinsen/Gebühren
#
# KEINE Tilgung.

calc_break_even_fixkosten <- function(
    betriebskosten,
    personal = NULL,
    finanzierung = NULL
) {

  basis <- calc_gesamtkosten_kapitaldienst(
    betriebskosten =
      betriebskosten,
    personal =
      personal,
    finanzierung =
      finanzierung
  )

  basis$kosten_ergebnis_monat
}


# ------------------------------------------
# 3. Business Break-even in Einheiten/Stunden
# ------------------------------------------

calc_business_break_even <- function(
    betriebskosten,
    personal = NULL,
    finanzierung = NULL,
    preis_pro_einheit,
    variable_kosten_pro_einheit = 0,
    verfuegbare_einheiten_monat = NA_real_
) {

  fixkosten <-
    calc_break_even_fixkosten(
      betriebskosten =
        betriebskosten,
      personal =
        personal,
      finanzierung =
        finanzierung
    )

  db_pro_einheit <-
    calc_db_business(
      preis_pro_einheit =
        preis_pro_einheit,
      variable_kosten_pro_einheit =
        variable_kosten_pro_einheit
    )

  if (is.na(db_pro_einheit)) {
    return(
      list(
        break_even_erreichbar = NA,
        fixkosten_monat = fixkosten,
        db_pro_einheit = NA_real_,
        break_even_einheiten = NA_real_,
        break_even_umsatz = NA_real_,
        break_even_auslastung = NA_real_
      )
    )
  }

  if (db_pro_einheit <= 0) {
    return(
      list(
        break_even_erreichbar = FALSE,
        fixkosten_monat = fixkosten,
        db_pro_einheit = db_pro_einheit,
        break_even_einheiten = Inf,
        break_even_umsatz = Inf,
        break_even_auslastung = Inf,
        hinweis =
          "Kein Business-Break-even möglich: Deckungsbeitrag je Einheit ist nicht positiv."
      )
    )
  }

  break_even_einheiten <-
    fixkosten /
    db_pro_einheit

  break_even_umsatz <-
    break_even_einheiten *
    preis_pro_einheit

  break_even_auslastung <- NA_real_
  erreichbar <- NA

  if (!is.na(verfuegbare_einheiten_monat)) {

    if (verfuegbare_einheiten_monat < 0) {
      stop(
        "Verfügbare Einheiten/Stunden dürfen nicht negativ sein."
      )
    }

    if (verfuegbare_einheiten_monat == 0) {
      break_even_auslastung <- Inf
      erreichbar <- FALSE
    } else {
      break_even_auslastung <-
        break_even_einheiten /
        verfuegbare_einheiten_monat

      erreichbar <-
        break_even_auslastung <= 1
    }
  }

  list(
    fixkosten_monat =
      fixkosten,

    preis_pro_einheit =
      preis_pro_einheit,

    variable_kosten_pro_einheit =
      variable_kosten_pro_einheit,

    db_pro_einheit =
      db_pro_einheit,

    break_even_einheiten =
      break_even_einheiten,

    break_even_umsatz =
      break_even_umsatz,

    break_even_auslastung =
      break_even_auslastung,

    break_even_erreichbar =
      erreichbar
  )
}


# ------------------------------------------
# 4. Ergebnis bei realer Auslastung
# ------------------------------------------

calc_business_result <- function(
    betriebskosten,
    personal = NULL,
    finanzierung = NULL,
    preis_pro_einheit,
    variable_kosten_pro_einheit = 0,
    verfuegbare_einheiten_monat,
    auslastung
) {

  if (is.na(auslastung)) {
    return(NULL)
  }

  if (auslastung > 1) {
    auslastung <- auslastung / 100
  }

  if (auslastung < 0 || auslastung > 1) {
    stop(
      "Auslastung muss zwischen 0 und 1 bzw. 0 und 100 Prozent liegen."
    )
  }

  if (
    is.na(verfuegbare_einheiten_monat) ||
    verfuegbare_einheiten_monat < 0
  ) {
    stop(
      "Verfügbare Einheiten/Stunden müssen >= 0 sein."
    )
  }

  be <- calc_business_break_even(
    betriebskosten =
      betriebskosten,
    personal =
      personal,
    finanzierung =
      finanzierung,
    preis_pro_einheit =
      preis_pro_einheit,
    variable_kosten_pro_einheit =
      variable_kosten_pro_einheit,
    verfuegbare_einheiten_monat =
      verfuegbare_einheiten_monat
  )

  reale_einheiten <-
    verfuegbare_einheiten_monat *
    auslastung

  umsatz <-
    reale_einheiten *
    preis_pro_einheit

  variable_kosten_gesamt <-
    reale_einheiten *
    variable_kosten_pro_einheit

  deckungsbeitrag_gesamt <-
    umsatz -
    variable_kosten_gesamt

  ergebnis_vor_tilgung <-
    deckungsbeitrag_gesamt -
    be$fixkosten_monat

  list(
    auslastung =
      auslastung,

    verfuegbare_einheiten_monat =
      verfuegbare_einheiten_monat,

    reale_einheiten_monat =
      reale_einheiten,

    umsatz_monat =
      umsatz,

    variable_kosten_gesamt =
      variable_kosten_gesamt,

    deckungsbeitrag_gesamt =
      deckungsbeitrag_gesamt,

    fixkosten_monat =
      be$fixkosten_monat,

    ergebnis_vor_tilgung =
      ergebnis_vor_tilgung,

    break_even_einheiten =
      be$break_even_einheiten,

    break_even_umsatz =
      be$break_even_umsatz,

    break_even_auslastung =
      be$break_even_auslastung,

    break_even_erreicht =
      reale_einheiten >=
      be$break_even_einheiten
  )
}


# ------------------------------------------
# 5. Kapitaldienst separat prüfen
# ------------------------------------------
#
# Gewinn-Break-even und Kapitaldienstfähigkeit
# sind zwei verschiedene Fragen:
#
# 1) Deckt der Deckungsbeitrag die Kosten?
# 2) Reicht die Liquidität zusätzlich für Tilgung?
#
# Deshalb separate Funktion.

calc_business_with_debt_service <- function(
    betriebskosten,
    personal = NULL,
    finanzierung = NULL,
    preis_pro_einheit,
    variable_kosten_pro_einheit = 0,
    verfuegbare_einheiten_monat,
    auslastung
) {

  result <- calc_business_result(
    betriebskosten =
      betriebskosten,
    personal =
      personal,
    finanzierung =
      finanzierung,
    preis_pro_einheit =
      preis_pro_einheit,
    variable_kosten_pro_einheit =
      variable_kosten_pro_einheit,
    verfuegbare_einheiten_monat =
      verfuegbare_einheiten_monat,
    auslastung =
      auslastung
  )

  kd <- calc_kapitaldienstfaehigkeit(
    umsatz_monat =
      result$umsatz_monat,
    betriebskosten =
      betriebskosten,
    personal =
      personal,
    finanzierung =
      finanzierung
  )

  list(
    business =
      result,

    kapitaldienst =
      kd
  )
}


# ------------------------------------------
# 6. Kompakte KPI-Ausgabe
# ------------------------------------------

summarise_business_break_even <- function(
    result
) {

  if (is.null(result)) {
    return(NULL)
  }

  data.frame(
    Kennzahl = c(
      "Fixkosten / Monat",
      "Deckungsbeitrag / Einheit",
      "Break-even-Einheiten / Monat",
      "Break-even-Umsatz / Monat",
      "Break-even-Auslastung"
    ),

    Wert = c(
      result$fixkosten_monat,
      result$db_pro_einheit,
      result$break_even_einheiten,
      result$break_even_umsatz,
      result$break_even_auslastung
    ),

    Einheit = c(
      "EUR",
      "EUR/Einheit",
      "Einheiten",
      "EUR",
      "Anteil"
    ),

    stringsAsFactors = FALSE
  )
}


# ------------------------------------------
# 7. Break-even Visualisierung
# ------------------------------------------
#
# R = Kontroll-/Methodikgrafik
# Web später nativ im Frontend.

plot_business_break_even <- function(
    result,
    title = "Business Break-even"
) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Paket 'ggplot2' fehlt. Bitte einmal installieren mit install.packages('ggplot2')."
    )
  }

  if (
    is.null(result$break_even_einheiten) ||
    !is.finite(result$break_even_einheiten)
  ) {
    stop(
      "Kein endlicher Break-even für die Grafik verfügbar."
    )
  }

  max_x <- max(
    result$break_even_einheiten * 1.35,
    1
  )

  x <- seq(
    0,
    max_x,
    length.out = 250
  )

  umsatz <-
    x *
    result$preis_pro_einheit

  gesamtkosten <-
    result$fixkosten_monat +
    x *
    result$variable_kosten_pro_einheit

  df <- data.frame(
    einheiten = x,
    Umsatz = umsatz,
    Gesamtkosten = gesamtkosten
  )

  ggplot2::ggplot(
    df,
    ggplot2::aes(x = einheiten)
  ) +
    ggplot2::geom_line(
      ggplot2::aes(
        y = Umsatz,
        linetype = "Umsatz"
      ),
      linewidth = 1
    ) +
    ggplot2::geom_line(
      ggplot2::aes(
        y = Gesamtkosten,
        linetype = "Gesamtkosten"
      ),
      linewidth = 1
    ) +
    ggplot2::geom_vline(
      xintercept =
        result$break_even_einheiten,
      linetype = "dashed"
    ) +
    ggplot2::annotate(
      "point",
      x =
        result$break_even_einheiten,
      y =
        result$break_even_umsatz,
      size = 3
    ) +
    ggplot2::annotate(
      "text",
      x =
        result$break_even_einheiten,
      y =
        result$break_even_umsatz,
      label = paste0(
        "Break-even: ",
        round(
          result$break_even_einheiten,
          1
        )
      ),
      vjust = -1
    ) +
    ggplot2::labs(
      title = title,
      x = "Absatz / abrechenbare Einheiten pro Monat",
      y = "EUR / Monat",
      linetype = NULL,
      caption =
        "Business-Break-even auf Ergebnisbasis; Tilgung wird separat über Kapitaldienstfähigkeit geprüft."
    ) +
    ggplot2::theme_minimal()
}


# ------------------------------------------
# 8. Architektur für Scenario Layer
# ------------------------------------------
#
# Für jedes Szenario werden später neu bestimmt:
#
# Betriebskosten_s
# Personalkosten_s
# Finanzierung_s
# Preis_s
# variable_Kosten_s
# Kapazität_s / Auslastung_s
#
# Dann:
#
# Break-even_s
# Ergebnis_s
# Kapitaldienstfähigkeit_s
#
# Somit kann FBC später vergleichen:
#
# Chancenszenario
# Basisszenario
# Risikoszenario
#
# und daraus Entscheidungen ableiten.
#
# ==========================================
