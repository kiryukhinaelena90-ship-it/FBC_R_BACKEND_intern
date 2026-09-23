# ============================================================
# 42_fbc_fazit.R
# FUTURE Business Cockpit
# Human-readable final conclusion builder for canonical P1 payload.
#
# Uses only fields actually present in the payload.
# Missing MC / Shapley / Differential-Influence information is omitted;
# nothing is invented to make the conclusion sound complete.
# ============================================================

`%||%` <- function(a,b) if(is.null(a) || length(a)==0L) b else a

fbc_num42 <- function(x){
  y <- suppressWarnings(as.numeric(x %||% NA_real_))
  if(length(y)==1L && is.finite(y)) y else NA_real_
}

fbc_money42 <- function(x){
  x <- fbc_num42(x)
  if(!is.finite(x)) return(NULL)
  paste0(format(round(x,2), big.mark=".", decimal.mark=",", nsmall=2, scientific=FALSE), " €")
}

fbc_pct42 <- function(x){
  x <- fbc_num42(x)
  if(!is.finite(x)) return(NULL)
  paste0(format(round(100*x,1), decimal.mark=",", nsmall=1, scientific=FALSE), " %")
}

fbc_change_phrase42 <- function(ch, lang="de"){
  if(is.null(ch) || !is.list(ch)) return(NULL)
  label <- as.character(ch$label %||% ch$lever %||% "")
  cur <- fbc_num42(ch$current)
  prop <- fbc_num42(ch$proposed)
  unit <- as.character(ch$unit %||% "")
  if(!nzchar(label) || !is.finite(prop)) return(NULL)

  fmt <- function(v){
    if(grepl("EUR",unit)) paste0(format(round(v,2),decimal.mark=",",nsmall=2), " ", if(grepl("/h",unit)) "€/h" else "€")
    else if(grepl("h",unit)) paste0(format(round(v,1),decimal.mark=",",nsmall=1), " h")
    else format(round(v,2),decimal.mark=",",nsmall=2)
  }

  if(lang=="ru"){
    if(is.finite(cur)) paste0(label, ": ", fmt(cur), " → ", fmt(prop)) else paste0(label, ": ", fmt(prop))
  } else {
    if(is.finite(cur)) paste0(label, ": ", fmt(cur), " → ", fmt(prop)) else paste0(label, ": ", fmt(prop))
  }
}

fbc_dominant_label42 <- function(payload){
  exp <- payload$recommendation$explanation %||% list()
  dom <- exp$dominant_label %||% exp$dominant_lever
  if(!is.null(dom) && length(dom) && nzchar(as.character(dom[1]))) return(as.character(dom[1]))

  contrib <- exp$contributions %||% list()
  if(length(contrib)){
    first <- contrib[[1]]
    if(is.list(first)) return(as.character(first$label %||% first$variable %||% first$lever %||% ""))
  }
  NULL
}

fbc_fazit42 <- function(payload, lang=c("de","ru")){
  lang <- match.arg(lang)
  if(is.null(payload) || !is.list(payload)) stop("payload must be a P1 payload list.")

  status <- as.character(payload$status %||% "")
  current <- fbc_num42(payload$current$expected_net)
  target <- fbc_num42(payload$current$monthly_target)
  projected <- fbc_num42(payload$recommendation$projected_net)
  remaining <- fbc_num42(payload$recommendation$remaining_gap_eur)
  changes <- payload$recommendation$changes %||% list()
  rob <- payload$robustness %||% list()

  reached <- payload$recommendation$target_reached
  if(!is.logical(reached) || length(reached)!=1L || is.na(reached)){
    reached <- payload$target_path$target_reached_within_horizon
  }
  if(!is.logical(reached) || length(reached)!=1L || is.na(reached)){
    reached <- is.finite(projected) && is.finite(target) && projected >= target - .01
  }
  if(identical(status,"target_not_reachable")) reached <- FALSE

  sentences <- character()

  if(lang=="de"){
    if(is.finite(current) && is.finite(target))
      sentences <- c(sentences, paste0("Aktuell liegt das erwartete Netto bei ",fbc_money42(current)," pro Monat; das Monatsziel beträgt ",fbc_money42(target),"."))

    if(identical(status,"target_not_reachable")){
      if(is.finite(projected) && is.finite(remaining))
        sentences <- c(sentences, paste0("Mit den bestätigten Grenzen ist das Ziel nicht vollständig erreichbar: Der beste gefundene Wert liegt bei ",fbc_money42(projected),", es fehlen noch ",fbc_money42(remaining),"."))
      else
        sentences <- c(sentences, "Mit den bestätigten Grenzen ist das Monatsziel nicht vollständig erreichbar.")
    } else if(isTRUE(reached)){
      if(is.finite(projected))
        sentences <- c(sentences, paste0("Mit der ausgewählten Kombination wird das Ziel erreicht; das erwartete Ergebnis nach der Änderung liegt bei ",fbc_money42(projected),"."))
      else sentences <- c(sentences, "Mit der ausgewählten Kombination wird das Monatsziel erreicht.")
    } else if(is.finite(projected)){
      sentences <- c(sentences, paste0("Nach der ausgewählten Änderung liegt das erwartete Ergebnis bei ",fbc_money42(projected),"."))
    }

    if(length(changes)){
      phrases <- Filter(Negate(is.null), lapply(head(changes,3), fbc_change_phrase42, lang="de"))
      if(length(phrases)) sentences <- c(sentences, paste0("Die konkrete Änderung lautet: ",paste(phrases,collapse="; "),"."))
    }

    dom <- fbc_dominant_label42(payload)
    if(!is.null(dom) && nzchar(dom)) sentences <- c(sentences, paste0("Den größten erklärten Beitrag liefert ",dom,"."))

    if(isTRUE(rob$available)){
      prob <- fbc_pct42(rob$target_probability)
      p10 <- fbc_money42(rob$p10); p90 <- fbc_money42(rob$p90)
      if(!is.null(prob) && !is.null(p10) && !is.null(p90))
        sentences <- c(sentences, paste0("Die Monte-Carlo-Prüfung ergibt eine Zielerreichungswahrscheinlichkeit von ",prob,"; der wahrscheinliche Bereich (P10–P90) liegt bei ",p10," bis ",p90,"."))
      else if(!is.null(prob))
        sentences <- c(sentences, paste0("Die Monte-Carlo-Prüfung ergibt eine Zielerreichungswahrscheinlichkeit von ",prob,"."))
    }
  } else {
    if(is.finite(current) && is.finite(target))
      sentences <- c(sentences, paste0("Сейчас ожидаемый чистый результат составляет ",fbc_money42(current)," в месяц, цель — ",fbc_money42(target),"."))

    if(identical(status,"target_not_reachable")){
      if(is.finite(projected) && is.finite(remaining))
        sentences <- c(sentences, paste0("В подтверждённых границах цель полностью недостижима: лучший найденный результат — ",fbc_money42(projected),", до цели остаётся ",fbc_money42(remaining),"."))
      else sentences <- c(sentences, "В подтверждённых границах месячная цель полностью недостижима.")
    } else if(isTRUE(reached)){
      if(is.finite(projected))
        sentences <- c(sentences, paste0("Выбранная комбинация позволяет достичь цели; ожидаемый результат после изменения — ",fbc_money42(projected),"."))
      else sentences <- c(sentences, "Выбранная комбинация позволяет достичь месячной цели.")
    } else if(is.finite(projected)){
      sentences <- c(sentences, paste0("После выбранного изменения ожидаемый результат составляет ",fbc_money42(projected),"."))
    }

    if(length(changes)){
      phrases <- Filter(Negate(is.null), lapply(head(changes,3), fbc_change_phrase42, lang="ru"))
      if(length(phrases)) sentences <- c(sentences, paste0("Конкретное изменение: ",paste(phrases,collapse="; "),"."))
    }

    dom <- fbc_dominant_label42(payload)
    if(!is.null(dom) && nzchar(dom)) sentences <- c(sentences, paste0("Наибольший объяснённый вклад даёт ",dom,"."))

    if(isTRUE(rob$available)){
      prob <- fbc_pct42(rob$target_probability)
      p10 <- fbc_money42(rob$p10); p90 <- fbc_money42(rob$p90)
      if(!is.null(prob) && !is.null(p10) && !is.null(p90))
        sentences <- c(sentences, paste0("Monte Carlo показывает вероятность достижения цели ",prob,"; вероятный диапазон (P10–P90) — от ",p10," до ",p90,"."))
      else if(!is.null(prob))
        sentences <- c(sentences, paste0("Monte Carlo показывает вероятность достижения цели ",prob,"."))
    }
  }

  list(
    available = length(sentences)>0,
    language = lang,
    sentence = paste(sentences, collapse=" "),
    components = as.list(sentences),
    sources_used = list(
      deterministic = TRUE,
      changes = length(changes)>0,
      shapley = !is.null(fbc_dominant_label42(payload)),
      monte_carlo = isTRUE(rob$available),
      differential_influence = FALSE
    )
  )
}

fbc_attach_fazit42 <- function(payload){
  out <- payload
  out$fazit <- list(
    de = fbc_fazit42(out,"de"),
    ru = fbc_fazit42(out,"ru")
  )
  if(is.null(out$production_meta)) out$production_meta <- list()
  methods <- out$production_meta$methods_used %||% character()
  out$production_meta$methods_used <- unique(c(methods,"rule-based FBC Fazit from executed payload layers"))
  out
}

cat("\n42 FBC Fazit builder loaded.\n")
