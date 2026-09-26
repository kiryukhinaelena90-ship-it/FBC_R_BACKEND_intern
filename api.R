# ============================================================
# FUTURE Business Cockpit — R Backend API
# Plumber service for FBC_R_INTEGRATION_STAGE1 frontend.
#
# Routes:
#   GET  /health
#   POST /analyze
#
# Default behavior:
# - factual/P0 response is enabled;
# - no evidence => factual P0 response with no optimizer lever;
# - evidence present => P1 runner executes via 43_run_p1_mc_fazit.R.
# ============================================================

suppressPackageStartupMessages({
  library(plumber)
  library(jsonlite)
})

ROOT <- normalizePath(getwd(), mustWork=TRUE)
source(file.path(ROOT,"24_financial_net_adapter.R"), local=.GlobalEnv)
source(file.path(ROOT,"25_price_elasticity.R"), local=.GlobalEnv)

# Source factual-state modules once.
source(file.path(ROOT,"18_cockpit_input_contract.R"), local=.GlobalEnv)
source(file.path(ROOT,"10_gesamtkosten_kapitaldienst.R"), local=.GlobalEnv)
source(file.path(ROOT,"11_business_break_even.R"), local=.GlobalEnv)
source(file.path(ROOT,"19_reality_constraints_V2.R"), local=.GlobalEnv)
source(file.path(ROOT,"44_team_decision_support.R"), local=.GlobalEnv)

`%||%` <- function(a,b) if(is.null(a) || length(a)==0L) b else a
num1 <- function(x, default=0){
  z <- suppressWarnings(as.numeric(x %||% default)[1])
  if(!is.finite(z)) default else z
}
flag1 <- function(x) isTRUE(x)

fbc_authorized <- function(req){
  expected <- Sys.getenv("FBC_R_API_KEY", unset="")
  if(!nzchar(expected)) return(TRUE)
  got <- req$HTTP_AUTHORIZATION %||% ""
  identical(got, paste0("Bearer ", expected))
}

fbc_validate_request <- function(x){
  errors <- character()

  if(is.null(x) || !is.list(x))
    return("body_missing")

  if(!identical(x$schema_version,"FBC_P1_COCKPIT_REQUEST_1.0"))
    errors <- c(errors,"schema_version")

  allowed_modes <- c(
    "solo",
    "owner_employee",
    "owner_team"
  )

  if(!as.character(x$mode %||% "") %in% allowed_modes)
    errors <- c(errors,"mode")

  common_required <- c(
    "owner",
    "operating_costs",
    "financing"
  )

  for(k in common_required){
    if(is.null(x[[k]]) || !is.list(x[[k]]))
      errors <- c(errors,k)
  }

  if(identical(x$mode,"owner_team")){
    if(
      is.null(x$employees) ||
      !is.list(x$employees) ||
      length(x$employees) < 1L ||
      length(x$employees) > 5L
    ){
      errors <- c(errors,"employees")
    }
  } else {
    if(is.null(x$employee) || !is.list(x$employee))
      errors <- c(errors,"employee")
  }

  unique(errors)
}

fbc_map_costs <- function(x){
  # Frontend calls this field "insurance"; canonical R state calls it "business_insurance".
  c(
    office=num1(x$office),
    energy=num1(x$energy),
    vehicle=num1(x$vehicle),
    fuel=num1(x$fuel),
    software=num1(x$software),
    accounting=num1(x$accounting),
    business_insurance=num1(x$business_insurance %||% x$insurance),
    material=num1(x$material),
    marketing=num1(x$marketing),
    other=num1(x$other)
  )
}

fbc_map_financing <- function(x){
  type <- as.character(x$type %||% "annuity")

  type_map <- c(
    annuity="annuity",
    tilgung="tilgung",
    bullet="endfaellig",
    line="credit_line",
    zero="zinsfrei",
    endfaellig="endfaellig",
    credit_line="credit_line",
    zinsfrei="zinsfrei"
  )

  if(!type %in% names(type_map)) type <- "annuity"
  type <- unname(type_map[[type]])

  list(
    active=flag1(x$active),
    type=type,
    amount=num1(x$amount),
    rate_pa=num1(x$rate_pa),
    months=max(1,round(num1(x$months,1))),
    fees_month=num1(x$fees_month),
    binding=as.character(x$binding %||% "fixed")
  )
}

fbc_build_factual_state <- function(cfg){
  owner <- cfg$owner
  employee <- cfg$employee

  state <- build_cockpit_decision_state18(
    owner=list(
      price=num1(owner$price),
      hours_week=num1(owner$hours_week),
      days_week=max(1,num1(owner$days_week,5)),
      vacation_days=num1(owner$vacation_days),
      billable_hours_month=max(0,num1(owner$billable_hours_month)),
      monthly_target=num1(owner$monthly_target),
      insurance_month=num1(owner$insurance_month),
      pension_mode=as.character(owner$pension_mode %||% "none"),
      pension_fixed_month=num1(owner$pension_fixed_month)
    ),
    operating_costs=fbc_map_costs(cfg$operating_costs),
    employee=list(
      form=as.character(employee$form %||% "part"),
      wage_hour=num1(employee$wage_hour),
      hours_week=num1(employee$hours_week),
      days_week=max(1,num1(employee$days_week,5)),
      vacation_days=num1(employee$vacation_days),
      direct_billing=flag1(employee$direct_billing),
      billable_hours_month=if(flag1(employee$direct_billing)) max(0,num1(employee$billable_hours_month)) else 0,
      customer_price=num1(employee$customer_price),
      extra_cost_month=num1(employee$extra_cost_month)
    ),
    financing=fbc_map_financing(cfg$financing),
    owner_holiday_days=num1(owner$holidays),
    employee_holiday_days=num1(employee$holidays),
    employer_addon_rate=num1(employee$employer_addon_rate)
  )

  # Critical factual separation:
  # contract/paid capacity is not the same thing as customer-billable hours.
  owner_bill <- max(0,num1(owner$billable_hours_month))
  emp_bill <- if(flag1(employee$direct_billing)) max(0,num1(employee$billable_hours_month)) else 0

  # Keep physical/paid capacity and commercial billable hours as separate facts.
  # available_hours_month stays the physical capacity created by module 18.
  state$owner$physical_available_hours_month <- state$owner$available_hours_month
  state$owner$billable_hours_month <- owner_bill

  state$employee$paid_available_hours_month <-
    num1(employee$paid_available_hours_month, state$employee$available_hours_month)
  state$employee$billable_hours_month <- emp_bill

  # Use factual personnel cost from the Cockpit when supplied. This preserves
  # employment-form logic already calculated in the current frontend.
  factual_pc <- num1(employee$personnel_cost_month, NA_real_)
  if(is.finite(factual_pc) && factual_pc >= 0){
    state$employee$personnel_cost_month <- factual_pc
  }

  if(!exists("fbc_apply_demand_to_state25", mode="function"))
    stop("fbc_apply_demand_to_state25() fehlt. 25_price_elasticity.R zuerst laden.")

  # Factual P0 state: no price change, so expected hours equal the explicitly
  # supplied commercial billable hours.
  fbc_apply_demand_to_state25(
    state=state,
    base_state=state
  )
}
# ============================================================
# Единая оценка текущей / тестовой точки для P0-анализа
# ============================================================
#
# Важно: эта функция использует ту же экономическую основу, что и P1:
# - цена проходит через модель эластичности спроса из модуля 25;
# - проценты и комиссии по финансированию остаются расходом результата;
# - тело кредита не считается доходом;
# - итоговый net_available рассчитывается существующим финансовым адаптером.
# ============================================================

fbc_eval_net_point <- function(
    state,
    cfg,
    owner_price = state$owner$price,
    owner_hours = state$owner$billable_hours_month,
    employee_price = state$employee$customer_price,
    employee_hours = state$employee$billable_hours_month,
    operating_costs = state$operating_costs
){

  demand <- fbc_demand_snapshot25(
    base_state = state,
    owner_price = owner_price,
    owner_planned_hours = owner_hours,
    employee_price = employee_price,
    employee_planned_hours = employee_hours
  )

  owner_revenue <- as.numeric(demand$owner$revenue_month)
  employee_revenue <- as.numeric(demand$employee$revenue_month)

  financing_result_cost <-
    suppressWarnings(
      as.numeric(
        state$financing$interest_plus_fees_month %||% 0
      )[1]
    )

  if(!is.finite(financing_result_cost))
    financing_result_cost <- 0

  result_before_owner_protection_tax <-
    owner_revenue +
    employee_revenue -
    sum(operating_costs) -
    state$employee$personnel_cost_month -
    financing_result_cost

  financial <- fbc_financial_point24(
    result_before_owner_protection_tax,
    state$owner,
    legal_form =
      as.character(
        cfg$legal_form %||% "freelance"
      ),
    trade_tax_rate =
      num1(cfg$trade_tax_rate)
  )

  list(
    net_available = as.numeric(financial$net_available),
    result_before_owner_protection_tax =
      as.numeric(result_before_owner_protection_tax),
    owner_revenue = owner_revenue,
    employee_revenue = employee_revenue,
    financing_result_cost = financing_result_cost,
    demand = demand
  )
}


# ============================================================
# Sensitivity payload for Decision Support
# Each factor is changed separately by 1%.
# All other values remain unchanged.
# This is diagnostic, not a recommendation.
# ============================================================

fbc_sensitivity_payload <- function(state, cfg){

  eval_net <- function(
      owner_price = state$owner$price,
      owner_hours = state$owner$billable_hours_month,
      employee_price = state$employee$customer_price,
      employee_hours = state$employee$billable_hours_month,
      operating_costs = state$operating_costs
  ){
    fbc_eval_net_point(
      state = state,
      cfg = cfg,
      owner_price = owner_price,
      owner_hours = owner_hours,
      employee_price = employee_price,
      employee_hours = employee_hours,
      operating_costs = operating_costs
    )$net_available
  }


  base_net <- eval_net()


  make_row <- function(
      lever,
      label,
      changed_net,
      perturbation_pct,
      current
  ){

    delta_net <-
      changed_net - base_net

    delta_pct <-
      if(
        is.finite(base_net) &&
        abs(base_net) > 1e-9
      ){
        100 * delta_net / abs(base_net)
      } else {
        NA_real_
      }

    list(
      lever = lever,
      label = label,

      current =
        as.numeric(current),

      perturbation_pct =
        as.numeric(perturbation_pct),

      net_change_eur =
        as.numeric(delta_net),

      net_change_pct =
        if(is.finite(delta_pct))
          as.numeric(delta_pct)
        else
          NULL
    )
  }


  rows <- list()


  # ----------------------------------------------------------
  # 1. Owner price +1 %
  # ----------------------------------------------------------

  rows[[length(rows) + 1L]] <-
    make_row(
      lever = "owner_price",
      label = "Inhaberpreis",

      changed_net =
        eval_net(
          owner_price =
            state$owner$price * 1.01
        ),

      perturbation_pct = 1,

      current =
        state$owner$price
    )


  # ----------------------------------------------------------
  # 2. Owner billable hours +1 %
  # ----------------------------------------------------------

  rows[[length(rows) + 1L]] <-
    make_row(
      lever = "owner_hours",
      label = "Abrechenbare Inhaberstunden",

      changed_net =
        eval_net(
          owner_hours =
            state$owner$billable_hours_month * 1.01
        ),

      perturbation_pct = 1,

      current =
        state$owner$billable_hours_month
    )


  # ----------------------------------------------------------
  # 3–4. Employee only if directly billed
  # ----------------------------------------------------------

  if(isTRUE(state$employee$direct_billing)){

    rows[[length(rows) + 1L]] <-
      make_row(
        lever = "employee_customer_price",
        label = "Mitarbeiter-Kundenpreis",

        changed_net =
          eval_net(
            employee_price =
              state$employee$customer_price * 1.01
          ),

        perturbation_pct = 1,

        current =
          state$employee$customer_price
      )


    rows[[length(rows) + 1L]] <-
      make_row(
        lever = "employee_billable_hours",
        label = "Abrechenbare Mitarbeiterstunden",

        changed_net =
          eval_net(
            employee_hours =
              state$employee$billable_hours_month * 1.01
          ),

        perturbation_pct = 1,

        current =
          state$employee$billable_hours_month
      )
  }


  # ----------------------------------------------------------
  # 5. Total operating costs −1 %
  # ----------------------------------------------------------

  rows[[length(rows) + 1L]] <-
    make_row(
      lever = "operating_costs",
      label = "Betriebskosten gesamt",

      changed_net =
        eval_net(
          operating_costs =
            state$operating_costs * 0.99
        ),

      perturbation_pct = -1,

      current =
        sum(state$operating_costs)
    )


  # ----------------------------------------------------------
  # Dominant factor
  # ----------------------------------------------------------

  effects <-
    vapply(
      rows,
      function(x){

        z <- suppressWarnings(
          as.numeric(
            x$net_change_pct %||% NA_real_
          )
        )

        if(is.finite(z))
          abs(z)
        else
          0
      },
      numeric(1)
    )


  dominant_index <-
    if(length(effects))
      which.max(effects)
    else
      integer(0)


  dominant_lever <-
    if(length(dominant_index))
      rows[[dominant_index]]$lever
    else
      NULL


  dominant_label <-
    if(length(dominant_index))
      rows[[dominant_index]]$label
    else
      NULL


  list(
    method =
      "local_1pct_financial_sensitivity",

    base_net =
      as.numeric(base_net),

    local =
      rows,

    dominant_lever =
      dominant_lever,

    dominant_label =
      dominant_label,

    demand_model = list(
      model = "constant_price_elasticity",
      epsilon = FBC_PRICE_ELASTICITY25
    ),

    note =
      paste(
        "Each factor is changed separately by 1 percent.",
        "Price changes include the constant demand response epsilon = -0.60.",
        "This analysis is diagnostic and is not an automatic recommendation."
      )
  )
}

# ============================================================
# Ориентация перед вторым прогоном
# ============================================================
#
# Задача этого слоя — не выбрать решение за пользователя, а показать,
# какие показатели математически имеет смысл проверить в следующем шаге.
#
# Сравнение делается без произвольного score:
# - цена: +1 % с учетом эластичности спроса;
# - часы: +1 %, но только устойчивый маржинальный прирост часов
#   учитывается при ранжировании;
# - общие Betriebskosten: -1 %.
#
# Реальные границы изменения здесь не придумываются. Их пользователь
# подтверждает в Handlungsspielräume перед P1-оптимизацией.
# ============================================================

fbc_decision_guidance_payload <- function(
    state,
    cfg,
    employee_break_even_hours = NA_real_
){

  base_net <-
    fbc_eval_net_point(
      state = state,
      cfg = cfg
    )$net_available

  candidates <- list()

  add_candidate <- function(
      actor,
      actor_label,
      lever,
      lever_label,
      direction,
      changed_net,
      test_change_pct,
      reason_codes,
      utilization = NULL,
      feasibility_hint = "needs_user_confirmation",
      raw_changed_net = changed_net,
      employee_cost_coverage_improvement_hours = NULL
  ){
    guidance_effect_eur <-
      as.numeric(changed_net) - as.numeric(base_net)

    commercial_effect_eur <-
      as.numeric(raw_changed_net) - as.numeric(base_net)

    guidance_effect_pct <-
      if(is.finite(base_net) && abs(base_net) > 1e-9){
        100 * guidance_effect_eur / abs(base_net)
      } else {
        NA_real_
      }

    commercial_effect_pct <-
      if(is.finite(base_net) && abs(base_net) > 1e-9){
        100 * commercial_effect_eur / abs(base_net)
      } else {
        NA_real_
      }

    # До второго прогона мы не придумываем минимальный "значимый" эффект.
    # Берем только строго положительные направления и затем ранжируем их.
    if(!is.finite(guidance_effect_eur) || guidance_effect_eur <= 0)
      return(invisible(NULL))

    candidates[[length(candidates) + 1L]] <<-
      list(
        actor = actor,
        actor_label = actor_label,
        lever = lever,
        lever_label = lever_label,
        direction = direction,
        test_change_pct = as.numeric(test_change_pct),

        # guidance_* используется для ранжирования перед вторым прогоном.
        guidance_net_effect_eur_1pct =
          as.numeric(guidance_effect_eur),
        guidance_net_effect_pct_1pct =
          if(is.finite(guidance_effect_pct))
            as.numeric(guidance_effect_pct)
          else
            NULL,

        # commercial_* сохраняет фактический денежный эффект полного 1 %-шага.
        # Для цены и расходов он совпадает с guidance_*; для часов guidance_*
        # дополнительно учитывает маржинальный коэффициент загрузки.
        commercial_net_effect_eur_1pct =
          if(is.finite(commercial_effect_eur))
            as.numeric(commercial_effect_eur)
          else
            NULL,
        commercial_net_effect_pct_1pct =
          if(is.finite(commercial_effect_pct))
            as.numeric(commercial_effect_pct)
          else
            NULL,

        # Совместимость с простым frontend: net_effect_* = guidance effect.
        net_effect_eur_1pct = as.numeric(guidance_effect_eur),
        net_effect_pct_1pct =
          if(is.finite(guidance_effect_pct))
            as.numeric(guidance_effect_pct)
          else
            NULL,

        employee_cost_coverage_improvement_hours =
          if(
            !is.null(employee_cost_coverage_improvement_hours) &&
            is.finite(as.numeric(employee_cost_coverage_improvement_hours)[1])
          )
            as.numeric(employee_cost_coverage_improvement_hours)[1]
          else
            NULL,

        utilization = utilization,
        feasibility_hint = feasibility_hint,
        reason_codes = as.list(unique(reason_codes))
      )

    invisible(NULL)
  }


  # ----------------------------------------------------------
  # 1. Цена владельца +1 %
  # ----------------------------------------------------------
  # Эффект цены всегда проходит через fbc_demand_snapshot25(),
  # поэтому ожидаемые часы меняются по epsilon = FBC_PRICE_ELASTICITY25.

  if(is.finite(as.numeric(state$owner$price)) && state$owner$price > 0){
    owner_price_net <-
      fbc_eval_net_point(
        state = state,
        cfg = cfg,
        owner_price = state$owner$price * 1.01
      )$net_available

    add_candidate(
      actor = "owner",
      actor_label = "Inhaber/in",
      lever = "owner_price",
      lever_label = "Kundenpreis Inhaber/in",
      direction = "increase",
      changed_net = owner_price_net,
      test_change_pct = 1,
      reason_codes = c(
        "positive_price_effect_after_elasticity"
      )
    )
  }


  # ----------------------------------------------------------
  # 2. Оплачиваемые часы владельца +1 %
  # ----------------------------------------------------------
  # Коммерческая выручка остается фактической. Для ориентации мы лишь
  # дисконтируем ДОПОЛНИТЕЛЬНЫЙ час коэффициентом устойчивой загрузки
  # из модуля 19. Это не физический cap и не изменение факта выручки.

  owner_h0 <- as.numeric(state$owner$billable_hours_month)

  if(is.finite(owner_h0) && owner_h0 > 0){
    owner_h1 <- owner_h0 * 1.01

    owner_u0 <-
      fbc_owner_utilization19(
        state,
        billable_hours = owner_h0
      )

    owner_u1 <-
      fbc_owner_utilization19(
        state,
        billable_hours = owner_h1
      )

    raw_delta <- owner_h1 - owner_h0
    effective_delta <-
      max(
        0,
        as.numeric(owner_u1$effective_hours) -
          as.numeric(owner_u0$effective_hours)
      )

    marginal_weight <-
      if(raw_delta > 0) effective_delta / raw_delta else NA_real_

    owner_util_payload <-
      list(
        utilization_rate =
          if(is.finite(owner_u0$utilization_rate))
            as.numeric(owner_u0$utilization_rate)
          else
            NULL,
        utilization_zone = as.character(owner_u0$zone),
        marginal_hour_weight =
          if(is.finite(marginal_weight))
            as.numeric(marginal_weight)
          else
            NULL,
        available_hours_month =
          if(is.finite(owner_u0$available_hours))
            as.numeric(owner_u0$available_hours)
          else
            NULL
      )

    # При перегрузке (>100 %) дополнительные часы не выдаем как
    # "возможный первый рычаг". При 85–100 % они остаются кандидатом,
    # но их маржинальный эффект уже снижен коэффициентом 0.85.
    if(!identical(as.character(owner_u0$zone), "overload")){
      owner_hours_raw_net <-
        fbc_eval_net_point(
          state = state,
          cfg = cfg,
          owner_hours = owner_h1
        )$net_available

      owner_hours_net <-
        fbc_eval_net_point(
          state = state,
          cfg = cfg,
          owner_hours = owner_h0 + effective_delta
        )$net_available

      add_candidate(
        actor = "owner",
        actor_label = "Inhaber/in",
        lever = "owner_hours",
        lever_label = "Abrechenbare Stunden Inhaber/in",
        direction = "increase",
        changed_net = owner_hours_net,
        raw_changed_net = owner_hours_raw_net,
        test_change_pct = 1,
        reason_codes = c(
          "positive_hours_effect_with_utilization",
          paste0("utilization_", as.character(owner_u0$zone))
        ),
        utilization = owner_util_payload,
        feasibility_hint =
          if(identical(as.character(owner_u0$zone), "capacity_unknown"))
            "capacity_unknown"
          else
            "capacity_available"
      )
    }
  }


  # ----------------------------------------------------------
  # 3–4. Цена и часы сотрудника
  # ----------------------------------------------------------

  employee_break_even_ok <- TRUE
  employee_hours_gap <- 0

  if(isTRUE(state$employee$direct_billing)){
    employee_h0 <-
      fbc_state_expected_employee_hours25(state)

    if(is.finite(employee_break_even_hours)){
      employee_break_even_ok <-
        employee_h0 + 1e-9 >= employee_break_even_hours

      employee_hours_gap <-
        max(0, employee_break_even_hours - employee_h0)
    }

    employee_reason_prefix <-
      if(isTRUE(employee_break_even_ok))
        character(0)
      else
        "employee_cost_coverage_open"

    if(
      is.finite(as.numeric(state$employee$customer_price)) &&
      state$employee$customer_price > 0
    ){
      employee_price_test <-
        state$employee$customer_price * 1.01

      employee_price_point <-
        fbc_eval_net_point(
          state = state,
          cfg = cfg,
          employee_price = employee_price_test
        )

      employee_price_net <-
        employee_price_point$net_available

      employee_price_be <-
        fbc_employee_break_even_hours19(
          state,
          customer_price = employee_price_test,
          variable_cost_per_hour =
            num1(cfg$employee_variable_cost_per_hour, 0)
        )

      employee_current_margin_hours <-
        if(is.finite(employee_break_even_hours))
          employee_h0 - employee_break_even_hours
        else
          NA_real_

      employee_price_margin_hours <-
        if(is.finite(employee_price_be))
          as.numeric(employee_price_point$demand$employee$expected_hours) -
            employee_price_be
        else
          NA_real_

      employee_price_coverage_improvement <-
        if(
          is.finite(employee_current_margin_hours) &&
          is.finite(employee_price_margin_hours)
        )
          employee_price_margin_hours - employee_current_margin_hours
        else
          NA_real_

      add_candidate(
        actor = "employee",
        actor_label = "Mitarbeiter/in",
        lever = "employee_customer_price",
        lever_label = "Kundenpreis Mitarbeiter/in",
        direction = "increase",
        changed_net = employee_price_net,
        raw_changed_net = employee_price_net,
        test_change_pct = 1,
        employee_cost_coverage_improvement_hours =
          employee_price_coverage_improvement,
        reason_codes = c(
          employee_reason_prefix,
          "positive_price_effect_after_elasticity"
        )
      )
    }

    employee_plan_h0 <-
      as.numeric(state$employee$billable_hours_month)

    if(is.finite(employee_plan_h0) && employee_plan_h0 > 0){
      employee_h1 <- employee_plan_h0 * 1.01

      employee_u0 <-
        fbc_employee_utilization19(
          state,
          billable_hours = employee_plan_h0
        )

      employee_u1 <-
        fbc_employee_utilization19(
          state,
          billable_hours = employee_h1
        )

      raw_delta <- employee_h1 - employee_plan_h0
      effective_delta <-
        max(
          0,
          as.numeric(employee_u1$effective_hours) -
            as.numeric(employee_u0$effective_hours)
        )

      marginal_weight <-
        if(raw_delta > 0) effective_delta / raw_delta else NA_real_

      employee_util_payload <-
        list(
          utilization_rate =
            if(is.finite(employee_u0$utilization_rate))
              as.numeric(employee_u0$utilization_rate)
            else
              NULL,
          utilization_zone = as.character(employee_u0$zone),
          marginal_hour_weight =
            if(is.finite(marginal_weight))
              as.numeric(marginal_weight)
            else
              NULL,
          available_hours_month =
            if(is.finite(employee_u0$available_hours))
              as.numeric(employee_u0$available_hours)
            else
              NULL
        )

      if(!identical(as.character(employee_u0$zone), "overload")){
        employee_hours_raw_net <-
          fbc_eval_net_point(
            state = state,
            cfg = cfg,
            employee_hours = employee_h1
          )$net_available

        employee_hours_net <-
          fbc_eval_net_point(
            state = state,
            cfg = cfg,
            employee_hours = employee_plan_h0 + effective_delta
          )$net_available

        employee_hours_coverage_improvement <-
          if(is.finite(employee_break_even_hours))
            employee_h1 - employee_plan_h0
          else
            NA_real_

        add_candidate(
          actor = "employee",
          actor_label = "Mitarbeiter/in",
          lever = "employee_billable_hours",
          lever_label = "Abrechenbare Stunden Mitarbeiter/in",
          direction = "increase",
          changed_net = employee_hours_net,
          raw_changed_net = employee_hours_raw_net,
          test_change_pct = 1,
          employee_cost_coverage_improvement_hours =
            employee_hours_coverage_improvement,
          reason_codes = c(
            employee_reason_prefix,
            "positive_hours_effect_with_utilization",
            paste0("utilization_", as.character(employee_u0$zone))
          ),
          utilization = employee_util_payload,
          feasibility_hint =
            if(identical(as.character(employee_u0$zone), "capacity_unknown"))
              "capacity_unknown"
            else
              "capacity_available"
        )
      }
    }
  }


  # ----------------------------------------------------------
  # 5. Betriebskosten gesamt -1 %
  # ----------------------------------------------------------
  # До подтверждения конкретных границ мы не утверждаем, что расходы
  # реально можно снизить. Здесь это только математический кандидат
  # для проверки пользователем.

  operating_total <- sum(state$operating_costs)

  if(is.finite(operating_total) && operating_total > 0){
    costs_net <-
      fbc_eval_net_point(
        state = state,
        cfg = cfg,
        operating_costs = state$operating_costs * 0.99
      )$net_available

    add_candidate(
      actor = "business",
      actor_label = "Betrieb",
      lever = "operating_costs",
      lever_label = "Betriebskosten gesamt",
      direction = "decrease",
      changed_net = costs_net,
      test_change_pct = -1,
      reason_codes = c(
        "positive_cost_reduction_effect"
      )
    )
  }


  # ----------------------------------------------------------
  # Ранжирование без искусственного score
  # ----------------------------------------------------------
  # Поскольку каждый кандидат проверен одинаковым относительным шагом 1 %,
  # сравниваем прямой прирост net_available в евро.

  if(length(candidates)){
    effects <-
      vapply(
        candidates,
        function(x) as.numeric(x$guidance_net_effect_eur_1pct),
        numeric(1)
      )

    ord <- order(effects, decreasing = TRUE, na.last = NA)
    candidates <- candidates[ord]

    for(i in seq_along(candidates))
      candidates[[i]]$priority <- as.integer(i)
  }

  top_candidates <- head(candidates, 3L)


  # ----------------------------------------------------------
  # Отдельный локальный фокус на сотруднике
  # ----------------------------------------------------------
  # Если сотрудник не покрывает свои Personalkosten, эта проблема не должна
  # потеряться только потому, что другой рычаг сильнее влияет на общий net.

  employee_focus <- NULL

  if(
    isTRUE(state$employee$direct_billing) &&
    !isTRUE(employee_break_even_ok)
  ){
    employee_candidates <-
      Filter(
        function(x) identical(x$actor, "employee"),
        candidates
      )

    employee_focus_candidate <- NULL

    if(length(employee_candidates)){
      coverage_effects <-
        vapply(
          employee_candidates,
          function(x){
            z <- suppressWarnings(
              as.numeric(
                x$employee_cost_coverage_improvement_hours %||% NA_real_
              )[1]
            )
            if(is.finite(z)) z else -Inf
          },
          numeric(1)
        )

      if(any(is.finite(coverage_effects) & coverage_effects > 0)){
        employee_focus_candidate <-
          employee_candidates[[which.max(coverage_effects)]]
      }
    }

    employee_focus <-
      list(
        needed = TRUE,
        cost_coverage_ok = FALSE,
        hours_gap = as.numeric(employee_hours_gap),
        best_candidate = employee_focus_candidate
      )
  } else if(isTRUE(state$employee$direct_billing)){
    employee_focus <-
      list(
        needed = FALSE,
        cost_coverage_ok = TRUE,
        hours_gap = 0,
        best_candidate = NULL
      )
  }


  list(
    available = length(top_candidates) > 0L,
    method = "local_1pct_guidance_with_elasticity_and_utilization",
    objective = "improve_monthly_net",
    candidates = top_candidates,
    employee_focus = employee_focus,
    assumptions = list(
      price_elasticity = as.numeric(FBC_PRICE_ELASTICITY25),
      owner_utilization_thresholds = as.list(FBC_OWNER_UTIL_THRESHOLDS19),
      owner_utilization_weights = as.list(FBC_OWNER_UTIL_WEIGHTS19),
      employee_utilization_thresholds = as.list(FBC_EMPLOYEE_UTIL_THRESHOLDS19),
      employee_utilization_weights = as.list(FBC_EMPLOYEE_UTIL_WEIGHTS19)
    ),
    note = paste(
      "Orientation only: standardized local 1 percent scenarios.",
      "Price effects include demand elasticity.",
      "Additional-hour effects use the marginal utilization model.",
      "Real feasible bounds are confirmed by the user before P1 optimization."
    )
  )
}


fbc_current_financing_payload <- function(state){
  if(!isTRUE(state$financing$active)){
    return(list(status="no_financing",current=NULL,alternative=NULL,comparison=NULL))
  }
  list(
    status="current_financing_only",
    current=list(
      rate_pa=state$financing$rate_pa,
      interest_plus_fees_month=state$financing$interest_plus_fees_month,
      principal_month=state$financing$principal_month,
      debt_service_month=state$financing$debt_service_month
    ),
    alternative=NULL,
    comparison=NULL
  )
}

fbc_p0_payload <- function(cfg){
  state <- fbc_build_factual_state(cfg)

  owner_rev <- fbc_state_owner_revenue25(state)
  emp_rev <- fbc_state_employee_revenue25(state)
  op <- sum(state$operating_costs)
  pc <- state$employee$personnel_cost_month
  fin_result <- state$financing$interest_plus_fees_month
  before <- owner_rev + emp_rev - op - pc - fin_result

  financial <- fbc_financial_point24(
    before,
    state$owner,
    legal_form=as.character(cfg$legal_form %||% "freelance"),
    trade_tax_rate=num1(cfg$trade_tax_rate)
  )

  target <- num1(state$owner$monthly_target)
  gap <- max(0,target-financial$net_available)
  gap_pct <- if(target>0) 100*gap/target else NA_real_

  total_h <- fbc_state_expected_owner_hours25(state) +
    if(isTRUE(state$employee$direct_billing)) fbc_state_expected_employee_hours25(state) else 0
  revenue <- owner_rev+emp_rev
  weighted_price <- if(total_h>0) revenue/total_h else NA_real_

  variable_keys <- as.character(unlist(cfg$variable_cost_keys %||% character()))
  variable_keys <- intersect(variable_keys,names(state$operating_costs))
  variable_month <- if(length(variable_keys)) sum(state$operating_costs[variable_keys]) else 0
  variable_per_h <- if(total_h>0) variable_month/total_h else 0
  fixed_costs <- op-variable_month+pc+fin_result
  db_h <- weighted_price-variable_per_h
  be_hours <- if(is.finite(db_h) && db_h>0) fixed_costs/db_h else Inf
  be_revenue <- if(is.finite(be_hours) && is.finite(weighted_price)) be_hours*weighted_price else Inf
  be_margin <- revenue-be_revenue

emp_be <- NA_real_
if(isTRUE(state$employee$direct_billing)){
  if(!exists("fbc_employee_break_even_hours19", mode="function"))
    stop("fbc_employee_break_even_hours19() fehlt. 19_reality_constraints_V2.R zuerst laden.")

  emp_be <- fbc_employee_break_even_hours19(
    state,
    customer_price = state$employee$customer_price,
    variable_cost_per_hour = num1(cfg$employee_variable_cost_per_hour, 0)
  )
}

ds <- state$financing$debt_service_month
debt <- calc_current_debt_capacity19(state, revenue)
dscr <- if(is.finite(ds) && ds>0) debt$debt_service_ratio else NULL

sensitivity <-
  fbc_sensitivity_payload(
    state,
    cfg
  )

decision_guidance <-
  fbc_decision_guidance_payload(
    state = state,
    cfg = cfg,
    employee_break_even_hours = emp_be
  )

  list(
  schema_version="fbc_decision_payload_v1",
  status="no_evidenced_lever",

  sensitivity =
    sensitivity,

  decision_guidance =
    decision_guidance,

  current=list(
      expected_net=financial$net_available,
      monthly_target=target,
      target_gap_eur=gap,
      target_gap_percent=gap_pct
    ),
    recommendation=list(
      candidate_id="no_change_without_evidence",
      active_levers=list(),
      changes=list(),
      projected_net=financial$net_available,
      explanation=list(
        available=FALSE,
        sentence="Keine belastbare Entscheidungsgrenze bestätigt; daher keine operative Maßnahme aus dem Optimizer."
      )
    ),
    target_path=list(
      time_to_target_months=NULL,
      target_reached_within_horizon=(gap<=1e-9),
      implementation_months_max=NULL,
      # P0 has no evidenced implementation path. A target gap is not
      # automatically a temporary liquidity bridge.
      liquidity_bridge_need_eur=0,
      path=list()
    ),
    post_decision=list(
      post_target_stable=NULL,
      max_post_target_gap_eur=NULL,
      mc_target_probability=NULL,
      mc_policy_ok=NULL,
      business_break_even_margin_eur=be_margin,
      business_break_even_ok=is.finite(be_margin) && be_margin>=0,
      employee_break_even_ok=if(isTRUE(state$employee$direct_billing)) is.finite(emp_be) && fbc_state_expected_employee_hours25(state)>=emp_be else TRUE,
      capital_service_ratio=dscr,
      capital_service_ok=if(is.null(dscr)) TRUE else dscr>=1
    ),
    break_even=list(
      business=list(
        weighted_price_per_billable_hour=weighted_price,
        variable_cost_per_billable_hour=variable_per_h,
        fixed_result_costs_month=fixed_costs,
        break_even_hours=be_hours,
        break_even_revenue=be_revenue,
        projected_revenue=revenue,
        safety_margin=be_margin
      ),
      employee=list(
        applicable=isTRUE(state$employee$direct_billing),
        customer_price=state$employee$customer_price,
        personnel_cost_month=pc,
        planned_billable_hours=state$employee$billable_hours_month,
        billable_hours=fbc_state_expected_employee_hours25(state),
        expected_billable_hours=fbc_state_expected_employee_hours25(state),
        revenue_month=emp_rev,
        result_contribution_month=emp_rev-pc,
        break_even_hours=emp_be,
        hours_above_break_even=if(isTRUE(state$employee$direct_billing)) fbc_state_expected_employee_hours25(state)-emp_be else NULL
      )
    ),
    financing=fbc_current_financing_payload(state),
    robustness=list(
      target_probability=NULL,
      p10=NULL,p50=NULL,p90=NULL
    ),
    production_meta=list(
      input_schema=cfg$schema_version,
      backend="FBC_R_BACKEND_P0_1.0",
      candidate_count=0,
      optimizer_levers=list(),
      employment_form=as.character(cfg$employee$form %||% ""),
      financing_separate=TRUE,
      methods_used=c(
        "R factual state",
        "constant price elasticity epsilon = -0.60",
        "2026 tax orientation",
        "Business Break-even",
        "Employee Break-even",
        "Decision guidance: elasticity + utilization",
        if(isTRUE(state$financing$active)) "Kapitaldienst current-state" else NULL
      )
    ),
    audit=list(
      path="P0",
      evidence_gate="no confirmed bounds -> no lever",
      demand_guard="free capacity is not treated as demand",
      demand_model=list(model="constant_price_elasticity",epsilon=FBC_PRICE_ELASTICITY25),
      tax_disclosure_de=financial$disclosure_de,
      tax_disclosure_ru=financial$disclosure_ru,
      mc_executed=FALSE,
      optimizer_executed=FALSE
    )
  )
}

fbc_has_evidence <- function(cfg){
  b <- cfg$confirmed_bounds
  e <- cfg$cost_evidence
  (is.list(b) && length(b)>0L) || (is.list(e) && length(e)>0L)
}

fbc_runner39_cfg <- function(cfg){
  out <- cfg
  out$operating_costs <- fbc_map_costs(cfg$operating_costs)
  # Convert frontend financing names (bullet/line/zero) to the canonical
  # financing-engine names before deterministic P1 / Monte Carlo sees them.
  out$financing <- fbc_map_financing(cfg$financing)
  out$owner_holiday_days <- num1(cfg$owner$holidays)
  out$employee_holiday_days <- num1(cfg$employee$holidays)
  out$employer_addon_rate <- num1(cfg$employee$employer_addon_rate)
  out$owner_pension_month <- 0
  out$tax_month <- 0
  out$employee$paid_hours_month <- num1(cfg$employee$paid_available_hours_month)
  out$employee$contract_hours_week <- num1(cfg$employee$hours_week)
  out$mc_file <- Sys.getenv(
    "FBC_MC_FILE",
    unset=file.path(ROOT,"data","processed","fbc_monte_carlo_draws.csv")
  )
  out
}

health_handler <- function(req, res) {
  list(
    status = "ok",
    service = "FUTURE Business Cockpit R backend",
    backend_version = "FBC_R_BACKEND_P0_1.0",
    backend_build = "team-p0-role-constraint-2026-09-25",
    p0 = TRUE,
    team_p0 = TRUE,
    team_p1 = FALSE,
p1_runner_enabled = TRUE,
p1_runner_trigger = "evidence_present",
    mc_file_present =
      file.exists(
        Sys.getenv(
          "FBC_MC_FILE",
          unset = file.path(
            ROOT,
            "data",
            "processed",
            "fbc_monte_carlo_draws.csv"
          )
        )
      )
  )
}

analyze_handler <- function(req, res) {

  if (!fbc_authorized(req)) {
    res$status <- 401
    return(list(error = "unauthorized"))
  }

  cfg <- req$body
# JSON-массив Team может быть распознан plumber/jsonlite как data.frame.
# Преобразуем строки обратно в список сотрудников.
if(
  identical(cfg$mode, "owner_team") &&
  is.data.frame(cfg$employees)
){
  cfg$employees <-
    lapply(
      seq_len(nrow(cfg$employees)),
      function(i){
        as.list(
          cfg$employees[i, , drop = FALSE]
        )
      }
    )
}
  errors <- fbc_validate_request(cfg)

if (length(errors)) {
  res$status <- 422
  return(
    list(
      error = "invalid_fbc_request",
      fields = as.list(errors),
      received_mode = cfg$mode,
      received_mode_type = typeof(cfg$mode)
    )
  )
}

  # Team phase 1:
  # первый прогон уже считает экономику 1–5 сотрудников, sensitivity,
  # индивидуальную Kostendeckung и ориентацию без role-score.
  # P1 для Team будет подключен отдельно, когда в optimizer появятся
  # многосотрудниковые bounds и жесткое монотонное ограничение цен.
  if(identical(cfg$mode,"owner_team")){
    if(fbc_has_evidence(cfg)){
      res$status <- 422
      return(
        list(
          error = "team_p1_not_enabled_yet",
          message =
            "Team P0 ist aktiv; Team-P1 mit bestätigten Grenzen wird im nächsten Backend-Schritt angeschlossen."
        )
      )
    }

    return(
      fbc_team_p0_payload44(cfg)
    )
  }

  if (!fbc_has_evidence(cfg)) {
    return(fbc_p0_payload(cfg))
  }

  runner_cfg <- fbc_runner39_cfg(cfg)

source(
  file.path(ROOT, "43_run_p1_mc_fazit.R"),
  local = .GlobalEnv
)

tryCatch(
  {
payload <- fbc_run_p1_mc_fazit_43(
  cfg = runner_cfg,
  root = ROOT,
  mc_file = runner_cfg$mc_file
)
payload$sensitivity <- fbc_sensitivity_payload(
  fbc_build_factual_state(cfg),
  cfg
)


return(payload)
  },
    error = function(e){
      res$status <- 500

      return(
        list(
          error = "p1_deterministic_failed",
          message = conditionMessage(e)
        )
      )
    }
  )
}
