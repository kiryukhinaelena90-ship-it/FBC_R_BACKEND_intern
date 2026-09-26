# ============================================================
# 44_team_decision_support.R
# FUTURE Business Cockpit
# Team P0: экономика, чувствительность и предварительная ориентация
# ============================================================
#
# ВАЖНО:
# - роль сотрудника НЕ получает числовой "вес ценности";
# - иерархия ролей используется только как порядковое ограничение
#   для проверки логичности структуры клиентских цен;
# - экономический приоритет определяется фактическим эффектом на net_available;
# - одинаковый тестовый шаг 1 % позволяет сравнивать рычаги без
#   искусственного суммарного score;
# - реальные границы пользователь подтверждает позже, перед P1.
# ============================================================

`%||%` <- function(a,b) if(is.null(a) || length(a)==0L) b else a

FBC_TEAM_ROLE_ORDER44 <- c(
  "helper",
  "office",
  "skilled",
  "science",
  "lead"
)

FBC_TEAM_ROLE_LABELS44 <- c(
  helper = "Hilfskraft",
  office = "Büro / Verwaltung",
  skilled = "Fachkraft",
  science = "Spezialist",
  lead = "Leitung",
  custom = "Eigene Eingabe"
)

fbc_team_role_level44 <- function(role){
  role <- as.character(role %||% "")[1]

  # Число здесь служит только техническим индексом порядка.
  # Оно никогда не умножается на прибыль, цену или приоритет.
  idx <- match(role, FBC_TEAM_ROLE_ORDER44)

  if(is.na(idx)) NA_integer_ else as.integer(idx)
}

fbc_team_role_label44 <- function(role){
  role <- as.character(role %||% "")[1]
  x <- FBC_TEAM_ROLE_LABELS44[[role]]
  if(is.null(x)) role else unname(x)
}

fbc_team_num44 <- function(x, default=0){
  z <- suppressWarnings(as.numeric(x %||% default)[1])
  if(!is.finite(z)) default else z
}

fbc_team_flag44 <- function(x){
  isTRUE(x)
}

fbc_team_normalize_member44 <- function(x, index){
  role <- as.character(x$role %||% "custom")[1]
  direct <- fbc_team_flag44(x$direct_billing)

  id <- as.character(x$id %||% paste0("employee_", index))[1]
  if(!nzchar(id)) id <- paste0("employee_", index)

  role_label <- fbc_team_role_label44(role)

  label <- as.character(
    x$label %||%
      paste0("Mitarbeiter ", index, " · ", role_label)
  )[1]

  list(
    id = id,
    index = as.integer(index),
    label = label,
    role = role,
    role_label = role_label,
    role_level = fbc_team_role_level44(role),
    form = as.character(x$form %||% "part")[1],

    direct_billing = direct,
    customer_price =
      if(direct) max(0, fbc_team_num44(x$customer_price)) else 0,
    billable_hours_month =
      if(direct) max(0, fbc_team_num44(x$billable_hours_month)) else 0,

    paid_available_hours_month =
      max(
        0,
        fbc_team_num44(
          x$paid_available_hours_month %||%
            x$available_hours_month
        )
      ),

    personnel_cost_month =
      max(0, fbc_team_num44(x$personnel_cost_month)),

    variable_cost_per_hour =
      max(0, fbc_team_num44(x$variable_cost_per_hour)),

    wage_hour =
      max(0, fbc_team_num44(x$wage_hour)),

    hours_week =
      max(0, fbc_team_num44(x$hours_week)),

    days_week =
      max(1, fbc_team_num44(x$days_week, 5)),

    vacation_days =
      max(0, fbc_team_num44(x$vacation_days)),

    holidays =
      max(0, fbc_team_num44(x$holidays)),

    employer_addon_rate =
      max(0, fbc_team_num44(x$employer_addon_rate))
  )
}

fbc_build_team_factual_state44 <- function(cfg){
  employees <- cfg$employees %||% list()

  if(!is.list(employees))
    stop("cfg$employees muss eine Liste sein.")

  if(length(employees) < 1L || length(employees) > 5L)
    stop("Team-Modus unterstützt 1 bis 5 Mitarbeiter/innen.")

  owner <- cfg$owner

  # Модуль 18 используется для владельца, расходов и финансирования.
  # Технический employee здесь нулевой; реальные сотрудники команды
  # нормализуются отдельно ниже.
  state <- build_cockpit_decision_state18(
    owner = list(
      price = num1(owner$price),
      hours_week = num1(owner$hours_week),
      days_week = max(1, num1(owner$days_week, 5)),
      vacation_days = num1(owner$vacation_days),
      billable_hours_month = max(0, num1(owner$billable_hours_month)),
      monthly_target = num1(owner$monthly_target),
      insurance_month = num1(owner$insurance_month),
      pension_mode = as.character(owner$pension_mode %||% "none"),
      pension_fixed_month = num1(owner$pension_fixed_month)
    ),
    operating_costs = fbc_map_costs(cfg$operating_costs),
    employee = list(
      form = "part",
      wage_hour = 0,
      hours_week = 0,
      days_week = 5,
      vacation_days = 0,
      direct_billing = FALSE,
      billable_hours_month = 0,
      customer_price = 0,
      extra_cost_month = 0
    ),
    financing = fbc_map_financing(cfg$financing),
    owner_holiday_days = num1(owner$holidays),
    employee_holiday_days = 0,
    employer_addon_rate = 0
  )

  state$legal_form <- as.character(cfg$legal_form %||% "freelance")
  state$trade_tax_rate <- num1(cfg$trade_tax_rate)

  state$owner$physical_available_hours_month <-
    state$owner$available_hours_month

  state$owner$billable_hours_month <-
    max(0, num1(owner$billable_hours_month))

  state$employees <-
    lapply(
      seq_along(employees),
      function(i)
        fbc_team_normalize_member44(
          employees[[i]],
          i
        )
    )

  state
}

fbc_team_member_state44 <- function(base_state, member){
  s <- base_state

  s$employee <- list(
    direct_billing = isTRUE(member$direct_billing),
    billable_hours_month = member$billable_hours_month,
    expected_billable_hours_month = member$billable_hours_month,
    customer_price = member$customer_price,
    personnel_cost_month = member$personnel_cost_month,
    paid_available_hours_month = member$paid_available_hours_month,
    available_hours_month = member$paid_available_hours_month
  )

  s
}

fbc_team_member_price44 <- function(member, prices=NULL){
  if(is.null(prices))
    return(as.numeric(member$customer_price))

  price_names <- names(prices)

  # Если передан именованный вектор, изменяем только указанного сотрудника.
  # Это важно для локального теста +1 %: цена employee_2 не должна случайно
  # подменить цену employee_1 только потому, что длина вектора равна единице.
  if(!is.null(price_names) && any(nzchar(price_names))){
    if(member$id %in% price_names)
      return(max(0, fbc_team_num44(prices[[member$id]], member$customer_price)))

    return(as.numeric(member$customer_price))
  }

  if(length(prices) >= member$index)
    return(max(0, fbc_team_num44(prices[[member$index]], member$customer_price)))

  as.numeric(member$customer_price)
}

fbc_team_member_hours44 <- function(member, hours=NULL){
  if(is.null(hours))
    return(as.numeric(member$billable_hours_month))

  hour_names <- names(hours)

  # Та же защита для часов: локальный сценарий одного сотрудника
  # не должен менять часы других участников команды.
  if(!is.null(hour_names) && any(nzchar(hour_names))){
    if(member$id %in% hour_names)
      return(max(0, fbc_team_num44(hours[[member$id]], member$billable_hours_month)))

    return(as.numeric(member$billable_hours_month))
  }

  if(length(hours) >= member$index)
    return(max(0, fbc_team_num44(hours[[member$index]], member$billable_hours_month)))

  as.numeric(member$billable_hours_month)
}

fbc_team_eval_point44 <- function(
    state,
    cfg,
    owner_price = state$owner$price,
    owner_hours = state$owner$billable_hours_month,
    employee_prices = NULL,
    employee_hours = NULL,
    operating_costs = state$operating_costs
){
  owner_demand <-
    fbc_price_response25(
      base_price = state$owner$price,
      new_price = owner_price,
      planned_hours = owner_hours,
      epsilon = FBC_PRICE_ELASTICITY25
    )

  owner_revenue <-
    owner_demand$new_price *
      owner_demand$expected_hours

  employee_points <- vector("list", length(state$employees))

  employee_revenue <- 0
  personnel_cost <- 0
  member_variable_cost <- 0

  for(i in seq_along(state$employees)){
    member <- state$employees[[i]]

    personnel_cost <-
      personnel_cost +
      member$personnel_cost_month

    if(!isTRUE(member$direct_billing)){
      employee_points[[i]] <- list(
        id = member$id,
        label = member$label,
        role = member$role,
        role_level =
          if(is.finite(member$role_level))
            member$role_level
          else
            NULL,
        price = 0,
        planned_hours = 0,
        expected_hours = 0,
        revenue_month = 0,
        variable_cost_month = 0,
        demand = NULL
      )
      next
    }

    p <- fbc_team_member_price44(member, employee_prices)
    h <- fbc_team_member_hours44(member, employee_hours)

    demand <-
      fbc_price_response25(
        base_price = member$customer_price,
        new_price = p,
        planned_hours = h,
        epsilon = FBC_PRICE_ELASTICITY25
      )

    revenue <-
      demand$new_price *
        demand$expected_hours

    variable_cost <-
      member$variable_cost_per_hour *
        demand$expected_hours

    employee_revenue <-
      employee_revenue +
      revenue

    member_variable_cost <-
      member_variable_cost +
      variable_cost

    employee_points[[i]] <- list(
      id = member$id,
      label = member$label,
      role = member$role,
      role_level =
        if(is.finite(member$role_level))
          member$role_level
        else
          NULL,
      price = as.numeric(p),
      planned_hours = as.numeric(h),
      expected_hours = as.numeric(demand$expected_hours),
      revenue_month = as.numeric(revenue),
      variable_cost_month = as.numeric(variable_cost),
      demand = demand
    )
  }

  financing_result_cost <-
    fbc_team_num44(
      state$financing$interest_plus_fees_month,
      0
    )

  result_before_owner_protection_tax <-
    owner_revenue +
    employee_revenue -
    sum(operating_costs) -
    personnel_cost -
    member_variable_cost -
    financing_result_cost

  financial <-
    fbc_financial_point24(
      result_before_owner_protection_tax,
      state$owner,
      legal_form =
        as.character(cfg$legal_form %||% "freelance"),
      trade_tax_rate =
        num1(cfg$trade_tax_rate)
    )

  list(
    net_available = as.numeric(financial$net_available),
    result_before_owner_protection_tax =
      as.numeric(result_before_owner_protection_tax),
    owner_revenue = as.numeric(owner_revenue),
    employee_revenue = as.numeric(employee_revenue),
    revenue_total =
      as.numeric(owner_revenue + employee_revenue),
    operating_costs = as.numeric(sum(operating_costs)),
    personnel_costs = as.numeric(personnel_cost),
    member_variable_costs = as.numeric(member_variable_cost),
    financing_result_cost = as.numeric(financing_result_cost),
    owner_demand = owner_demand,
    employees = employee_points,
    financial = financial
  )
}

fbc_team_price_structure44 <- function(
    state,
    employee_prices = NULL
){
  billable <-
    Filter(
      function(x)
        isTRUE(x$direct_billing) &&
          is.finite(x$role_level),
      state$employees
    )

  violations <- list()

  if(length(billable) >= 2L){
    for(i in seq_along(billable)){
      for(j in seq_along(billable)){
        if(i == j) next

        lower <- billable[[i]]
        higher <- billable[[j]]

        if(lower$role_level >= higher$role_level)
          next

        p_low <- fbc_team_member_price44(lower, employee_prices)
        p_high <- fbc_team_member_price44(higher, employee_prices)

        # Только монотонность: более сложная роль не должна автоматически
        # оказаться дешевле более низкой. Никакого +5 %, +10 % и т.п.
        if(is.finite(p_low) && is.finite(p_high) && p_low > p_high + 1e-8){
          violations[[length(violations) + 1L]] <-
            list(
              lower_actor = lower$id,
              lower_label = lower$label,
              lower_role = lower$role,
              lower_price = as.numeric(p_low),
              higher_actor = higher$id,
              higher_label = higher$label,
              higher_role = higher$role,
              higher_price = as.numeric(p_high),
              rule = "higher_role_price_not_below_lower_role"
            )
        }
      }
    }
  }

  list(
    ok = length(violations) == 0L,
    comparable_roles =
      as.list(FBC_TEAM_ROLE_ORDER44),
    custom_role_comparable = FALSE,
    rule =
      "ordinal_monotonic_price_structure_without_fixed_percentage_gap",
    violations = violations
  )
}

fbc_team_employee_economics44 <- function(state){
  out <- vector("list", length(state$employees))

  for(i in seq_along(state$employees)){
    member <- state$employees[[i]]

    if(!isTRUE(member$direct_billing)){
      out[[i]] <- list(
        id = member$id,
        label = member$label,
        role = member$role,
        role_label = member$role_label,
        role_level =
          if(is.finite(member$role_level))
            member$role_level
          else
            NULL,
        direct_billing = FALSE,
        personnel_cost_month = member$personnel_cost_month,
        customer_price = NULL,
        planned_billable_hours = 0,
        expected_billable_hours = 0,
        revenue_month = 0,
        variable_cost_month = 0,
        result_contribution_month =
          -member$personnel_cost_month,
        economic_factor = NULL,
        break_even_hours = NULL,
        hours_above_break_even = NULL,
        cost_coverage_ok = NULL,
        utilization_rate = NULL,
        utilization_zone = "not_applicable",
        status = "internal_non_billable"
      )
      next
    }

    member_state <-
      fbc_team_member_state44(
        state,
        member
      )

    util <-
      fbc_employee_utilization19(
        member_state,
        billable_hours =
          member$billable_hours_month
      )

    be <-
      fbc_employee_break_even_hours19(
        member_state,
        customer_price = member$customer_price,
        variable_cost_per_hour =
          member$variable_cost_per_hour
      )

    demand <-
      fbc_price_response25(
        base_price = member$customer_price,
        new_price = member$customer_price,
        planned_hours =
          member$billable_hours_month,
        epsilon = FBC_PRICE_ELASTICITY25
      )

    expected_hours <-
      as.numeric(demand$expected_hours)

    revenue <-
      member$customer_price *
        expected_hours

    variable_cost <-
      member$variable_cost_per_hour *
        expected_hours

    contribution <-
      revenue -
      variable_cost -
      member$personnel_cost_month

    economic_factor <-
      if(member$personnel_cost_month > 0)
        (revenue - variable_cost) /
          member$personnel_cost_month
      else
        NA_real_

    covered <-
      is.finite(be) &&
      expected_hours + 1e-8 >= be

    out[[i]] <- list(
      id = member$id,
      label = member$label,
      role = member$role,
      role_label = member$role_label,
      role_level =
        if(is.finite(member$role_level))
          member$role_level
        else
          NULL,
      direct_billing = TRUE,
      personnel_cost_month =
        as.numeric(member$personnel_cost_month),
      customer_price =
        as.numeric(member$customer_price),
      planned_billable_hours =
        as.numeric(member$billable_hours_month),
      expected_billable_hours =
        as.numeric(expected_hours),
      revenue_month =
        as.numeric(revenue),
      variable_cost_month =
        as.numeric(variable_cost),
      result_contribution_month =
        as.numeric(contribution),
      economic_factor =
        if(is.finite(economic_factor))
          as.numeric(economic_factor)
        else
          NULL,
      break_even_hours =
        if(is.finite(be))
          as.numeric(be)
        else
          NULL,
      hours_above_break_even =
        if(is.finite(be))
          as.numeric(expected_hours - be)
        else
          NULL,
      cost_coverage_ok =
        isTRUE(covered),
      utilization_rate =
        if(is.finite(util$utilization_rate))
          as.numeric(util$utilization_rate)
        else
          NULL,
      utilization_zone =
        as.character(util$zone),
      effective_billable_hours =
        as.numeric(util$effective_hours),
      available_hours_month =
        if(is.finite(util$available_hours))
          as.numeric(util$available_hours)
        else
          NULL,
      status =
        if(isTRUE(covered))
          "covered"
        else
          "coverage_open"
    )
  }

  out
}

fbc_team_sensitivity_payload44 <- function(
    state,
    cfg
){
  base <-
    fbc_team_eval_point44(
      state,
      cfg
    )

  base_net <- base$net_available
  rows <- list()

  make_row <- function(
      actor,
      actor_label,
      role,
      role_level,
      lever,
      label,
      perturbation_pct,
      current,
      changed_net
  ){
    delta <- as.numeric(changed_net) - base_net

    delta_pct <-
      if(is.finite(base_net) && abs(base_net) > 1e-9)
        100 * delta / abs(base_net)
      else
        NA_real_

    list(
      actor = actor,
      actor_label = actor_label,
      role = role,
      role_level =
        if(is.finite(role_level))
          as.integer(role_level)
        else
          NULL,
      lever = lever,
      label = label,
      current = as.numeric(current),
      perturbation_pct =
        as.numeric(perturbation_pct),
      net_change_eur =
        as.numeric(delta),
      net_change_pct =
        if(is.finite(delta_pct))
          as.numeric(delta_pct)
        else
          NULL
    )
  }

  if(state$owner$price > 0){
    p <-
      fbc_team_eval_point44(
        state,
        cfg,
        owner_price =
          state$owner$price * 1.01
      )

    rows[[length(rows) + 1L]] <-
      make_row(
        "owner",
        "Inhaber/in",
        NULL,
        NA_real_,
        "owner_price",
        "Kundenpreis Inhaber/in",
        1,
        state$owner$price,
        p$net_available
      )
  }

  if(state$owner$billable_hours_month > 0){
    p <-
      fbc_team_eval_point44(
        state,
        cfg,
        owner_hours =
          state$owner$billable_hours_month * 1.01
      )

    rows[[length(rows) + 1L]] <-
      make_row(
        "owner",
        "Inhaber/in",
        NULL,
        NA_real_,
        "owner_hours",
        "Abrechenbare Stunden Inhaber/in",
        1,
        state$owner$billable_hours_month,
        p$net_available
      )
  }

  for(member in state$employees){
    if(!isTRUE(member$direct_billing))
      next

    if(member$customer_price > 0){
      prices <- setNames(
        member$customer_price * 1.01,
        member$id
      )

      p <-
        fbc_team_eval_point44(
          state,
          cfg,
          employee_prices = prices
        )

      rows[[length(rows) + 1L]] <-
        make_row(
          member$id,
          member$label,
          member$role,
          member$role_level,
          paste0(member$id, "_customer_price"),
          paste0("Kundenpreis · ", member$label),
          1,
          member$customer_price,
          p$net_available
        )
    }

    if(member$billable_hours_month > 0){
      hours <- setNames(
        member$billable_hours_month * 1.01,
        member$id
      )

      p <-
        fbc_team_eval_point44(
          state,
          cfg,
          employee_hours = hours
        )

      rows[[length(rows) + 1L]] <-
        make_row(
          member$id,
          member$label,
          member$role,
          member$role_level,
          paste0(member$id, "_billable_hours"),
          paste0("Abrechenbare Stunden · ", member$label),
          1,
          member$billable_hours_month,
          p$net_available
        )
    }
  }

  operating_total <- sum(state$operating_costs)

  if(operating_total > 0){
    p <-
      fbc_team_eval_point44(
        state,
        cfg,
        operating_costs =
          state$operating_costs * 0.99
      )

    rows[[length(rows) + 1L]] <-
      make_row(
        "business",
        "Betrieb",
        NULL,
        NA_real_,
        "operating_costs",
        "Betriebskosten gesamt",
        -1,
        operating_total,
        p$net_available
      )
  }

  abs_effect <-
    vapply(
      rows,
      function(x)
        abs(
          fbc_team_num44(
            x$net_change_pct,
            0
          )
        ),
      numeric(1)
    )

  dominant <-
    if(length(abs_effect))
      rows[[which.max(abs_effect)]]
    else
      NULL

  list(
    method =
      "local_1pct_financial_sensitivity_team",
    base_net =
      as.numeric(base_net),
    local = rows,
    dominant_lever =
      dominant$lever %||% NULL,
    dominant_label =
      dominant$label %||% NULL,
    demand_model = list(
      model = "constant_price_elasticity",
      epsilon =
        as.numeric(FBC_PRICE_ELASTICITY25)
    ),
    note = paste(
      "Each Team lever is changed separately by one percent.",
      "Role level is not used as an economic score."
    )
  )
}

fbc_team_guidance_payload44 <- function(
    state,
    cfg,
    economics
){
  base <-
    fbc_team_eval_point44(
      state,
      cfg
    )

  base_net <- base$net_available
  candidates <- list()

  econ_by_id <-
    setNames(
      economics,
      vapply(
        economics,
        function(x)
          as.character(x$id),
        character(1)
      )
    )

  add_candidate <- function(
      actor,
      actor_label,
      role = NULL,
      role_level = NA_real_,
      lever,
      lever_label,
      direction,
      guidance_net,
      raw_net = guidance_net,
      test_change_pct,
      reason_codes,
      utilization = NULL,
      feasibility_hint =
        "needs_user_confirmation",
      coverage_improvement_hours =
        NULL
  ){
    effect <-
      as.numeric(guidance_net) -
      base_net

    raw_effect <-
      as.numeric(raw_net) -
      base_net

    if(!is.finite(effect) || effect <= 0)
      return(invisible(NULL))

    candidates[[length(candidates) + 1L]] <<-
      list(
        actor = actor,
        actor_label = actor_label,
        role = role,
        role_level =
          if(is.finite(role_level))
            as.integer(role_level)
          else
            NULL,
        lever = lever,
        lever_label = lever_label,
        direction = direction,
        test_change_pct =
          as.numeric(test_change_pct),
        guidance_net_effect_eur_1pct =
          as.numeric(effect),
        commercial_net_effect_eur_1pct =
          if(is.finite(raw_effect))
            as.numeric(raw_effect)
          else
            NULL,
        net_effect_eur_1pct =
          as.numeric(effect),
        utilization = utilization,
        feasibility_hint =
          feasibility_hint,
        employee_cost_coverage_improvement_hours =
          if(
            !is.null(coverage_improvement_hours) &&
            is.finite(
              as.numeric(
                coverage_improvement_hours
              )[1]
            )
          )
            as.numeric(
              coverage_improvement_hours
            )[1]
          else
            NULL,
        reason_codes =
          as.list(unique(reason_codes))
      )

    invisible(NULL)
  }

  # ----------------------------------------------------------
  # Владелец: цена
  # ----------------------------------------------------------
  if(state$owner$price > 0){
    p <-
      fbc_team_eval_point44(
        state,
        cfg,
        owner_price =
          state$owner$price * 1.01
      )

    add_candidate(
      actor = "owner",
      actor_label = "Inhaber/in",
      lever = "owner_price",
      lever_label = "Kundenpreis Inhaber/in",
      direction = "increase",
      guidance_net = p$net_available,
      test_change_pct = 1,
      reason_codes =
        "positive_price_effect_after_elasticity"
    )
  }

  # ----------------------------------------------------------
  # Владелец: часы с учетом маржинальной зоны загрузки
  # ----------------------------------------------------------
owner_h0 <-
  as.numeric(
    state$owner$billable_hours_month
  )

owner_cap <-
  fbc_team_num44(
    state$owner$physical_available_hours_month,
    NA_real_
  )

if(is.finite(owner_h0) && owner_h0 > 0){

  owner_h1 <-
    if(is.finite(owner_cap))
      min(
        owner_h0 * 1.01,
        owner_cap
      )
    else
      owner_h0 * 1.01

    u0 <-
      fbc_owner_utilization19(
        state,
        billable_hours = owner_h0
      )

    u1 <-
      fbc_owner_utilization19(
        state,
        billable_hours = owner_h1
      )

    raw_delta <- owner_h1 - owner_h0

    effective_delta <-
      max(
        0,
        as.numeric(u1$effective_hours) -
          as.numeric(u0$effective_hours)
      )

    marginal_weight <-
      if(raw_delta > 0)
        effective_delta / raw_delta
      else
        NA_real_

    if(!identical(as.character(u0$zone), "overload")){
      raw_point <-
        fbc_team_eval_point44(
          state,
          cfg,
          owner_hours = owner_h1
        )

      guidance_point <-
        fbc_team_eval_point44(
          state,
          cfg,
          owner_hours =
            owner_h0 + effective_delta
        )

      add_candidate(
        actor = "owner",
        actor_label = "Inhaber/in",
        lever = "owner_hours",
        lever_label =
          "Abrechenbare Stunden Inhaber/in",
        direction = "increase",
        guidance_net =
          guidance_point$net_available,
        raw_net =
          raw_point$net_available,
        test_change_pct = 1,
        reason_codes = c(
          "positive_hours_effect_with_utilization",
          paste0(
            "utilization_",
            as.character(u0$zone)
          )
        ),
        utilization = list(
          utilization_rate =
            if(is.finite(u0$utilization_rate))
              as.numeric(u0$utilization_rate)
            else
              NULL,
          utilization_zone =
            as.character(u0$zone),
          marginal_hour_weight =
            if(is.finite(marginal_weight))
              as.numeric(marginal_weight)
            else
              NULL,
          available_hours_month =
            if(is.finite(u0$available_hours))
              as.numeric(u0$available_hours)
            else
              NULL
        )
      )
    }
  }

  # ----------------------------------------------------------
  # Каждый сотрудник: цена и часы
  # ----------------------------------------------------------
  for(member in state$employees){
    if(!isTRUE(member$direct_billing))
      next

    econ <- econ_by_id[[member$id]]
    current_be <-
      fbc_team_num44(
        econ$break_even_hours,
        NA_real_
      )

    current_expected <-
      fbc_team_num44(
        econ$expected_billable_hours,
        member$billable_hours_month
      )

    current_margin <-
      if(is.finite(current_be))
        current_expected - current_be
      else
        NA_real_

    coverage_open <-
      identical(
        econ$status,
        "coverage_open"
      )

    prefix <-
      if(coverage_open)
        "employee_cost_coverage_open"
      else
        character(0)

    # Цена +1 %: через ту же elasticity.
    if(member$customer_price > 0){
      test_price <-
        member$customer_price * 1.01

      prices <-
        setNames(
          test_price,
          member$id
        )

      p <-
        fbc_team_eval_point44(
          state,
          cfg,
          employee_prices = prices
        )

      member_state <-
        fbc_team_member_state44(
          state,
          member
        )

      be_test <-
        fbc_employee_break_even_hours19(
          member_state,
          customer_price = test_price,
          variable_cost_per_hour =
            member$variable_cost_per_hour
        )

      expected_test <-
        fbc_price_response25(
          base_price =
            member$customer_price,
          new_price =
            test_price,
          planned_hours =
            member$billable_hours_month,
          epsilon =
            FBC_PRICE_ELASTICITY25
        )$expected_hours

      test_margin <-
        if(is.finite(be_test))
          as.numeric(expected_test) -
            be_test
        else
          NA_real_

      coverage_improvement <-
        if(
          is.finite(current_margin) &&
          is.finite(test_margin)
        )
          test_margin - current_margin
        else
          NA_real_

      structure <-
        fbc_team_price_structure44(
          state,
          employee_prices = prices
        )

      introduces_conflict <-
        any(
          vapply(
            structure$violations,
            function(v)
              identical(v$lower_actor, member$id) ||
              identical(v$higher_actor, member$id),
            logical(1)
          )
        )

      add_candidate(
        actor = member$id,
        actor_label = member$label,
        role = member$role,
        role_level = member$role_level,
        lever =
          paste0(
            member$id,
            "_customer_price"
          ),
        lever_label =
          paste0(
            "Kundenpreis · ",
            member$label
          ),
        direction = "increase",
        guidance_net =
          p$net_available,
        test_change_pct = 1,
        reason_codes = c(
          prefix,
          "positive_price_effect_after_elasticity",
          if(introduces_conflict)
            "role_price_structure_check"
          else
            NULL
        ),
        feasibility_hint =
          if(introduces_conflict)
            "role_price_structure_check"
          else
            "needs_user_confirmation",
        coverage_improvement_hours =
          coverage_improvement
      )
    }

    # Часы +1 %: экономический эффект + коэффициент текущей загрузки.
    h0 <- member$billable_hours_month

    if(h0 > 0){
      h1 <- h0 * 1.01

      member_state <-
        fbc_team_member_state44(
          state,
          member
        )

      u0 <-
        fbc_employee_utilization19(
          member_state,
          billable_hours = h0
        )

      u1 <-
        fbc_employee_utilization19(
          member_state,
          billable_hours = h1
        )

      raw_delta <- h1 - h0

      effective_delta <-
        max(
          0,
          as.numeric(u1$effective_hours) -
            as.numeric(u0$effective_hours)
        )

      marginal_weight <-
        if(raw_delta > 0)
          effective_delta / raw_delta
        else
          NA_real_

      if(!identical(as.character(u0$zone), "overload")){
        raw_hours <- setNames(h1, member$id)
        guidance_hours <-
          setNames(
            h0 + effective_delta,
            member$id
          )

        raw_point <-
          fbc_team_eval_point44(
            state,
            cfg,
            employee_hours =
              raw_hours
          )

        guidance_point <-
          fbc_team_eval_point44(
            state,
            cfg,
            employee_hours =
              guidance_hours
          )

        coverage_improvement <-
          if(is.finite(current_be))
            h1 - h0
          else
            NA_real_

        add_candidate(
          actor = member$id,
          actor_label = member$label,
          role = member$role,
          role_level = member$role_level,
          lever =
            paste0(
              member$id,
              "_billable_hours"
            ),
          lever_label =
            paste0(
              "Abrechenbare Stunden · ",
              member$label
            ),
          direction = "increase",
          guidance_net =
            guidance_point$net_available,
          raw_net =
            raw_point$net_available,
          test_change_pct = 1,
          reason_codes = c(
            prefix,
            "positive_hours_effect_with_utilization",
            paste0(
              "utilization_",
              as.character(u0$zone)
            )
          ),
          utilization = list(
            utilization_rate =
              if(is.finite(u0$utilization_rate))
                as.numeric(u0$utilization_rate)
              else
                NULL,
            utilization_zone =
              as.character(u0$zone),
            marginal_hour_weight =
              if(is.finite(marginal_weight))
                as.numeric(marginal_weight)
              else
                NULL,
            available_hours_month =
              if(is.finite(u0$available_hours))
                as.numeric(u0$available_hours)
              else
                NULL
          ),
          coverage_improvement_hours =
            coverage_improvement
        )
      }
    }
  }

  # ----------------------------------------------------------
  # Betriebskosten -1 %
  # ----------------------------------------------------------
  op <- sum(state$operating_costs)

  if(is.finite(op) && op > 0){
    p <-
      fbc_team_eval_point44(
        state,
        cfg,
        operating_costs =
          state$operating_costs * 0.99
      )

    add_candidate(
      actor = "business",
      actor_label = "Betrieb",
      lever = "operating_costs",
      lever_label = "Betriebskosten gesamt",
      direction = "decrease",
      guidance_net = p$net_available,
      test_change_pct = -1,
      reason_codes =
        "positive_cost_reduction_effect"
    )
  }

  # ----------------------------------------------------------
  # Глобальный порядок: только фактический эффект на net_available.
  # Роль НЕ является множителем или score.
  # ----------------------------------------------------------
  if(length(candidates)){
    effects <-
      vapply(
        candidates,
        function(x)
          fbc_team_num44(
            x$guidance_net_effect_eur_1pct,
            -Inf
          ),
        numeric(1)
      )

    ord <-
      order(
        effects,
        decreasing = TRUE,
        na.last = NA
      )

    candidates <- candidates[ord]

    for(i in seq_along(candidates))
      candidates[[i]]$priority <-
        as.integer(i)
  }

  # ----------------------------------------------------------
  # Отдельный фокус по каждому сотруднику с открытой Kostendeckung.
  # Сильный рычаг всего бизнеса и лучший рычаг конкретного сотрудника
  # могут быть разными — поэтому эти два вывода не смешиваются.
  # ----------------------------------------------------------
  employee_focus <- list()

  for(econ in economics){
    if(!identical(econ$status, "coverage_open"))
      next

    actor_candidates <-
      Filter(
        function(x)
          identical(
            x$actor,
            econ$id
          ),
        candidates
      )

    best <- NULL

    if(length(actor_candidates)){
      coverage_effects <-
        vapply(
          actor_candidates,
          function(x)
            fbc_team_num44(
              x$employee_cost_coverage_improvement_hours,
              -Inf
            ),
          numeric(1)
        )

      if(any(is.finite(coverage_effects) & coverage_effects > 0))
best <-
  actor_candidates[[which.max(coverage_effects)]]
    }

    employee_focus[[length(employee_focus) + 1L]] <-
      list(
        actor = econ$id,
        actor_label = econ$label,
        role = econ$role,
        coverage_open = TRUE,
        hours_gap =
          max(
            0,
            -fbc_team_num44(
              econ$hours_above_break_even,
              0
            )
          ),
        best_candidate = best
      )
  }

  # ----------------------------------------------------------
  # Видимая управленческая ориентация для Team
  # ----------------------------------------------------------
  # Важно различать:
  # 1) глобальную чувствительность общего net_available;
  # 2) структурную проблему конкретного сотрудника, который пока
  #    не покрывает собственные затраты.
  #
  # Поэтому сначала выводим сотрудников с открытой Kostendeckung,
  # а затем отдельным пунктом — самый сильный общий рычаг бизнеса.
  # Для Team-P1 при этом по-прежнему подтверждается максимум 3 рычага.
  # ----------------------------------------------------------

  global_candidates <-
    head(candidates, 3L)

  display_guidance <- list()
  confirmation_candidates <- list()
  used_confirmation_levers <- character()

  add_confirmation_candidate <- function(x){
    if(is.null(x) || !is.list(x))
      return(invisible(NULL))

    lever <- as.character(x$lever %||% "")[1]

    if(
      !nzchar(lever) ||
      lever %in% used_confirmation_levers ||
      length(confirmation_candidates) >= 3L
    )
      return(invisible(NULL))

    confirmation_candidates[[length(confirmation_candidates) + 1L]] <<- x
    used_confirmation_levers <<-
      c(
        used_confirmation_levers,
        lever
      )

    invisible(NULL)
  }

  # Каждый сотрудник с открытым покрытием получает отдельный
  # структурный сигнал независимо от размера его 1%-эффекта на общий net.
  for(focus in employee_focus){
    actor_candidates <-
      Filter(
        function(x)
          identical(
            x$actor,
            focus$actor
          ),
        candidates
      )

    suggested_levers <-
      if(length(actor_candidates))
        as.list(
          vapply(
            actor_candidates,
            function(x)
              as.character(x$lever),
            character(1)
          )
        )
      else
        list()

    display_guidance[[length(display_guidance) + 1L]] <-
      list(
        guidance_type =
          "employee_cost_coverage",
        actor =
          focus$actor,
        actor_label =
          focus$actor_label,
        role =
          focus$role,
        coverage_open =
          TRUE,
        hours_gap =
          focus$hours_gap,
        suggested_levers =
          suggested_levers,
        best_candidate =
          focus$best_candidate
      )

    add_confirmation_candidate(
      focus$best_candidate
    )
  }

  # Самый сильный общий рычаг показываем отдельно от структурных
  # проблем сотрудников. Если возможно, избегаем повторения того же
  # сотрудника, который уже выведен как coverage_open.
  global_for_display <- NULL

  if(length(candidates)){
    uncovered_actors <-
      if(length(employee_focus))
        vapply(
          employee_focus,
          function(x)
            as.character(x$actor),
          character(1)
        )
      else
        character()

    distinct_global <-
      Filter(
        function(x)
          !as.character(x$actor) %in%
            uncovered_actors,
        candidates
      )

    global_for_display <-
      if(length(distinct_global))
        distinct_global[[1]]
      else
        candidates[[1]]

    global_item <- global_for_display
    global_item$guidance_type <-
      "global_strongest"

    display_guidance[[length(display_guidance) + 1L]] <-
      global_item

    add_confirmation_candidate(
      global_for_display
    )
  }

  # Если после структурных сигналов осталось меньше трех предложений
  # для подтверждения, заполняем свободные места следующими сильными
  # глобальными кандидатами без дублей.
  if(length(candidates)){
    for(x in candidates){
      if(length(confirmation_candidates) >= 3L)
        break

      add_confirmation_candidate(x)
    }
  }

  list(
    available =
      length(display_guidance) > 0L ||
      length(confirmation_candidates) > 0L,
    method =
      "team_structural_coverage_plus_global_guidance_without_role_score",
    objective =
      "improve_monthly_net",
    # Backward-compatible field used by older frontend versions:
    # max. 3 concrete levers suggested for Team-P1 confirmation.
    candidates =
      confirmation_candidates,
    # Pure global ranking remains available separately for audit/display.
    global_candidates =
      global_candidates,
    # Human-facing guidance: uncovered employees first, then strongest
    # overall business lever.
    display_guidance =
      display_guidance,
    confirmation_candidates =
      confirmation_candidates,
    employee_focus =
      employee_focus,
    role_price_structure =
      fbc_team_price_structure44(state),
    assumptions = list(
      role_priority_model =
        "ordinal_constraint_only_no_economic_score",
      price_elasticity =
        as.numeric(
          FBC_PRICE_ELASTICITY25
        ),
      owner_utilization_thresholds =
        as.list(
          FBC_OWNER_UTIL_THRESHOLDS19
        ),
      owner_utilization_weights =
        as.list(
          FBC_OWNER_UTIL_WEIGHTS19
        ),
      employee_utilization_thresholds =
        as.list(
          FBC_EMPLOYEE_UTIL_THRESHOLDS19
        ),
      employee_utilization_weights =
        as.list(
          FBC_EMPLOYEE_UTIL_WEIGHTS19
        )
    ),
    note = paste(
      "Role hierarchy is used only as an ordinal price-structure check.",
      "Economic priority is based on standardized local change in net_available.",
      "No fixed percentage gap between roles is imposed."
    )
  )
}

fbc_team_business_break_even44 <- function(
    state,
    cfg,
    point
){
  expected_hours <-
    as.numeric(
      point$owner_demand$expected_hours
    )

  if(length(point$employees)){
    expected_hours <-
      expected_hours +
      sum(
        vapply(
          point$employees,
          function(x)
            fbc_team_num44(
              x$expected_hours,
              0
            ),
          numeric(1)
        )
      )
  }

  revenue <-
    point$revenue_total

  weighted_price <-
    if(expected_hours > 0)
      revenue / expected_hours
    else
      NA_real_

  variable_keys <-
    as.character(
      unlist(
        cfg$variable_cost_keys %||%
          character()
      )
    )

  variable_keys <-
    intersect(
      variable_keys,
      names(state$operating_costs)
    )

  op_variable <-
    if(length(variable_keys))
      sum(
        state$operating_costs[
          variable_keys
        ]
      )
    else
      0

  variable_total <-
    op_variable +
    point$member_variable_costs

  variable_per_hour <-
    if(expected_hours > 0)
      variable_total / expected_hours
    else
      0

  fixed_result_costs <-
    sum(state$operating_costs) -
    op_variable +
    point$personnel_costs +
    point$financing_result_cost

  db_per_hour <-
    weighted_price -
    variable_per_hour

  reachable <-
    is.finite(db_per_hour) &&
    db_per_hour > 0

  be_hours <-
    if(reachable)
      fixed_result_costs /
        db_per_hour
    else
      Inf

  be_revenue <-
    if(
      reachable &&
      is.finite(weighted_price)
    )
      be_hours *
        weighted_price
    else
      Inf

  safety_margin <-
    revenue -
    be_revenue

  list(
    weighted_price_per_billable_hour =
      if(is.finite(weighted_price))
        as.numeric(weighted_price)
      else
        NULL,
    variable_cost_per_billable_hour =
      as.numeric(variable_per_hour),
    fixed_result_costs_month =
      as.numeric(fixed_result_costs),
    break_even_hours =
      if(is.finite(be_hours))
        as.numeric(be_hours)
      else
        NULL,
    break_even_revenue =
      if(is.finite(be_revenue))
        as.numeric(be_revenue)
      else
        NULL,
    projected_revenue =
      as.numeric(revenue),
    safety_margin =
      if(is.finite(safety_margin))
        as.numeric(safety_margin)
      else
        NULL,
    ok =
      isTRUE(reachable) &&
      is.finite(safety_margin) &&
      safety_margin >= -1e-8,
    note =
      "Team Business Break-even uses a weighted average price only as a safety diagnostic."
  )
}

fbc_team_p0_payload44 <- function(cfg){
  state <-
    fbc_build_team_factual_state44(cfg)

  point <-
    fbc_team_eval_point44(
      state,
      cfg
    )

  economics <-
    fbc_team_employee_economics44(
      state
    )

  sensitivity <-
    fbc_team_sensitivity_payload44(
      state,
      cfg
    )

  guidance <-
    fbc_team_guidance_payload44(
      state,
      cfg,
      economics
    )

  business_be <-
    fbc_team_business_break_even44(
      state,
      cfg,
      point
    )

  target <-
    fbc_team_num44(
      state$owner$monthly_target,
      0
    )

  gap <-
    max(
      0,
      target -
      point$net_available
    )

  billable_coverage <-
    Filter(
      function(x)
        isTRUE(x$direct_billing),
      economics
    )

  all_billable_covered <-
    if(length(billable_coverage))
      all(
        vapply(
          billable_coverage,
          function(x)
            isTRUE(x$cost_coverage_ok),
          logical(1)
        )
      )
    else
      TRUE

  financing_payload <-
    if(exists(
      "fbc_current_financing_payload",
      mode = "function"
    ))
      fbc_current_financing_payload(state)
    else
      list(
        status =
          if(isTRUE(state$financing$active))
            "current_financing_only"
          else
            "no_financing"
      )

  list(
    schema_version =
      "fbc_team_decision_payload_v1",
    status =
      "team_p0_no_evidenced_lever",

    current = list(
      expected_net =
        as.numeric(point$net_available),
      monthly_target =
        as.numeric(target),
      target_gap_eur =
        as.numeric(gap),
      target_gap_percent =
        if(target > 0)
          as.numeric(100 * gap / target)
        else
          NULL,
      revenue_total =
        as.numeric(point$revenue_total),
      owner_revenue =
        as.numeric(point$owner_revenue),
      employee_revenue =
        as.numeric(point$employee_revenue),
      operating_costs =
        as.numeric(point$operating_costs),
      personnel_costs =
        as.numeric(point$personnel_costs),
      result_before_owner_protection_tax =
        as.numeric(
          point$result_before_owner_protection_tax
        )
    ),

    sensitivity = sensitivity,

    team_economics = list(
      employees = economics,
      uncovered_count =
        sum(
          vapply(
            economics,
            function(x)
              identical(
                x$status,
                "coverage_open"
              ),
            logical(1)
          )
        ),
      role_price_structure =
        fbc_team_price_structure44(state)
    ),

    decision_guidance = guidance,

    break_even = list(
      business = business_be,
      employees = economics
    ),

    recommendation = list(
      candidate_id =
        "no_change_without_evidence",
      active_levers = list(),
      changes = list(),
      projected_net =
        as.numeric(point$net_available),
      explanation = list(
        available = FALSE,
        sentence =
          "Noch keine realistischen Änderungsgrenzen bestätigt; daher noch keine Team-Optimierung."
      )
    ),

    post_decision = list(
      business_break_even_ok =
        isTRUE(business_be$ok),
      employee_break_even_ok =
        isTRUE(all_billable_covered),
      # Для Team-P0 пока не выдаем положительное заключение по DSCR:
      # текущая single-employee функция не учитывает суммарную Team-выручку.
      capital_service_ok = NULL
    ),

    financing = financing_payload,

    robustness = list(
      target_probability = NULL,
      p10 = NULL,
      p50 = NULL,
      p90 = NULL
    ),

    production_meta = list(
      backend =
        "FBC_R_BACKEND_TEAM_P0_1.0",
      team_member_count =
        length(state$employees),
      optimizer_executed = FALSE,
      financing_separate = TRUE,
      role_priority_model =
        "ordinal_constraint_only_no_score",
      methods_used = c(
        "Team factual state",
        "constant price elasticity epsilon = -0.60",
        "piecewise sustainable utilization",
        "employee cost coverage",
        "ordinal role price-structure check",
        "local 1 percent decision guidance"
      )
    ),

    audit = list(
      path = "TEAM_P0",
      evidence_gate =
        "team P1 requires 1 to 3 user-confirmed bounds",
      role_priority_guard =
        "role order is never multiplied into economic effect",
      fixed_role_gap =
        FALSE,
      mc_executed = FALSE,
      optimizer_executed = FALSE
    )
  )
}
