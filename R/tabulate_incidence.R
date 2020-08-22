##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param final_predictions
tabulate_incidence <- function(final_predictions, data) {

  events <- data %>%
    select(time, status, m0_device_ty, m0_px_profile, patient_id)

  tbl_data <- final_predictions %>%
    left_join(events, by = 'patient_id') %>%
    mutate(
      risk_cat_m1 = case_when(
        m0.00_risk_high == 'yes' & m1.00_risk_high == 'yes' ~ 'both_high',
        m0.00_risk_high == 'no'  & m1.00_risk_high == 'no' ~ 'both_low',
        m0.00_risk_high == 'no'  & m1.00_risk_high == 'yes' ~ 'm1_only',
        m0.00_risk_high == 'yes' & m1.00_risk_high == 'no' ~ 'm0_only'
      ),
      risk_cat_w1 = case_when(
        m0.00_risk_high == 'yes' & m0.25_risk_high == 'yes' ~ 'both_high',
        m0.00_risk_high == 'no'  & m0.25_risk_high == 'no' ~ 'both_low',
        m0.00_risk_high == 'no'  & m0.25_risk_high == 'yes' ~ 'm1_only',
        m0.00_risk_high == 'yes' & m0.25_risk_high == 'no' ~ 'm0_only'
      ),
      across(
        starts_with('risk_cat'),
        ~ fct_relevel(.x, 'both_low', 'm0_only', 'm1_only')
      )
    ) %>%
    select(-matches('risk$|risk_high$')) %>%
    mutate(
      m0_px_profile = recode(
        m0_px_profile,
        "x1_critical_cardiogenic_shock" = "Patient profile 1: Critical cardiogenic shock",
        "x2_progressive_decline" = "Patient profile 2: Progressive decline",
        "x3_stable_but_inotrope_dependent" = "Patient profile 3: Stable but inotrope dependent",
        "x4_resting_symptoms" = "Patient profile 4 or higher",
        "x5_exertion_intolerant" = "Patient profile 4 or higher",
        "x6_exertion_limited" = "Patient profile 4 or higher",
        "x7_advanced_nyha_class_3" = "Patient profile 4 or higher",
      )
    )

  overall <- .tabulate_incidence(data = tbl_data, risk_cat_m1) %>%
    mutate(group = 'Overall')

  by_prof <- tbl_data %>%
    split(f = .$m0_px_profile) %>%
    map_dfr(.f = .tabulate_incidence, risk_cat_m1, .id = 'group')

  m1 <- bind_rows(overall, by_prof)

  overall <- .tabulate_incidence(data = tbl_data, risk_cat_w1) %>%
    mutate(group = 'Overall')

  by_prof <- tbl_data %>%
    split(f = .$m0_px_profile) %>%
    map_dfr(.f = .tabulate_incidence, risk_cat_w1, .id = 'group')

  w1 <- bind_rows(overall, by_prof)

  list(m1 = m1, w1 = w1)

}


.tabulate_incidence <- function(data, .risk_cat){

  .risk_cat_quo <- enquo(.risk_cat)

  .df <- data %>%
    rename(risk_cat = !!.risk_cat_quo)

  event_counts <- .df %>%
    group_by(risk_cat) %>%
    summarize(N = n(),
              events = as.integer(sum(status)),
              time = round(sum(time))) %>%
    add_row(risk_cat = 'Overall',
            N = sum(.$N),
            events = sum(.$events),
            time = sum(.$time),
            .before = 1)

  incidence_rates <- event_counts %>%
    split(f = .$risk_cat) %>%
    map_dfr(
      .f = ~ {
        est <- 1000 * with(.x, events / sum(time))
        fit <- with(.x, glm(events ~ offset(log(time)), family=poisson))
        ci <- 1000 * exp(confint(fit))
        tibble(est = est, lwr = min(ci), upr = max(ci))
      },
      .id = 'risk_cat'
    ) %>%
    transmute(
      risk_cat,
      tbl_ir_estimate = tbl_string("{est} ({lwr}, {upr})")
    )

  incidence_ratios <-
    coxph(Surv(time, status) ~ risk_cat, data = .df) %>%
    tidy(exponentiate = TRUE, conf.int = TRUE) %>%
    transmute(
      risk_cat = str_remove(term, '^risk_cat'),
      tbl_ir_ratio = tbl_string("{estimate} ({conf.low}, {conf.high})")
    )

  df <- event_counts %>%
    left_join(incidence_rates) %>%
    left_join(incidence_ratios) %>%
    mutate(
      # need to use 2 * 100 b/c overall was added
      N = tbl_string("{N} ({2 * 100 * N / sum(N)}%)"),
      time = tbl_val(time),
      events = tbl_val(events),
      tbl_ir_ratio = replace(
        tbl_ir_ratio,
        risk_cat == 'both_low',
        '1 (ref)'
      ),
      tbl_ir_ratio = replace(
        tbl_ir_ratio,
        risk_cat == 'Overall',
        '---'
      )
    )

  t(df) %>%
    set_colnames(.[1, , drop = TRUE]) %>%
    as_tibble(rownames = 'term') %>%
    slice(-1)

}
