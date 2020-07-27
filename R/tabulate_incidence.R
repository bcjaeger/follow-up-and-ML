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
      risk_cat = case_when(
        m0.00_risk_high == 'yes' & m1.00_risk_high == 'yes' ~ 'both_high',
        m0.00_risk_high == 'no' & m1.00_risk_high == 'no' ~ 'both_low',
        m0.00_risk_high == 'no' & m1.00_risk_high == 'yes' ~ 'm1_only',
        m0.00_risk_high == 'yes' & m1.00_risk_high == 'no' ~ 'm0_only'
      ),
      risk_cat = fct_relevel(risk_cat, 'both_low', 'm0_only', 'm1_only')
    ) %>%
    select(-matches('risk$|risk_high$')) %>%
    mutate(
      m0_px_profile = recode(
        m0_px_profile,
        "x1_critical_cardiogenic_shock" = "Stage 1: Critical cardiogenic shock",
        "x2_progressive_decline" = "Stage 2: Progressive decline",
        "x3_stable_but_inotrope_dependent" = "Stage 3: Stable but inotrope dependent",
        "x4_resting_symptoms" = "Stage 4 or higher",
        "x5_exertion_intolerant" = "Stage 4 or higher",
        "x6_exertion_limited" = "Stage 4 or higher",
        "x7_advanced_nyha_class_3" = "Stage 4 or higher",
      )
    )

  overall <- .tabulate_incidence(data = tbl_data) %>%
    mutate(group = 'Overall')

  by_prof <- tbl_data %>%
    split(f = .$m0_px_profile) %>%
    map_dfr(.f = .tabulate_incidence, .id = 'group')

  bind_rows(overall, by_prof)

}


.tabulate_incidence <- function(data){

  event_counts <- data %>%
    group_by(risk_cat) %>%
    summarize(N = n(),
              events = as.integer(sum(status)),
              time = round(sum(time)))

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
    coxph(Surv(time, status) ~ risk_cat, data = data) %>%
    tidy(exponentiate = TRUE) %>%
    transmute(
      risk_cat = str_remove(term, '^risk_cat'),
      tbl_ir_ratio = tbl_string("{estimate} ({conf.low}, {conf.high})")
    )

  df <- event_counts %>%
    left_join(incidence_rates) %>%
    left_join(incidence_ratios) %>%
    mutate(N = tbl_string("{N} ({100 * N / sum(N)}%)"),
           time = tbl_val(time),
           events = tbl_val(events),
           tbl_ir_ratio = replace(tbl_ir_ratio,
                                  is.na(tbl_ir_ratio),
                                  '1 (ref)'))

  t(df) %>%
    set_colnames(.[1, , drop = TRUE]) %>%
    as_tibble(rownames = 'term') %>%
    slice(-1)

}
