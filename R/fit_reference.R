

fit_reference <- function(trn,
                          tst = NULL,
                          visit,
                          params,
                          return_fit = FALSE,
                          predict_horizon,
                          n_predictors) {

  ref_data <- trn %>%
    select(
      time,
      status,
      m0_age_deident,
      age2,
      bmi,
      ccs,
      periph_vasc,
      current_smoker,
      non_comp,
      contains('ra_pres'),
      contains('cv_pres'),
      contains('bili_total_mg_dl'),
      contains('lvedd'),
      contains('intervention_48_hrs_dialysis..yes'),
      contains('bun_mg_dl'),
      contains('albumin_g_dl'),
      contains('prev_cardiac_oper_none'),
      -ends_with('..new')
    )

  mdl <- coxph(formula = Surv(time, status) ~ ., data = ref_data, x = TRUE)

  if(return_fit) return(mdl)

  if(is.null(tst)) stop("tst must be supplied if return_fit = FALSE",
                        call. = FALSE)

  predictRisk(mdl, newdata = tst, times = predict_horizon) %>%
    set_colnames('risk') %>%
    as_tibble() %>%
    mutate(patient_id = tst$patient_id)


}
