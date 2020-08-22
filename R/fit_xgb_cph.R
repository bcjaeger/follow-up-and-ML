

fit_xgb_cph <- function(trn,
                        tst = NULL,
                        visit = NULL,
                        params,
                        return_fit = FALSE,
                        predict_horizon = NULL,
                        n_predictors){

  trn_x <- as.matrix(select(trn, -c(time, status, patient_id, age2)))
  trn_y <- as.matrix(select(trn, c(time, status)))

  xgb_label <- trn_y[, 1]
  censored <- trn_y[, 2] == 0
  xgb_label[censored] <- xgb_label[censored] * (-1)

  xgb_fit_full <- sgb_fit(
    sgb_df = sgb_data(trn_x, xgb_label),
    nrounds = 250,
    verbose = FALSE,
    params = params
  )

  pred_names <- xgb.importance(model = xgb_fit_full$fit) %>%
    as_tibble() %>%
    arrange(desc(Gain)) %>%
    slice(seq(n_predictors)) %>%
    pull(Feature)

  trn_cph <- as_tibble(cbind(trn_y, trn_x[, pred_names]))

  mdl <- coxph(formula = Surv(time, status) ~ .,
               data = trn_cph,
               x = TRUE)

  if(return_fit)
    return(
      list(fit = tidy(mdl, exponentiate = TRUE, conf.int = TRUE),
           risk = mdl %>%
             predictRisk(newdata = trn_cph, times = predict_horizon) %>%
             set_colnames('risk') %>%
             as_tibble() %>%
             mutate(patient_id = trn$patient_id))
    )

  if(is.null(tst)) stop("tst must be supplied if return_fit = FALSE",
                        call. = FALSE)

  tst_x <- as.matrix(select(tst, -c(time, status, patient_id)))
  tst_y <- as.matrix(select(tst, c(time, status)))
  tst_cph <- as_tibble(cbind(tst_y, tst_x[, pred_names]))

  predicted_risk <- try(expr = {
    predictRisk(mdl, newdata = tst_cph, times = predict_horizon) %>%
      set_colnames('risk') %>%
      as_tibble() %>%
      mutate(patient_id = tst$patient_id)
  })

  predicted_risk

}
