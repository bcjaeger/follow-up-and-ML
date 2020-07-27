
fit_xgb <- function(trn,
                    tst = NULL,
                    visit,
                    params,
                    return_fit = FALSE,
                    predict_horizon,
                    n_predictors) {

  trn_x <- as.matrix(select(trn, -c(time, status, patient_id, age2)))
  trn_y <- as.matrix(select(trn, c(time, status)))


  xgb_label <- trn_y[, 1]
  censored <- trn_y[, 2] == 0
  xgb_label[censored] <- xgb_label[censored] * (-1)

  xgb_fit_full <- sgb_fit(
    sgb_df = sgb_data(trn_x, xgb_label),
    nrounds = 200,
    verbose = FALSE,
    params = params
  )

  xgb_rdcd_names <- xgb.importance(model = xgb_fit_full$fit) %>%
    as_tibble() %>%
    arrange(desc(Gain)) %>%
    slice(seq(n_predictors)) %>%
    pull(Feature)

  xgb_cv <- xgb.cv(
    nfold = 10,
    data = trn_x[, xgb_rdcd_names],
    label = xgb_label,
    nrounds = 5000,
    params = params,
    print_every_n = 50,
    early_stopping_rounds = 100
  )

  xgb_fit <- sgb_fit(
    sgb_df = sgb_data(trn_x[, xgb_rdcd_names], xgb_label),
    nrounds = xgb_cv$best_iteration,
    params = params
  )

  if(return_fit) return(xgb_fit)

  if(is.null(tst)) stop("tst must be supplied if return_fit = FALSE",
                        call. = FALSE)

  tst_x <- as.matrix(select(tst, -c(time, status, patient_id)))

  xgb_risk <- 1 - predict(xgb_fit,
                          new_data = tst_x[, xgb_rdcd_names],
                          eval_times = predict_horizon,
                          smooth = FALSE)

  xgb_risk %>%
    set_colnames('risk') %>%
    as_tibble() %>%
    mutate(patient_id = tst$patient_id)
}
