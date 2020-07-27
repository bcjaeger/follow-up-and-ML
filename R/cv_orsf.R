##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @param data
##' @param splits
##' @param predict_horizon
##' @param n_predictors
##'
##' @title

cv_orsf <- function(data,
                    visit,
                    splits,
                    predict_horizon,
                    n_predictors) {

  adjust_time <- switch (visit,
                         'm0.00' = 0,
                         'm0.25' = 1/4,
                         'm1.00' = 1,
                         'm3.00' = 3
  )

  .predict_horizon <- predict_horizon - adjust_time

  fit_data <-
    switch (visit,
            'm0.00' = select(data, -matches("^m3_|^m1_|^m0_25_")),
            'm0.25' = select(data, -matches('^m3_|^m1_')),
            'm1.00' = select(data, -matches('^m3_')),
            'm3.00' = data
    ) %>%
    mutate(
      time = if_else(time < adjust_time,
                     true = time,
                     false = time - adjust_time)
    )


  recipe = make_recipe(fit_data)

  predictions <- vector(mode = 'list', length = nrow(splits))

  for(i in seq_along(splits$test_ids)){

    message('beginning CV replicate no. ', i)

    test_set <- splits$test_ids[[i]]

    training <- filter(fit_data, !(patient_id %in% test_set))
    testing  <- filter(fit_data, patient_id %in% test_set)

    prepped_recipe <- prep(recipe, training = training)

    trn <- juice(prepped_recipe)
    tst <- bake(prepped_recipe, new_data = testing)

    trn_x <- as.matrix(select(trn, -c(time, status, patient_id)))
    trn_y <- as.matrix(select(trn, c(time, status)))

    tst_x <- as.matrix(select(tst, -c(time, status, patient_id)))
    tst_y <- as.matrix(select(tst, c(time, status)))

    xgb_label <- trn_y[, 1]
    censored <- trn_y[, 2] == 0
    xgb_label[censored] <- xgb_label[censored] * (-1)

    params <- list(
      eta                = 0.02,
      num_parallel_trees = 1,
      max_depth          = 2,
      gamma              = 1/3,
      min_child_weight   = 1,
      subsample          = 1/2,
      colsample_bynode   = 1/2,
      objective          = "survival:cox",
      eval_metric        = "cox-nloglik"
    )

    xgb_fit_full <- sgb_fit(
      sgb_df = sgb_data(trn_x, xgb_label),
      nrounds = 200,
      verbose = FALSE,
      params = params
    )

    pred_names <- xgb.importance(model = xgb_fit_full$fit) %>%
      as_tibble() %>%
      arrange(desc(Gain)) %>%
      slice(seq(n_predictors)) %>%
      pull(Feature)

    trn_orsf <- as_tibble(cbind(trn_y, trn_x[, pred_names]))
    tst_orsf <- as_tibble(cbind(tst_y, tst_x[, pred_names]))

    orsf_fit <- ORSF(data = trn_orsf,
                     ntree = 1000,
                     eval_times = 12,
                     min_events_to_split_node = 10,
                     min_obs_to_split_node = 20,
                     nsplit = 10,
                     use.cv = FALSE)

    predictions[[i]] <- 1 - predict(orsf_fit,
                                    newdata = tst_orsf,
                                    times = .predict_horizon)

    predictions[[i]] %<>%
      set_colnames('risk') %>%
      as_tibble() %>%
      mutate(patient_id = tst$patient_id)

  }

  risk_name <- paste(visit, 'orsf', sep = '_')

  bind_rows(predictions) %>%
    rename(!!risk_name := risk)


}


