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

cv_fitter <- function(data,
                      visit,
                      splits,
                      params,
                      predict_horizon,
                      n_predictors,
                      .fitter,
                      .label) {

  predictions <- vector(mode = 'list', length = nrow(splits))

  for(i in seq_along(splits$test_ids)){

    message('beginning CV replicate no. ', i)

    test_set <- splits$test_ids[[i]]

    training <- filter(data, !(patient_id %in% test_set))
    testing  <- filter(data, patient_id %in% test_set)

    preproc <- make_preproc(training, testing, visit, predict_horizon)

    predictions[[i]] <- .fitter(trn = preproc$trn,
                                tst = preproc$tst,
                                visit = visit,
                                params = params,
                                return_fit = FALSE,
                                predict_horizon = preproc$horizon,
                                n_predictors = n_predictors)

  }

  risk_name <- paste(visit, .label, sep = '_')

  bind_rows(predictions) %>%
    rename(!!risk_name := risk)


}


