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

cv_xgb <- function(data,
                   visit,
                   splits,
                   params,
                   predict_horizon,
                   n_predictors) {

  cv_fitter(
    data = data,
    visit = visit,
    splits = splits,
    params = params,
    predict_horizon = predict_horizon,
    n_predictors = n_predictors,
    .fitter = fit_xgb,
    .label = 'xgb'
  )


}


