##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param analysis
##' @param recipe
cv_reference <- function(data,
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
    .fitter = fit_reference,
    .label = 'ref'
  )

}
