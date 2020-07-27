
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @param data
##' @param splits
##' @param n_predictors
##'
##' @title

final_fitter <- function(data,
                         visit,
                         params,
                         predict_horizon,
                         n_predictors,
                         .fitter,
                         .label) {

  preproc <- make_preproc(training = data,
                          visit = visit,
                          predict_horizon = predict_horizon)

  fit_final <- .fitter(trn = preproc$trn,
                       visit = visit,
                       params = params,
                       return_fit = TRUE,
                       predict_horizon = preproc$horizon,
                       n_predictors = n_predictors)

  risk_name <- paste(visit, .label, sep = '_')
  fit_final$risk <- fit_final$risk %>%
    rename(!!risk_name := risk)

  fit_final


}



