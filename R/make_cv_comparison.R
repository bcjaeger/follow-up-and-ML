##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param cv_predictions
make_cv_comparison <- function(data, cv_predictions, predict_horizon) {

  .pred <- reduce(cv_predictions, left_join, by = 'patient_id') %>%
    left_join(select(data, patient_id, time, status))

  .object <- as.list(select(.pred, matches("ref$|xgb$|xgb_cph$")))

  Score(.object,
        formula = Surv(time, status) ~ 1,
        data = .pred,
        times = predict_horizon,
        summary = 'IPA',
        plots = 'Calibration')

}


