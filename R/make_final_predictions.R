##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param final_fit_combined
make_final_predictions <- function(final_fit_combined, risk_cutpoint) {

  map(final_fit_combined, 'risk') %>%
    reduce(left_join, by = 'patient_id') %>%
    rename_with(
      .cols = -patient_id,
      .fn = ~ str_replace(.x, 'xgb_cph', 'risk')
    ) %>%
    mutate(
      across(
        .cols = -patient_id,
        .fns = list(high = ~ if_else(.x > risk_cutpoint, 'yes', 'no'))
      )
    )

}
