##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param final_fit_combined
make_final_estimates <- function(final_fit_combined) {

  map_dfr(final_fit_combined, ~.x$fit, .id = 'model') %>%
    mutate(
      model = recode(model,
                     '1' = 'Pre-implant',
                     '2' = 'Week 1',
                     '3' = 'Month 1' ,
                     '4' = 'Month 3')
    ) %>%
    select(-statistic)

}
