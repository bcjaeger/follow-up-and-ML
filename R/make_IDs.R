##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param visit
make_IDs <- function(visit, min_impl_year) {

  .visit <- str_replace(visit, '\\.', '-') %>%
    toupper() %>%
    paste0('data/',.,'.csv')

  read_csv(.visit, guess_max = 20000) %>%
    filter(PT_OUTCOME_MONTHS > 0,
           M0_IMPL_YR >= min_impl_year) %>%
    pull(PATIENT_ID)

}
