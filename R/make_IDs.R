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

  fup_variable <- switch(
    visit,
    'm0.00' = NULL,
    'm0.25' = "M0_25_INTOP1FU",
    'm1.00' = "M1_INTOP1FU"
  )

  intermacs <- read_csv(.visit, guess_max = 20000) %>%
    filter(PT_OUTCOME_MONTHS > 0,
           M0_IMPL_YR >= min_impl_year)

  # exclude participants with events before the visit,
  # but keep participants with events at the same time as the visit.
  if (!is.null(fup_variable)) {
    intermacs %<>%
      filter(.data[[fup_variable]] <= PT_OUTCOME_MONTHS)
  }

  pull(intermacs, PATIENT_ID)

}
