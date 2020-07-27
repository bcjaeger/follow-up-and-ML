##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param .ids
make_splits <- function(.ids, v = 10) {

  assignments <- sample(x = seq(v), size = length(.ids), replace = TRUE)

  map(seq(v), ~.ids[which(assignments == .x)]) %>%
    enframe(name = 'fold', value = 'test_ids')

}
