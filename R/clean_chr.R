##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param x
##' @param case
clean_chr <- function(x, case = 'snake'){

  ux <- unique(na.omit(x))
  ux_clean <- make_clean_names(ux, case = case)
  names(ux_clean) <- ux
  recode(x, !!!ux_clean)

}
