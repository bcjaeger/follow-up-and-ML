##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param cv_comparison
tidy_cv <- function(cv_comparison) {

  estimates_auc <-
    cv_comparison$AUC$score %>%
    as_tibble() %>%
    transmute(model, reference = model,
              est = AUC, lwr = lower, upr = upper, pval = NA_real_)

  contrasts_auc <-
    cv_comparison$AUC$contrasts %>%
    as_tibble() %>%
    transmute(model, reference,
              est = delta.AUC, lwr = lower, upr = upper, pval = p)

  estimates_bri <-
    cv_comparison$Brier$score %>%
    as_tibble() %>%
    filter(model != 'Null model') %>%
    transmute(model, reference = model,
              est = Brier, lwr = lower, upr = upper, pval = NA_real_)

  contrasts_bri <-
    cv_comparison$Brier$contrasts %>%
    as_tibble() %>%
    filter(model != 'Null model', reference != 'Null model') %>%
    transmute(model, reference,
              est = delta.Brier, lwr = lower, upr = upper, pval = p)

  auc <- bind_rows(estimates_auc, contrasts_auc)
  bri <- bind_rows(estimates_bri, contrasts_bri)

  bind_rows(auc = auc, bri = bri, .id = 'metric')

}
