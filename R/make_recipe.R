##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param outcome
make_recipe <- function(data) {

  library(embed)

  naming_fun <- function(var, lvl, ordinal = FALSE, sep = '..'){
    dummy_names(var = var, lvl = lvl, ordinal = ordinal, sep = sep)
  }

  recipe(time + status ~ ., data = data) %>%
    update_role(patient_id, new_role = 'ID') %>%
    step_rm(starts_with("pt_outcome"),
            oper_id,
            # same values as another platelet variable
            contains('platelet_x10_9_l')) %>%
    step_nzv(all_predictors(), freq_cut = 999, unique_cut = 0.025) %>%
    step_modeimpute(all_nominal(), -all_outcomes()) %>%
    step_medianimpute(all_numeric(), -all_outcomes()) %>%
    step_log(contains('bun_mg_dl')) %>%
    # step_mutate(tmp = as.numeric(status == 1 & time < 12)) %>%
    # step_discretize_xgb(
    #   contains('bili_total_mg_dl'),
    #   contains('bun_mg_dl'),
    #   contains('platelet_x10_3_ul'),
    #   contains('wbc_x10_3_ul'),
    #   outcome = vars(tmp),
    #   learn_rate = 0.05,
    #   num_breaks = 4
    # ) %>%
    # step_rm(tmp) %>%
    step_normalize(all_numeric(), -all_outcomes(),
                   -patient_id, -contains('ae_count')) %>%
    # step_pca(starts_with('m0_cc_'), starts_with("m0_cc2_"),
    #          num_comp = 3, prefix = 'CC_comp') %>%
    step_novel(all_nominal(), -all_outcomes()) %>%
    step_dummy(all_nominal(), -all_outcomes(),
               naming = naming_fun, one_hot = FALSE)

}
