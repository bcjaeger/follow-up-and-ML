##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param outcome
make_recipe <- function(data) {

  naming_fun <- function(var, lvl, ordinal = FALSE, sep = '..'){
    dummy_names(var = var, lvl = lvl, ordinal = ordinal, sep = sep)
  }

  recipe(time + status ~ ., data = data) %>%
    update_role(patient_id, new_role = 'ID') %>%
    step_rm(
      # patient outcomes
      starts_with("pt_outcome"),
      # operation id
      oper_id,
      # same values as another platelet variable
      contains('platelet_x10_9_l'),
      # same values as another sodium variable
      contains('sodium_mmol_l'),
      # same values as another hemoglobin variable
      contains('hemoglobin_mmol_l'),
      # highly correlated with other bili
      contains('bili_direct_mg_dl'),
      # also correlated with other bili
      contains('total_bili_peak')
    ) %>%
    step_nzv(all_predictors(), freq_cut = 999, unique_cut = 0.025) %>%
    step_knnimpute(
      ends_with('nyha'),
      neighbors = 10,
      impute_with = imp_vars(ends_with('nyha'),
                             ends_with("hemoglobin_g_dl"),
                             ends_with("resp_ae_count_total"),
                             ends_with("albumin_g_dl"))
    ) %>%
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


# to find variables used for imputation
#
# df <- select(data, m1_nyha, is.numeric, -patient_id) %>%
#   mutate(m1_nyha = factor(m1_nyha),
#          m1_nyha = as.numeric(m1_nyha) - 1) %>%
#   drop_na(m1_nyha)
#
# x = as.matrix(select(df, -m1_nyha))
#
# f <- xgb.cv(data = x, label = df$m1_nyha,
#             nrounds = 100,
#             objective = 'multi:softmax',
#             num_class = 4,
#             nfold = 10)
#
# f <- xgboost(data = x, label = df$m1_nyha,
#              nrounds = 40,
#              objective = 'multi:softmax',
#              num_class = 4)
#
# xgb.importance(model = f)
