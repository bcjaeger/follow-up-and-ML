
# TODO:
# add tables for w1 / pre-implant model (DONE)
# make kaplan meier figure for the risk groups (DONE)
# add overall column to change in risk table (DONE)
# create a table median (iqr) for same risk groups at 1wk & 1m0 (DONE)


..visits <- c('m0.00', 'm0.25', 'm1.00')
..cv_funs <- rlang::syms(c('cv_reference', 'cv_xgb', 'cv_xgb_cph'))

the_plan <- drake_plan(

  predict_horizon = 12,
  conditional_visit = 'm1.00',
  min_impl_year = 2012,
  n_predictors = 20,

  xgb_params = list(
    eta                = 0.01,
    num_parallel_trees = 3,
    max_depth          = 3,
    gamma              = 1/3,
    min_child_weight   = 1,
    subsample          = 1/2,
    colsample_bynode   = 1/2,
    objective          = "survival:cox",
    eval_metric        = "cox-nloglik"
  ),

  IDs = make_IDs(conditional_visit, min_impl_year),

  splits = make_splits(IDs, v = 10),

  data = clean_intermacs(visit = conditional_visit,
                         outcome = 'dead',
                         conditional_ids = IDs,
                         miss_threshold = 0.50),

  # data_labels = make_labels(data),
  #
  # preds = target(
  #   .f(data = data,
  #      visit = visit,
  #      splits = splits,
  #      params = xgb_params,
  #      predict_horizon = predict_horizon,
  #      n_predictors = n_predictors),
  #   transform = cross(visit = !!..visits, .f = !!..cv_funs)
  # ),
  #
  # cv_predictions = target(preds, transform = combine(preds)),
  # cv_comparison  = make_cv_comparison(data, cv_predictions, predict_horizon),
  # cv_tidy        = tidy_cv(cv_comparison),
  #
  # fig_cv_m1  = visualize_cv_blocks(cv_tidy,
  #                                  models = c('ref', 'xgb', 'xgb_cph'),
  #                                  times = c('m0.00', 'm1.00'),
  #                                  add_squares = TRUE),
  #
  # fig_cv_w1  = visualize_cv_blocks(cv_tidy,
  #                                  models = c('ref', 'xgb', 'xgb_cph'),
  #                                  times = c('m0.00', 'm0.25'),
  #                                  add_squares = TRUE),
  #
  # final_fit = target(
  #   command = final_fitter(
  #     data = data,
  #     visit = visit,
  #     params = xgb_params,
  #     predict_horizon = predict_horizon,
  #     n_predictors = n_predictors,
  #     .fitter = fit_xgb_cph,
  #     .label = 'xgb_cph'
  #   ),
  #   transform = map(visit = !!..visits)
  # ),
  #
  # final_fit_combined = target(final_fit, transform = combine(final_fit)),
  #
  # final_estimates = make_final_estimates(final_fit_combined),
  #
  # final_predictions = make_final_predictions(final_fit_combined,
  #                                            risk_cutpoint = 0.20),
  #
  # fig_kaplans = visualize_kaplans(data, final_predictions),
  #
  # tbl_incidence = tabulate_incidence(final_predictions, data),
  #
  # tbl_final_fit = tabulate_cph(final_estimates,
  #                              data,
  #                              data_labels),
  #
  # report = compile_report(fname = 'INTERMACS_FUP_and_ML',
  #                         tbl_final_fit = tbl_final_fit,
  #                         tbl_incidence_m1 = tbl_incidence$m1,
  #                         tbl_incidence_w1 = tbl_incidence$w1,
  #                         fig_cv_m1 = fig_cv_m1,
  #                         fig_cv_w1 = fig_cv_w1,
  #                         fig_kap_m1 = fig_kaplans$m1,
  #                         fig_kap_w1 = fig_kaplans$w1)

)


# plotCalibration(cv_comparison, method = 'q')
#
# plotCalibration(
#   models = 'm1.00_xgb',
#   cens.method = 'local',
#   cv_comparison,
#   method = 'q',
#   bars = TRUE,
#   show.frequencies = TRUE,
#   hanging = TRUE
# )

#
# scores$AUC$score
# scores$AUC$contrasts
# scores$Brier$score







