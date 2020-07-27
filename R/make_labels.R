##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title

make_labels <- function() {

  tribble(
    ~time,        ~variable,                   ~label,
    'Pre-implant', "m0_age_deident",            "Patient age, years",
    'Pre-implant', "m0_lvedd",                  "LVEDD",
    'Week 1',      "m0_25_bun_mg_dl",           "BUN, md/dL",
    'Week 1',      "m0_25_platelet_x10_3_ul",   "Platelet count",
    'Month 1',     "m1_bili_total_mg_dl",       "Total bilirubin, mg/dL",
    'Month 1',     "m1_bun_mg_dl",              "BUN, mg/dL",
    'Month 1',     "m1_platelet_x10_3_ul",      "Platelet count",
    'Month 1',     "m1_intub..yes",             "Intubated",
    'Month 1',     "m1_rt_hr_fail_ino..yes",    "Right heart failure, INO",
    'Month 1',     "m1_fol_status..outpatient", "Followup status: outpatient",
    'Month 1',     "m1_wbc_x10_3_ul",           "White blood cell count",
    'Month 1',     "m1_sgot_ast",               "SGOT",
    'Month 1',     "m1_ldh_l",                  "LDH",
    'Month 1',     "m1_nyha..class_iv_unable_to_carry_on_minimal_physical_activity", "NYHA: unable to carry on minimal physical activity",
    'Month 1',     "m1_albumin_g_dl",           "Albumin, g/dL",
    'Month 1',     "m1_dialysis..yes",          "On dialysis",
    'Month 1',     "m1_rt_hr_fail_ino_norepi..yes", "Right heart failure, INO NOREPI",
    'Month 1',     "m1_resp_ae_count_total",    "Respiratory adverse event count",
    'Month 1',     "m1_sodium_meq_l",           "Sodium, meq/L",
    'Month 1',     "m1_resp_ae_free",           "Days w/out respiratory adverse event",
    'Month 1',     "m1_hr_rate",                "Heart rate, beats per minute"
  ) %>%
    mutate(
      variable_original = str_remove(variable, pattern = '\\.\\.(.+)'),
      level = str_extract(variable, pattern = '\\.\\.(.+)'),
      level = str_remove(level, '\\.\\.')
    )

}
