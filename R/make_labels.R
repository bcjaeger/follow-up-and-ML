##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title

make_labels <- function(data) {

  varying <- c('Pre-implant', 'Week 1', 'Month 1')

  terms <- c(
    "age_deident",
    "lvedd",
    "race_white..yes",
    "bun_mg_dl",
    "platelet_x10_3_ul",
    "sys_bp",
    "bili_total_mg_dl",
    "intub..yes",
    "rt_hr_fail_ino..yes",
    "fol_status..outpatient",
    "wbc_x10_3_ul",
    "sgot_ast",
    "ldh_l",
    "nyha..class_iv_unable_to_carry_on_minimal_physical_activity",
    "albumin_g_dl",
    "dialysis..yes",
    "rt_hr_fail_ino_norepi..yes",
    "resp_ae_count_total",
    "sodium_meq_l",
    "resp_ae_free",
    "hr_rate",
    "warfarin..yes",
    "pre_albumin_mg_dl",
    "resp_ae_count",
    "total_bili_peak",
    "infect_ae_free",
    "hemoglobin_g_dl",
    "sgpt_alt",
    "device_ty..lvad",
    "prev_cardiac_oper_none",
    "prev_cardiac_oper_cabg..yes",
    "device_strategy..destination_therapy_patient_definitely_not_eligible_for_transplant",
    "concom_surg_rvad_implant..yes",
    "surgery_time",
    "ecg_rhythm..sinus",
    "cv_pres",
    "lymph_cnt_percent",
    "kccq12",
    "creat_mg_dl",
    "dia_bp",
    "eq_index"
  )

  expand.grid(time = varying, term = terms, stringsAsFactors = FALSE) %>%
    as_tibble() %>%
    mutate(
      prefix = recode(
        time,
        'Pre-implant' = 'm0',
        'Week 1' = 'm0_25',
        'Month 1' = 'm1'),
      label = recode(
        term,
        'age_deident'                 = 'Age, years',
        'lvedd'                       = 'Left-ventricular end diastolic dimension',
        'race_white..yes'             = 'White',
        'platelet_x10_3_ul'           = "Platelet count",
        'sys_bp'                      = "Systolic blood pressure, mm Hg",
        'bili_total_mg_dl'            = "Total bilirubin, mg/dL",
        'bun_mg_dl'                   = "BUN, mg/dL",
        'intub..yes'                  = "Intubated",
        'rt_hr_fail_ino..yes'         = "Right heart failure, INO",
        'fol_status..outpatient'      = "Followup status: outpatient",
        'wbc_x10_3_ul'                = "White blood cell count",
        'sgot_ast'                    = "SGOT",
        'ldh_l'                       = "LDH",
        'nyha..class_iv_unable_to_carry_on_minimal_physical_activity' = "NYHA 4: unable to carry on minimal physical activity",
        'albumin_g_dl'                = "Albumin, g/dL",
        'dialysis..yes'               = "On dialysis",
        'rt_hr_fail_ino_norepi..yes'  = "Norepinephrine prescribed since week 1",
        'resp_ae_count_total'         = "Respiratory adverse event count",
        'sodium_meq_l'                = "Sodium, meq/L",
        'resp_ae_free'                = "Days w/out respiratory adverse event",
        'hr_rate'                     = "Heart rate, beats per minute",
        'warfarin..yes'               = "Warfarin",
        'pre_albumin_mg_dl'           = "Pre-albumin, mg/dL",
        'resp_ae_count'               = "Number of respiratory events between week 1 and month 1 visit",
        'total_bili_peak'             = "Total bilirubin peak",
        'infect_ae_free'              = "Days w/out infection adverse event",
        'hemoglobin_g_dl'             = "Hemoglobin, g/dL",
        'sgpt_alt'                    = "SGPT ALT",
        "device_ty..lvad"             = "Device type: LVAD",
        "prev_cardiac_oper_none"      = "No previous cardiac operation",
        "prev_cardiac_oper_cabg..yes" = "Previous operation: CABG",
        "creat_mg_dl"                 = "Creatinine, mg/dL",
        "device_strategy..destination_therapy_patient_definitely_not_eligible_for_transplant" = "Destination therapy",
        "concom_surg_rvad_implant..yes" = "Concomitant RVAD implant",
        "surgery_time"                = "Surgery time",
        "ecg_rhythm..sinus"           = "ECG Rhythm: sinus",
        "cv_pres"                     = "CV pressure",
        "lymph_cnt_percent"           = "Lymph count, percent",
        "kccq12"                      = "KCCQ12",
        "dia_bp"                      = "Diastolic blood pressure, mm Hg",
        "eq_index"                    = "EQ5D index"
      )
    ) %>%
    unite(prefix, term, col = 'variable', sep = '_') %>%
    mutate(
      variable_original = str_remove(variable, pattern = '\\.\\.(.+)'),
      level = str_extract(variable, pattern = '\\.\\.(.+)'),
      level = str_remove(level, '\\.\\.')
    ) %>%
    filter(variable_original %in% names(data)) %>%
    add_row(time = "Pre-implant",
            variable = "bmi",
            label = "Body mass index",
            variable_original = "bmi",
            level = NA_character_)


}

