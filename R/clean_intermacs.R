##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param visit
clean_intermacs <- function(visit = "m1.00",
                            conditional_ids,
                            na_string = c('',' ',"NA","Missing","Unknown"),
                            outcome = 'dead',
                            miss_threshold = 0.50) {


  .visit <- str_replace(visit, '\\.', '-') %>%
    toupper() %>%
    paste0('data/',.,'.csv')

  .visit_num <- switch(
    visit,
    'm0.00' = 1,
    'm0.25' = 2,
    'm1.00' = 3
  )

  intermacs <- read_csv(.visit,
                        guess_max = 20000,
                        na = na_string)

  status_colname <- glue('pt_outcome_{outcome}')

  # TODO: fix the skip pattern missingness

  im <- intermacs %>%
    # drop the additional patient ID columns.
    select(-matches("M.+_PATIENT_ID|M.+_PATIENT_ID.+"),
           -M0_GENDER_1) %>%
    clean_names() %>%
    filter(patient_id %in% conditional_ids) %>%
    remove_constant(na.rm = TRUE, quiet = TRUE) %>%
    remove_empty(which = 'cols') %>%
    remove_empty(which = 'rows') %>%
    mutate(across(where(is.character), clean_chr),
           across(matches('^m0_cc_|^m0_cc2_'), ~as.numeric(.x == 'yes')),
           time = pt_outcome_months,
           status = .data[[status_colname]]
           # this isn't needed since Rama recoded reference model variables.
           # ccs = as.numeric(m0_px_profile == 'x1_critical_cardiogenic_shock'),
           # current_smoker = if_else(
           #   m0_cc_curr_smoking_m == 1 | m0_cc2_curr_smoking_m == 1,
           #   true = 1,
           #   false = 0
           # ),
           # periph_vasc = if_else(
           #   m0_cc_periph_vasc_disease_m == 1 |
           #   m0_cc2_periph_vasc_disease_m == 1,
           #   true = 1,
           #   false = 0
           # ),
           # non_comp = if_else(
           #   m0_cc2_rptd_non_compliance_m == 1 |
           #   m0_cc_rptd_non_compliance_m == 1,
           #   true = 1,
           #   false = 0
           # ),
           # age2 = m0_age_deident^2,
           # bmi = m0_wgt_kg / (m0_hgt_cm/100)^2
    )

  # fix NYHA missing values at baseline ----

  profile_1_thru_4 <- c("x1_critical_cardiogenic_shock",
                        "x2_progressive_decline",
                        "x3_stable_but_inotrope_dependent",
                        "x4_resting_symptoms")

  profile_5_thru_6 <- c("x5_exertion_intolerant",
                        "x6_exertion_limited")

  nyha_4 <- "class_iv_unable_to_carry_on_minimal_physical_activity"
  nyha_3 <- "class_iii_marked_limitation"

  im %<>%
    mutate(
      m0_nyha = case_when(
        !is.na(m0_nyha) ~ m0_nyha,
        m0_px_profile %in% profile_1_thru_4 ~ nyha_4,
        m0_px_profile %in% profile_5_thru_6 ~ nyha_3,
        TRUE ~ NA_character_
      )
    )

  if (.visit_num >= 2) {
    im %<>%
      mutate(
        across(
          .cols = contains('m0_25_rt_hr_fail_ino_'),
          .fns = ~ {
            if_else(m0_25_rt_hr_fail_ino == 'no',
                    true = 'no',
                    false = .x)
          }
        )
      )
  }

  if (.visit_num >= 3) {
    im %<>%
      mutate(
        across(
          .cols = contains('m1_rt_hr_fail_ino_'),
          .fns = ~ {
            if_else(m1_rt_hr_fail_ino == 'no',
                    true = 'no',
                    false = .x)
          }
        )
      )
  }

  if(miss_threshold > 0){

    too_many_missing <- miss_var_summary(im) %>%
      filter(pct_miss >= 100 * miss_threshold) %>%
      pull(variable)

    im[, too_many_missing] <- NULL

  }

  im %>%
    mutate_if(is.character, as.factor)

}
