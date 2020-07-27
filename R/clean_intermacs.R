##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param visit
clean_intermacs <- function(visit = "m0.00",
                            conditional_ids,
                            na_string = c('',' ',"NA","Missing","Unknown"),
                            outcome = 'dead') {


  .visit <- str_replace(visit, '\\.', '-') %>%
    toupper() %>%
    paste0('data/',.,'.csv')

  intermacs <- read_csv(.visit,
                        guess_max = 20000,
                        na = na_string)

  status_colname <- glue('pt_outcome_{outcome}')

  intermacs %>%
    # drop the additional patient ID columns.
    select(-matches("M.+_PATIENT_ID|M.+_PATIENT_ID.+")) %>%
    clean_names() %>%
    filter(patient_id %in% conditional_ids) %>%
    remove_constant(na.rm = TRUE, quiet = TRUE) %>%
    remove_empty(which = 'cols') %>%
    remove_empty(which = 'rows') %>%
    mutate(across(where(is.character), clean_chr),
           across(matches('^m0_cc_|^m0_cc2_'), ~as.numeric(.x == 'yes')),
           time = pt_outcome_months,
           status = .data[[status_colname]],
           ccs = as.numeric(m0_px_profile == 'x1_critical_cardiogenic_shock'),
           current_smoker = if_else(
             m0_cc_curr_smoking_m == 1 | m0_cc2_curr_smoking_m == 1,
             true = 1,
             false = 0
           ),
           periph_vasc = if_else(
             m0_cc_periph_vasc_disease_m == 1 |
             m0_cc2_periph_vasc_disease_m == 1,
             true = 1,
             false = 0
           ),
           non_comp = if_else(
             m0_cc2_rptd_non_compliance_m == 1 |
             m0_cc_rptd_non_compliance_m == 1,
             true = 1,
             false = 0
           ),
           age2 = m0_age_deident^2,
           bmi = m0_wgt_kg / (m0_hgt_cm/100)^2)

}
