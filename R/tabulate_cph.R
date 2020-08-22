##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##'
##' @param final_estimates
##' @param data
##' @param data_labels
##'
tabulate_cph <- function(final_estimates, data, data_labels) {

  unique_vars <- unique(data_labels$variable_original)

  tbl1_stats <- data[, unique_vars] %>%
    tibble_one(formula = ~ .,
               add_perc_to_cats = FALSE,
               include_freq = TRUE) %>%
    rename(level = labels,
           tb1_stat = Overall,
           variable_original = variable) %>%
    mutate(
      variable_original = as.character(variable_original),
      level = replace(level,
                      tolower(level) == variable_original,
                      NA_character_),
      tb1_stat = if_else(
        is.na(level), true = tb1_stat,
        false = str_replace(tb1_stat, '\\)', replacement = '%)')
      )
    ) %>%
    select(-group)

  miss_stats <- miss_var_summary(data[, unique_vars]) %>%
    transmute(
      variable_original = variable,
      tbl_miss = tbl_string('{n_miss} ({pct_miss}%)')
    )

  trn <- juice(prep(make_recipe(data)))

  unadjusted_hr <- function(variable_name, data){

    formula <- as.formula(glue("Surv(time, status) ~ {variable_name}"))

    tidy(coxph(formula, data), exponentiate = TRUE, conf.int = TRUE)

  }

  final_estimates %>%
    mutate(
      model = factor(model, levels = c("Pre-implant", "Week 1", "Month 1"))
    ) %>%
    group_by(model) %>%
    group_map(.f = ~ {

      unadjusted_hrs <- intersect(data_labels$variable, .x$term) %>%
        map_dfr(.f = unadjusted_hr, data = trn)

      models <- list("mdl_one" = unadjusted_hrs, 'mdl_two' = .x)

      tbl_data <- map_dfr(
        .x = models,
        ~ .x %>%
          transmute(
            variable = term,
            tbl_value = tbl_string("{estimate} ({conf.low}, {conf.high})",
                                   decimals = c(2,2,1))
          ) %>%
          left_join(data_labels, by = 'variable'),
        .id = 'model'
      )

      time_levels <- c("Pre-implant", "Week 1", "Month 1")
      time_labels <- paste(time_levels, 'variables')

      tbl_data %>%
        relocate(label, .before = 1) %>%
        pivot_wider(values_from = tbl_value, names_from = model) %>%
        left_join(tbl1_stats, by = c('variable_original', 'level')) %>%
        left_join(miss_stats, by = 'variable_original') %>%
        mutate(time = factor(time, levels = time_levels, labels = time_labels)) %>%
        arrange(time) %>%
        select(time, label, tbl_miss, tb1_stat, mdl_one, mdl_two)

    })


}
