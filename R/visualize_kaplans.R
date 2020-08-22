##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data
##' @param final_predictions
visualize_kaplans <- function(data, final_predictions) {

  data_kaplan <- select(data, patient_id, time, status) %>%
    left_join(final_predictions) %>%
    mutate(
      risk_cat_m1 = case_when(
        m0.00_risk_high == 'yes' & m1.00_risk_high == 'yes' ~ 'both_high',
        m0.00_risk_high == 'no'  & m1.00_risk_high == 'no'  ~ 'both_low',
        m0.00_risk_high == 'no'  & m1.00_risk_high == 'yes' ~ 'm1_only',
        m0.00_risk_high == 'yes' & m1.00_risk_high == 'no'  ~ 'm0_only'
      ),
      risk_cat_m0_25 = case_when(
        m0.00_risk_high == 'yes' & m0.25_risk_high == 'yes' ~ 'both_high',
        m0.00_risk_high == 'no'  & m0.25_risk_high == 'no'  ~ 'both_low',
        m0.00_risk_high == 'no'  & m0.25_risk_high == 'yes' ~ 'm1_only',
        m0.00_risk_high == 'yes' & m0.25_risk_high == 'no'  ~ 'm0_only'
      )
    )

  cml_inc_m1 <- cuminc(
    ftime   = data_kaplan$time,
    fstatus = data_kaplan$status,
    group   = data_kaplan$risk_cat_m1
  )

  cml_inc_m0_25 <- cuminc(
    ftime   = data_kaplan$time,
    fstatus = data_kaplan$status,
    group   = data_kaplan$risk_cat_m0_25
  )

  cml_inc_overall <- cuminc(
    ftime   = data_kaplan$time,
    fstatus = data_kaplan$status
  )

  counts_m1 <- count(data_kaplan, risk_cat_m1) %>%
    rename(group = risk_cat_m1)

  counts_m0_25 <- count(data_kaplan, risk_cat_m0_25) %>%
    rename(group = risk_cat_m0_25)

  list(
    w1 = .vis_kap(
      cml_inc_bygroups = cml_inc_m0_25,
      cml_inc_overall = cml_inc_overall,
      group_counts = counts_m0_25,
      group_label = 'week-1'
    ),
    m1 = .vis_kap(
      cml_inc_bygroups = cml_inc_m1,
      cml_inc_overall = cml_inc_overall,
      group_counts = counts_m1,
      group_label = 'month-1'
    )
  )

}


.vis_kap <- function(
  cml_inc_bygroups,
  cml_inc_overall,
  group_counts,
  group_label
) {

  group_counts %<>% add_row(
    group = 'Overall', n = sum(group_counts$n)
  )

  ggdat <- map_dfr(
    .x = list(overall = cml_inc_overall,
              bygroups = cml_inc_bygroups),
    .f = ~ ggcompetingrisks(.x, conf.int = T) %>%
      use_series('data') %>%
      as_tibble()
  ) %>%
    mutate(group = recode(group, '1' = 'Overall')) %>%
    filter(time <= 12) %>%
    left_join(group_counts, by = 'group') %>%
    mutate(
      nudge_y = as.numeric(recode(
        group,
        'Overall'   =  if (group_label == 'week-1') '0.000' else '0.025',
        'both_high' =  '0.001',
        'both_low'  =  '0.000',
        'm1_only'   = '-0.001',
        'm0_only'   =  '0.000'
      )),
      group = recode(
        group,
        'Overall'   = glue("Overall"),
        'both_high' = glue('High risk at both visits'),
        'both_low'  = glue('Low risk at both visits'),
        'm1_only'   = glue('High risk at {group_label} only'),
        'm0_only'   = glue('High risk at pre-implant only')
      ),
      group = fct_relevel(
        group,
        'High risk at both visits',
        glue('High risk at {group_label} only'),
        'High risk at pre-implant only',
        'Low risk at both visits',
        "Overall"
      )
    )

  ggdat_labels <- ggdat %>%
    group_by(group) %>%
    arrange(desc(time)) %>%
    slice(1)

  ggplot(ggdat, aes(x = time, y = est)) +
    geom_line(mapping = aes(col = group),
              size = 1) +
    geom_label(data = ggdat_labels,
               mapping = aes(x = 12,
                             label = group,
                             col = group,
                             fill = group),
               hjust = 0,
               alpha = 0.05,
               nudge_y = ggdat_labels$nudge_y,
               nudge_x = 0.25) +
    scale_x_continuous(limits = c(0, 20),
                       breaks = 1:12) +
    theme_bw() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.border = element_blank(),
      axis.title.x = element_text(hjust = 0.225),
      text = element_text(size = 13, color = 'black', face = 'bold'),
      legend.position = ''
    ) +
    scale_y_continuous(label = scales::percent) +
    labs(x = '\nTime since implant, months',
         y = 'Cumulative Incidence of mortality\n') +
    scale_color_manual(
      values = c(
        'purple',
        'orange',
        'cadetblue4',
        'chartreuse4',
        'grey'
      )
    ) +
    scale_fill_manual(
      values = c(
        'purple',
        'orange',
        'cadetblue4',
        'chartreuse4',
        'grey'
      )
    )

}
