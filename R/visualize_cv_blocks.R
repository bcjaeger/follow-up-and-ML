##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param cv_tidy
visualize_cv_blocks <- function(cv_tidy, models, times, add_squares = T) {

  comparisons <- expand.grid(
    mdls = models,
    time = times
  ) %>%
    glue_data("{time}_{mdls}")

  .cv_tidy <- cv_tidy %>%
    filter(across(.cols = c(model, reference), ~.x %in% comparisons)) %>%
    droplevels()

  center <- filter(.cv_tidy, model == reference) %>%
    mutate(
      i = as.numeric(factor(model, levels = comparisons)),
      j = as.numeric(factor(reference, levels = comparisons)),
      hjust     = if_else(metric == 'bri', 'right', 'left'),
      vjust     = if_else(metric == 'bri', 'bottom', 'top'),
      nudge_x   = if_else(metric == 'bri', +0.1, -0.1),
      nudge_y   = if_else(metric == 'bri', -0.1, +0.1),
      i_strings = if_else(metric == 'bri', i-1/2, i+1/2) + nudge_x,
      j_strings = if_else(metric == 'bri', j+1/2, j-1/2) + nudge_y,
      .metric = recode(metric, 'auc' = 'AUC', 'bri' = 'Brier'),
      tbv = tbl_string("{.metric}: {100*est}", decimals = c(2,2,1)),
      .model = as.character(model),
      .model = str_replace(.model, 'm0\\.00', 'Baseline'),
      .model = str_replace(.model, 'm0\\.25', 'Week 1'),
      .model = str_replace(.model, 'm1\\.00', 'Month 1'),
      .model = str_replace(.model, '_ref$', '\nReference model'),
      .model = str_replace(.model, '_xgb$', '\nxgboost model'),
      .model = str_replace(.model, '_xgb_cph$', '\nCox PH model'),
      model = factor(model, levels = comparisons),
      reference = factor(reference, levels = comparisons)
    )

  .labels <- c("Small", "Moderate", "Large")

  # j > i ==> Brier

  sides_auc <- filter(.cv_tidy, model != reference, metric == 'auc') %>%
    mutate(
      i = as.numeric(factor(model, levels = comparisons)),
      j = as.numeric(factor(reference, levels = comparisons)),
      .m = reference,
      .r = model,
      reference = if_else(j < i, .r, reference),
      model = if_else(j < i, .m, model),
      tbv = if_else(j < i,
                    tbl_string("{100*est}\n({100*lwr}, {100*upr})",
                               decimals = c(2, 2, 1)),
                    tbl_string("{-100*est}\n({-100*upr}, {-100*lwr})",
                               decimals = c(2, 2, 1))),
      fig_fill = cut(100*abs(est), breaks = c(0, 2, 6, Inf), labels = .labels)
    )

  sides_bri <- filter(.cv_tidy, model != reference, metric == 'bri') %>%
    mutate(
      i = as.numeric(factor(model, levels = comparisons)),
      j = as.numeric(factor(reference, levels = comparisons)),
      .m = reference,
      .r = model,
      reference = if_else(j > i, .r, reference),
      model = if_else(j > i, .m, model),
      tbv = if_else(j > i,
                    tbl_string("{-100*est}\n({-100*upr}, {-100*lwr})",
                               decimals = c(2, 2, 1)),
                    tbl_string("{100*est}\n({100*lwr}, {100*upr})",
                               decimals = c(2, 2, 1))),
      fig_fill = cut(100*abs(est), breaks = c(0, 0.5, 1.5, Inf), labels = .labels)
    )

  sides <- bind_rows(sides_auc, sides_bri)

  colors_blocks <- paletteer_d("calecopal::desert")[c(1, 3, 4)]
  colors_text <- c('black', 'black', 'white')
  box_color <- paletteer_d("calecopal::desert")[5]

  txt_size <- 2.5

  p <- ggplot(center) +
    aes(x = model, y = reference, label = tbv) +
    geom_tile(fill = 'white', col = 'white') +
    geom_text(
      mapping = aes(
        x = j_strings, y = i_strings,
        hjust = hjust, vjust = vjust
      ),
      size = txt_size,
      fontface = 'bold'
    ) +
    geom_text(aes(label = .model), size = txt_size, fontface = 'italic') +
    geom_tile(data = sides, aes(fill = fig_fill), col = 'white') +
    geom_text(
      data = sides,
      mapping = aes(color = fig_fill),
      show.legend = FALSE,
      size = txt_size
    ) +
    scale_fill_manual(values = colors_blocks) +
    scale_color_manual(values = colors_text) +
    theme_minimal() +
    labs(
      x = '',
      y = '',
      fill = 'Difference in validation metric'
    ) +
    theme(
      legend.position = 'top',
      plot.margin = unit(c(1, 3, 1, 1), "lines"),
      panel.grid = element_blank(),
      text = element_text(size = 11),
      axis.text = element_blank()
    )

  if(!add_squares) return(p)

  .time <- switch(
    times[length(times)],
    'm1.00' = 'month 1',
    'm0.25' = 'week 1',
    'm0.00' = 'pre-implant'
  )

  p <- p +
    coord_cartesian(clip = 'off') +
    annotate("rect", size = 1.5,
             xmin = 1/2, xmax = 3.5,
             ymin = 1/2, ymax = 3.5,
             alpha = 0, color = box_color
    ) +
    annotate("rect", size = 1.5,
             xmin = 3.5, xmax = 6.5,
             ymin = 3.5, ymax = 6.5,
             alpha = 0, color = box_color
    ) +
    annotate(
      geom = 'text',
      x = 5,
      y = 6.75,
      label = glue('Use data prior to and including {.time} visit')
    ) +
    annotate(
      geom = 'text',
      x = 2,
      y = 0.25,
      label = 'Use pre-implant data only'
    )


}
