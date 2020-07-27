##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param fname
compile_report <- function(fname,
                           tbl_fit_final,
                           tbl_incidence,
                           fig_cv_m1,
                           fig_cv_w1) {

  # setup ----

  abbrevs <- c(
    CI = 'confidence interval',
    BP = 'blood pressure',
    CVD = 'cardiovascular disease',
    HDL = 'High density lipoprotein'
  )

  fts <- c(
    '\u2A',
    '\u2020',
    '\u2021',
    '\uA7',
    '\u2016',
    '\uB6',
    '#',
    '\u2a\u2a',
    '\u2020\u2020',
    '\u2021\u2021'
  )

  tbls_main <- tbls_supp <- tibble(
    object = list(),
    caption = NA_character_
  )

  figs_main <- figs_supp <- tibble(
    object = list(),
    caption = character(),
    legend = character(),
    width = numeric(),
    height = numeric()
  )

  # table: final fit summary (tbl_fit_final) ----

  .tbl_fit_final <- tbl_fit_final %>%
    as_grouped_data(group = 'time') %>%
    as_flextable(hide_grouplabel = TRUE) %>%
    set_header_labels(
      tbl_miss = 'Number (%)\nmissing values',
      tb1_stat = 'Mean (SD) or\nNumber (%)',
      mdl_one = 'Unadjusted',
      mdl_two = 'Adjusted'
    ) %>%
    add_header_row(values = c("", "Hazard ratio (95% CI)"),
                   colwidths = c(3, 2)) %>%
    width(width = 1.3) %>%
    width(j = 1, width = 2) %>%
    theme_box() %>%
    align(part = 'all', align = 'center') %>%
    align(part = 'all', j = 1, align = 'left') %>%
    italic(i = ~!is.na(time)) %>%
    bg(i = ~!is.na(time), bg = 'grey')

  tbls_main %<>% add_row(
    object = list(.tbl_fit_final),
    caption = "Patient characteristics selected for inclusion in the final mortality risk prediction model."
  )

  # table: risk group assessment (tbl_incidence) ----

  .tbl_incidence <- tbl_incidence %>%
    mutate(
      term = recode(
        term,
        N = 'Number (%) of patients',
        events = 'Number of events',
        time = 'Months of follow up',
        tbl_ir_estimate = 'Incidence rate (95% CI)',
        tbl_ir_ratio = 'Incidence ratio (95% CI)'
      )
    ) %>%
    as_grouped_data(group = 'group') %>%
    as_flextable(hide_grouplabel = TRUE) %>%
    set_header_labels(
      term = '',
      both_low = 'Low risk at\nboth visits',
      m0_only = 'High risk at\npre-implant only',
      m1_only = 'High risk at\nmonth 1 only',
      both_high = 'High risk at\nboth visits'
    ) %>%
    add_header_row(values = c("", "1-year Mortality risk classification"),
                   colwidths = c(1, 4)) %>%
    width(width = 1.3) %>%
    width(j = 1, width = 2) %>%
    theme_box() %>%
    align(part = 'all', align = 'center') %>%
    align(part = 'all', j = 1, align = 'left') %>%
    italic(i = ~!is.na(group)) %>%
    bg(i = ~!is.na(group), bg = 'grey')

  tbls_main %<>% add_row(
    object = list(.tbl_incidence),
    caption = "Incidence rates and ratios for patients classified as high or low risk at pre-implant and one month following implant of mechanical circulatory support device."
  )

  # figure: model comparison (xgboost versus reference) ----

  figs_main %<>%
    add_row(
      object  = list(fig_cv_m1),
      caption = "Benchmark comparison of modeling algorithms.",
      legend  = '',
      width   = 6.5,
      height  = 5.5
    )

  figs_supp %<>%
    add_row(
      object  = list(fig_cv_w1),
      caption = "Benchmark comparison modeling algorithms.",
      legend  = '',
      width   = 6.5,
      height  = 5.5
    )

  # Add pre-caption ---------------------------------------------------------

  tbls_main %<>%
    mutate(
      pre_cap = glue("Table {1:nrow(tbls_main)}"),
      caption = glue("{pre_cap}: {caption}")
    ) %>%
    select(-pre_cap)

  if(nrow(tbls_supp) > 0) tbls_supp %<>%
    mutate(
      pre_cap = glue("Table S{1:nrow(tbls_supp)}"),
      caption = glue("{pre_cap}: {caption}")
    ) %>%
    select(-pre_cap)


  if(nrow(figs_main) > 0) figs_main %<>%
    mutate(
      pre_cap = glue("Figure {1:nrow(.)}"),
      caption = glue("{pre_cap}: {caption}")
    ) %>%
    select(-pre_cap)

  if(nrow(figs_supp) > 0) figs_supp %<>%
    mutate(
      pre_cap = glue("Figure S{1:n()}"),
      caption = glue("{pre_cap}: {caption}")
    ) %>%
    select(-pre_cap)


  font_size = 11
  font_name = "Calibri"

  # uniform font and font size for tables ----

  tbls_main %<>%
    mutate(
      object = map(
        .x = object,
        .f = ~ .x %>%
          font(fontname = font_name, part = 'all') %>%
          fontsize(size = font_size, part = 'all') %>%
          height(height = 2, part = 'footer')
      )
    )

  # Begin document in word ----


  my_doc <- read_docx(
    'doc/template.docx'
  ) %>%
    cursor_begin() %>%
    cursor_reach(keyword = 'RESULTS') %>%
    body_add_break()

  ntbl_main <- nrow(tbls_main)
  nfig_main <- nrow(figs_main)

  # add main tables to word doc ----

  if(ntbl_main > 0){
    for(i in seq(ntbl_main)){
      my_doc %<>%
        body_add_par(tbls_main$caption[[i]]) %>%
        body_add_flextable(tbls_main$object[[i]]) %>%
        body_add_break()
    }
  }

  # add main figures to word doc ----

  if(nfig_main > 0){

    for(i in seq(nfig_main)){

      if( inherits(figs_main$object[[i]], 'gg') ) {

        filename <- tempfile(fileext = ".emf")

        emf(
          file = filename,
          width = figs_main$width[i],
          height = figs_main$height[i]
        )

        print(figs_main$object[[i]])

        dev.off()

      } else {

        # This assumes the above has been taken care of already
        filename <- figs_main$object[[i]]

      }

      my_doc %<>%
        body_add_par(figs_main$caption[i]) %>%
        body_add_img(
          filename,
          width = figs_main$width[i],
          height = figs_main$height[i]
        )

      if( length(figs_main$legend[i])>0 ){

        my_doc %<>% body_add_par(figs_main$legend[i])

      }

      if(i < nfig_main) my_doc %<>% body_add_break()

    }
  }

  # add supplemental tables to word doc ----

  my_doc %<>%
    cursor_reach(keyword = 'SUPPLEMENT') %>%
    body_add_break()

  tbls_supp %<>%
    mutate(
      object = map(
        .x = object,
        .f = ~ .x %>%
          font(fontname = font_name, part = 'all') %>%
          fontsize(size = font_size, part = 'all')
      )
    )

  ntbl_supp <- nrow(tbls_supp)
  nfig_supp <- nrow(figs_supp)

  if(ntbl_supp > 0){
    for(i in seq(ntbl_supp)){
      my_doc %<>%
        body_add_par(tbls_supp$caption[[i]]) %>%
        body_add_flextable(tbls_supp$object[[i]]) %>%
        body_add_break()
    }
  }


  # add supplemental figures to word doc ----

  if(nfig_supp > 0){

    for(i in seq(nfig_supp)){

      if( inherits(figs_supp$object[[i]], 'gg') ) {

        filename <- tempfile(fileext = ".emf")

        emf(
          file = filename,
          width = figs_supp$width[i],
          height = figs_supp$height[i]
        )

        print(figs_supp$object[[i]])

        dev.off()

      } else {

        # This assumes the above has been taken care of already
        filename <- figs_supp$object[[i]]

      }

      my_doc %<>%
        body_add_par(figs_supp$caption[i]) %>%
        body_add_img(
          filename,
          width = figs_supp$width[i],
          height = figs_supp$height[i]
        )

      if( length(figs_supp$legend[i])>0 ){

        my_doc %<>% body_add_par(figs_supp$legend[i])

      }

      if(i < nfig_supp) my_doc %<>% body_add_break()


    }
  }

  # Output ----

  my_doc %>%
    print(
      file.path(
        'doc',
        glue("{Sys.Date()}-{fname}.docx")
      )
    )


}

