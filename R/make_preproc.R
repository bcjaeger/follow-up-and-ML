##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param training
##' @param testing
##' @param visit
##' @param predict_horizon
make_preproc <- function(training, testing = NULL, visit, predict_horizon) {

  adjust_time <- switch(
    visit,
    'm0.00' = 0,
    'm0.25' = 1/4,
    'm1.00' = 1,
    'm3.00' = 3
  )

  .predict_horizon <- predict_horizon - adjust_time

  adjust_variable <- switch(
    visit,
    'm0.00' = NULL,
    'm0.25' = 'm0_25_intop1fu',
    'm1.00' = 'm1_intop1fu',
    'm3.00' = 'm3_intop1fu'
  )

  .training <-
    switch (
      visit,
      'm0.00' = select(training, -matches("^m3_|^m1_|^m0_25_")),
      'm0.25' = select(training, -matches('^m3_|^m1_')),
      'm1.00' = select(training, -matches('^m3_')),
      'm3.00' = training
    )

  if(!is.null(adjust_variable)){

    .training %<>%
      mutate(
        time = if_else(
          condition = time <= .data[[adjust_variable]],
          true = 1 / 60, # 1/2 of a day
          false = time - .data[[adjust_variable]]
        )
      ) %>%
      select(-.data[[adjust_variable]])

  }

  #if(visit == 'm1.00') browser()

  recipe = make_recipe(.training)

  prepped_recipe <- prep(recipe, training = .training)
  trn <- juice(prepped_recipe)

  if(is.null(testing)){
    tst <- NULL
  }  else {
    tst <- bake(prepped_recipe, new_data = testing)
  }

  list(trn = trn, tst = tst, horizon = .predict_horizon)

}
