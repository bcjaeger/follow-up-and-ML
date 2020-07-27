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

  adjust_time <- switch (visit,
                         'm0.00' = 0,
                         'm0.25' = 1/4,
                         'm1.00' = 1,
                         'm3.00' = 3
  )

  .predict_horizon <- predict_horizon - adjust_time

  .training <-
    switch (visit,
            'm0.00' = select(training, -matches("^m3_|^m1_|^m0_25_")),
            'm0.25' = select(training, -matches('^m3_|^m1_')),
            'm1.00' = select(training, -matches('^m3_')),
            'm3.00' = training
    ) %>%
    mutate(
      time = if_else(time < adjust_time,
                     true = time,
                     false = time - adjust_time)
    )

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
