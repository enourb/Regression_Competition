# RF Tuning

library(tidyverse)
library(tidymodels)
library(stacks)

set.seed(333)

load("../data/temp_05/loan_setup.rda")


# define model

rf_model <- rand_forest(mtry = tune(), min_n = tune()) %>%
  set_engine("ranger") %>%
  set_mode("regression")


# set up tuning grid

rf_params <- parameters(rf_model) %>%
  update(mtry = mtry(range = c(7, 11)),
         min_n = min_n(range = c(10,2)))


# define grid

rf_grid <- grid_regular(rf_params, levels = 5)


# workflow

rf_workflow <- workflow() %>%
  add_model(rf_model) %>%
  add_recipe(loan_recipe)


# Tuning/fitting

rf_tune <- rf_workflow %>%
  tune_grid(resamples = loan_fold,
            grid = rf_grid)


# Write out results and workflow
save(rf_tune, rf_workflow, file = "../data/temp_05/rf_tune.rda")
