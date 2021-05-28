# Boosted Tree Tuning

library(tidyverse)
library(tidymodels)
library(stacks)

set.seed(333)

load("../data/temp_03/loan_setup.rda")


# define model

bt_model <- boost_tree(mtry = tune(), min_n = tune(), learn_rate = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("regression")


# set up tuning grid

bt_params <- parameters(bt_model) %>%
  update(mtry = mtry(range = c(8, 18)))


# define grid

bt_grid <- grid_regular(bt_params, levels = 5)


# workflow

bt_workflow <- workflow() %>%
  add_model(bt_model) %>%
  add_recipe(loan_recipe)


# Tuning/fitting

bt_tune <- bt_workflow %>%
  tune_grid(resamples = loan_fold,
            grid = bt_grid,
            control = control_stack_grid())


# Write out results and workflow
save(bt_tune, bt_workflow, file = "../data/temp_03/bt_tune.rda")
