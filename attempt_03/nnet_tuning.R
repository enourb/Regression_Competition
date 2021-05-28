# Neural Network Tuning

library(tidyverse)
library(tidymodels)
library(stacks)

set.seed(333)

load("../data/temp_03/loan_setup.rda")


# define model

nnet_model <- mlp(hidden_units = tune(), penalty = tune()) %>%
  set_engine("nnet") %>%
  set_mode("regression")


# set up tuning grid

nnet_params <- parameters(nnet_model)


# define grid

nnet_grid <- grid_regular(nnet_params, levels = 5)


# workflow

nnet_workflow <- workflow() %>%
  add_model(nnet_model) %>%
  add_recipe(loan_recipe)


# Tuning/fitting

nnet_tune <- nnet_workflow %>%
  tune_grid(resamples = loan_fold,
            grid = nnet_grid,
            control = control_stack_grid())


# Write out results and workflow
save(nnet_tune, nnet_workflow, file = "../data/temp_03/nnet_tune.rda")
