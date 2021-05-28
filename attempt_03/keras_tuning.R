# Keras Tuning

library(tidyverse)
library(tidymodels)
library(stacks)
library(keras)
library(tensorflow)
use_condaenv("r-reticulate", required = TRUE)

set.seed(333)

load("../data/temp_02/loan_setup.rda")


# define model

keras_model <- mlp(hidden_units = tune(), penalty = tune()) %>%
  set_engine("keras") %>%
  set_mode("regression")


# set up tuning grid

keras_params <- parameters(keras_model)


  # define grid

keras_grid <- grid_regular(keras_params, levels = 5)


# workflow

keras_workflow <- workflow() %>%
  add_model(keras_model) %>%
  add_recipe(loan_recipe)


# Tuning/fitting

keras_tune <- keras_workflow %>%
  tune_grid(resamples = loan_fold,
            grid = keras_grid,
            control = control_stack_grid())


# Write out results and workflow
save(keras_tune, keras_workflow, file = "../data/temp_03/keras_tune.rda")
