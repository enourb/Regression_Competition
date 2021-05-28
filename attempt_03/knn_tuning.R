# Knn Tuning

library(tidyverse)
library(tidymodels)
library(stacks)

set.seed(333)

load("../data/temp_03/loan_setup.rda")


# define model

knn_model <- nearest_neighbor(neighbors = tune()) %>%
  set_engine("kknn") %>%
  set_mode("regression")


# set up tuning grid

knn_params <- parameters(knn_model)


# define grid

knn_grid <- grid_regular(knn_params, levels = 5)


# workflow
knn_recipe <- loan_recipe


knn_workflow <- workflow() %>%
  add_model(knn_model) %>%
  add_recipe(knn_recipe)


# Tuning/fitting


knn_tune <- knn_workflow %>%
  tune_grid(resamples = loan_fold,
            grid = knn_grid,
            control = control_stack_grid())


# Write out results and workflow
save(knn_tune, knn_workflow, file = "../data/temp_03/knn_tune.rda")
