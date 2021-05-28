# Elastic net Tuning

library(tidyverse)
library(tidymodels)
library(stacks)

set.seed(333)

load("../data/temp_02/loan_setup.rda")


# define model

elastic_model <- linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet") %>%
  set_mode("regression")


# set up tuning grid

elastic_params <- parameters(elastic_model)


# define grid

elastic_grid <- grid_regular(elastic_params, levels = 5)


# workflow
elastic_recipe <- loan_recipe


elastic_workflow <- workflow() %>%
  add_model(elastic_model) %>%
  add_recipe(elastic_recipe)


# Tuning/fitting


elastic_tune <- elastic_workflow %>%
  tune_grid(resamples = loan_fold,
            grid = elastic_grid,
            control = control_stack_grid())


# Write out results and workflow
save(elastic_tune, elastic_workflow, file = "../data/temp_02/elastic_tune.rda")
