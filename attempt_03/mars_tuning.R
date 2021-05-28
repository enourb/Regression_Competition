# MARS Tuning

library(tidyverse)
library(tidymodels)
library(stacks)

set.seed(333)

load("../data/temp_03/loan_setup.rda")


# define model

mars_model <- mars(num_terms = tune(), prod_degree = tune()) %>%
  set_engine("earth") %>%
  set_mode("regression")


# set up tuning grid

mars_params <- parameters(mars_model) %>%
  update(num_terms = num_terms(range = c(5, 8)))


# define grid

mars_grid <- grid_regular(mars_params, levels = 5)


# workflow

mars_workflow <- workflow() %>%
  add_model(mars_model) %>%
  add_recipe(loan_recipe)


# Tuning/fitting

mars_tune <- mars_workflow %>%
  tune_grid(resamples = loan_fold,
            grid = mars_grid,
            control = control_stack_grid())

# Write out results and workflow
save(mars_tune, mars_workflow, file = "../data/temp_03/mars_tune.rda")
