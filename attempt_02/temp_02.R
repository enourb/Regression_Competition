library(tidymodels)
library(tidyverse)
library(skimr)
library(stacks)
library(lubridate)
set.seed(333)

load("data/loan.rda")


loan_split <- initial_split(loan, prop = .8, strata = money_made_inv)
loan_train <- training(loan_split)
loan_test <- testing(loan_split)

loan_fold <- vfold_cv(loan_train, v = 10, repeats = 5, strata = money_made_inv)

loan_recipe <- recipe(money_made_inv ~ ., data = loan_train) %>%
  step_rm(sub_grade, id, emp_title) %>%
  step_date(earliest_cr_line, features = c("year")) %>%
  step_date(last_credit_pull_d, features = c("year")) %>%
  step_rm(earliest_cr_line, last_credit_pull_d) %>%
  step_other(addr_state, threshold = .05) %>%
  step_dummy(all_nominal(), -all_outcomes(), one_hot = T) %>%
  step_rm(emp_length_n.a) %>%
  step_nzv(all_predictors()) %>%
  step_normalize(all_predictors(), -all_outcomes())


skim_without_charts(loan_recipe %>%
                      prep(loan_train) %>%
                      bake(new_data = NULL))

save(loan_fold, loan_recipe, loan_split, loan_train, file = "data/temp_02/loan_setup.rda")


load("data/temp_02/rf_tune.rda")
load("data/temp_02/bt_tune.rda")
load("data/temp_02/mars_tune.rda")
load("data/temp_02/nnet_tune.rda")
load("data/temp_02/elastic_tune.rda")
load("data/temp_02/knn_tune.rda")
load("data/temp_02/keras_tune.rda")

loan_data_stack <- stacks() %>%
  add_candidates(nnet_tune) %>%
  add_candidates(rf_tune) %>%
  add_candidates(bt_tune) %>%
  add_candidates(mars_tune) %>%
  add_candidates(elastic_tune) %>%
  add_candidates(knn_tune) %>%
  add_candidates(keras_tune)


blend_penalty <- c(10^(-6:-1), 0.5, 1, 1.5, 2)


loan_model_stack <-
  loan_data_stack %>%
  blend_predictions(penalty = blend_penalty)


loan_model_stack <-
  loan_model_stack %>%
  fit_members()

autoplot(loan_model_stack, type = "weights") +
  theme_minimal()

test <- loan_model_stack %>%
  predict(loan_test) %>%
  bind_cols(loan_test)



member_preds <- loan_model_stack %>%
  predict(test, members = TRUE) %>%
  bind_cols(test %>% select(money_made_inv)) %>%
  rename(ensemble = .pred)


member_preds %>%
  map_dfr(rmse, truth = money_made_inv, data = member_preds) %>%
  mutate(model = colnames(member_preds)) %>%
  filter(model != "money_made_inv") %>%
  arrange(.estimate)





testing <- read.csv(file = "data/test.csv") %>%
  mutate(
    initial_list_status = as.factor(initial_list_status),
    verification_status = as.factor(verification_status),
    last_credit_pull_d = paste(last_credit_pull_d, "-01"),
    last_credit_pull_d = myd(last_credit_pull_d),
    earliest_cr_line = paste(earliest_cr_line, "-01"),
    earliest_cr_line = myd(earliest_cr_line),
    addr_state = as.factor(addr_state),
    emp_title = as.factor(emp_title),
    home_ownership = as.factor(home_ownership),
    application_type = as.factor(application_type),
    grade = as.factor(grade),
    sub_grade = as.factor(sub_grade),
    term = as.factor(term),
    emp_length = as.factor(emp_length)
  )

valid_purpose <- c("credit_card", "debt_consolidation", "home_improvement")

testing <- testing %>%
  mutate(
    purpose = ifelse(purpose %in% valid_purpose, purpose, "other"),
    purpose = as.factor(purpose)
  )

final_predict <- loan_model_stack %>%
  predict(testing) %>%
  bind_cols(testing %>% select(id)) %>%
  rename(Predicted = .pred) %>%
  rename(Id = id) %>%
  select(Id, Predicted)


write.csv(final_predict, file = "data/temp_02/test_pred.csv", row.names = FALSE)
