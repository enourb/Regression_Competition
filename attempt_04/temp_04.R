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


# remove unnecessary info and vars with too many categories
loan_recipe <- recipe(money_made_inv ~ ., data = loan_train) %>%
  step_rm(sub_grade, id, emp_title) %>%
  step_date(earliest_cr_line, last_credit_pull_d, features = c("year")) %>%
  step_rm(earliest_cr_line, last_credit_pull_d) %>%
  step_other(addr_state, threshold = .05) %>%
  step_dummy(all_nominal(), one_hot = T) %>%
  step_nzv(all_predictors()) %>%
  step_normalize(all_predictors())


skim_without_charts(loan_recipe %>%
                      prep(loan_train) %>%
                      bake(new_data = NULL))

save(loan_fold, loan_recipe, loan_split, loan_train, file = "data/temp_04/loan_setup.rda")


load("data/temp_04/rf_tune.rda")

final_workflow_tuned <- rf_workflow %>%
  finalize_workflow(select_best(rf_tune, metric = "rmse"))

final_results <- fit(final_workflow_tuned, loan_train)



rf_tune %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  slice_min(mean)

rf_tune %>%
  autoplot(metric = "rmse")


# clean testing data to match the format of training
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
    purpose = as.factor(purpose),
    last_credit_feb = ifelse(months.Date(last_credit_pull_d) == "February", 1, 0),
    last_credit_feb = as.factor(last_credit_feb)
  )


final_predict <- predict(final_results, new_data = testing) %>%
  bind_cols(testing %>% select(id)) %>%
  rename(Predicted = .pred) %>%
  rename(Id = id) %>%
  select(Id, Predicted)


write.csv(final_predict, file = "data/temp_04/test_pred.csv", row.names = FALSE)
