library(tidymodels)
library(tidyverse)
library(readr)
library(skimr)
library(lubridate)


# turn categorical vars into factors
# put dates in standard format
loan <- read.csv(file = "data/train.csv") %>%
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
    purpose = as.factor(purpose),
    application_type = as.factor(application_type),
    grade = as.factor(grade),
    sub_grade = as.factor(sub_grade),
    term = as.factor(term),
    emp_length = as.factor(emp_length)
  )


## as.numeric(gsub("([0-9]+).*$", "\\1", emp_length))
# add february dummy since significant month from eda
loan <- loan %>%
  mutate(
    last_credit_feb = ifelse(months.Date(last_credit_pull_d) == "February", 1, 0),
    last_credit_feb = as.factor(last_credit_feb),
  )

save(loan, file = "data/loan.rda")

