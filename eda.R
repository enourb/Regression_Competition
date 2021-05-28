library(tidymodels)
library(tidyverse)
library(skimr)
library(stacks)
library(lubridate)
set.seed(333)

load("data/loan.rda")

skim(loan)

ggplot(loan, aes(addr_state))+
  geom_bar()


ggplot(loan, aes(sub_grade))+
  geom_bar()


ggplot(loan, aes(sub_grade, money_made_inv, color = grade))+
  geom_boxplot()

ggplot(loan, aes(initial_list_status, money_made_inv))+
  geom_boxplot()

ggplot(loan, aes(annual_inc, money_made_inv))+
  geom_point()

# check number in each category to verify usefulness
loan %>%
  count(last_credit_feb) %>%
  arrange(desc(n))

ggplot(loan, aes(term, money_made_inv))+
  geom_boxplot()

# check boxplot for:
#initial_list_status YES
#verification_status NO
#last_credit_pull_d NO
#last_credit_pull_d NO
#addr_state NO
#home_ownership NO
#purpose NO
#application_type NO
#grade YES
#sub_grade NO
#term YES
#emp_length NO

ggplot(loan, aes(initial_list_status, money_made_inv))+
  geom_boxplot()

# February dummy worth keeping
ggplot(loan, aes(last_credit_feb, money_made_inv)) +
  geom_boxplot()

loan_numeric <- select_if(loan, is.numeric)


ggcorrplot::ggcorrplot(cor(loan_numeric))
# correlated variables:
# out_prncp_inv
# loan_amnt
# bc_util



ggplot(loan, aes(addr_state, money_made_inv)) +
  geom_boxplot()
