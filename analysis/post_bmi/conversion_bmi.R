rm(list = ls())


library(here)
library(tidyverse)
library(survey)

source(here("utils/find_EI.R"))

df <- read_csv(here("inputs/processed/hse_2019.csv"))

# Pooling all 4 studies, each 10% decrease in the price of fruits and vegetables was associated with a non-significant 0.04 kg/m2 (95% CI: 0 to 0.08) lower BMI.

# BMI formula bw/ht^2 = bmi

# assumption: 
# 1 - everyone loses 0.04 units of BMI
# 2 - 6 month duration


df_new <- df %>% 
  mutate(new_bmi = bmi - 0.04) %>% 
  mutate(new_weight = new_bmi * (height/100)^2) %>% 
  mutate(sex = ifelse(sex == 1, "male", "female"))

ei_estimate <- list(id = df_new$id,
                    bw = df_new$weight,
                    ht = df_new$height/100,
                    age = df_new$age,
                    sex = df_new$sex,
                    weight_goal = df_new$new_weight,
                    days = 365/2,
                    ei_limit = 1000,
                    pal = 1.6) %>% 
  pmap_dfr(., find_EI) %>% 
  left_join(df, df_new, by = "id")

svy <- svydesign(ids=~ei_estimate$psu, 
                 nest = T,
                 data=ei_estimate,
                 strata = ei_estimate$strata,
                 weights=ei_estimate$wt_int)

svymean(~ei, svy)
