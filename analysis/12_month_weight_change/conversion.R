rm(list = ls())


library(here)
library(tidyverse)
library(survey)

source(here("utils/find_EI.R"))

df <- read_csv(here("inputs/processed/hse.2019"))


# 2.2 kg changes 12 month weight change

# assumption: everyone loses 2.2kg

df_lost <- df %>% mutate(new_weight = weight - 2.2)

ei_estimate <- list(id = df_lost$id,
     bw = df_lost$weight,
     ht = df_lost$height/100,
     age = df_lost$age,
     sex = df_lost$sex,
     weight_goal = df_lost$new_weight,
     days = 365,
     ei_limit = 1000,
     pal = 1.6) %>% 
  pmap_dfr(., find_EI) %>% 
  left_join(df, lost, by = "id")

svy <- svydesign(ids=~ei_estimate$psu, 
                 nest = T,
                 data=ei_estimate,
                 strata = ei_estimate$strata,
                 weights=ei_estimate$wt_int)

svymean(~ei, svy)

