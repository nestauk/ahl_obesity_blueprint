

# setup
rm(list = ls())
gc()
library(tidyverse)
library(here)

source(file = "requirements.R")
source(file = "pre_processing/pre_processing_adult_test.R")
# source(file = "models/adult_model_calorie.R")
# source(file = "models/child_model_calorie.R")




# 1.1. Cleaning the input/ baseline data:
process_clean_save_test(file_path = "inputs/raw/hse_2019_eul_20211006.tab", nation = "England", population_group = "Adult")

# 1.2. Estimating the impact of the interv


df2019 <- read.table(here("inputs/raw/hse_2019_eul_20211006.tab"), sep = "\t", header = TRUE) 

df2019_1 = df2019 %>% 
  filter(WtVal>0 & HtVal>0 & Age35g >=7 )


file_path = "inputs/raw/hse_2019_eul_20211006.tab"

df_2019_adult <- read.table(here("inputs/raw/hse_2019_eul_20211006.tab"), sep = "\t", header = TRUE) 

df_2019_adult_1 = df_2019_adult %>% 
  filter(WtVal>0 & HtVal>0 & Age35g >=7 )


df_1 = df_2019_adult %>% 
  filter(WtVal>0 & HtVal>0 & Age35g >7 ) %>% # remove missing height and weight and children; 
  mutate(age = case_when(#Age35g == 7 ~ (16+19)/2,
    Age35g == 8 ~ (20+24)/2, 
    Age35g == 9 ~ (25+29)/2,
    Age35g == 10 ~ (30+34)/2,
    Age35g == 11 ~ (35+39)/2,
    Age35g == 12 ~ (40+44)/2,
    Age35g == 13 ~ (45+49)/2,
    Age35g == 14 ~ (50+54)/2,
    Age35g == 15 ~ (55+59)/2,
    Age35g == 16 ~ (60+64)/2,
    Age35g == 17 ~ (65+69)/2,
    Age35g == 18 ~ (70+74)/2,
    Age35g == 19 ~ (75+79)/2,
    Age35g == 20 ~ (80+84)/2,
    Age35g == 21 ~ (85+89)/2,
    Age35g == 22 ~ (90),
    TRUE ~ 0)) %>%
  #browser() %>%
  mutate(age_grp = case_when(#Age35g == 7 ~ (16+19)/2,
    Age35g == 8 ~ "20-24", 
    Age35g == 9 ~ "25-29",
    Age35g == 10 ~ "30-34",
    Age35g == 11 ~ "35-39",
    Age35g == 12 ~ "40-44",
    Age35g == 13 ~ "45-49",
    Age35g == 14 ~ "50-54",
    Age35g == 15 ~ "55-59",
    Age35g == 16 ~ "60-64",
    Age35g == 17 ~ "65-69",
    Age35g == 18 ~ "70-74",
    Age35g == 19 | Age35g == 20 | Age35g == 21 | Age35g == 22  ~ "75+",
    TRUE ~ "NA")) %>%
  # browser() %>%
  rename(weight = WtVal,
         height = HtVal,
         sex = Sex,
         bmi = BMIVal,
         qimd = qimd19,
         number_children = Nofch3,
         income_JSA = srcin05d, # Job Seekers Allowance
         income_IS = srcin07d,  # Income Support
         income_PC = srcin08d,  # Pension Credit
         income_CTC = srcin10d, # Child Tax Credit
         income_UC = srcin14d,  # Universal Credit 
         ethnicity = origin2,
         diabetes = diabete2,
         cardiovd = CardioTakg2,
         id = SerialA,
         psu = PSU_SCR,
         strata = cluster94) %>%
  # browser() %>%
  mutate(income_support_status = case_when((income_JSA == 1 | income_IS == 1 | income_PC == 1 | income_CTC == 1 | income_UC == 1) ~ 1,
                                           TRUE ~ 0)) %>%
  mutate(children_updated = case_when(number_children == 0 ~ 0,
                                      TRUE ~ 1)) %>%
  dplyr::select(id, weight, height, age_grp, age, sex, bmi, BMIOK, qimd, children_updated, income_support_status, ethnicity, diabetes, cardiovd, wt_int, psu, strata )  %>% # select variables needed
  mutate(qimd_updated = case_when((qimd == 4 | qimd == 5) ~ 1,
                                  TRUE ~ 0)) %>%
  mutate(pal = 1.6, # pal assumed to be 1.6 for the entire population to indicate a sendentary/ light active lifestyle
         rmr = case_when(sex == 1 ~ ((10 * weight) + (6.25 * height) - (5 * age) + 5),
                         TRUE ~ ((10 * weight) + (6.25 * height) - (5 * age) - 161))) %>% # sex = 2 female; rmr is calculated using Mifflin St Jeor Equations from Mifflin et al (1990)
  mutate(bmi_class = case_when(bmi <= 18.5 ~ "underweight",
                               bmi > 18.5 & bmi < 25 ~ "normal",
                               bmi >= 25 & bmi < 30 ~ "overweight",
                               bmi >= 30 & bmi < 40 ~ "obese",
                               bmi >= 40 ~ "morbidly obese",
                               TRUE ~ "NA")) %>% 
  # browser() %>%
  mutate(intake = pal*rmr)  # calculating value of energy intake 