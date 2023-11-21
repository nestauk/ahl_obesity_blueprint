library(here)
library(tidyverse)

## include access info for database

df_2019_adult <- read.table(here("inputs/raw/hse_2019_eul_20211006.tab"), sep = "\t", header = TRUE) %>% 
  filter(WtVal>0 & HtVal>0 & Age35g >7 ) %>% # remove missing height and weight and children
  mutate(age = case_when(Age35g == 7 ~ (16+19)/2,
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
  rename(weight = WtVal,
         height = HtVal,
         sex = Sex,
         bmi = BMIVal,
         id = SerialA,
         psu = PSU_SCR,
         strata = cluster94) %>% 
  dplyr::select(id, weight, height, age, sex, bmi, wt_int, psu, strata )  %>% # select variables needed
  mutate(pal = 1.6,
         rmr = case_when(sex == 1 ~ ((10 * weight) + (6.25 * height) - (5 * age) + 5),
                         TRUE ~ ((10 * weight) + (6.25 * height) - (5 * age) - 161))) %>% # sex = 2 female Miffin & St.Jeor
  mutate(bmi_class = case_when(bmi <= 18.5 ~ "underweight",
                               bmi > 18.5 & bmi < 25 ~ "normal",
                               bmi >= 25 & bmi < 30 ~ "overweight",
                               bmi >= 30 & bmi < 40 ~ "obese",
                               bmi >= 40 ~ "morbidly obese",
                               TRUE ~ "NA")) %>% 
  mutate(intake = pal*rmr)  # calculating value of energy intake 

write_csv(df_2019_adult, here("inputs/processed/hse_2019.csv"))
