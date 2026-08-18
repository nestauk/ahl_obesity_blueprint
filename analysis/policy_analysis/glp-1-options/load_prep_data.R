# DATA PREPARATION:
# Covers the following steps:
# 1. Identifies comorbidity based on medication status
# 2. Blows up survey weights to represent population counts
# 3. checks if population counts match england adult population (should be <= 1)

# Setup and function:
library(tidyverse)
library(here)
library(writexl)
library(readxl)
library(aws.s3)
library(arrow)

# Datasets:

# Reading in the required datasets:

# Step 0: Read in 2019 HSE dataset:
df_2019_adult_with_pop <- s3read_using(FUN = read.csv,
                                       bucket = "ahl-private-data",
                                       object = "hse/processed/hse_2019_16.csv") %>%
  # Step 1: Comorbidity identification:
  # Assign '1' if an individual has a condition, then sum to an eligibility
  mutate(
    cond_ascvd = case_when(
      cardiovd == 1 | platlets == 1 | ace_inhibitors == 1 | diuretics == 1 | lipid == 1 ~ 1,
      TRUE ~ 0
    ),
    cond_hypertension = case_when(hypertension == 1 ~ 1, TRUE ~ 0),
    cond_dyslipidaemia = case_when(lipid == 1 ~ 1, TRUE ~ 0),
    cond_diabetes = case_when(
      diabetes_type == 1 | metformin == 1 | anti_diabetics == 1 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  mutate(
    eligibility_score = cond_ascvd + cond_hypertension + cond_dyslipidaemia + cond_diabetes
  ) %>%
  # Step 2: blow up survey weights to population counts
  mutate(pop_share = wt_int / sum(wt_int)) %>%
  mutate(pop_estimate = round(pop_share * ENGLAND_ADULT_POPULATION, 0))

message(
  "Pop check (blown-up - ONS): ",
  sum(df_2019_adult_with_pop$pop_estimate) - ENGLAND_ADULT_POPULATION
)
