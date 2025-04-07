
# setup
rm(list = ls())
library(tidyverse)
library(here)

source(file = "requirements.R")

file_path = "inputs/raw/gold_ahl_nsw_nsw_20192020_1.csv"

# functions required for data cleaning:

adjust_measurements <- function(df, height_col, weight_col, sex_col, age_col) {
  
  # Check if required columns exist
  required_cols <- c(height_col, weight_col, sex_col, age_col)
  if (!all(required_cols %in% names(df))) {
    stop("One or more specified columns do not exist in the data frame.")
  }
  
  # Extract required columns
  height <- df[[height_col]]
  weight <- df[[weight_col]]
  sex <- df[[sex_col]]
  age <- df[[age_col]]
  
  # Validate Sex input
  if (!all(sex %in% c(1, 2))) {
    stop("Invalid Sex values found. Use 1 for Female, 2 for Male.")
  }
  
  # Adjusted Height Calculation
  df$height_adjusted <- ifelse(sex == 1,
                               292.227 + (-3.3763324 * height) + (0.0217649 * height^2) + (-0.0000364 * height^3) + 
                                 (0.0788123 * age) + (-0.0012560 * age^2),
                               160.3468 + (-1.7360242 * height) + (0.0159004 * height^2) + (-0.0000321 * height^3) + 
                                 (0.1119681 * age) + (-0.0016046 * age^2))
  
  # Adjusted Weight Calculation
  df$weight_adjusted <- ifelse(sex == 1,
                               9.9126 + (0.6653765 * weight) + (0.0036857 * weight^2) + (-0.0000125 * weight^3) + 
                                 (0.0319996 * age) + (-0.0001959 * age^2),
                               1.281195 + (0.9487828 * weight) + (0.001376 * weight^2) + (-0.00000755 * weight^3) + 
                                 (0.0083899 * age) + (-0.0000739 * age^2))
  
  return(df)
}




df_2019_adult_wales_raw = read.csv(file = here(file_path), sep = ",", header = TRUE)


df_2019_adult_wales_raw_filtered <- df_2019_adult_wales_raw %>% # reading in the raw data file into a dataframe
  filter(dvwtkg>0 & dvhtcm>0 & age >= 18) %>% # removing observations with missing height and weight information as well as children as this is an adult model
  rename(weight_orig = dvwtkg,
         height_orig = dvhtcm,
         sex = sex,
         bmi_orig = dvbmi,
         wimd = dvwimdovr5,
         diabetes = dvillness2,
         cardiovd = dvillchap7,
         ethnicity = ethnicity,
         id = caseno,
         wt_int = samplepophlthweight) %>%
  mutate(age_grp = case_when(age >= 16 & age <= 19 ~ "16-19",
                             age >= 20 & age <= 24 ~ "20-24",
                             age >= 25 & age <= 29 ~ "25-29",
                             age >= 30 & age <= 34 ~ "30-34",
                             age >= 35 & age <= 39 ~ "35-39",
                             age >= 40 & age <= 44 ~ "40-44",
                             age >= 45 & age <= 49 ~ "45-49", 
                             age >= 50 & age <= 54 ~ "50-54",
                             age >= 55 & age <= 59 ~ "55-59",
                             age >= 60 & age <= 64 ~ "60-64",
                             age >= 65 & age <= 69 ~ "65-69",
                             age >= 70 & age <= 74 ~ "70-74",
                             age >= 75 ~ "75+",
                             TRUE ~ "NA")) %>%
  dplyr::select(id, weight_orig, height_orig, age_grp, age, sex, bmi_orig, wimd, diabetes, cardiovd, ethnicity, wt_int) %>% # select variables needed for analysis and would be inputs for modelling
  mutate(simd_updated = case_when((wimd == 1 | wimd == 2) ~ 1,
                                  TRUE ~ 0)) %>%
  mutate(bmi_class_orig =  case_when(bmi_orig <= 18.5 ~ "underweight",
                                     bmi_orig > 18.5 & bmi_orig < 25 ~ "normal",
                                     bmi_orig >= 25 & bmi_orig < 30 ~ "overweight",
                                     bmi_orig >= 30 & bmi_orig < 40 ~ "obese",
                                     bmi_orig >= 40 ~ "morbidly obese",
                                     TRUE ~ "NA")) # classifying individuals into BMI categories based on BMI values at baseline


df_2019_adult_wales_adjusted = adjust_measurements(df = df_2019_adult_wales_raw_filtered, height_col = "height_orig", weight_col = "weight_orig", sex_col = "sex", age_col = "age") %>%
  mutate(bmi_adjusted = weight_adjusted / (height_adjusted/100)^2) %>%
  rename(height = height_adjusted,
         weight = weight_adjusted,
         bmi = bmi_adjusted) %>%
  mutate(bmi_class = case_when(bmi <= 18.5 ~ "underweight",
                               bmi > 18.5 & bmi < 25 ~ "normal",
                               bmi >= 25 & bmi < 30 ~ "overweight",
                               bmi >= 30 & bmi < 40 ~ "obese",
                               bmi >= 40 ~ "morbidly obese",
                               TRUE ~ "NA")) %>% # classifying individuals into BMI categories based on BMI values at baseline
  mutate(pal = 1.6,
         rmr = case_when(sex == 1 ~ ((10 * weight) + (6.25 * height) - (5 * age) + 5),
                         TRUE ~ ((10 * weight) + (6.25 * height) - (5 * age) - 161))) %>% # Calculating an individuals resting metabolic rate using equations published in Mifflin & St.Jeor (1990)
  mutate(intake = pal*rmr)  # calculating energy intake at baseline

# distribution plots:

ggplot(df_2019_adult_wales_adjusted, aes(x = bmi)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "BMI Adjusted Distribution", x = "BMI Adjusted", y = "Count") +
  theme_minimal()


ggplot(df_2019_adult_wales_adjusted, aes(x = bmi_orig)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "BMI Adjusted Distribution", x = "BMI Original", y = "Count") +
  theme_minimal()


ggplot(df_2019_adult_wales_adjusted, aes(x = bmi)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot of Adjusted BMI", x = "BMI Adjusted", y = "Density") +
  theme_minimal()


ggplot(df_2019_adult_wales_adjusted, aes(x = bmi_orig)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot of Adjusted BMI", x = "BMI Original", y = "Density") +
  theme_minimal()


bmi_prevalence_pre_adjustment = df_2019_adult_wales_adjusted %>%
  count(bmi_class_orig, wt = wt_int) %>% 
  mutate(freq = n/sum(n)*100,
         type = "baseline - original") %>% 
  mutate(bmi_class_orig = factor(bmi_class_orig, levels = c("underweight", "normal", "overweight", "obese", "morbidly obese"))) %>% 
  rename(BMI = bmi_class_orig)


bmi_prevalence_post_adjustment = df_2019_adult_wales_adjusted %>%
  count(bmi_class, wt = wt_int) %>% 
  mutate(freq = n/sum(n)*100,
         type = "baseline - post adjustment") %>% 
  mutate(bmi_class = factor(bmi_class, levels = c("underweight", "normal", "overweight", "obese", "morbidly obese"))) %>%
  rename(BMI = bmi_class)



write_csv(df_2019_adult_wales_adjusted, here("inputs/processed/nsw_2019.csv"))
print("Output csv with processed Welsh data is saved here: inputs/processed/hse_2019.csv" )
