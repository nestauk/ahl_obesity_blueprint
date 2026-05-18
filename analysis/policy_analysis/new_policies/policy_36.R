
#########################################################################################
# Policy 36 (MUP): Implement a 65p minimum unit pricing on alcohol sold in England      #
#########################################################################################

# All details for this policy can be found here - MUP - estimating daily kcal reduction [https://docs.google.com/document/d/1dWnjV9dU5eSV3GdUpn45eQzampI3e3iz4SRJCnmsGHs/edit?usp=sharing]

rm(list = ls())
gc()
library(tidyverse)
library(here)
library(bw)
library(survey)
library(Hmisc)

source(file = "post_processing/post_processing.R")
source(file = "requirements.R")
source(file = "pre_processing/pre_processing_adult.R")
source(file = "models/adult_model_calorie.R")

table_outputs = list() # creating a list of table outputs to be saved as an excel file

# reading in HSE 2019 data and preparing it for implementing the Hall Model

# Cleaning the input/ baseline data:
process_clean_save(file_path = "inputs/raw/hse_2019_eul_20211006.tab", nation = "England", population_group = "Adult")


# reading in the cleaned processed baseline data file:
df_2019_adult = read_csv(here("inputs/processed/hse_2019.csv"))


# Estimating the effect size, i.e. reduction in energy intake per person

# Based on calculations in the linked document above (see Table 4)
# we assign change in energy intake depending on drinking behaviour measured by number of units of alcohol per week
ei_change_0	= 0
ei_change_0_14 = -2
ei_change_14_21 =	-4
ei_change_21_35 =	-7
ei_change_35_50 =	-11
ei_change_50 = -14

# we set the bmi threshold as 25 as we would like to apply the change in energy intake only for those with a BMI >=25
bmi_threshold = 25

# Values and Labels for alcohol_overall [HSE variable = alcbase_19]
# Value = 1.0	Label = Never drank
# Value = 2.0	Label = Ex-drinker
# Value = 3.0	Label = Trivial drinker
# Value = 4.0	Label = Non-zero, but under 1
# Value = 5.0	Label = 1-7
# Value = 6.0	Label = Over 7-10
# Value = 7.0	Label = Over 10-14
# Value = 8.0	Label = Over 14-21
# Value = 9.0	Label = Over 21-28
# Value = 10.0	Label = Over 28-35
# Value = 11.0	Label = Over 35-50
# Value = 12.0	Label = Over 50
# Value = -9.0	Label = Refused
# Value = -8.0	Label = Don't know
# Value = -1.0	Label = Not applicable

# As discussed above the intake change is assigned based on if an individuals alcohol consumption per week and
# their BMI status. If these two conditions are not met then their intake change is assigned '0'.

df_2019_adult_final = df_2019_adult %>%
  mutate(intake_change = case_when(alcohol_overall %in% c(1,2,3) ~ ei_change_0,
                                   alcohol_overall %in% c(4,5,6,7) & bmi >= bmi_threshold ~ ei_change_0_14,
                                   alcohol_overall %in% c(8) & bmi >= bmi_threshold ~ ei_change_14_21,
                                   alcohol_overall %in% c(9,10) & bmi >= bmi_threshold ~ ei_change_21_35,
                                   alcohol_overall %in% c(11) & bmi >= bmi_threshold ~ ei_change_35_50,
                                   alcohol_overall %in% c(12) & bmi >= bmi_threshold ~ ei_change_50,
                                   TRUE ~ ei_change_0)) %>%
  mutate(sex = ifelse(sex == 1, "male", "female"))


# Implementation duration is the duration for which we assume that the policy will be implemented and the duration
# during which we are interdted in estimating the impact for.
implmentation_duration = 365*5

# For each individual/ observation in the HSE, we apply the energy expenditure as a result of the activity,
# (converting it into type numeric), then creating a vector, which essentially repeats the intake diff
# value 365*5 times (i.e. for a five year period), which is then transposed to match the input requirements
# of the Hall Model.
ei_change <- t(apply(df_2019_adult_final, 1, function(x) rep(as.numeric(x["intake_change"]), implmentation_duration)))


# A matrix of change in salt consumption set to zero is another input to the model. This is set to zero as
# information on change in salt consumption is not available from our rapid reviews
nachange <- t(apply(df_2019_adult_final, 1, function(x) rep(0, implmentation_duration)))


# the bw package has a function called [adult_weight] that takes the following inputs:
# baseline body weight, height (in meters), age, sex and energy expenditure (for a five year period)
model_weight <- adult_weight(bw = df_2019_adult_final$weight,
                             ht = df_2019_adult_final$height/100,
                             age = df_2019_adult_final$age,
                             sex = df_2019_adult_final$sex,
                             EIchange = ei_change,
                             NAchange = nachange,
                             days = implmentation_duration)


# Extracting BMI values from the model and joining them to the HSE dataset for further analysis 
# and output generation. 'bmi_model' is a matrix of day wise change in BMI of the population as a result of
# the intervention. 
bmi_model = model_weight[["Body_Mass_Index"]]
post_df = cbind(df_2019_adult_final, bmi_model)


# Creating a new dataframe with variables of interest and BMI values at the end of each of the five years
# of the intervention. Subsequently, categorising observations into BMI categories for each year.
post_df_adult = post_df %>%
  select("id", "weight", "height", "age", "sex", "bmi", "wt_int", "psu", "strata", "pal",
         "rmr", "bmi_class", "intake", "intake_change", "1", "365", "730", "1095", "1460", "1825" ) %>%
  rename(bmi_5 = "1825", bmi_0 = "1", bmi_1 = "365", bmi_2 = "730", bmi_3 = "1095", bmi_4 = "1460" ) %>%
  mutate(bmi_0_class = case_when(bmi_0 <= 18.5 ~ "underweight",
                                 bmi_0 > 18.5 & bmi_0 < 25 ~ "normal",
                                 bmi_0 >= 25 & bmi_0 < 30 ~ "overweight",
                                 bmi_0 >= 30 & bmi_0 < 40 ~ "obese",
                                 bmi_0 >= 40 ~ "morbidly obese",
                                 TRUE ~ "NA"),
         bmi_1_class = case_when(bmi_1 <= 18.5 ~ "underweight",
                                 bmi_1 > 18.5 & bmi_1 < 25 ~ "normal",
                                 bmi_1 >= 25 & bmi_1 < 30 ~ "overweight",
                                 bmi_1 >= 30 & bmi_1 < 40 ~ "obese",
                                 bmi_1 >= 40 ~ "morbidly obese",
                                 TRUE ~ "NA"),
         bmi_2_class = case_when(bmi_2 <= 18.5 ~ "underweight",
                                 bmi_2 > 18.5 & bmi_2 < 25 ~ "normal",
                                 bmi_2 >= 25 & bmi_2 < 30 ~ "overweight",
                                 bmi_2 >= 30 & bmi_2 < 40 ~ "obese",
                                 bmi_2 >= 40 ~ "morbidly obese",
                                 TRUE ~ "NA"),
         bmi_3_class = case_when(bmi_3 <= 18.5 ~ "underweight",
                                 bmi_3 > 18.5 & bmi_3 < 25 ~ "normal",
                                 bmi_3 >= 25 & bmi_3 < 30 ~ "overweight",
                                 bmi_3 >= 30 & bmi_3 < 40 ~ "obese",
                                 bmi_3 >= 40 ~ "morbidly obese",
                                 TRUE ~ "NA"),
         bmi_4_class = case_when(bmi_4 <= 18.5 ~ "underweight",
                                 bmi_4 > 18.5 & bmi_4 < 25 ~ "normal",
                                 bmi_4 >= 25 & bmi_4 < 30 ~ "overweight",
                                 bmi_4 >= 30 & bmi_4 < 40 ~ "obese",
                                 bmi_4 >= 40 ~ "morbidly obese",
                                 TRUE ~ "NA"),
         bmi_5_class = case_when(bmi_5 <= 18.5 ~ "underweight",
                                 bmi_5 > 18.5 & bmi_5 < 25 ~ "normal",
                                 bmi_5 >= 25 & bmi_5 < 30 ~ "overweight",
                                 bmi_5 >= 30 & bmi_5 < 40 ~ "obese",
                                 bmi_5 >= 40 ~ "morbidly obese",
                                 TRUE ~ "NA"))



# creating a table of year wise distribution of BMI categories
bmi_change = rbind(
  post_df_adult %>% 
    count(bmi_5_class, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Year 5") %>% 
    rename(BMI = bmi_5_class),
  post_df_adult %>% 
    count(bmi_4_class, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Year 4") %>% 
    rename(BMI = bmi_4_class),
  post_df_adult %>% 
    count(bmi_3_class, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Year 3") %>% 
    rename(BMI = bmi_3_class),
  post_df_adult %>% 
    count(bmi_2_class, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Year 2") %>% 
    rename(BMI = bmi_2_class),
  post_df_adult %>% 
    count(bmi_1_class, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Year 1") %>% 
    rename(BMI = bmi_1_class),
  post_df_adult %>% 
    count(bmi_class, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Year 0") %>% 
    rename(BMI = bmi_class))

bmi_change = bmi_change %>%
  mutate(BMI = factor(BMI, levels = c("underweight", "normal", "overweight", "obese", "morbidly obese"))) %>%
  as.data.frame()

bmi_change_year = bmi_change %>%
  select(-c(n)) %>%
  pivot_wider(., names_from = BMI, values_from = freq) %>%
  select(type, underweight, normal, overweight, obese, `morbidly obese`)

bmi_change_year


# post processing
# extracting the reduction in obesity prevalence 

annual_obesity_prevalence_england = extract_relative_change(data = bmi_change_year)

# Relative reduction in obesity prevalence in England = 1%

# Adding to table outputs:
table_outputs[["annual_obesity_prevalence_eng"]] = annual_obesity_prevalence_england

# Estimating the annual value to government (benefit):

annual_benefit_to_gov = extract_pound_benefit(data = annual_obesity_prevalence_england, 
                                              cost = MODEL_CONSTANTS$COST_OF_OBESITY_IN_BILLIONS,
                                              duration = MODEL_CONSTANTS$MODEL_DURATION)

# Average annual value to government compared to baseline = £0.58 billions

# Adding to table outputs:
table_outputs[["annual_benefit_to_gov"]] = annual_benefit_to_gov




# Outputs:

# Output 1: Table of year wise prevalence of obesity


table_outputs[["england_adult"]] = bmi_change_year


# Output 2: Plot of BMI distribution(bar charts)
# Plot of year on year BMI category distribution
adult_bar_plot = bmi_change %>%
  ggplot(., aes(y = freq, x = BMI, fill = type)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme_ipsum() +
  labs(fill = "", 
       title = "BMI Categories Distribution", 
       y = "Frequency",
       subtitle = "Population") +
  theme_ipsum(base_size = 8, axis_title_size = 8) + #, base_family="Averta"
  theme(legend.position = "top")

adult_bar_plot


ggsave(here("outputs/new_policies/policy_36/policy_36.png"), 
       plot = adult_bar_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')


# Outputs 3: summary results and detailed individual table:
# bmi year on year prevalence:
write_xlsx(path = "outputs/new_policies/policy_36/policy_36.xlsx", x = table_outputs)

# Output 3: Cost Modelling input files:
write.csv(post_df_adult, file = "outputs/new_policies/policy_36/policy_36_adult_england_bmi.csv")



