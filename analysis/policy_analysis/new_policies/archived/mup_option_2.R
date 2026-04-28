
#########################################################################################
# MUP Policy Option 2: Implement a 65p minimum unit pricing on alcohol sold in Englad   #
#########################################################################################


# All details for this policy can be found here - https://docs.google.com/document/d/1dWnjV9dU5eSV3GdUpn45eQzampI3e3iz4SRJCnmsGHs/edit?usp=sharing
# Option 2 is an estimation of the daily kcals from alcohol using Kantar 2021 data. (See Table 4, "op 2 mup_daily_kcal_reduction" sheet in this workbook - https://docs.google.com/spreadsheets/d/1oSHJFYu5ht8ioHIvL-gS5uFvQ8rPfs9H-ZRPMu16riQ/edit?usp=sharing)

# Setup:
rm(list = ls())
gc()
library(tidyverse)
library(here)
library(bw)
library(survey)
library(Hmisc)


table_outputs = list() # creating a list of table outputs to be saved as an excel file


# Setting up the data frame for running the model:

df_2019_adult <- read.table(here("inputs/raw/hse_2019_eul_20211006.tab"), sep = "\t", header = TRUE) %>% 
  # browser() %>%
  filter(WtVal>0 & HtVal>0 & Age35g >7 ) %>% # remove missing height and weight and children; 
  mutate(age = case_when(Age35g == 7 ~ ((16+19)/2) + 0.5,
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
  # browser() %>%
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
         alcohol_overall = alcbase_19,
         alcohol_male = alcbsmt_19,
         alcohol_female = alcbswt_19,
         id = SerialA,
         psu = PSU_SCR,
         strata = cluster94) %>%
  # browser() %>%
  mutate(alc_updated = case_when(alcohol_overall >=1 & alcohol_overall<=3 ~ "non_drinker",
                                 alcohol_overall >=4 & alcohol_overall<=7 ~ "low_risk",
                                 alcohol_overall >=8 & alcohol_overall<=11 ~ "inc_risk",
                                 alcohol_overall >=12 ~ "high_risk",
                                 TRUE ~ "NA")) %>%
  mutate(alc_female_updated = case_when(alcohol_female >=1 & alcohol_female<=2 ~ "non_drinker",
                                        alcohol_female >=3 & alcohol_female<=5 ~ "low_risk",
                                        alcohol_female >=6 & alcohol_female<=7 ~ "inc_risk",
                                        alcohol_female >=8 ~ "high_risk",
                                        TRUE ~ "NA")) %>%
  mutate(alc_male_updated = case_when(alcohol_male >=1 & alcohol_male<=2 ~ "non_drinker",
                                      alcohol_male >=3 & alcohol_male<=5 ~ "low_risk",
                                      alcohol_male >=6 & alcohol_male<=7 ~ "inc_risk",
                                      alcohol_male >=8 ~ "high_risk",
                                      TRUE ~ "NA")) %>%
  mutate(income_support_status = case_when((income_JSA == 1 | income_IS == 1 | income_PC == 1 | income_CTC == 1 | income_UC == 1) ~ 1,
                                           TRUE ~ 0)) %>%
  mutate(children_updated = case_when(number_children == 0 ~ 0,
                                      TRUE ~ 1)) %>%
  dplyr::select(id, weight, height, age_grp, age, sex, alc_updated, alcohol_overall, alc_male_updated,  alcohol_male, alc_female_updated, alcohol_female,  bmi, qimd, children_updated, income_support_status, ethnicity, diabetes, cardiovd, wt_int, psu, strata )  %>% # select variables needed
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



# Change in energy intake for diferent population groups:
# Each one represents the drinking behaviour category, for example, ei_change_14_21 is the change in
# energy intake for those who consume 14 to 21 units.

ei_change_0	= 0
ei_change_0_14 = -2
ei_change_14_21 =	-5
ei_change_21_35 =	-8
ei_change_35_50 =	-12
ei_change_50 = -16

# we apply the effect size to those who are living with obesity and drink:
bmi_threshold = 25

# Creating a new column called 'intake_change'
df_2019_adult_final = df_2019_adult %>%
  mutate(intake_change = case_when(alcohol_overall %in% c(1,2,3) ~ ei_change_0,
                                   alcohol_overall %in% c(4,5,6,7) & bmi >= bmi_threshold ~ ei_change_0_14,
                                   alcohol_overall %in% c(8) & bmi >= bmi_threshold ~ ei_change_14_21,
                                   alcohol_overall %in% c(9,10) & bmi >= bmi_threshold ~ ei_change_21_35,
                                   alcohol_overall %in% c(11) & bmi >= bmi_threshold ~ ei_change_35_50,
                                   alcohol_overall %in% c(12) & bmi >= bmi_threshold ~ ei_change_50,
                                   TRUE ~ ei_change_0)) %>%
  mutate(sex = ifelse(sex == 1, "male", "female"))




implmentation_duration = 365*5

# For each individual/ observation in the HSE, we apply the change in energy intake as a result of the MUP policy,
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


# Outputs:

# Output 1: Table of year wise prevalence of obesity

bmi_change_year = bmi_change %>%
  select(-c(n)) %>%
  pivot_wider(., names_from = BMI, values_from = freq) %>%
  select(type, underweight, normal, overweight, obese, `morbidly obese`)

bmi_change_year

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


ggsave(here("outputs/new_policies/mup_option_2/mup_option_2.png"), 
       plot = adult_bar_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')


# Outputs 3: summary results and detailed individual table:
# bmi year on year prevalence:
write_xlsx(path = "outputs/new_policies/mup_option_2/policy_mup_2_updated_england.xlsx", x = table_outputs)
write.csv(post_df_adult, file = "outputs/new_policies/mup_option_2/policy_mup_2_updated_england_bmi.csv")


