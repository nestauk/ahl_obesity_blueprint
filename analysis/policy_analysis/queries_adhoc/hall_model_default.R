# default implementation of Hall model using kcal change and sodium change as inputs:

# clearing the environment
rm(list = ls())

# installing the hall model package:
devtools::install_github("INSP-RH/bw")

# adding in necessary libraries:
library(tidyverse)
library(here)
library(bw)
library(survey)
library(Hmisc)

# reading in population health survey data:
df = read_csv(here("inputs/processed/hse_2019_1.csv"))

# chnage in sodium intake
na_intake_change = -5 # mg per day of salt


# change in kcal intake per day
# can give same or different values for intake change in different BMI groups:
intake_change_ov = -0
intake_change_ob = -0     
intake_change_mob = -0    

# this is the duration for which the we will simulate the weight loss
# set to five year, but can change as per requirement to how many every years required.
implmentation_duration = 365 * 5 # five years


df = df %>%  # processed dataset with variables such as height, weight, bmi, daily energy intake passed as input to the function 
  mutate(sex = ifelse(sex == 1, "male", "female")) %>%  # modifying the variable sex in a form expected by the Hall Model
  mutate(intake_diff = ifelse(bmi_class %in% c("underweight", "normal"), 0, 99)) %>% # creating a variable to store the change in energy intake due to intervention and applying it only to those with BMI >= 25
  mutate(intake_diff = case_when(bmi_class == "overweight" ~ intake_change_ov,
                                 bmi_class == "obese" ~ intake_change_ob,
                                 bmi_class == "morbidly obese" ~ intake_change_mob,
                                 TRUE ~ intake_diff)) %>%
  mutate(intervention = ifelse(bmi_class %in% c("underweight", "normal"), "No", "Yes")) %>%
  mutate(na_intake_diff = ifelse(bmi_class %in% c("underweight", "normal"), 0, na_intake_change))



# creating the matrix (for the duration of implementation of the policy) with change in daily energy
# intake due to the policy
ei_change <- t(apply(df, 1, function(x) rep(as.numeric(x["intake_diff"]), implmentation_duration)))



# A matrix of change in salt consumption set to zero is another input to the model. This is set to the 
# sodium intake change or duration of policy
nachange <- t(apply(df, 1, function(x) rep(as.numeric(x["na_intake_diff"]), implmentation_duration)))

# the bw package has a function called [adult_weight] that takes the following inputs:
# baseline body weight, height (in meters), age, sex and energy expenditure (for a five year period)
model_weight <- adult_weight(bw = df$weight,
                             ht = df$height/100,
                             age = df$age,
                             sex = df$sex,
                             EIchange = ei_change,
                             NAchange = nachange,
                             days = implmentation_duration)


# Extracting BMI values from the model and joining them to the HSE dataset for further analysis 
# and output generation. 'bmi_model' is a matrix of day wise change in BMI of the population as a result of
# the intervention. 
bmi_model = model_weight[["Body_Mass_Index"]]

post_df = cbind(df, bmi_model)


# Creating a new dataframe with variables of interest and BMI values at the end of each of the five years
# of the intervention. Subsequently, categorising observations into BMI categories for each year.
post_df_adult = post_df %>%
  select("id", "weight", "height", "age", "sex", "bmi", "wt_int", "psu", "strata", "pal",
         "rmr", "bmi_class", "intake", "intervention", "intake_diff", "1", "365", "730", "1095", "1460", "1825" ) %>%
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




# survey design specification to estimate population level effects of the intervention. Survey design spec is
# created using weightings from HSE 2019.
design <-  svydesign(ids=~post_df_adult$psu, 
                     nest = T,
                     data=post_df_adult,
                     weights=post_df_adult$wt_int)



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




# Output 2: Table of year wise prevalence of obesity
# open and view this table to see the per year reduction in prevalence:
bmi_change_year = bmi_change %>%
  select(-c(n)) %>%
  pivot_wider(., names_from = BMI, values_from = freq) %>%
  select(type, underweight, normal, overweight, obese, `morbidly obese`)

bmi_change_year
