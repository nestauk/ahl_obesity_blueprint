
rm(list = ls())
library(tidyverse)
library(here)
library(bw)
library(survey)
library(Hmisc)

df = read_csv(here("inputs/processed/hse_2019_1.csv"))

# intake_change = -38 # kcals

intake_change_ov = -0.64 #-8
intake_change_ob = -0.64 #-9     # 54
intake_change_mob = -0.64  #-10      # 64

implmentation_duration = 365 * 5 # five years


df = df %>%  # processed dataset with variables such as height, weight, bmi, daily energy intake passed as input to the function 
  mutate(sex = ifelse(sex == 1, "male", "female")) %>%  # modifying the variable sex in a form expected by the Hall Model
  mutate(intake_diff = ifelse(bmi_class %in% c("underweight", "normal"), 0, 99)) %>% # creating a variable to store the change in energy intake due to intervention and applying it only to those with BMI >= 25
  mutate(intake_diff = case_when(bmi_class == "overweight" ~ intake_change_ov,
                                 bmi_class == "obese" ~ intake_change_ob,
                                 bmi_class == "morbidly obese" ~ intake_change_mob,
                                 TRUE ~ intake_diff)) %>%
  mutate(intervention = ifelse(bmi_class %in% c("underweight", "normal"), "No", "Yes"))



# creating the matrix (for the duration of implementation of the policy) with change in daily energy intake due to the policy
ei_change <- t(apply(df, 1, function(x) rep(as.numeric(x["intake_diff"]), implmentation_duration)))



# A matrix of change in salt consumption set to zero is another input to the model. This is set to zero as
# information on change in salt consumption is not available from our rapid reviews
nachange <- t(apply(df, 1, function(x) rep(0, implmentation_duration)))


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

bmi_change_year = bmi_change %>%
  select(-c(n)) %>%
  pivot_wider(., names_from = BMI, values_from = freq) %>%
  select(type, underweight, normal, overweight, obese, `morbidly obese`)

bmi_change_year





## hse 2019 input data:

df_2019_adult <- read.table(here("inputs/raw/hse_2019_eul_20211006.tab"), sep = "\t", header = TRUE) %>% 
  # browser() %>%
  filter(WtVal>0 & HtVal>0 & Age35g >=7 ) %>% # remove missing height and weight and children; 
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
         id = SerialA,
         psu = PSU_SCR,
         strata = cluster94) %>%
  # browser() %>%
  mutate(income_support_status = case_when((income_JSA == 1 | income_IS == 1 | income_PC == 1 | income_CTC == 1 | income_UC == 1) ~ 1,
                                           TRUE ~ 0)) %>%
  mutate(children_updated = case_when(number_children == 0 ~ 0,
                                      TRUE ~ 1)) %>%
  dplyr::select(id, weight, height, age_grp, age, sex, bmi, qimd, children_updated, income_support_status, ethnicity, diabetes, cardiovd, wt_int, psu, strata )  %>% # select variables needed
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


bmi_class_share = df_2019_adult %>% 
  count(bmi_class, wt = wt_int) %>% 
  mutate(freq = n/sum(n)*100)

# mean intake by bmi class:
mean_intake_df = df_2019_adult %>%
  group_by(bmi_class) %>%
  summarise(intakeM = round(wtd.mean(intake, weight = wt_int),1)) 


# mean intake for all living with excess weight
mean_intake_excess_weight = df_2019_adult %>%
  filter(bmi_class %in% c("overweight", "obese", "morbidly obese")) %>%
  summarise(intakeM = round(wtd.mean(intake, weight = wt_int),1)) %>%
  as.numeric()

# mean intake for the full population
mean_intake_pop = df_2019_adult %>%
  summarise(intakeM = round(wtd.mean(intake, weight = wt_int),1)) %>%
  as.numeric()

# updating the mean intake df with excess weight and population mean intake values
mean_intake_df = mean_intake_df %>%
  add_row(bmi_class = c("excess weight - mean", "population - mean"),
          intakeM = c(mean_intake_excess_weight, mean_intake_pop))



write_csv(df_2019_adult, here("inputs/processed/hse_2019_1.csv"))
# print("Output csv with processed data is saved here: inputs/processed/hse_2019.csv" )


df_2018 = read.table("C:/git/ahl_prevention/data/hse_2018.tab", sep = "\t", header = TRUE)

write.csv(df_2018, "C:/git/ahl_prevention/data/hse_2018.csv", row.names = FALSE)

test_df_2018 = read.csv(file = "C:/git/ahl_prevention/data/hse_2018.csv", header = TRUE)
