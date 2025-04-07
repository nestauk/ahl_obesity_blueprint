
#############################################################################################
# Policy 23 : Fund and roll out a mass media campaign aiming to promote healthy eating      #
#############################################################################################

# Description:

# The evidence from the rapid review 
# (https://docs.google.com/document/d/1V4IoNvfDgNZu91VtdZhuIqPEBCbFBIGbEe_K46Qgz9U/edit) 
# (quality assured by the EAG) showed that the interventions led to a reduction in 
# SSB consumption of 8% of those who are BMI ≥ 25. 
# In the UK, an average adult consumes 210 kcals of energy from SSB (Collins et al 2015).
# Assuming that a reduction in frequent SSB consumption equals 5% reduction in daily energy intake
# the reduction in daily energy intake is 10.5 kcals for 8% of those living with overweight or obesity.

# Citations:
# Collins B, Capewell S, O'Flaherty M, Timpson H, Razzaq A, Cheater S, Ireland R, Bromley H. Modelling the Health 
# Impact of an English Sugary Drinks Duty at National and Local Levels. PLoS One. 2015 Jun 29;10(6):e0130770. 
# doi: 10.1371/journal.pone.0130770. PMID: 26121677; PMCID: PMC4486083.



# setup
rm(list = ls())
gc()
library(tidyverse)
library(here)

source(file = "requirements.R")
source(file = "pre_processing/pre_processing_adult.R")

table_outputs = list() # creating a list of table outputs to be saved as an excel file


# Estimating the impact of the policy in:

# 1. Adults in Wales:

# 1.1. Cleaned input/ baseline data:

df_wales_cleaned = read_csv(here("inputs/processed/nsw_2019.csv"))

# 1.2. Estimating the impact of the intervention on prevalence of obesity:

# Inputs to the model:
# Effect size [A]: ﹣10.5 kcal
# Population segment impacted by policy [B]: Adults with BMI ≥ 25
# Compensation effect [C]: 23% of [A] = 2.42
# Duration [D]: 5 years ~ 365 * 5 days

# Based on [A] and [C], the intake change = effect size - compensation effect = -8.09 kcals
intake_change = -8.09
implementation_duration = 365*5

# Reading the processed Wales 2019 data that contains energy intake, BMI weight class and other variables
# created during the pre-processing stage.
# The intake change is applied only to those who are overweight, obese and morbidly obese
df_adult <- df_wales_cleaned %>%  
  mutate(sex = ifelse(sex == 1, "male", "female")) %>%
  mutate(intake_diff = ifelse(bmi_class %in% c("overweight", "obese"),
                              ifelse(runif(n()) <= 0.08, intake_change, 0),
                              0)) %>%
  mutate(intervention = ifelse(intake_diff != 0, "Yes", "No"))

# For each individual/ observation in the Wales NSW, we apply the difference in energy intake[intake_diff],
# (converting it into type numeric), then creating a vector, which essentially repeats the intake diff
# value 365*5 times (i.e. for a five year period), which is then transposed to match the input requirements
# of the Hall Calorie Weight Model.
eichange <- t(apply(df_adult, 1, function(x) rep(as.numeric(x["intake_diff"]), implementation_duration)))

# A matrix of change in salt consumption set to zero is another input to the model. This is set to zero as
# information on change in salt consumption is not available from our rapid reviews
nachange <- t(apply(df_adult, 1, function(x) rep(0, implementation_duration)))

# the bw package has a function called [adult_weight] that takes the following inputs:
# baseline body weight, height (in meters), age, sex and change in energy intake (for a five year period)

model_weight <- adult_weight(bw = df_adult$weight,
                             ht = df_adult$height/100,
                             age = df_adult$age,
                             sex = df_adult$sex,
                             EIchange = eichange,
                             NAchange = nachange,
                             days = implementation_duration)

# Extracting BMI values from the model and joining them to the Wales dataset for further analysis 
# and output generation. 'bmi_model' is a matrix of day wise change in BMI of the population as a result of
# the intervention. 
bmi_model = model_weight[["Body_Mass_Index"]]
new_df = cbind(df_adult, bmi_model)


# Creating a new dataframe with variables of interest and BMI values at the end of each of the five years
# of the intervention. Subsequently, categorising observations into BMI categories for each year.
post_df_adult = new_df %>%
  select("id", "weight", "height", "age", "sex", "bmi", "wt_int",  "intervention",
         "pal", "rmr", "bmi_class", "intake", "intake_diff", "1", "365", "730", "1095", "1460", "1825" ) %>%
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


# 1.3. Outputs:

# Output 1: Plot of year on year BMI category distribution
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

# Output 2: Table of year wise prevalence of obesity
bmi_change_year = bmi_change %>%
  select(-c(n)) %>%
  pivot_wider(., names_from = BMI, values_from = freq)%>%
  select(type, underweight, normal, overweight, obese, `morbidly obese`)

table_outputs[["wales_adult"]] = bmi_change_year

ggsave(here("outputs/policy_23/policy_23_impact_wales_adult.png"),
       plot = adult_bar_plot,
       width = 10,
       height = 6,
       bg='#ffffff')

write_xlsx(path = "outputs/policy_23/policy_23_wales.xlsx", x = table_outputs)

write.csv(post_df_adult, file = "outputs/policy_23/policy_23_adult_wales_bmi.csv")


