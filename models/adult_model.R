# Policy 4a :  Ban BOGOF price promotions on HFSS products for medium and large retailers
# Effect size from rapid reviews = -22 kcal reduction
# Intervention group: Adults with BMI >= 25 [overweight, obese and morbidly obese]
# Outputs: Year wise BMI bar plot and corresponding table

#rm(list = ls())
install.packages("svydesign")
install.packages("ggplot2")

library(tidyverse)
library(here)
library(bw)
library(survey)


# variable for change in intake as a result of the policy
intake_change = -22

# Reading the processed HSE 2019 data that contains energy intake, BMI weight class and other variables
# created during the pre-processing stage.
# The intake change is applied only to those who are overweight, obese and morbidly obese
df_adult <- read_csv(here("inputs/processed/hse_2019.csv")) %>%  
  mutate(sex = ifelse(sex == 1, "male", "female")) %>%
  mutate(intake_diff = ifelse(bmi_class %in% c("underweight", "normal"), 0, intake_change))

# For each individual/ observation in the HSE, we apply the difference in energy intake[intake_diff],
# (converting it into type numeric), then creating a vector, which essentially repeats the intake diff
# value 365*5 times (i.e. for a five year period), which is then transposed to match the input requirements
# of the Hall Calorie Weight Model.
eichange <- t(apply(df_adult, 1, function(x) rep(as.numeric(x["intake_diff"]), 365*5)))

# A matrix of change in salt consumption set to zero is another input to the model. This is set to zero as
# information on change in salt consumption is not available from our rapid reviews
nachange <- t(apply(df_adult, 1, function(x) rep(0, 365*5)))

# the bw package has a function called [adult_weight] that takes the following inputs:
# baseline body weight, height (in meters), age, sex and change in energy intake (for a five year period)
model_weight <- adult_weight(df_adult$weight, df_adult$height/100, df_adult$age, df_adult$sex, eichange, nachange, days = 365*5)

model_weight <- adult_weight(bw = df_adult$weight,
                             ht = df_adult$height/100,
                             age = df_adult$age,
                             sex = df_adult$sex,
                             EIchange = eichange,
                             NAchange = nachange,
                             days = 365*5)

# Extracting BMI values from the model and joining them to the HSE dataset for further analysis 
# and output generation. 'bmi_model' is a matrix of day wise change in BMI of the population as a result of
# the intervention. 
bmi_model = model_weight[["Body_Mass_Index"]]
new_df = cbind(df_adult, bmi_model)


# Creating a new dataframe with variables of interest and BMI values at the end of each of the five years
# of the intervention. Subsequently, categorising observations into BMI categories for each year.
post_df_adult = new_df %>%
  select("id", "weight", "height", "age", "sex", "bmi", "wt_int", "psu", 
         "strata", "pal", "rmr", "bmi_class", "intake", "intake_diff", "1", "365", "730", "1095", "1460", "1825" ) %>%
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

ggsave(here("outputs/figures/adult_bmi_distrib_policy4a.png"), plot = adult_bar_plot, width = 10, height = 6, bg='#ffffff')

# Output 2: Table of year wise prevalence of obesity
bmi_change_year = bmi_change %>%
  select(-c(n)) %>%
  pivot_wider(., names_from = BMI, values_from = freq)

write_csv(bmi_change_year, here("outputs/data/adult_bmi_distrib_policy4a.csv"))

