
##########################################################################################
# Policy 13 : Continued universal BMI monitoring for children in reception and year 6    #
#                                                                                        #
##########################################################################################

# Description:

# Continue the existing practice of BMI monitoring of children in reception and year six.
# The evidence comes from the rapid review 
# (https://docs.google.com/document/d/1RmVe5HAXthBYS3EImuuMDMZniN5OF0xyTg6ZQbVdzIU/edit) found an
# effect of reduction prevalence of overweight children by 4.3%. However, the result wasn't statistically
# significant. Therefore, in this case, we take the effect size of the intervention as '0'.
# The population segment for this intervention would be children aged 4-5 and those aged 10 - 11.
# Hence, in the absence of any intervention, we are unlikely to see any change in the prevalence of
# overweight or obesity. 

# This script is specific to England


rm(list = ls())
library(tidyverse)
library(here)
library(writexl)

source(file = "requirements.R")
source(file = "pre_processing/pre_processing_adult.R")
source(file = "models/adult_model_calorie.R")
source(file = "models/model_utils.R")
#source(file = "models/child_model_calorie.R")

# 
effect_size = 0

bmi_refdata_100centiles = generate_bmi_refdata_100centiles(sitar::uk90)


process_clean_save(file_path = "inputs/raw/hse_2019_eul_20211006.tab",
                   nation = "England",
                   population_group = "Children")
generate_bmi_refdata_100centiles
lookup_bmi_percentile_category
process_clean_save
calculate_bmi_from_eichange

df_child <- read_csv(here("inputs/processed/hse_2019_children.csv")) %>%
  rowwise() %>%
  #mutate(bmi_category_baseline = calculate_bmi_category(age = age, sex = sex, bmi = bmi, df_B = uk90_bmi_refdata)) %>%
  mutate(bmi_category_baseline = lookup_bmi_percentile_category(age = age, sex = sex, bmi = bmi, data_B = bmi_refdata_100centiles, value_to_calculate = "bmi_category")) %>%
  ungroup() %>%
  mutate(selection = ifelse(bmi_category_baseline %in% c("normal", "overweight", "obese") & 
                                        (age_grp %in% c("5-7", "8-10", "11-12")),
                                      "Yes", "No")) %>%
  mutate(bmi_change = ifelse(selection == "Yes", effect_size, 0 ))5


df_child = df_child %>%
  mutate(bmi_post_intervention = bmi + bmi_change) %>%
  rowwise() %>%
  mutate(bmi_category_post_intervention = lookup_bmi_percentile_category(age = age,
                                                                         sex = sex,
                                                                         bmi = bmi_post_intervention,
                                                                         data_B = bmi_refdata_100centiles,
                                                                         value_to_calculate = "bmi_category")) %>%
  ungroup()%>%
  mutate(bmi_category_year2 = bmi_category_post_intervention,
         bmi_category_year3 = bmi_category_post_intervention,
         bmi_category_year4 = bmi_category_post_intervention,
         bmi_category_year5 = bmi_category_post_intervention)

child_bmi_change = rbind(
  df_child %>% 
    count(bmi_category_baseline, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Baseline") %>% 
    rename(BMI = bmi_category_baseline),
  df_child %>% 
    count(bmi_category_post_intervention, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Year 1") %>% 
    rename(BMI = bmi_category_post_intervention),
  df_child %>% 
    count(bmi_category_year2, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Year 2") %>% 
    rename(BMI = bmi_category_year2),
  df_child %>% 
    count(bmi_category_year3, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Year 3") %>% 
    rename(BMI = bmi_category_year3),
  df_child %>% 
    count(bmi_category_year4, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Year 4") %>% 
    rename(BMI = bmi_category_year4),
  df_child %>% 
    count(bmi_category_year5, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Year 5") %>% 
    rename(BMI = bmi_category_year5)) %>%
  mutate(BMI = factor(BMI, levels = c("underweight", "normal", "overweight", "obese"))) %>%
  as.data.frame()


# output bar plot of BMI categories distributions
child_bar_plot = child_bmi_change %>%
  ggplot(., aes(y = freq, x = BMI, fill = type)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme_ipsum() +
  labs(fill = "", 
       title = "BMI Distribution", 
       y = "Prevalence - %",
       subtitle = "Children - England") +
  theme_ipsum(base_size = 8, axis_title_size = 8) + #, base_family="Averta"
  theme(legend.position = "top")


child_bar_plot

ggsave(here("outputs/policy_13/policy_13_impact_england_child.png"), 
       plot = child_bar_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')


# Output 2: Table of year wise prevalence of obesity
child_bmi_change_year = child_bmi_change %>%
  select(-c(n)) %>%
  pivot_wider(., names_from = BMI, values_from = freq) %>%
  select("type", "underweight", "normal", "overweight", "obese")

child_bmi_change_year

write_xlsx(path = "outputs/policy_13/policy_13_england.xlsx", x = child_bmi_change_year)

write.csv(df_child, file = "outputs/policy_13/policy_13_child_england_bmi.csv")
