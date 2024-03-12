
#################################################################################################
# Policy 22 : Expand PA education classes and extracurricular activity provision to ensure      #
#             every child achieves 60 minutes of physical activity a day                        #
#                                                                                               #
#################################################################################################

# Description:

# The policy aims to introduce/ increase activities to ensure that each child is engaged in 60
# minutes of physical activity every day. The techincal report of the rapid review for the 
# intervention can be found here: https://docs.google.com/document/d/1_BUvPiDKc-cW6ATAXDp77yQbjTlVyH9Xy_UDyc5_FtE/edit
# The mean difference in BMI was found to be 0.07 kg/m2

# This script is specific to England

rm(list = ls())
library(tidyverse)
library(here)
library(writexl)

source(file = "requirements.R")
source(file = "pre_processing/pre_processing_adult.R")
source(file = "models/model_utils.R")


effect_size = -0.07

# geenrating reference BMI percentile wise data
bmi_refdata_100centiles = generate_bmi_refdata_100centiles(sitar::uk90)

# cleaning and saving child data from HSE 2019
process_clean_save(file_path = "inputs/raw/hse_2019_eul_20211006.tab",
                   nation = "England",
                   population_group = "Children")


# calculating the bmi category at baseline and then identifying individuals who require treatment
# underweight children are excluded from receiving the intervention as in multiple cases food environment
# interventions to improve people's diet have little to no effect on underweight prevalence among children

df_child <- read_csv(here("inputs/processed/hse_2019_children.csv")) %>%
  rowwise() %>%
  mutate(bmi_category_baseline = lookup_bmi_percentile_category(age = age, sex = sex, bmi = bmi, data_B = bmi_refdata_100centiles, value_to_calculate = "bmi_category")) %>%
  ungroup() %>%
  mutate(intervention_status = ifelse(bmi_category_baseline %in% c("normal", "overweight", "obese"),"Yes", "No")) %>%
  mutate(bmi_change = ifelse(intervention_status == "Yes", effect_size, 0 ))


# applying bmi change to the baseline year
df_child = df_child %>%
  mutate(bmi_post_intervention = bmi + bmi_change) %>%
  rowwise() %>%
  mutate(bmi_category_post_intervention = lookup_bmi_percentile_category(age = age,
                                                                         sex = sex,
                                                                         bmi = bmi_post_intervention,
                                                                         data_B = bmi_refdata_100centiles,
                                                                         value_to_calculate = "bmi_category")) %>%
  ungroup() %>%
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

ggsave(here("outputs/policy_22/policy_22_impact_england_child.png"), 
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

write_xlsx(path = "outputs/policy_22/policy_22_england.xlsx", x = child_bmi_change_year)

