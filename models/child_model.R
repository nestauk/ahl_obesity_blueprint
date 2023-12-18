# Policy 4a :  Ban BOGOF price promotions on HFSS products for medium and large retailers
# Effect size from rapid reviews = -22 kcal reduction
# Intervention group: Children identified as overweight, obese or severely obese
# Outputs: BMI bar plot comparison between baseline and endline years and corresponding table


#rm(list = ls())
install.packages("svydesign")
install.packages("ggplot2")

library(tidyverse)
library(here)
library(bw)
library(survey)


child_intake_change = -5

df_child <- read_csv(here("inputs/processed/hse_2019_children.csv")) %>%  # we are reading a file that contains energy intake, BMI weight class and other variables reated during the pre-processing stage.
  mutate(sex = ifelse(sex == 1, "male", "female")) %>% 
  filter(fm_hudda > 0) %>%
  mutate(intake_diff = ifelse(bmi_cat_baseline %in% c("underweight", "normal"), 0, child_intake_change ))




#eichange_child <- t(apply(df_child, 1, function(x) rep(as.numeric(x["intake_diff"]), 365*1)))

#t_eichange_child <- t(apply(df_child, 1, function(x) rep(as.numeric(x["intake"]) + as.numeric(x["intake_diff"]), 365*1)))

energy_intake = apply(df_child, 1, function(x) rep(as.numeric(x["intake_hox"]) + as.numeric(x["intake_diff"]), 365*5))


# model 1 with fm and ffm
child_model_weight = child_weight(age = df_child$age,
                                  sex =  df_child$sex, 
                                  FM = df_child$fm_hudda, 
                                  FFM = df_child$ffm_hudda, 
                                  EI = energy_intake,   
                                  days = (365*5),
                                  checkValues = TRUE)


child_bw_h = as.data.frame(child_model_weight[["Body_Weight"]]) %>%
  mutate(bw_diff = V1825 - V1) %>%
  select(c(V1, V365, V730, V1095, V1460, V1825, bw_diff)) %>%
  rename(bw_1 = V1, bw_365 = V365, bw_730 = V730, 
         bw_1095 = V1095, bw_1460 = V1460, bw_1825 = V1825)



child_age_h = as.data.frame(child_model_weight[["Age"]]) %>%
  mutate(age_diff = V1825 - V1) %>%
  select(c(V1, V365, V730, V1095, V1460, V1825, age_diff)) %>%
  rename(age_1 = V1, age_365 = V365, age_730 = V730, 
         age_1095 = V1095, age_1460 = V1460, age_1825 = V1825)


child_ffm_h = as.data.frame(child_model_weight[["Fat_Free_Mass"]]) %>%
  mutate(ffm_diff = V1825 - V1) %>%
  select(c(V1, V365, V730, V1095, V1460, V1825, ffm_diff)) %>%
  rename(ffm_1 = V1, ffm_365 = V365, ffm_730 = V730,
         ffm_1095 = V1095, ffm_1460 = V1460, ffm_1825 = V1825)



child_fm_h = as.data.frame(child_model_weight[["Fat_Mass"]]) %>%
  mutate(fm_diff = V1825 - V1) %>%
  select(c(V1, V365, V730, V1095, V1460, V1825, fm_diff))  %>%
  rename(fm_1 = V1, fm_365 = V365, fm_730 = V730,
         fm_1095 = V1095, fm_1460 = V1460, fm_1825 = V1825)



child_age_bw_ffm_fm = cbind(child_bw_h, child_age_h, child_ffm_h, child_fm_h) %>%
  select(age_1, bw_1, fm_1, ffm_1,
         # age_365, bw_365, fm_365, ffm_365,
         # age_730, bw_730, fm_730, ffm_730,
         # age_1095, bw_1095, fm_1095, ffm_1095,
         # age_1460, bw_1460, fm_1460, ffm_1460,
         age_1825, bw_1825, fm_1825, ffm_1825)




new_df_child = cbind(df_child, child_age_bw_ffm_fm) %>%
  mutate(bf_percent_end = ((fm_1825/ bw_1825)*100)) %>%
  mutate(bmi_cat_endline = case_when(age == 6 ~ (case_when(sex == "male" ~ case_when(bf_percent_end <= 12.4 ~ "underweight",
                                                                                  bf_percent_end >= 19.5 & bf_percent_end < 22.7 ~ "overweight",
                                                                                  bf_percent_end >12.4 & bf_percent_end < 19.5 ~ "normal",
                                                                                  bf_percent_end >= 22.7 ~ "obese"),
                                                        sex == "female" ~ case_when(bf_percent_end <= 14.4 ~ "underweight",
                                                                                    bf_percent_end >14.4 & bf_percent_end < 23 ~ "normal",
                                                                                    bf_percent_end >= 23 & bf_percent_end < 26.2 ~ "overweight",
                                                                                    bf_percent_end >= 26.2 ~ "obese") )),
                                  age == 9 ~ (case_when(sex == "male" ~ case_when(bf_percent_end <= 12.8 ~ "underweight",
                                                                             bf_percent_end >12.8 & bf_percent_end < 22.2 ~ "normal",
                                                                             bf_percent_end >= 22.2 & bf_percent_end < 26.8 ~ "overweight",
                                                                             bf_percent_end >= 26.8 ~ "obese"),
                                                        sex == "female" ~ case_when(bf_percent_end <= 15.7 ~ "underweight",
                                                                             bf_percent_end >15.7 & bf_percent_end < 27.2 ~ "normal",
                                                                             bf_percent_end >= 27.2 & bf_percent_end < 31.2 ~ "overweight",
                                                                             bf_percent_end >= 31.2 ~ "obese") )),
                                  age == 11.5 ~ (case_when(sex == "male" ~ case_when(bf_percent_end <= 12.6 ~ "underweight",
                                                                                bf_percent_end >12.6 & bf_percent_end < 23 ~ "normal",
                                                                                bf_percent_end >= 23 & bf_percent_end < 28.3 ~ "overweight",
                                                                                bf_percent_end >= 28.3 ~ "obese"),
                                                           sex == "female" ~ case_when(bf_percent_end <= 16.1 ~ "underweight",
                                                                                bf_percent_end >16.1 & bf_percent_end < 28.8 ~ "normal",
                                                                                bf_percent_end >= 28.8 & bf_percent_end < 32.8 ~ "overweight",
                                                                                bf_percent_end >= 32.8 ~ "obese") )),
                                  age == 14 ~ (case_when(sex == "male" ~ case_when(bf_percent_end <= 10.9 ~ "underweight",
                                                                              bf_percent_end >10.9 & bf_percent_end < 21.3 ~ "normal",
                                                                              bf_percent_end >= 21.3 & bf_percent_end < 25.9 ~ "overweight",
                                                                              bf_percent_end >= 25.9 ~ "obese"),
                                                         sex == "female" ~ case_when(bf_percent_end <= 16 ~ "underweight",
                                                                              bf_percent_end > 16 & bf_percent_end < 29.6 ~ "normal",
                                                                              bf_percent_end >= 29.6 & bf_percent_end < 33.6 ~ "overweight",
                                                                              bf_percent_end >= 33.6 ~ "obese") )),
                                  age == 17 ~ (case_when(sex == "male" ~ case_when(bf_percent_end <= 9.8 ~ "underweight",
                                                                                   bf_percent_end > 9.8 & bf_percent_end < 20.1 ~ "normal",
                                                                                   bf_percent_end >= 20.1 & bf_percent_end < 23.9 ~ "overweight",
                                                                                   bf_percent_end >= 23.9 ~ "obese"),
                                                         sex == "female" ~ case_when(bf_percent_end <= 15.1 ~ "underweight",
                                                                                   bf_percent_end > 15.1 & bf_percent_end < 30.4 ~ "normal",
                                                                                   bf_percent_end >= 30.4 & bf_percent_end < 34.4 ~ "overweight",
                                                                                   bf_percent_end >= 34.4 ~ "obese") )) ))



child_bmi_change = rbind(
  new_df_child %>% 
    count(bmi_cat_endline, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Year 5") %>% 
    rename(BMI = bmi_cat_endline),
  new_df_child %>% 
    count(bmi_cat_baseline, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Year 0") %>% 
    rename(BMI = bmi_cat_baseline))

child_bmi_change = child_bmi_change %>%
  mutate(BMI = factor(BMI, levels = c("underweight", "normal", "overweight", "obese", "morbidly obese"))) %>%
  as.data.frame()



# Output 1: Plot of year on year BMI category distribution
child_bar_plot = child_bmi_change %>%
  ggplot(., aes(y = freq, x = BMI, fill = type)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme_ipsum() +
  labs(fill = "", 
       title = "BMI Categories Distribution", 
       y = "Frequency",
       subtitle = "Population") +
  theme_ipsum(base_size = 8, axis_title_size = 8) + #, base_family="Averta"
  theme(legend.position = "top")

child_bar_plot

ggsave(here("outputs/figures/child_bmi_distrib_policy4a.png"), plot = child_bar_plot, width = 10, height = 6, bg='#ffffff')

# Output 2: Table of year wise prevalence of obesity
child_bmi_change_year = child_bmi_change %>%
  select(-c(n)) %>%
  pivot_wider(., names_from = BMI, values_from = freq)

write_csv(child_bmi_change_year, here("outputs/data/child_bmi_distrib_policy4a.csv"))
