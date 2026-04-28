### case 1 and 1b
#rm(list = ls())
install.packages("svydesign")
install.packages("ggplot2")

library(tidyverse)
library(here)
library(bw)
library(survey)
library(sitar)
library(jsonlite)

#57.5
intake_change = 5
oplist = list()

calculate_intake_change = function(age, sex, intake_change, data_B){
  #browser()
  
  age = floor(age)
  
  if (sex == "female" | sex == 2){
    sex = 2
  } else { if(sex == "male" | sex == 1){
    
    sex = 1
    
  } }
  
  effect_weight = data_B$weight[data_B$age == age & data_B$sex == sex]
  
  prop_intake_change = effect_weight*intake_change
  
  
  return(prop_intake_change)
  
}



sacn_dietary_intake = generate_sacn_dietary_intake("C:/Users/Anish.Chacko/Downloads/energy_ref_data/sacn_dietary_intake.csv")

df_child <- read_csv(here("inputs/processed/hse_2019_children.csv")) %>%  # we are reading a file that contains energy intake, BMI weight class and other variables reated during the pre-processing stage.
  mutate(sex = ifelse(sex == 1, "male", "female")) %>% 
  filter(fm_hudda > 0) %>%
  #mutate(intake_diff = ifelse(bmi_cat_baseline %in% c("underweight", "normal"), 0, child_intake_change )) %>%
  rowwise() %>%
  mutate(bmi_cat_b = calculate_bmi_category(age = age, sex = sex, bmi = bmi, df_B = generate_bmi_refdata(sitar::uk90))) %>%
  mutate(intake_change = calculate_intake_change(age = age, sex = sex, intake_change = -intake_change, 
                                                 data_B = read.csv(file = "C:/Users/Anish.Chacko/Downloads/energy_ref_data/effect_weighting.csv")))



energy_intake = apply(df_child, 1, function(x) rep(as.numeric(x["intake_hox"]) + as.numeric(x["intake_change"]), 365*5))






df_child <- df_child %>%
  mutate(sex = ifelse(sex == 1, "male", "female")) 

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
         age_365, bw_365, fm_365, ffm_365,
         age_730, bw_730, fm_730, ffm_730,
         age_1095, bw_1095, fm_1095, ffm_1095,
         age_1460, bw_1460, fm_1460, ffm_1460,
         age_1825, bw_1825, fm_1825, ffm_1825)


uk90_height_refdata = generate_height_refdata("C:/Users/Anish.Chacko/Downloads/ht_ref_data/cole-nine-centiles-uk-who-female-height.json",
                                              "C:/Users/Anish.Chacko/Downloads/ht_ref_data/cole-nine-centiles-uk-who-male-height.json")

height_refdata_100centile = generate_height_refdata_100centiles(sitar::uk90)

height_refdata_100centile = height_refdata_100centile %>%
  select(-c(L.ht, M.ht, S.ht)) %>%
  unite(col = "sex_years", "sex", "years", sep = "_") %>%
  pivot_longer(cols= c(starts_with("p")), names_to = "centile", values_to = "height") %>%
  separate("sex_years", into = c("sex", "age"), sep = "_") %>%
  mutate(centile = substr(centile, 3, nchar(centile)))



uk90_bmi_refdata = generate_bmi_refdata(sitar::uk90)


new_df_child = cbind(df_child, child_age_bw_ffm_fm) %>%
  rowwise() %>%
  mutate(ht_365  = lookup_projected_height(age, sex, height, data_B = height_refdata_100centile, years_added = 1)) %>%
  mutate(ht_730  = lookup_projected_height(age, sex, height, data_B = height_refdata_100centile, years_added = 2)) %>%
  mutate(ht_1095 = lookup_projected_height(age, sex, height, data_B = height_refdata_100centile, years_added = 3)) %>%
  mutate(ht_1460 = lookup_projected_height(age, sex, height, data_B = height_refdata_100centile, years_added = 4)) %>%
  mutate(ht_1825 = lookup_projected_height(age, sex, height, data_B = height_refdata_100centile, years_added = 5)) %>%
  ungroup() %>%
  mutate(bmi_365  = bw_365  / (height/100)^2,
         bmi_730  = bw_730  / (height/100)^2,
         bmi_1095 = bw_1095 / (height/100)^2,
         bmi_1460 = bw_1460 / (height/100)^2,
         bmi_1825 = bw_1825 / (height/100)^2) %>%
  rowwise() %>%
  mutate(bmi_cat_1 = calculate_bmi_category(age = age, sex = sex, bmi = bmi, df_B = uk90_bmi_refdata),
         bmi_cat_365 = calculate_bmi_category(age = age + 1, sex = sex, bmi = bmi_365, df_B = uk90_bmi_refdata),
         bmi_cat_730 = calculate_bmi_category(age = age + 2, sex = sex, bmi = bmi_730, df_B = uk90_bmi_refdata),
         bmi_cat_1095 = calculate_bmi_category(age = age + 3, sex = sex, bmi = bmi_1095, df_B = uk90_bmi_refdata),
         bmi_cat_1460 = calculate_bmi_category(age = age + 4, sex = sex, bmi = bmi_1460, df_B = uk90_bmi_refdata),
         bmi_cat_1825 = calculate_bmi_category(age = age + 5, sex = sex, bmi = bmi_1825, df_B = uk90_bmi_refdata)) %>%
  ungroup()


child_bmi_change = rbind(
  new_df_child %>% 
    count(bmi_cat_1, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Baseline") %>% 
    rename(BMI = bmi_cat_1),
  new_df_child %>% 
    count(bmi_cat_365, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Year 1") %>% 
    rename(BMI = bmi_cat_365),
  new_df_child %>% 
    count(bmi_cat_730, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Year 2") %>% 
    rename(BMI = bmi_cat_730),
  new_df_child %>% 
    count(bmi_cat_1095, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Year 3") %>% 
    rename(BMI = bmi_cat_1095),
  new_df_child %>% 
    count(bmi_cat_1460, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Year 4") %>% 
    rename(BMI = bmi_cat_1460),
  new_df_child %>% 
    count(bmi_cat_1825, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Year 5") %>% 
    rename(BMI = bmi_cat_1825))

child_bmi_change = child_bmi_change %>%
  mutate(BMI = factor(BMI, levels = c("underweight", "normal", "overweight", "obese"))) %>%
  as.data.frame()


# output bar plot of BMI categories distributions
child_bar_plot = child_bmi_change %>%
  ggplot(., aes(y = freq, x = BMI, fill = type)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme_ipsum() +
  labs(fill = "", 
       title = "BMI Distribution: Case - 1b", 
       y = "Prevalence - %",
       subtitle = "Children - Proportional kcals - reduction = 5kcals") +
  theme_ipsum(base_size = 8, axis_title_size = 8) + #, base_family="Averta"
  theme(legend.position = "top")


child_bar_plot

# ggsave(here("outputs/figures/child_bmi_distrib_policy4a.png"), plot = child_bar_plot, width = 10, height = 6, bg='#ffffff')

# Output 2: Table of year wise prevalence of obesity
child_bmi_change_year = child_bmi_change %>%
  select(-c(n)) %>%
  pivot_wider(., names_from = BMI, values_from = freq)

#write.csv(child_bmi_change_year, file = "C:/git/ahl_obesity_blueprint/outputs/data/child_bmi_1_w_outexcess.csv"  )
write.csv(child_bmi_change_year, file = "C:/git/ahl_obesity_blueprint/outputs/data/approach/child_bmi_1.csv"  )





test_1 = child_weight(age = df_child$age,
                                  sex =  df_child$sex, 
                                  #FM = df_child$fm_hudda, 
                                  #FFM = df_child$ffm_hudda, 
                                  #EI = energy_intake,   
                                  days = (365*5),
                                  checkValues = TRUE)


test_2 = cbind(df_child, child_model_weight$Body_Weight) %>%
  filter(age >= 17) %>%
  select(-c(1:18))

test_3 = child_reference_EI(age = df_child$age, 
                             sex = df_child$sex, 
                             FM = df_child$fm_hudda, 
                             FFM = df_child$ffm_hudda,
                             days = 365*5)



t_child_model_weight = child_weight(age = df_child$age,
                                  sex =  df_child$sex, 
                                  FM = df_child$fm_hudda, 
                                  FFM = df_child$ffm_hudda, 
                                  EI = test_3,   
                                  days = (365*5),
                                  checkValues = TRUE)

test_2 = cbind(df_child, t_child_model_weight$Body_Weight)

