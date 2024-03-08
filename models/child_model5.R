### approach running model for 1 year; five times (assuming a child is frozen in time)
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
  rowwise() %>%
  mutate(bmi_cat_b = calculate_bmi_category(age = age, sex = sex, bmi = bmi, df_B = generate_bmi_refdata(sitar::uk90))) %>%
  mutate(intake_change = calculate_intake_change(age = age, sex = sex, intake_change = -intake_change, 
                                                 data_B = read.csv(file = "C:/Users/Anish.Chacko/Downloads/energy_ref_data/effect_weighting.csv")))



energy_intake = apply(df_child, 1, function(x) rep(as.numeric(x["intake_hox"]) + as.numeric(x["intake_change"]), 365*1))


df_child <- df_child %>%
  mutate(sex = ifelse(sex == 1, "male", "female")) 


### year 1:

child_model_weight = child_weight(age = df_child$age,
                                  sex =  df_child$sex, 
                                  FM = df_child$fm_hudda, 
                                  FFM = df_child$ffm_hudda, 
                                  EI = energy_intake,   
                                  days = (365*1),
                                  checkValues = TRUE)



child_bw_h = as.data.frame(child_model_weight[["Body_Weight"]]) %>%
  mutate(bw_diff = V365 - V1) %>%
  select(c(V1, V365, bw_diff)) %>%
  rename(bw_1 = V1, bw_365 = V365)



child_age_h = as.data.frame(child_model_weight[["Age"]]) %>%
  mutate(age_diff = V365 - V1) %>%
  select(c(V1, V365, age_diff)) %>%
  rename(age_1 = V1, age_365 = V365)


child_ffm_h = as.data.frame(child_model_weight[["Fat_Free_Mass"]]) %>%
  mutate(ffm_diff = V365 - V1) %>%
  select(c(V1, V365, ffm_diff)) %>%
  rename(ffm_1 = V1, ffm_365 = V365)



child_fm_h = as.data.frame(child_model_weight[["Fat_Mass"]]) %>%
  mutate(fm_diff = V365 - V1) %>%
  select(c(V1, V365, fm_diff))  %>%
  rename(fm_1 = V1, fm_365 = V365)



child_age_bw_ffm_fm = cbind(child_bw_h, child_age_h, child_ffm_h, child_fm_h) %>%
  select(age_1, bw_1, fm_1, ffm_1,
         age_365, bw_365, fm_365, ffm_365)




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
  #mutate(ht_365  = lookup_projected_height(age, sex, height, data_B = height_refdata_100centile, years_added = 1)) %>%
  #ungroup() %>%
  mutate(bmi_365  = bw_365  / (height/100)^2) %>%
  rowwise() %>%
  mutate(#bmi_cat_1 = calculate_bmi_category(age = age, sex = sex, bmi = bmi, df_B = uk90_bmi_refdata),
         bmi_cat_365 = calculate_bmi_category(age = age, sex = sex, bmi = bmi_365, df_B = uk90_bmi_refdata)) %>%
  ungroup()

child_bmi_change = rbind(
  new_df_child %>% 
    count(bmi_cat_b, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Baseline") %>% 
    rename(BMI = bmi_cat_b),
  new_df_child %>% 
    count(bmi_cat_365, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Year 1") %>% 
    rename(BMI = bmi_cat_365)
)


child_bmi_change = child_bmi_change %>%
  mutate(BMI = factor(BMI, levels = c("underweight", "normal", "overweight", "obese"))) %>%
  as.data.frame()

oplist[["year_1_table"]] = child_bmi_change

# output bar plot of BMI categories distributions
child_bar_plot = child_bmi_change %>%
  ggplot(., aes(y = freq, x = BMI, fill = type)) + 
  geom_bar(stat = "identity", position = "dodge") +
  #theme_ipsum() +
  labs(fill = "", 
       title = "BMI Distribution", 
       y = "Prevalence - %",
       subtitle = "Children - Proportional kcals - year 1") +
  theme_ipsum(base_size = 8, axis_title_size = 8) + #, base_family="Averta"
  theme(legend.position = "top")


child_bar_plot
oplist[["year_1_plot"]] = child_bar_plot

# ggsave(here("outputs/figures/child_bmi_distrib_policy4a.png"), plot = child_bar_plot, width = 10, height = 6, bg='#ffffff')

# Output 2: Table of year wise prevalence of obesity
child_bmi_change_year = child_bmi_change %>%
  select(-c(n)) %>%
  pivot_wider(., names_from = BMI, values_from = freq)

#write.csv(child_bmi_change_year, file = "C:/git/ahl_obesity_blueprint/outputs/data/child_bmi_1_w_outexcess.csv"  )
write.csv(child_bmi_change_year, file = "C:/git/ahl_obesity_blueprint/outputs/data/approach/child_bmi_year1.csv"  )



## Year 2:


child_model_weight_2 = child_weight(age = new_df_child$age,
                                  sex =  new_df_child$sex, 
                                  FM = new_df_child$fm_365, 
                                  FFM = new_df_child$ffm_365, 
                                  EI = energy_intake,   
                                  days = (365*1),
                                  checkValues = TRUE)



## year 2:



child_bw_h_2 = as.data.frame(child_model_weight_2[["Body_Weight"]]) %>%
  mutate(bw_diff = V365 - V1) %>%
  select(c(V365, bw_diff)) %>%
  rename(bw_730 = V365)



child_age_h_2 = as.data.frame(child_model_weight_2[["Age"]]) %>%
  mutate(age_diff = V365 - V1) %>%
  select(c(V365, age_diff)) %>%
  rename(age_730 = V365)


child_ffm_h_2 = as.data.frame(child_model_weight_2[["Fat_Free_Mass"]]) %>%
  mutate(ffm_diff = V365 - V1) %>%
  select(c(V365, ffm_diff)) %>%
  rename(ffm_730 = V365)



child_fm_h_2 = as.data.frame(child_model_weight_2[["Fat_Mass"]]) %>%
  mutate(fm_diff = V365 - V1) %>%
  select(c(V365, fm_diff))  %>%
  rename(fm_730 = V365)



child_age_bw_ffm_fm_2 = cbind(child_bw_h_2, child_age_h_2, child_ffm_h_2, child_fm_h_2) %>%
  select(age_730, bw_730, fm_730, ffm_730)




new_df_child_2 = cbind(new_df_child, child_age_bw_ffm_fm_2) %>%
  rowwise() %>%
  #mutate(ht_365  = lookup_projected_height(age, sex, height, data_B = height_refdata_100centile, years_added = 1)) %>%
  #ungroup() %>%
  mutate(bmi_730  = bw_730  / (height/100)^2) %>%
  rowwise() %>%
  mutate(#bmi_cat_1 = calculate_bmi_category(age = age, sex = sex, bmi = bmi, df_B = uk90_bmi_refdata),
    bmi_cat_730 = calculate_bmi_category(age = age, sex = sex, bmi = bmi_730, df_B = uk90_bmi_refdata)) %>%
  ungroup()

child_bmi_change_2 = rbind(
  # new_df_child_2 %>% 
  #   count(bmi_cat_365, wt = wt_int) %>% 
  #   mutate(freq = n/sum(n)*100,
  #          type = "Year 1") %>% 
  #   rename(BMI = bmi_cat_365),
  new_df_child_2 %>% 
    count(bmi_cat_730, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Year 2") %>% 
    rename(BMI = bmi_cat_730)
)


child_bmi_change_2 = child_bmi_change_2 %>%
  mutate(BMI = factor(BMI, levels = c("underweight", "normal", "overweight", "obese"))) %>%
  as.data.frame()

oplist[["year_2_table"]] = child_bmi_change_2

# output bar plot of BMI categories distributions
child_bar_plot_2 = child_bmi_change_2 %>%
  ggplot(., aes(y = freq, x = BMI, fill = type)) + 
  geom_bar(stat = "identity", position = "dodge") +
  #theme_ipsum() +
  labs(fill = "", 
       title = "BMI Distribution", 
       y = "Prevalence - %",
       subtitle = "Children - Proportional kcals - year 2") +
  theme_ipsum(base_size = 8, axis_title_size = 8) + #, base_family="Averta"
  theme(legend.position = "top")


child_bar_plot_2
oplist[["year_2_plot"]] = child_bar_plot_2

# ggsave(here("outputs/figures/child_bmi_distrib_policy4a.png"), plot = child_bar_plot, width = 10, height = 6, bg='#ffffff')

# Output 2: Table of year wise prevalence of obesity
child_bmi_change_year_2 = child_bmi_change_2 %>%
  select(-c(n)) %>%
  pivot_wider(., names_from = BMI, values_from = freq)

#write.csv(child_bmi_change_year, file = "C:/git/ahl_obesity_blueprint/outputs/data/child_bmi_1_w_outexcess.csv"  )
write.csv(child_bmi_change_year_2, file = "C:/git/ahl_obesity_blueprint/outputs/data/approach/child_bmi_year2.csv"  )




## Year 3:


child_model_weight_3 = child_weight(age = new_df_child_2$age,
                                    sex =  new_df_child_2$sex, 
                                    FM = new_df_child_2$fm_730, 
                                    FFM = new_df_child_2$ffm_730, 
                                    EI = energy_intake,   
                                    days = (365*1),
                                    checkValues = TRUE)







child_bw_h_3 = as.data.frame(child_model_weight_3[["Body_Weight"]]) %>%
  mutate(bw_diff = V365 - V1) %>%
  select(c(V365, bw_diff)) %>%
  rename(bw_1095 = V365)



child_age_h_3 = as.data.frame(child_model_weight_3[["Age"]]) %>%
  mutate(age_diff = V365 - V1) %>%
  select(c(V365, age_diff)) %>%
  rename(age_1095 = V365)


child_ffm_h_3 = as.data.frame(child_model_weight_3[["Fat_Free_Mass"]]) %>%
  mutate(ffm_diff = V365 - V1) %>%
  select(c(V365, ffm_diff)) %>%
  rename(ffm_1095 = V365)



child_fm_h_3 = as.data.frame(child_model_weight_3[["Fat_Mass"]]) %>%
  mutate(fm_diff = V365 - V1) %>%
  select(c(V365, fm_diff))  %>%
  rename(fm_1095 = V365)



child_age_bw_ffm_fm_3 = cbind(child_bw_h_3, child_age_h_3, child_ffm_h_3, child_fm_h_3) %>%
  select(age_1095, bw_1095, fm_1095, ffm_1095)




new_df_child_3 = cbind(new_df_child_2, child_age_bw_ffm_fm_3) %>%
  rowwise() %>%
  #mutate(ht_365  = lookup_projected_height(age, sex, height, data_B = height_refdata_100centile, years_added = 1)) %>%
  #ungroup() %>%
  mutate(bmi_1095  = bw_1095  / (height/100)^2) %>%
  rowwise() %>%
  mutate(#bmi_cat_1 = calculate_bmi_category(age = age, sex = sex, bmi = bmi, df_B = uk90_bmi_refdata),
    bmi_cat_1095 = calculate_bmi_category(age = age, sex = sex, bmi = bmi_1095, df_B = uk90_bmi_refdata)) %>%
  ungroup()

child_bmi_change_3 = rbind(
  # new_df_child_3 %>% 
  #   count(bmi_cat_730, wt = wt_int) %>% 
  #   mutate(freq = n/sum(n)*100,
  #          type = "Year 2") %>% 
  #   rename(BMI = bmi_cat_730),
  new_df_child_3 %>% 
    count(bmi_cat_1095, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Year 3") %>% 
    rename(BMI = bmi_cat_1095)
)


child_bmi_change_3 = child_bmi_change_3 %>%
  mutate(BMI = factor(BMI, levels = c("underweight", "normal", "overweight", "obese"))) %>%
  as.data.frame()

oplist[["year_3_table"]] = child_bmi_change_3

# output bar plot of BMI categories distributions
child_bar_plot_3 = child_bmi_change_3 %>%
  ggplot(., aes(y = freq, x = BMI, fill = type)) + 
  geom_bar(stat = "identity", position = "dodge") +
  #theme_ipsum() +
  labs(fill = "", 
       title = "BMI Distribution", 
       y = "Prevalence - %",
       subtitle = "Children - Proportional kcals - year 3") +
  theme_ipsum(base_size = 8, axis_title_size = 8) + #, base_family="Averta"
  theme(legend.position = "top")


child_bar_plot_3
oplist[["year_3_plot"]] = child_bar_plot_3

# ggsave(here("outputs/figures/child_bmi_distrib_policy4a.png"), plot = child_bar_plot, width = 10, height = 6, bg='#ffffff')

# Output 2: Table of year wise prevalence of obesity
child_bmi_change_year_3 = child_bmi_change_3 %>%
  select(-c(n)) %>%
  pivot_wider(., names_from = BMI, values_from = freq)

#write.csv(child_bmi_change_year, file = "C:/git/ahl_obesity_blueprint/outputs/data/child_bmi_1_w_outexcess.csv"  )
write.csv(child_bmi_change_year_3, file = "C:/git/ahl_obesity_blueprint/outputs/data/approach/child_bmi_year3.csv"  )





## Year 4:


new_df_child_3 <- new_df_child_3 %>%
  filter(fm_1095 > 0 & ffm_1095 >0)


child_model_weight_4 = child_weight(age = new_df_child_3$age,
                                    sex =  new_df_child_3$sex, 
                                    FM = new_df_child_3$fm_1095, 
                                    FFM = new_df_child_3$ffm_1095, 
                                    EI = energy_intake,   
                                    days = (365*1),
                                    checkValues = TRUE)







child_bw_h_4 = as.data.frame(child_model_weight_4[["Body_Weight"]]) %>%
  mutate(bw_diff = V365 - V1) %>%
  select(c(V365, bw_diff)) %>%
  rename(bw_1460 = V365)



child_age_h_4 = as.data.frame(child_model_weight_4[["Age"]]) %>%
  mutate(age_diff = V365 - V1) %>%
  select(c(V365, age_diff)) %>%
  rename(age_1460 = V365)


child_ffm_h_4 = as.data.frame(child_model_weight_4[["Fat_Free_Mass"]]) %>%
  mutate(ffm_diff = V365 - V1) %>%
  select(c(V365, ffm_diff)) %>%
  rename(ffm_1460 = V365)



child_fm_h_4 = as.data.frame(child_model_weight_4[["Fat_Mass"]]) %>%
  mutate(fm_diff = V365 - V1) %>%
  select(c(V365, fm_diff))  %>%
  rename(fm_1460 = V365)



child_age_bw_ffm_fm_4 = cbind(child_bw_h_4, child_age_h_4, child_ffm_h_4, child_fm_h_4) %>%
  select(age_1460, bw_1460, fm_1460, ffm_1460)




new_df_child_4 = cbind(new_df_child_3, child_age_bw_ffm_fm_4) %>%
  rowwise() %>%
  #mutate(ht_365  = lookup_projected_height(age, sex, height, data_B = height_refdata_100centile, years_added = 1)) %>%
  #ungroup() %>%
  mutate(bmi_1460  = bw_1460  / (height/100)^2) %>%
  rowwise() %>%
  mutate(#bmi_cat_1 = calculate_bmi_category(age = age, sex = sex, bmi = bmi, df_B = uk90_bmi_refdata),
    bmi_cat_1460 = calculate_bmi_category(age = age, sex = sex, bmi = bmi_1460, df_B = uk90_bmi_refdata)) %>%
  ungroup()

child_bmi_change_4 = rbind(
  # new_df_child_4 %>% 
  #   count(bmi_cat_1095, wt = wt_int) %>% 
  #   mutate(freq = n/sum(n)*100,
  #          type = "Year 3") %>% 
  #   rename(BMI = bmi_cat_1095),
  new_df_child_4 %>% 
    count(bmi_cat_1460, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Year 4") %>% 
    rename(BMI = bmi_cat_1460)
)


child_bmi_change_4 = child_bmi_change_4 %>%
  mutate(BMI = factor(BMI, levels = c("underweight", "normal", "overweight", "obese"))) %>%
  as.data.frame()

oplist[["year_4_table"]] = child_bmi_change_4

# output bar plot of BMI categories distributions
child_bar_plot_4 = child_bmi_change_4 %>%
  ggplot(., aes(y = freq, x = BMI, fill = type)) + 
  geom_bar(stat = "identity", position = "dodge") +
  #theme_ipsum() +
  labs(fill = "", 
       title = "BMI Distribution", 
       y = "Prevalence - %",
       subtitle = "Children - Proportional kcals - year 4") +
  theme_ipsum(base_size = 8, axis_title_size = 8) + #, base_family="Averta"
  theme(legend.position = "top")


child_bar_plot_4
oplist[["year_4_plot"]] = child_bar_plot_4

# ggsave(here("outputs/figures/child_bmi_distrib_policy4a.png"), plot = child_bar_plot, width = 10, height = 6, bg='#ffffff')

# Output 2: Table of year wise prevalence of obesity
child_bmi_change_year_4 = child_bmi_change_4 %>%
  select(-c(n)) %>%
  pivot_wider(., names_from = BMI, values_from = freq)

#write.csv(child_bmi_change_year, file = "C:/git/ahl_obesity_blueprint/outputs/data/child_bmi_1_w_outexcess.csv"  )
write.csv(child_bmi_change_year_4, file = "C:/git/ahl_obesity_blueprint/outputs/data/approach/child_bmi_year4.csv"  )


test_table = rbind(oplist$year_1_table, oplist$year_2_table, oplist$year_3_table, oplist$year_4_table)


test_plot = test_table %>%
  ggplot(., aes(y = freq, x = BMI, fill = type)) + 
  geom_bar(stat = "identity", position = "dodge") +
  #theme_ipsum() +
  labs(fill = "", 
       title = "BMI Distribution: Case - 2", 
       y = "Prevalence - %",
       subtitle = "Children - Proportional kcals - reduction = 5kcals") +
  theme_ipsum(base_size = 8, axis_title_size = 8) + #, base_family="Averta"
  theme(legend.position = "top")


test_plot


