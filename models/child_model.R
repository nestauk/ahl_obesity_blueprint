# Policy 4a :  Ban BOGOF price promotions on HFSS products for medium and large retailers
# Effect size from rapid reviews = -3 kcal reduction
# Intervention group: Children identified as overweight, obese or severely obese
# Outputs: BMI bar plot comparison between baseline and endline years and corresponding table


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
child_intake_change = 0

lookup_energy_intake = function(age, sex, bmi_class, data_B){
  #browser()
  # checks that the inout is for a child i.e. less than 19 years of age
  if (age < 19 & bmi_class %in% c("normal", "overweight", "obese")){
    
    age = ceiling(age) # round up the age as the published DRI values are for integer ages, eg. child is 5.5, it rounds up to 6.
    age_row_prev <- data_B[data_B$age == age - 1 & data_B$sex == sex, ] # get row for the previous age, eg. datarow for age 5
    age_row <- data_B[data_B$age == age & data_B$sex == sex, ] # get row for current age, eg. datarow for age 6
    
    ei_age_prev = age_row_prev$population_kcal # get the recommended DRI for previous age
    ei_age = age_row$population_kcal # get recommended DRI for current age
    
    # get population level mean excess energy intake for combinations of age and sex
    # excess energy intake values for sex and age group taken from Calorie Reformulation Report (DHSC - UK Gov, 2017)
    ei_excess = case_when(age >= 5 & age <= 10 & sex == 1 ~ 21,
                          age >= 11 & age <= 15 & sex == 1 ~ 69,
                          age >= 16 & age <= 18 & sex == 1 ~ 104,
                          age >= 5 & age <= 10 & sex == 2 ~ 34,
                          age >= 11 & age <= 15 & sex == 2 ~ 63,
                          age >= 16 & age <= 18 & sex == 2 ~ 44)
    
     ei_excess = 0 # set to zero for now as we decide who should receive excess calorie intake - specific BMI groups or all children
    
    net_energy_intake = (ei_age - ei_age_prev) + ei_excess # net increment in energy intake for child growth
    
  } else{ if(age < 19 & bmi_class  %in% c("underweight")){
    
    
    age = ceiling(age) # round up the age as the published DRI values are for integer ages, eg. child is 5.5, it rounds up to 6.
    age_row_prev <- data_B[data_B$age == age - 1 & data_B$sex == sex, ] # get row for the previous age, eg. datarow for age 5
    age_row <- data_B[data_B$age == age & data_B$sex == sex, ] # get row for current age, eg. datarow for age 6
    
    ei_age_prev = age_row_prev$population_kcal # get the recommended DRI for previous age
    ei_age = age_row$population_kcal # get recommended DRI for current age
    
    net_energy_intake = (ei_age - ei_age_prev)  # net increment in energy intake for child growth
    
    
  } else{
    
    # incase of children aged 19 and above, set incremental energy intake for growth = 0 kcals
    net_energy_intake = 0
    
  }
    
  }
  return(net_energy_intake)
  
}


#sacn_dietary_intake = read_csv("C:/Users/Anish.Chacko/Downloads/energy_ref_data/sacn_dietary_intake.csv")

sacn_dietary_intake = generate_sacn_dietary_intake("C:/Users/Anish.Chacko/Downloads/energy_ref_data/sacn_dietary_intake.csv")

df_child <- read_csv(here("inputs/processed/hse_2019_children.csv")) %>%  # we are reading a file that contains energy intake, BMI weight class and other variables reated during the pre-processing stage.
  #mutate(sex = ifelse(sex == 1, "male", "female")) %>% 
  filter(fm_hudda > 0) %>%
  #mutate(intake_diff = ifelse(bmi_cat_baseline %in% c("underweight", "normal"), 0, child_intake_change )) %>%
  rowwise() %>%
  mutate(bmi_cat_b = calculate_bmi_category(age = age, sex = sex, bmi = bmi, df_B = generate_bmi_refdata(sitar::uk90))) %>%
  mutate(ei_365 = as.numeric(intake_hox + lookup_energy_intake(age = age + 1, sex = sex, bmi_class = bmi_cat_b,  data_B = sacn_dietary_intake)),
         ei_730 = as.numeric(ei_365 + lookup_energy_intake(age = age + 2, sex = sex, bmi_class = bmi_cat_b,  data_B = sacn_dietary_intake)),
         ei_1095 = as.numeric(ei_730 + lookup_energy_intake(age = age + 3, sex = sex, bmi_class = bmi_cat_b,  data_B = sacn_dietary_intake)),
         ei_1460 = as.numeric(ei_1095 + lookup_energy_intake(age = age + 4, sex = sex, bmi_class = bmi_cat_b,  data_B = sacn_dietary_intake)),
         ei_1825 = as.numeric(ei_1460 + lookup_energy_intake(age = age + 5, sex = sex, bmi_class = bmi_cat_b, data_B = sacn_dietary_intake))) %>%
  ungroup()

# 
# generate_interpolated_energy <- function(row) {
#   #browser()
#   energy_values <- c(as.numeric(row["intake_hox"]), as.numeric(row["ei_365"]), as.numeric(row["ei_730"]), as.numeric(row["ei_1095"]), as.numeric(row["ei_1460"]), as.numeric(row["ei_1825"]))
#   interpolated_energy <- energy_build(energy_values, c(0, 365, 730, 1095, 1460, 1825), interpolation = "Brownian")
#   return(interpolated_energy)
# }

energy_matrix = apply(df_child, 1, generate_interpolated_energy)

#energy_matrix = t(energy_matrix)

#updated_df = cbind(df_child, energy_matrix)

# df_b = generate_bmi_refdata(sitar::uk90)

#df_tidy = updated_df %>%
#  rowwise()%>%
#  mutate(bmi_cat = calculate_bmi_category(age = age, bmi = bmi, sex = sex, df_B = df_b )) %>%
#  select(-c("id", "weight", "height", "bmi", "wt_int", "psu", "strata", "origin2", "pal", "rmr_hox", "intake_hox", "fm_hudda",
#            "ffm_hudda", "ei_365", "ei_730", "ei_1095", "ei_1460", "ei_1825"))
#  pivot_longer(cols = -c(age, sex, bmi_cat, age_grp),
#               names_to = "time_period",
#               values_to = "energy_intake")

# energy_matrix = energy_matrix - child_intake_change



#energy_intake = apply(df_child, 1, function(x) rep(as.numeric(x["intake_hox"]) + as.numeric(x["intake_diff"]), 365*5))


df_child <- df_child %>%
  mutate(sex = ifelse(sex == 1, "male", "female")) 

# model 1 with fm and ffm
child_model_weight = child_weight(age = df_child$age,
                                  sex =  df_child$sex, 
                                  FM = df_child$fm_hudda, 
                                  FFM = df_child$ffm_hudda, 
                                  EI = energy_matrix,   
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



## bmi_ref_data_calculatded and used

# library(sitar)
# 
# generate_bmi_refdata = function(data_B){
#   
#   bmi_refdata = data_B %>%
#     select(years, sex, L.bmi, M.bmi, S.bmi) %>%
#     subset(years >= as.double(4) & years <= as.double(20)) %>%
#     mutate(p_2 = (M.bmi*(1 + L.bmi*S.bmi*-2.054)^(1/L.bmi)),
#            p_85 = (M.bmi*(1 + L.bmi*S.bmi*1.036)^(1/L.bmi)),
#            p_95 = (M.bmi*(1 + L.bmi*S.bmi*1.645)^(1/L.bmi))) %>%
#     select(years, sex, p_2, p_85, p_95) %>%
#     rename(age = years)
#   
#   return(bmi_refdata)
#   
# }

uk90_bmi_refdata = generate_bmi_refdata(sitar::uk90)


new_df_child = cbind(df_child, child_age_bw_ffm_fm) %>%
  rowwise() %>%
  mutate(ht_365  = lookup_projected_height(age, sex, height, data_B = uk90_height_refdata, years_added = 1)) %>%
  mutate(ht_730  = lookup_projected_height(age, sex, height, data_B = uk90_height_refdata, years_added = 2)) %>%
  mutate(ht_1095 = lookup_projected_height(age, sex, height, data_B = uk90_height_refdata, years_added = 3)) %>%
  mutate(ht_1460 = lookup_projected_height(age, sex, height, data_B = uk90_height_refdata, years_added = 4)) %>%
  mutate(ht_1825 = lookup_projected_height(age, sex, height, data_B = uk90_height_refdata, years_added = 5)) %>%
  ungroup() %>%
  mutate(bmi_365  = bw_365  / (ht_365/100)^2,
         bmi_730  = bw_730  / (ht_730/100)^2,
         bmi_1095 = bw_1095 / (ht_1095/100)^2,
         bmi_1460 = bw_1460 / (ht_1460/100)^2,
         bmi_1825 = bw_1825 / (ht_1825/100)^2) %>%
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
       title = "BMI Categories Distribution", 
       y = "Frequency",
       subtitle = "Population") +
  theme_ipsum(base_size = 8, axis_title_size = 8) + #, base_family="Averta"
  theme(legend.position = "top")


child_bar_plot

# ggsave(here("outputs/figures/child_bmi_distrib_policy4a.png"), plot = child_bar_plot, width = 10, height = 6, bg='#ffffff')

# Output 2: Table of year wise prevalence of obesity
child_bmi_change_year = child_bmi_change %>%
  select(-c(n)) %>%
  pivot_wider(., names_from = BMI, values_from = freq)

write.csv(child_bmi_change_year, file = "C:/git/ahl_obesity_blueprint/outputs/data/child_bmi_1_w_outexcess.csv"  )




