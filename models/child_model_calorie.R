
#rm(list = ls())

library(tidyverse)
library(here)
library(bw)
library(survey)


source(file = "models/model_utils.R")

# function to calculate obesity prevalance based on change in calorie intake
calculate_child_bmi_from_eichange = function(df, intake_change, implementation_duration, use_bodyfat_curves){
  
  output_list = list()
  
  sacn_dietary_intake = generate_sacn_dietary_intake(here("inputs/ref_data/sacn_dietary_intake.csv"))
  
  df <- df %>%  # we are reading a file that contains energy intake, BMI weight class and other variables reated during the pre-processing stage.
    #mutate(sex = ifelse(sex == 1, "male", "female")) %>% 
    filter(fm_hudda > 0) %>%
    #mutate(intake_diff = ifelse(bmi_cat_baseline %in% c("underweight", "normal"), intake_change, intake_change )) %>%
    #mutate(intervention = ifelse(bmi_cat_baseline %in% c("underweight", "normal"), "Yes", "Yes"))
    rowwise() %>%
    mutate(ei_365 = as.numeric(intake_hox + lookup_energy_intake(age = age + 1, sex = sex, data_B = sacn_dietary_intake)),
           ei_730 = as.numeric(ei_365 + lookup_energy_intake(age = age + 2, sex = sex, data_B = sacn_dietary_intake)),
           ei_1095 = as.numeric(ei_730 + lookup_energy_intake(age = age + 3, sex = sex, data_B = sacn_dietary_intake)),
           ei_1460 = as.numeric(ei_1095 + lookup_energy_intake(age = age + 4, sex = sex, data_B = sacn_dietary_intake)),
           ei_1825 = as.numeric(ei_1460 + lookup_energy_intake(age = age + 5, sex = sex, data_B = sacn_dietary_intake))) %>%
    ungroup() %>%
    mutate(sex = ifelse(sex == 1, "male", "female")) 

  
  energy_matrix = apply(df, 1, generate_interpolated_energy)
  
  energy_matrix = energy_matrix - intake_change
  
  #energy_intake = apply(df, 1, function(x) rep(as.numeric(x["intake_hox"]) + as.numeric(x["intake_diff"]), implementation_duration))
  #browser()
  # implementing Hall model
  child_model_weight = child_weight(age = df$age,
                                    sex =  df$sex, 
                                    FM = df$fm_hudda, 
                                    FFM = df$ffm_hudda, 
                                    EI = energy_matrix,   
                                    days = implementation_duration,
                                    checkValues = TRUE)
  
  output_list[["hall_model_op"]] = child_model_weight
  
  # Extracting body weight from Hall model outputs
  child_bw_h = as.data.frame(child_model_weight[["Body_Weight"]]) %>%
    mutate(bw_diff = V1825 - V1) %>%
    select(c(V1, V365, V730, V1095, V1460, V1825, bw_diff)) %>%
    rename(bw_1 = V1, bw_365 = V365, bw_730 = V730, 
           bw_1095 = V1095, bw_1460 = V1460, bw_1825 = V1825)
  
  
  # Extracting age from Hall model outputs
  child_age_h = as.data.frame(child_model_weight[["Age"]]) %>%
    mutate(age_diff = V1825 - V1) %>%
    select(c(V1, V365, V730, V1095, V1460, V1825, age_diff)) %>%
    rename(age_1 = V1, age_365 = V365, age_730 = V730, 
           age_1095 = V1095, age_1460 = V1460, age_1825 = V1825)
  
  # Extracting fat free mass from Hall model outputs
  child_ffm_h = as.data.frame(child_model_weight[["Fat_Free_Mass"]]) %>%
    mutate(ffm_diff = V1825 - V1) %>%
    select(c(V1, V365, V730, V1095, V1460, V1825, ffm_diff)) %>%
    rename(ffm_1 = V1, ffm_365 = V365, ffm_730 = V730,
           ffm_1095 = V1095, ffm_1460 = V1460, ffm_1825 = V1825)
  
  
  # Extracting fat mass from Hall model outputs
  child_fm_h = as.data.frame(child_model_weight[["Fat_Mass"]]) %>%
    mutate(fm_diff = V1825 - V1) %>%
    select(c(V1, V365, V730, V1095, V1460, V1825, fm_diff))  %>%
    rename(fm_1 = V1, fm_365 = V365, fm_730 = V730,
           fm_1095 = V1095, fm_1460 = V1460, fm_1825 = V1825)
  
  
  # combining all the extracted tables and binding them to the original dataframe
  child_age_bw_ffm_fm = cbind(child_bw_h, child_age_h, child_ffm_h, child_fm_h) %>%
    select(age_1, bw_1, fm_1, ffm_1,
           age_365, bw_365, fm_365, ffm_365,
           age_730, bw_730, fm_730, ffm_730,
           age_1095, bw_1095, fm_1095, ffm_1095,
           age_1460, bw_1460, fm_1460, ffm_1460,
           age_1825, bw_1825, fm_1825, ffm_1825)
  
  
  uk90_height_refdata = generate_height_refdata(here("inputs/ref_data/cole-nine-centiles-uk-who-female-height.json"),
                                                here("inputs/ref_data/cole-nine-centiles-uk-who-male-height.json"))
  
  uk90_bmi_refdata = generate_bmi_refdata(sitar::uk90)
  
  
  df_child = cbind(df, child_age_bw_ffm_fm) %>%
    rowwise() %>%
    mutate(ht_365  = lookup_projected_height(age, sex, height, data_B = uk90_height_refdata, years_added = 1)) %>%
    mutate(ht_730  = lookup_projected_height(age, sex, height, data_B = uk90_height_refdata, years_added = 2)) %>%
    mutate(ht_1095 = lookup_projected_height(age, sex, height, data_B = uk90_height_refdata, years_added = 3)) %>%
    mutate(ht_1460 = lookup_projected_height(age, sex, height, data_B = uk90_height_refdata, years_added = 4)) %>%
    mutate(ht_1825 = lookup_projected_height(age, sex, height, data_B = uk90_height_refdata, years_added = 5)) %>%
    mutate(bmi_365  = bw_365  / (ht_365/100)^2,
           bmi_730  = bw_730  / (ht_730/100)^2,
           bmi_1095 = bw_1095 / (ht_1095/100)^2,
           bmi_1460 = bw_1460 / (ht_1460/100)^2,
           bmi_1825 = bw_1825 / (ht_1825/100)^2) %>%
    mutate(bmi_cat_1 = calculate_bmi_category(age = age, sex = sex, bmi = bmi, df_B = uk90_bmi_refdata),
           bmi_cat_365 = calculate_bmi_category(age = age + 1, sex = sex, bmi = bmi_365, df_B = uk90_bmi_refdata),
           bmi_cat_730 = calculate_bmi_category(age = age + 2, sex = sex, bmi = bmi_730, df_B = uk90_bmi_refdata),
           bmi_cat_1095 = calculate_bmi_category(age = age + 3, sex = sex, bmi = bmi_1095, df_B = uk90_bmi_refdata),
           bmi_cat_1460 = calculate_bmi_category(age = age + 4, sex = sex, bmi = bmi_1460, df_B = uk90_bmi_refdata),
           bmi_cat_1825 = calculate_bmi_category(age = age + 5, sex = sex, bmi = bmi_1825, df_B = uk90_bmi_refdata)) %>%
    ungroup()
  
  
  output_list[["post_df"]] = df_child # Dataframe added to outputs list
  
  
  child_bmi_change = rbind(
    df_child %>% 
      count(bmi_cat_1, wt = wt_int) %>% 
      mutate(freq = n/sum(n)*100,
             type = "Baseline") %>% 
      rename(BMI = bmi_cat_1),
    df_child %>% 
      count(bmi_cat_365, wt = wt_int) %>% 
      mutate(freq = n/sum(n)*100,
             type = "Year 1") %>% 
      rename(BMI = bmi_cat_365),
    df_child %>% 
      count(bmi_cat_730, wt = wt_int) %>% 
      mutate(freq = n/sum(n)*100,
             type = "Year 2") %>% 
      rename(BMI = bmi_cat_730),
    df_child %>% 
      count(bmi_cat_1095, wt = wt_int) %>% 
      mutate(freq = n/sum(n)*100,
             type = "Year 3") %>% 
      rename(BMI = bmi_cat_1095),
    df_child %>% 
      count(bmi_cat_1460, wt = wt_int) %>% 
      mutate(freq = n/sum(n)*100,
             type = "Year 4") %>% 
      rename(BMI = bmi_cat_1460),
    df_child %>% 
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
  
  output_list[["bmi_category_plot"]] = child_bar_plot # BMI category distribution plot is added to the outputs list
  
  
  child_bmi_change_year = child_bmi_change %>%
    select(-c(n)) %>%
    pivot_wider(., names_from = BMI, values_from = freq)
  
  
  output_list[["bmi_percent_prevalence"]] = child_bmi_change_year # Year wise BMI category percentage prevalance table is added to outputs list
  
  return(output_list) # outputs are returned

}







## to delete:

# # looping throuhg five years to calculate body fat percentage and categorise into BMI groups
# years <- c("y0","y1", "y2", "y3", "y4", "y5")
# 
# for (year in years) {
#   # Calculate bf_percent for each year
#   bf_percent <- paste0("bf_percent_", year)
#   bw_col <- paste0("bw_", year)
#   fm_col <- paste0("fm_", year)
#   
#   # Check if the column already exists in df_child
#   if (!(bf_percent %in% colnames(df_child))) {
#     # Add bf_percent column to df_child
#     df_child <- df_child %>%
#       mutate(!!sym(bf_percent) := ((!!sym(fm_col) / !!sym(bw_col)) * 100))
#     
#     
#     
#     if (use_bodyfat_curves == 0) {
#       
#       # Categorize bf_percent using body fat reference curves from McCarthy et al. (2008)
#       df_child <- df_child %>%
#         mutate(
#           !!sym(paste0("bmi_cat_", year)) := case_when(
#             age == 6 ~ (
#               ifelse(
#                 sex == "male",
#                 case_when(
#                   !!sym(bf_percent) <= 12.4 ~ "underweight",
#                   !!sym(bf_percent) >= 19.5 & !!sym(bf_percent) < 22.7 ~ "overweight",
#                   !!sym(bf_percent) > 12.4 & !!sym(bf_percent) < 19.5 ~ "normal",
#                   !!sym(bf_percent) >= 22.7 ~ "obese"
#                 ),
#                 case_when(
#                   !!sym(bf_percent) <= 14.4 ~ "underweight",
#                   !!sym(bf_percent) > 14.4 & !!sym(bf_percent) < 23 ~ "normal",
#                   !!sym(bf_percent) >= 23 & !!sym(bf_percent) < 26.2 ~ "overweight",
#                   !!sym(bf_percent) >= 26.2 ~ "obese"
#                 )
#               )
#             ),
#             age == 9 ~ (
#               ifelse(
#                 sex == "male",
#                 case_when(
#                   !!sym(bf_percent) <= 12.8 ~ "underweight",
#                   !!sym(bf_percent) > 12.8 & !!sym(bf_percent) < 22.2 ~ "normal",
#                   !!sym(bf_percent) >= 22.2 & !!sym(bf_percent) < 26.8 ~ "overweight",
#                   !!sym(bf_percent) >= 26.8 ~ "obese"
#                 ),
#                 case_when(
#                   !!sym(bf_percent) <= 15.7 ~ "underweight",
#                   !!sym(bf_percent) > 15.7 & !!sym(bf_percent) < 27.2 ~ "normal",
#                   !!sym(bf_percent) >= 27.2 & !!sym(bf_percent) < 31.2 ~ "overweight",
#                   !!sym(bf_percent) >= 31.2 ~ "obese"
#                 )
#               )
#             ),
#             age == 11.5 ~ (
#               ifelse(
#                 sex == "male",
#                 case_when(
#                   !!sym(bf_percent) <= 12.6 ~ "underweight",
#                   !!sym(bf_percent) > 12.6 & !!sym(bf_percent) < 23 ~ "normal",
#                   !!sym(bf_percent) >= 23 & !!sym(bf_percent) < 28.3 ~ "overweight",
#                   !!sym(bf_percent) >= 28.3 ~ "obese"
#                 ),
#                 case_when(
#                   !!sym(bf_percent) <= 16.1 ~ "underweight",
#                   !!sym(bf_percent) > 16.1 & !!sym(bf_percent) < 28.8 ~ "normal",
#                   !!sym(bf_percent) >= 28.8 & !!sym(bf_percent) < 32.8 ~ "overweight",
#                   !!sym(bf_percent) >= 32.8 ~ "obese"
#                 )
#               )
#             ),
#             age == 14 ~ (
#               ifelse(
#                 sex == "male",
#                 case_when(
#                   !!sym(bf_percent) <= 10.9 ~ "underweight",
#                   !!sym(bf_percent) > 10.9 & !!sym(bf_percent) < 21.3 ~ "normal",
#                   !!sym(bf_percent) >= 21.3 & !!sym(bf_percent) < 25.9 ~ "overweight",
#                   !!sym(bf_percent) >= 25.9 ~ "obese"
#                 ),
#                 case_when(
#                   !!sym(bf_percent) <= 16 ~ "underweight",
#                   !!sym(bf_percent) > 16 & !!sym(bf_percent) < 29.6 ~ "normal",
#                   !!sym(bf_percent) >= 29.6 & !!sym(bf_percent) < 33.6 ~ "overweight",
#                   !!sym(bf_percent) >= 33.6 ~ "obese"
#                 )
#               )
#             ),
#             age == 17 ~ (
#               ifelse(
#                 sex == "male",
#                 case_when(
#                   !!sym(bf_percent) <= 9.8 ~ "underweight",
#                   !!sym(bf_percent) > 9.8 & !!sym(bf_percent) < 20.1 ~ "normal",
#                   !!sym(bf_percent) >= 20.1 & !!sym(bf_percent) < 23.9 ~ "overweight",
#                   !!sym(bf_percent) >= 23.9 ~ "obese"
#                 ),
#                 case_when(
#                   !!sym(bf_percent) <= 15.1 ~ "underweight",
#                   !!sym(bf_percent) > 15.1 & !!sym(bf_percent) < 30.4 ~ "normal",
#                   !!sym(bf_percent) >= 30.4 & !!sym(bf_percent) < 34.4 ~ "overweight",
#                   !!sym(bf_percent) >= 34.4 ~ "obese"
#                 )
#               )
#             ),
#             TRUE ~ NA_character_
#           )
#         )
#       
#       
#     } else if (use_bodyfat_curves == 1){
#       # Categorize bf_percent using body fat reference curves from McCarthy et al. (2008)
#       
#       df_child <- df_child %>%
#         mutate(
#           !!sym(paste0("bmi_cat_", year)) := case_when(
#             !!sym(bf_percent) < p2 ~ "underweight",
#             !!sym(bf_percent) >= p95 ~ "obese",
#             !!sym(bf_percent) >= p85 & !!sym(bf_percent) < p95 ~ "overweight",
#             TRUE ~ "normal"
#           )
#         )
#       
#     }
#     
#     
#   }
# }




