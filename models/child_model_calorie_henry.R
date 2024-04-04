library(tidyverse)
library(here)
library(bw)
library(survey)
library(sitar)


source(file = "models/model_utils.R")

# calculate_bmi_from_ei_change(df = read_csv(here("inputs/processed/hse_2019_children.csv")), intake_change = 50)

# Function to calculate new bmi and weight category for children using Henry (2005) equations.

calculate_bmi_from_eichange_hox = function(df, daily_ei_change, nation, tags){
  
  print("This model uses Henry equations")
  
  #browser()
  # Initialising an empty list to store function outputs
  output_list = list()
  
  # reading in a table which assigns a proportional effect size based on age and sex of a child
  # This is based on the idea that any change in daily energy intake will be proportional to the 
  # age and sex of a child. So, for example, if a policy takes out 20 kcals of an 18 year old males diet,
  # the effect will not be same in case of a 7 year old male. Therefore, a fraction of the effect is applied
  # in case of the 7 year old male.
  # The weight (or proportion) to be multiplied to the effect size has been developed from the SACN 2011 
  # guidelines, by taking the ratio of recommended daily energy intake at each age and recommended daily
  # energy intake at 18. Therefore, the full reduction in daily energy intake applies at 18 years, and 
  # then reduces as the age lowers.
  effect_weighting = read.csv(here("inputs/ref_data/effect_weighting.csv"))
  
  library(sitar)
  
  # generating bmi refdata for each centile
  uk90_bmi_refdata_3centiles = generate_bmi_refdata(sitar::uk90)
  
  uk90_bmi_refdata_100centiles = generate_bmi_refdata_100centiles(sitar::uk90)
  
  # uk90_bmi_refdata_100centiles = uk90_bmi_refdata_100centiles %>%
  #   select(-c(L.bmi, M.bmi, S.bmi)) %>%
  #   unite(col = "sex_years", "sex", "years", sep = "_") %>%
  #   pivot_longer(cols= c(starts_with("p")), names_to = "centile", values_to = "bmi") %>%
  #   separate("sex_years", into = c("sex", "age"), sep = "_") %>%
  #   mutate(centile = substr(centile, 3, nchar(centile)))
  # 
  
  # read_csv(here("inputs/processed/hse_2019_children.csv"))
  
  # applying the henry equations
  df <- df %>%
    rowwise() %>%
    mutate(baseline_bmi_category = calculate_bmi_category(age = age, sex = sex, bmi = bmi, df_B = uk90_bmi_refdata_3centiles)) %>% # calculating baseline bmi catgeories
    #mutate(bmi_centile = look_up_percentile(age = age, sex = sex, bmi = bmi, data_B = uk90_bmi_refdata_100centiles)) %>%
    mutate(intake_change = ifelse(baseline_bmi_category %in% c( "overweight", "obese"), # "normal",
                                  calculate_proportional_ei_change(age = age,
                                                                   sex = sex,
                                                                   bmi = bmi, 
                                                                   intake_change = -daily_ei_change, 
                                                                   prop_weight_data = effect_weighting, 
                                                                   bmi_ref_data = uk90_bmi_refdata_100centiles), 0)) %>% # assigning proportional daily intake change based on age, sex and bmi
    ungroup() %>%
    mutate(intervention_status = ifelse(intake_change == 0, "No", "Yes")) %>% # creating a dummy variable to indicate intervetion status for each child
    mutate(policy_period_energy_intake = intake_hox + intake_change) %>% # calculating the new energy intake once policy is in effect, by applying the intake change
    mutate(policy_period_rmr = policy_period_energy_intake/pal) %>% # calculating the resting metabolic rate
    mutate(post_policy_weight = case_when(sex == 1 ~ (((policy_period_rmr * 4.184) - 2876)/66.9),
                                          sex == 2 ~ (((policy_period_rmr * 4.184) - 3230)/47.9))) %>% # calculating the new body weight based on the new energy intake 
    mutate(weight_diff = weight - post_policy_weight) %>%
    mutate(post_policy_bmi = post_policy_weight/(height/100)^2) %>% # calculating the new bmi as a result of the new body weight
    rowwise() %>%
    mutate(post_bmi_category = calculate_bmi_category(age = age, sex = sex, bmi = post_policy_bmi, df_B = uk90_bmi_refdata_3centiles)) %>% # calculating the new bmi category
    ungroup() %>%
    mutate(bmi_diff = bmi - post_policy_bmi)
  
  
  # Output 1:  adding the model dataframe to outputs
  output_list[["post_df"]] = df

  
  # Creating a table for generating plots
  child_bmi_change = rbind(
    df %>% 
      count(baseline_bmi_category, wt = wt_int) %>% 
      mutate(freq = n/sum(n)*100,
             type = "Baseline") %>% 
      rename(BMI = baseline_bmi_category),
    df %>% 
      count(post_bmi_category, wt = wt_int) %>% 
      mutate(freq = n/sum(n)*100,
             type = "Endline") %>% 
      rename(BMI = post_bmi_category))
  
  
  
  child_bmi_change = child_bmi_change %>%
    mutate(BMI = factor(BMI, levels = c("underweight", "normal", "overweight", "obese"))) %>%
    as.data.frame()
  
  
  # Creating a readable output table
  child_bmi_change_year = child_bmi_change %>%
    select(-c(n)) %>%
    pivot_wider(., names_from = BMI, values_from = freq)
  
  # Output 3: Table of prevalence of obesity
  output_list[["bmi_prevalence_table"]] = child_bmi_change_year
  
  
  
  # bar plot of BMI categories distributions
  child_bar_plot = child_bmi_change %>%
    ggplot(., aes(y = freq, x = BMI, fill = type)) + 
    geom_bar(stat = "identity", position = "dodge") +
    theme_ipsum() +
    labs(fill = "", 
         title = paste("BMI Distribution - ", tags), 
         y = "Prevalence - %",
         subtitle = paste("Children | Nation:", nation)) +
    theme_ipsum(base_size = 8, axis_title_size = 8) + #, base_family="Averta"
    theme(legend.position = "top")
  
  # Output 3: Bar plot of BMI category distributions
  output_list[["bmi_prevalence_plot"]] = child_bar_plot
  
  
  
  
  return(output_list)
  
}
