
##############################################################################################
# Policy 16: Introducing universal free school meals for all primary school children         #
#            during term time                                                                #
#                                                                                            #
##############################################################################################

# Scope: England

# Description:

# The evidence from the rapid review (https://docs.google.com/document/d/1X0LBgHr6ikmySD_aLqv9U8GN-fKjOGEq1A1jaGFOzXQ/edit?usp=sharing)
# shows that a universal free school meals programme for primary school children reduced obesity prevalence
# by 5.6% among year 6 (10 - 11 year old) students and 9.3% in reception year (4 - 5 year old).
# based on this, we assumed a linear relationship and estimated the values for each year.
# 5 - 7 year olds = 8.7%
# 8 - 10 year olds = 6.5%
# 11 - 12 year olds = 2.8%

# Population make up of 5 - 12 year old children:
# According to ONS mid year population estimates that children aged 5 - 12 make up 5,603,080 of the population in England [1]
# 11-12 year olds = 794,127
# 10-11 year olds = 787647
# 9-10 year olds = 797010
# 8-9 year olds = 812,581
# 7-8 year olds = 825,785
# 6-7 year olds = 801,776
# 5-6 year olds = 784,154

# Summary table of age group % reductions and populations aligned with HSE data

#############################################################
#   Age group    |   % obesity reduction  |   population    #
# --------------------------------------------------------- #
#    5 - 7       |           8.7%         |  1,998,822.50   #
#    8 - 10      |           6.5%         |  2,416,307      #
#   11 - 12      |           2.8%         |  1,187,950.5    #
# --------------------------------------------------------- #
#    5 - 12      |            --          |  5,603,080      #
#############################################################


# Eligibility:
# All students in primary school children aged 5 - 12 years are eligible for the policy and 
# we apply a reduction in obesity prevalence to a percentage of children in that age group


# Modelling Approach:
# (1) Set eligibility for all children aged [5 - 12) year old
# (2) We then select all children living with obesity within this age group to receive the 
#     effect of the policy
# (3) Then, we filter for children living with obesity in 5-7 age group and loop through 
#     them to choose 8.7% of the children
# (4) These selected children that receive the effect of the intervention have their bmi 
#     values updated to the 94th percentile for their age and sex
# (5) We loop through the remaining age groups: 8-10 years & 11-12 years and select 6.5% & 
#     2.8% respectively of the children living with obesity in those age groups
# (6) We follow the process for updating their bmi values as described in point 4
# (7) Then, re-estimate the obesity prevalence in the child population

# References:
# [1] Office for National Statistics (2022). Estimates of the population for the UK, England and Wales, Scotland and Northern Ireland - Office for National Statistics. [online] Ons.gov.uk. 
#     Available at: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland.


# setup
rm(list = ls())
gc()
library(tidyverse)
library(here)

source(file = "requirements.R")
source(file = "pre_processing/pre_processing_adult.R")
source(file = "models/model_utils.R")
# source(file = "models/adult_model_calorie.R")
# source(file = "models/child_model_calorie_henry.R")


table_outputs = list() # creating a list of table outputs to be saved as an excel file


# required functions:

select_intervention_sample <- function(data, bmi_threshold, weight_var, bmi_var,
                                       num_years, req_proportion_list = 0, age_grps_list,
                                       criteria_1=0, criteria_1_value=0) {
  # browser()
  # Add intervention columns for each year to indicate intervention status, that is if an individual receives intervention.
  intervention_cols <- paste0("intervention_year", 1:num_years)
  data[, intervention_cols] <- "No"
  
  # Initialize a vector to store the individuals who have already received the intervention
  intervention_history <- rep(FALSE, nrow(data))
  
  for (year in 1:num_years) {
    
    # browser()
    # Subset the data frame to include only individuals meeting the criteria
    
    subset_data <- data[(data[[criteria_1]] == criteria_1_value) &  !intervention_history, ]
    
    for(idx in 1:3){
      
      age_grp_value = age_grps_list[idx]
    
      subset_data_filt <- subset_data[subset_data[["age_grp"]] == age_grp_value,]
      
    
    
    # Calculate the total weight of the full dataset
    total_weight <- sum(data[[weight_var]])
    eligible_weight = sum(subset_data_filt[[weight_var]])
    
    # Check if the required proportion is not greater than 1
    if (req_proportion_list[idx] > 1) {
      stop("The desired sample is greater than the population meeting the criteria.")
    }
    
    
    # Calculate the desired weight sum for the sample
    # req_prop = req_proportion_list[idx]
    
    desired_weight_sum <- req_proportion_list[idx] * sum(subset_data_filt[[weight_var]])
    
    
    # Initialize a vector to store the selected individuals for the current year
    
    selected_individuals <- rep(FALSE, nrow(subset_data_filt))
    
    # Initialize a variable to keep track of the current weight sum
    current_weight_sum <- 0
    
    # Perform weighted random sampling until the desired weight sum is reached
    while (current_weight_sum < desired_weight_sum) {
      remaining_indices <- which(!selected_individuals)
      
      if (length(remaining_indices) == 0) {
        break
      }
      
      selected_index <- sample(remaining_indices[subset_data_filt[[bmi_var]][remaining_indices] == bmi_threshold], size = 1,
                               prob = subset_data_filt[[weight_var]][remaining_indices[subset_data_filt[[bmi_var]][remaining_indices] == bmi_threshold]])
      selected_individuals[selected_index] <- TRUE
      
      current_weight_sum <- current_weight_sum + subset_data_filt[[weight_var]][selected_index]
    }
    
    # Check if the weighted sum of selected individuals is less than the desired weight sum
    selected_indices <- which(selected_individuals)
    
    if (sum(subset_data_filt[[weight_var]][selected_indices]) < desired_weight_sum) {
      remaining_indices <- which(!selected_individuals)
      additional_index <- sample(remaining_indices, size = 1, prob = subset_data_filt[[weight_var]][remaining_indices])
      selected_individuals[additional_index] <- TRUE
    }
    
    
    selected_indices_original <- which((data[[criteria_1]] == criteria_1_value & data[["age_grp"]] == age_grp_value) &
                                         !intervention_history)[selected_individuals]
    
    
    # Update the intervention column for the current year
    data[selected_indices_original, intervention_cols[year]] <- "Yes"
    
    # Update the intervention history
    intervention_history[selected_indices_original] <- TRUE
    
    final_wt = sum(data$wt_int[data$intervention_year1 == "Yes"])
    
  }
  }
  
  return(data)
}



bmi_refdata_100centiles = generate_bmi_refdata_100centiles(sitar::uk90)



process_clean_save(file_path = "inputs/raw/hse_2019_eul_20211006.tab",
                   nation = "England",
                   population_group = "Children")


df = read_csv(here("inputs/processed/hse_2019_children.csv")) %>%
  mutate(eligibility = "Yes") %>%
  mutate(apply_effect = case_when(age_grp %in% c("5-7","8-10", "11-12") ~ "Yes",
                                  TRUE ~ "No")) %>%
  rowwise() %>%
  mutate(baseline_bmi_category = lookup_bmi_percentile_category(age = age, 
                                                                sex = sex, 
                                                                bmi = bmi,
                                                                data_B = bmi_refdata_100centiles,
                                                                value_to_calculate = "bmi_category")) %>%
  ungroup()




# for reproducibility
set.seed(1611)

df_selected = select_intervention_sample(data = df,
                                         bmi_var = "baseline_bmi_category",
                                         bmi_threshold = "obese",
                                         weight_var = "wt_int",
                                         num_years = 1,
                                         criteria_1 = "apply_effect",
                                         criteria_1_value = "Yes",
                                         req_proportion_list = c(0.087, 0.065, 0.028),
                                         age_grps_list = c("5-7", "8-10", "11-12"))


# test to check the final selection proportions
test = df_selected %>%
  group_by(age_grp, intervention_year1) %>%
  summarise(counts = sum(wt_int),
            count_1 = n()) %>%
  mutate(percent_share = (counts/sum(counts))*100)


df_selected = df_selected %>%
  rowwise() %>%
  mutate(post_bmi_value = case_when(intervention_year1 == "Yes" ~ update_bmi(age = age,
                                                                             sex = sex,
                                                                             update_to_centile = 94,
                                                                             data_B = bmi_refdata_100centiles),
                                    TRUE ~ bmi)) %>%
  mutate(post_bmi_category = lookup_bmi_percentile_category(age = age, 
                                                            sex = sex, 
                                                            bmi = post_bmi_value,
                                                            data_B = bmi_refdata_100centiles,
                                                            value_to_calculate = "bmi_category")) %>%
  ungroup()




child_bmi_change = rbind(
  df_selected %>% 
    count(baseline_bmi_category, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Baseline") %>% 
    rename(BMI = baseline_bmi_category),
  df_selected %>% 
    count(post_bmi_category, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Post-Intervention") %>% 
    rename(BMI = post_bmi_category)) %>%
  mutate(BMI = factor(BMI, levels = c("underweight", "normal", "overweight", "obese")))





# plot of child BMI prevalance
child_bar_plot = child_bmi_change %>%
  ggplot(., aes(y = freq, x = BMI, fill = type)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme_ipsum() +
  labs(fill = "", 
       title = "BMI Distribution", 
       y = "Prevalence - %",
       subtitle = "Children - England | Policy 16") +
  theme_ipsum(base_size = 8, axis_title_size = 8) + #, base_family="Averta"
  theme(legend.position = "top")


child_bar_plot


ggsave(here("outputs/policy_16/policy_16_updated_impact_England_child.png"), 
       plot = child_bar_plot, 
       width = 10, 
       height = 6,
       bg='#ffffff')



# Table of year wise prevalence of obesity
child_bmi_change_year = child_bmi_change %>%
  select(-c(n)) %>%
  pivot_wider(., names_from = BMI, values_from = freq) %>%
  select("type", "underweight", "normal", "overweight", "obese")

child_bmi_change_year


table_outputs[["england_child"]] = child_bmi_change_year

write_xlsx(path = "outputs/policy_16/policy_16_updated_england.xlsx", x = table_outputs)



# full datafile outputs with intervention assignment
write.csv(df_selected, file = "outputs/policy_16/policy_16_updated_child_england_bmi.csv")




