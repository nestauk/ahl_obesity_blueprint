# New sampling methodology:

# # Option 1 cohort:
# OP_1_COHORT_ALLOCATION <- list(year1 = list(c1 = 28000),
#                               year2 = list(c1 = 14000, c2 = 47500),
#                               year3 = list(c2 = 47500, c3 = 85714),
#                               year4 = list(c3 = 114285, c4 = 58462),
#                               year5 = list(c4 = 175384))
# 
# # Option 2 cohort:
# OP_2_COHORT_ALLOCATION <- list(year1 = list(c1 = 21000),
#                               year2 = list(c1 = 8400, c2 = 33600),
#                               year3 = list(c2 = 32900, c3 = 58100),
#                               year4 = list(c3 = 81900, c4 = 44100),
#                               year5 = list(c4 = 126000))
COHORT_3_4M_5Y <- list(
  year1 = list(c1 = 476000),
  year2 = list(c1 = 476000),
  year3 = list(c1 = 476000),
  year4 = list(c1 = 476000),
  year5 = list(c1 = 476000)
)

COHORT_1_6M_5Y <- list(
  year1 = list(c1 = 224000),
  year2 = list(c1 = 224000),
  year3 = list(c1 = 224000),
  year4 = list(c1 = 224000),
  year5 = list(c1 = 224000)
)


source(file = "requirements.R")
source(file = "pre_processing/pre_processing_adult.R")
source(file = "models/adult_model_calorie.R")
source(file = "config/config.R")
source(file = "post_processing/post_processing.R")
library(data.table)


# Constants:
ENGLAND_ADULT_POPULATION = 44263393
WEIGHT_LOSS_WITH_T2D = 0.138
WEIGHT_LOSS_WITHOUT_T2D = 0.185
WEIGHT_REGAIN_POST_TREATMENT = 0

# functions:
get_bmi_category <- function(bmi) {
  case_when(
    bmi < 18.5 ~ "underweight",
    bmi >= 18.5 & bmi < 25 ~ "normal",
    bmi >= 25 & bmi < 30 ~ "overweight",
    bmi >= 30 & bmi < 40 ~ "obese",
    bmi >= 40 ~ "morbidly obese",
    TRUE ~ NA_character_
  )
}

# function to select intervention sample:
select_intervention_sample_dt <- function(data, cohort_allocations,
                                          weight_var, num_years, cohort_var, weight_var_2,
                                          n_selection_per_draw = 1) {
  
  # browser()
  # Convert to data.table and add tracking columns
  setDT(data)
  print(paste("start time = ", Sys.time()))
  
  intervention_cols <- paste0("intervention_year", 1:num_years)
  pop_selected_cols <- paste0("pop_selected_year", 1:num_years)
  rem_weight_cols <- paste0("rem_weight_year", 1:num_years)
  status_col <- "status"
  
  # Add tracking columns 
  data[, (intervention_cols) := "No"]
  data[, (pop_selected_cols) := 0]
  data[, (status_col) := "not_picked"]
  data[, remaining_weight := get(weight_var_2)]
  data[, (rem_weight_cols):= get(weight_var_2)]
  
  comment_list <- list()
  
  # Step 1: Iterating through years and cohorts in a year:
  for (year in 1:num_years) {
    year_name <- paste0("year", year)
    if (!year_name %in% names(cohort_allocations)) next
    
    year_allocations <- cohort_allocations[[year_name]]
    
    for (cohort in names(year_allocations)) {
      cohort_value <- as.numeric(gsub("c", "", cohort))
      sample_size <- year_allocations[[cohort]]
      
      # Step 2: Identifying eligible individuals
      cohorts_to_consider <- if (cohort_value == 1) {
        cohorts_to_consider <- 1:3 
      } else {
        cohorts_to_consider <- cohort_value
      }
      eligible_population <- data[get(cohort_var) %in% cohorts_to_consider, sum(remaining_weight)]
      
      # If population is insufficient, expand the cohort pool, first look in previous cohorts then in later cohorts
      if (is.na(eligible_population) || sample_size > eligible_population) {
        
        cohorts_to_consider <- 1:cohort_value
        eligible_population <- data[get(cohort_var) %in% cohorts_to_consider, sum(remaining_weight)]
        
        if (is.na(eligible_population) || sample_size > eligible_population) {
          
          cohorts_to_consider <- 1:(cohort_value + 1)
          
        }
      }
      
      # Step 3: Sampling:
      desired_weight_sum <- sample_size
      current_weight_sum <- 0
      
      while (current_weight_sum < desired_weight_sum) {
        # Find remaining eligible indices for this iteration
        remaining_indices <- which(
          data[[cohort_var]] %in% cohorts_to_consider &
            data$remaining_weight > 0
        )
        
        # if enough observations not found then save message to be printed out later
        if (length(remaining_indices) == 0) {
          comment_list <- c(comment_list, paste0("Ran out of individuals for year ", year, ", cohort ", cohort))
          break
        }
        
        # Identify the highest priority (lowest cohort number) among those remaining
        highest_priority_cohort <- min(data[remaining_indices, get(cohort_var)])
        priority_indices <- which(
          data[[cohort_var]] == highest_priority_cohort &
            data$remaining_weight > 0
        )
        
        # if there is only one observation, then select it, if more than one exists, then
        # then use probability and population weight
        if (length(priority_indices) == 1) {
          
          # If there's only one choice, don't sample, select them
          selected_index <- priority_indices
          
        } else {
          
          # If there are multiple choices, run sample() with probability = population weight
          selected_index <- sample(priority_indices, size = 1, prob = data[priority_indices, remaining_weight])
          
        }
        
        # Step 4: Calculate the portion of the population weight to choose
        # if the remaining weight is < 100, all of those individuals are assigned to treatment to avoid
        # negative values (weight cannot go < 0)
        # in all other cases, we take a random number of people between 50 and 100 to receive treatment
        weight_available <- data[selected_index, remaining_weight]
        
        if (weight_available < 100) {
          
          n_chosen <- weight_available
          
        } else {
          
          n_chosen <- sample(50:100, 1)
          
        }
        
        # Additional checks on n_chosen to ensure we don't take more than what 
        # is required and stop if a value <=0 is chosen, in this case we resample
        if (current_weight_sum + n_chosen > desired_weight_sum) {
          n_chosen <- desired_weight_sum - current_weight_sum
        }
        if (n_chosen <= 0) break
        
        
        # Subtract the population taken for treatment from the 'remaining_weight' column
        data[selected_index, remaining_weight := remaining_weight - n_chosen]
        
        # Append to the status message
        current_status <- data[selected_index, get(status_col)]
        message_to_add <- paste0(n_chosen, " is chosen")
        new_status <- ifelse(current_status == "not_picked", message_to_add, paste(current_status, message_to_add, sep = ", "))
        data[selected_index, (status_col) := new_status]
        
        # update current weight sum with the n_chosen value
        current_weight_sum <- current_weight_sum + n_chosen
      }
      
      # print status message:
      print(paste0("completed Year =", year, ", cohort = ", cohort, ", selected = ", current_weight_sum))
      
      
    }
    
    # Step 5: Find all observations who were selected in a given year to update intervention status
    rows_updated_this_cohort <- which(data[[status_col]] != "not_picked")
    
    if (length(rows_updated_this_cohort) > 0) {
      
      data[rows_updated_this_cohort, (intervention_cols[year]) := "Yes"]
      data[rows_updated_this_cohort, (pop_selected_cols[year]) := pop_estimate - remaining_weight]
      data[rows_updated_this_cohort, (rem_weight_cols[year]) := remaining_weight ]
      
    }
  }
  
  # Step 6: After all years are processed, create new columns with the actual number of people selected for treatment each year
  actual_cols <- paste0("actual_selected_year", 1:num_years)
  
  # Year 1 is already correct (not cumulative)
  data[, (actual_cols[1]) := get(pop_selected_cols[1])]
  
  # For years 2 to 5, subtract the previous year's cumulative
  for (year in 2:num_years) {
    prev_year_col <- paste0("pop_selected_year", year - 1)
    curr_year_col <- paste0("pop_selected_year", year)
    
    data[, (actual_cols[year]) := get(curr_year_col) - get(prev_year_col)]
  }
  
  print(paste("finish time = ", Sys.time()))
  
  # returning output as dataframe
  df_output = as.data.frame(data)
  
  return(df_output)
}


# Function to assign weight loss:
assign_weight_loss <- function(data, bodyweight_var, num_years, weight_loss_percent, weight_loss_percent_with_diabetes) {
  # browser()
  data = data %>%
    mutate(intervention_status_overall = case_when(intervention_year1 == "Yes" ~ "Yes",
                                                   intervention_year2 == "Yes" ~ "Yes",
                                                   intervention_year3 == "Yes" ~ "Yes",
                                                   intervention_year4 == "Yes" ~ "Yes",
                                                   intervention_year5 == "Yes" ~ "Yes",
                                                   TRUE ~ "No")) %>%
    mutate(weight_loss = case_when(intervention_status_overall == "Yes" & cond_diabetes == 1 ~ -weight_loss_percent_with_diabetes*.data[[bodyweight_var]],
                                   intervention_status_overall == "Yes" & cond_diabetes == 0 ~ -weight_loss_percent*.data[[bodyweight_var]],
                                   TRUE ~ 0))
  
  return(data)
}


estimate_new_weights_bmi <- function(df,
                                     new_weight_col_name = "new_weight",
                                     new_bmi_col_name = "new_bmi",
                                     new_bmi_category = "new_bmi_class",
                                     baseline_weight_col = "weight",
                                     weight_loss_col = "weight_loss",
                                     baseline_height_col = "height") {
  df <- df %>%
    mutate(
      # Compute new weight
      {{ new_weight_col_name }} := .data[[baseline_weight_col]] + .data[[weight_loss_col]],
      # Compute new BMI
      {{new_bmi_col_name}} := round(.data[[new_weight_col_name]] / (.data[[baseline_height_col]] / 100)^2, 7)
    ) %>%
    mutate({{new_bmi_category}} := get_bmi_category(.data[[new_bmi_col_name]]))
  
  return(df)
}


get_yearwise_bmi_category_counts <- function(data, pop_weight_col, treatment_cols, intervention_cols, untreated_cat_col, treated_cat_col) {
  
  W_total <- sum(data[[pop_weight_col]], na.rm = TRUE)
  
  # Ensure there are exactly 5 treatment columns
  if (length(treatment_cols) != 5) {
    stop("Error: 'treatment_cols' must be a character vector with exactly 5 column names.")
  }
  
  # step 1: get cumulative number of people treated each year
  processed_df <- data %>%
    mutate(
      treated_y0 = 0,
      treated_y1 = .data[[treatment_cols[1]]],
      treated_y2 = .data[[treatment_cols[1]]] + .data[[treatment_cols[2]]],
      treated_y3 = .data[[treatment_cols[1]]] + .data[[treatment_cols[2]]] + .data[[treatment_cols[3]]],
      treated_y4 = .data[[treatment_cols[1]]] + .data[[treatment_cols[2]]] + .data[[treatment_cols[3]]] + .data[[treatment_cols[4]]],
      treated_y5 = .data[[treatment_cols[1]]] + .data[[treatment_cols[2]]] + .data[[treatment_cols[3]]] + .data[[treatment_cols[4]]] + .data[[treatment_cols[5]]]
    ) %>%
    # step 2: rename columns
    mutate(
      bmi_cat_untreated = .data[[untreated_cat_col]],
      bmi_cat_treated = .data[[treated_cat_col]]
    ) %>%
    # step 3: select only required columns
    select(id, wt_int, height, weight, pop_estimate, bmi, new_bmi,
           starts_with("intervention"), starts_with("treated_y"),
           bmi_cat_untreated, bmi_cat_treated) %>%
    # step 4: pivot longer so each row indicates the yearly status of observation
    pivot_longer(
      cols = starts_with("treated_y"),
      names_to = "year",
      values_to = "cumulative_treated",
      names_prefix = "treated_y"
    ) %>%
    # step 5: for each year get the number of people treated and untreated
    mutate(pop_untreated = pop_estimate - cumulative_treated,
           pop_treated = cumulative_treated) %>%
    # step 6: pivot longer - for each observation for each year has a treated and untreated row with counts
    pivot_longer(
      cols = c(pop_untreated, pop_treated),
      names_to = "group_type",
      values_to = "group_weight"
    ) %>%
    # step 7: assign bmi category based on treatment status
    mutate(
      bmi_category = if_else(group_type == "pop_untreated", bmi_cat_untreated, bmi_cat_treated)
    ) %>%
    # step 8: get the summary of year and bmi category
    summarise(
      total_weight = sum(group_weight),
      .by = c(year, bmi_category)
    ) %>%
    mutate(prevalence = (total_weight / W_total)*100) %>%
    arrange(year, bmi_category) %>%
    mutate(year = paste0("Year ", year)) %>%
    rename(type = year)
  
  
  return(processed_df)
}


# Main analysis starts here:

# Step 0: Choose option 1 (~3.4M people) or Option 2 (1.6M people)
cohort = COHORT_1_6M_5Y


# Step 1: reading in the cleaned processed baseline data file:
df_2019_adult = read_csv(here("inputs/processed/hse_2019.csv"))

# Step 2: Applying eligibility criteria.
# First we assign '1' if an individual has a condition
# Then we calculate the eligibility score (1 - 4) where 4 = 4 comobidities and 1 = 1 comorbidity

df_2019_adult_eligibility = df_2019_adult %>%
  mutate(cond_ascvd = case_when(cardiovd == 1 | platlets == 1 | ace_inhibitors == 1 | diuretics == 1 | lipid == 1 ~ 1,
                                TRUE ~ 0),
         cond_hypertension =  case_when(hypertension == 1 ~ 1,
                                        TRUE ~ 0),
         cond_dyslipidaemia = case_when(lipid == 1 ~ 1,
                                        TRUE ~ 0),
         cond_diabetes = case_when(diabetes_type == 1 | metformin == 1 | anti_diabetics == 1 ~ 1,
                                   TRUE ~ 0)) %>%
  mutate(eligibility_score = cond_ascvd + cond_hypertension + cond_dyslipidaemia + cond_diabetes)



# STEP 3: ASSIGN COHORTS
# Everyone BMI 40+ is Tier 1. 
# BMI 37.5+ is the "Overflow" to hit 1.6M target.
# ==========================================================
df_2019_adult_with_cohorts <- df_2019_adult_eligibility %>%
  mutate(eligibility = case_when(
    # --- COHORT 1: BMI 40+ ---
    (bmi >= 40) | (bmi >= 37.5 & ethnicity %in% 2:5) ~ 1,
    
    # --- COHORT 2: BMI 37.5 - 40 ---
    (bmi >= 37.5) | (bmi >= 35 & ethnicity %in% 2:5) ~ 2,
    
    # --- COHORT 3: BMI 35 - 37.5 ---
    (bmi >= 35) | (bmi >= 32.5 & ethnicity %in% 2:5) ~ 3,
    
    # --- COHORT 4: BMI 32.5 - 35 ---
    (bmi >= 32.5) | (bmi >= 30 & ethnicity %in% 2:5) ~ 4,
    
    # --- COHORT 5: BMI 30 - 32.5 (Final Buffer) ---
    (bmi >= 30) | (bmi >= 27.5 & ethnicity %in% 2:5) ~ 5,
    
    TRUE ~ 0
  ))


# Step 4: Blowing up survey weights:
# Blowing up the survey weights so that each observation represents a number of people in the population:
df_2019_adult_with_pop = df_2019_adult_with_cohorts %>%
  mutate(pop_share = wt_int/sum(wt_int)) %>%
  mutate(pop_estimate = round(pop_share * ENGLAND_ADULT_POPULATION, 0))

# check difference between ONS mid year pop estimate for 2019 and sum of blown up survey weights:
sum(df_2019_adult_with_pop$pop_estimate)-  ENGLAND_ADULT_POPULATION


# Step 5: Selecting intervention sample:
# Selecting the intervention sample each year who will receive the intervention:
# From those eligible, we select a number of people for each cohort in an year
intervention_sample = select_intervention_sample_dt(data = df_2019_adult_with_pop, 
                                                    cohort_allocations = cohort,
                                                    #population_size = ENGLAND_ADULT_POPULATION,
                                                    weight_var = "wt_int",
                                                    num_years = MODEL_CONSTANTS$MODEL_DURATION,
                                                    cohort_var = "eligibility", weight_var_2 = "pop_estimate",
                                                    n_selection_per_draw = n_per_draw)

# Step 6: Assign weight loss
# Assign weight changes to individuals who were selected in the previous step
# We apply a weight loss of 18.5% for those without T2D and 13.8% for those with T2D
post_df_adult = assign_weight_loss(data = intervention_sample,
                                   bodyweight_var = "weight",
                                   num_years = MODEL_CONSTANTS$MODEL_DURATION,
                                   weight_loss_percent = WEIGHT_LOSS_WITHOUT_T2D,
                                   weight_loss_percent_with_diabetes = WEIGHT_LOSS_WITH_T2D)


# Step 7: Calculate new weights and BMI
post_df_adult_updated_weights_bmi = estimate_new_weights_bmi(df = post_df_adult)

# Step 8: Get year wise BMI category percents and counts
year_wise_prevalence = get_yearwise_bmi_category_counts(data = post_df_adult_updated_weights_bmi,pop_weight_col = "pop_estimate",
                                                        intervention_cols = post_df_adult_updated_weights_bmi %>%
                                                          select(starts_with("intervention")) %>% colnames(),
                                                        treatment_cols = post_df_adult_updated_weights_bmi %>%
                                                          select(starts_with("actual")) %>% colnames(),
                                                        untreated_cat_col = "bmi_class",
                                                        treated_cat_col = "new_bmi_class" )



year_wise_prevalence_pivoted <- year_wise_prevalence %>%
  select(bmi_category, type, prevalence) %>%
  pivot_wider(
    names_from = bmi_category,
    values_from = prevalence,
  ) %>%
  select("type", "underweight", "normal",	"overweight",	"obese",	"morbidly obese")

# year and category wise counts:
year_wise_counts_pivoted <- year_wise_prevalence %>%
  select(bmi_category, type, total_weight) %>%
  pivot_wider(
    names_from = bmi_category,
    values_from = total_weight,
  ) %>%
  select("type", "underweight", "normal",	"overweight",	"obese",	"morbidly obese")


# extract relative change:
bmi_change_year = extract_relative_change(data = year_wise_prevalence_pivoted)

# extract pound benefit:
extract_pound_benefit_by_class(data = bmi_change_year, duration = 5)

col_name <- grep("morbidly obese", names(year_wise_counts_pivoted), ignore.case = TRUE, value = TRUE)

baseline_val <- as.numeric(unlist(year_wise_counts_pivoted[tolower(year_wise_counts_pivoted$type) == "year 0", col_name]))
final_val    <- as.numeric(unlist(year_wise_counts_pivoted[tolower(year_wise_counts_pivoted$type) == "year 5", col_name]))

perc_change_class3 <- ((final_val - baseline_val) / baseline_val) * 100

print(perc_change_class3)

# # year wise bmi plots:
# adult_bar_plot = year_wise_prevalence %>%
#   ggplot(., aes(y = prevalence, x = bmi_category, fill = type)) + 
#   geom_bar(stat = "identity", position = "dodge") +
#   theme_ipsum() +
#   labs(fill = "", 
#        title = "NHS Rollout of Tirzepatide (Mounjaro) - Option 2", 
#        y = "Prevalence - %",
#        x = "BMI group",
#        subtitle = "Per year distribution by BMI Category") +
#   theme_ipsum(base_size = 7, axis_title_size = 6, axis_text_size = 7) + #, base_family="Averta"
#   theme(legend.position = "top",
#         legend.text = element_text(size = 9),
#         axis.title.y = element_text(size = 10, hjust = 0.5),  # y-axis title
#         axis.title.x = element_text(size = 10, hjust = 0.5, vjust = 0.9),  # x-axis title
#         axis.text.x = element_text(size = 7),    # x-axis tick labels
#         axis.text.y = element_text(size = 7)  )   # y-axis tick labels)



# --- Constants from Frontier Economics & User Input ---
TOTAL_OBESITY_COST_BN <- 107.0      # Total annual economic/societal cost 
TOTAL_NHS_OBESITY_COST_BN <- 9.3   # Annual financial cost to NHS 
TOTAL_OBESE_POP_M <- 18.1          # UK population with obesity 
BASE_DRUG_COST <- 1677.1      # average cost based off calculation for GLP-1 modelling in blueprint trizepatide modelling 1.1 - cell number N48

# --- Means Testing Constants (Scenario 4.1 and 4.2) ---
TIER3_SHARE <- 0.35
ANNUAL_PRESCRIPTION_COST <- 9.9 * 1 * 12  # £118.80

# Net cost per person after Tier 3 prescription recovery
MEANS_TEST_4_1_COST <- BASE_DRUG_COST - (TIER3_SHARE * ANNUAL_PRESCRIPTION_COST)

# Net cost per person after Tier 3 prescription + 50% wraparound recovery
# Wraparound per person derived from total GP cost in Scenario 1.1
WRAPAROUND_PER_PERSON <- 1110628089 / total_treated_pop
MEANS_TEST_4_2_COST <- MEANS_TEST_4_1_COST - (TIER3_SHARE * WRAPAROUND_PER_PERSON * 0.50)

# 1. Calculate the 'Gross Prizes' (Total value saved by treating 3.4M people)
actual_cols <- grep("actual_selected_year", names(post_df_adult_updated_weights_bmi), value = TRUE)
total_treated_pop <- sum(colSums(post_df_adult_updated_weights_bmi[, actual_cols], na.rm = TRUE))
treated_share <- total_treated_pop / (TOTAL_OBESE_POP_M * 1e6)

# Total Gross Benefits (Total value saved before subtracting costs)
gross_econ_prize_bn <- treated_share * TOTAL_OBESITY_COST_BN
gross_nhs_prize_bn  <- treated_share * TOTAL_NHS_OBESITY_COST_BN

# 2. Define All Scenarios
cost_points <- c(
  BASE_DRUG_COST,
  BASE_DRUG_COST * 0.60,
  BASE_DRUG_COST * 0.30,
  250,
  400,
  MEANS_TEST_4_1_COST,
  MEANS_TEST_4_2_COST
)

scenario_names <- c(
  "0% Discount",
  "40% Discount",
  "70% Discount",
  "Oviva Cost",
  "NHS Digital Cost",
  "Means Testing 4.1 (Drugs Only)",
  "Means Testing 4.2 (Drugs + 50% Wraparound)"
)
# 3. Create the Net Benefit Table
benefit_scenario_analysis <- map2_dfr(cost_points, scenario_names, function(cp, name) {
  
  total_investment_bn <- (total_treated_pop * cp) / 1e9
  
  data.frame(
    Scenario = name,
    Cost_Per_Person = round(cp, 2),
    Total_Investment_BN = round(total_investment_bn, 3),
    Net_Economic_Benefit_BN = round(gross_econ_prize_bn - total_investment_bn, 3),
    Net_NHS_Savings_BN = round(gross_nhs_prize_bn - total_investment_bn, 3)
  )
})

# Display the table
print(benefit_scenario_analysis)