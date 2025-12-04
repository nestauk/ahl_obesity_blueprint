



source(file = "requirements.R")
source(file = "pre_processing/pre_processing_adult.R")
source(file = "models/adult_model_calorie.R")
source(file = "config/config.R")
source(file = "post_processing/post_processing.R")
source(file = "analysis/policy_analysis/new_policies/testing/new_sampling_utils.R")

# Main analysis starts here:


# Step 1: reading in the cleaned processed baseline data file:
df_2019_adult = read_csv(here("inputs/processed/hse_2019.csv"))

df_2019_adult_eligibility = df_2019_adult %>%
  mutate(cond_ascvd = case_when(cardiovd == 1 | platlets == 1 | ace_inhibitors == 1 | diuretics == 1 | lipid == 1 ~ 1,
                                TRUE ~ 0),
         cond_hypertension =  case_when(hypertension == 1 ~ 1,
                                        TRUE ~ 0),
         cond_dyslipidaemia = case_when(lipid == 1 ~ 1,
                                        TRUE ~ 0),
         cond_diabetes = case_when(diabetes_type == 1 | metformin == 1 | anti_diabetics == 1 ~ 1,
                                   TRUE ~ 0)) %>%
  mutate(eligibility_score = cond_ascvd + cond_hypertension + cond_dyslipidaemia + cond_diabetes) %>%
  mutate(eligibility = case_when(
    (bmi >= 30) ~ 1,
    # (bmi >= 32.5 & ethnicity %in% c(2, 3, 4, 5)) ~ 1,
    TRUE ~ 0))

df_2019_adult_with_pop = df_2019_adult_eligibility %>%
  mutate(pop_share = wt_int/sum(wt_int)) %>%
  mutate(pop_estimate = round(pop_share * MODEL_CONSTANTS$ENGLAND_ADULT_POPULATION, 0))


set.seed(318)
# Old sampling method:

# Selecting the intervention sample each year who will receive the intervention:
# From those eligible, we select a number of people for each cohort in an year
intervention_sample = select_intervention_sample_method_1(data = df_2019_adult_with_pop, 
                                                 cohort_allocations = COHORT_10K_1Y,
                                                 population_size = MODEL_CONSTANTS$ENGLAND_ADULT_POPULATION,
                                                 weight_var = "wt_int",
                                                 num_years = MODEL_CONSTANTS$MODEL_DURATION,
                                                 cohort_var = "eligibility")

# test_df = intervention_sample$data
# Assign weight changes to individuals who were selected in the previous step
# We apply a weight loss of 18.5% for those without T2D and 13.8% for those with T2D
post_df_adult = assign_weight_changes(data = intervention_sample$data,
                                      bodyweight_var = "weight",
                                      num_years = MODEL_CONSTANTS$MODEL_DURATION,
                                      weight_loss_percent = TIRZ_MODEL_INPUTS$WEIGHT_LOSS_WITHOUT_T2D,
                                      weight_loss_percent_with_diabetes = TIRZ_MODEL_INPUTS$WEIGHT_LOSS_WITH_T2D,
                                      weight_regain = TIRZ_MODEL_INPUTS$WEIGHT_REGAIN_POST_TREATMENT)


# Calculating the new body weight, BMI and BMI Class for each individual:
post_df_adult = post_df_adult %>%
  mutate(bw_y1 = weight + weight_loss_y1 + weight_regain_y1,
         bw_y2 = bw_y1 + weight_loss_y2 + weight_regain_y2,
         bw_y3 = bw_y2 + weight_loss_y3 + weight_regain_y3,
         bw_y4 = bw_y3 + weight_loss_y4 + weight_regain_y4,
         bw_y5 = bw_y4 + weight_loss_y5 + weight_regain_y5) %>%
  mutate(bmi_y1 = bw_y1/ (height/100)^2,
         bmi_y2 = bw_y2/ (height/100)^2,
         bmi_y3 = bw_y3/ (height/100)^2,
         bmi_y4 = bw_y4/ (height/100)^2,
         bmi_y5 = bw_y5/ (height/100)^2) %>%
  mutate(bmi_1_class = case_when(bmi_y1 <= 18.5 ~ "underweight",
                                 bmi_y1 > 18.5 & bmi_y1 < 25 ~ "normal",
                                 bmi_y1 >= 25 & bmi_y1 < 30 ~ "overweight",
                                 bmi_y1 >= 30 & bmi_y1 < 40 ~ "obese",
                                 bmi_y1 >= 40 ~ "morbidly obese",
                                 TRUE ~ "NA"),
         bmi_2_class = case_when(bmi_y2 <= 18.5 ~ "underweight",
                                 bmi_y2 > 18.5 & bmi_y2 < 25 ~ "normal",
                                 bmi_y2 >= 25 & bmi_y2 < 30 ~ "overweight",
                                 bmi_y2 >= 30 & bmi_y2 < 40 ~ "obese",
                                 bmi_y2 >= 40 ~ "morbidly obese",
                                 TRUE ~ "NA"),
         bmi_3_class = case_when(bmi_y3 <= 18.5 ~ "underweight",
                                 bmi_y3 > 18.5 & bmi_y3 < 25 ~ "normal",
                                 bmi_y3 >= 25 & bmi_y3 < 30 ~ "overweight",
                                 bmi_y3 >= 30 & bmi_y3 < 40 ~ "obese",
                                 bmi_y3 >= 40 ~ "morbidly obese",
                                 TRUE ~ "NA"),
         bmi_4_class = case_when(bmi_y4 <= 18.5 ~ "underweight",
                                 bmi_y4 > 18.5 & bmi_y4 < 25 ~ "normal",
                                 bmi_y4 >= 25 & bmi_y4 < 30 ~ "overweight",
                                 bmi_y4 >= 30 & bmi_y4 < 40 ~ "obese",
                                 bmi_y4 >= 40 ~ "morbidly obese",
                                 TRUE ~ "NA"),
         bmi_5_class = case_when(bmi_y5 <= 18.5 ~ "underweight",
                                 bmi_y5 > 18.5 & bmi_y5 < 25 ~ "normal",
                                 bmi_y5 >= 25 & bmi_y5 < 30 ~ "overweight",
                                 bmi_y5 >= 30 & bmi_y5 < 40 ~ "obese",
                                 bmi_y5 >= 40 ~ "morbidly obese",
                                 TRUE ~ "NA"))



# A new dataframe is created to capture population level prevalence of different BMI categories in each year and is saved as a dataframe
bmi_change = rbind(
  post_df_adult %>% 
    count(bmi_5_class, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Year 5") %>% 
    rename(BMI = bmi_5_class),
  post_df_adult %>% 
    count(bmi_4_class, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Year 4") %>% 
    rename(BMI = bmi_4_class),
  post_df_adult %>% 
    count(bmi_3_class, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Year 3") %>% 
    rename(BMI = bmi_3_class),
  post_df_adult %>% 
    count(bmi_2_class, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Year 2") %>% 
    rename(BMI = bmi_2_class),
  post_df_adult %>% 
    count(bmi_1_class, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Year 1") %>% 
    rename(BMI = bmi_1_class),
  post_df_adult %>% 
    count(bmi_class, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Year 0") %>% 
    rename(BMI = bmi_class))


# creating a table of year wise distribution of BMI categories
bmi_change = bmi_change %>%
  mutate(BMI = factor(BMI, levels = c("underweight", "normal", "overweight", "obese", "morbidly obese"))) %>%
  as.data.frame()


bmi_change_year = bmi_change %>%
  select(-c(n)) %>%
  pivot_wider(., names_from = BMI, values_from = freq) %>%
  select(type, underweight, normal, overweight, obese, `morbidly obese`)

bmi_change_year

table_outputs = list() # creating a list of table outputs to be saved as an excel file

table_outputs[["england_adult"]] = bmi_change_year



# Outputs:

# Output 1: Table of year wise prevalence of obesity
bmi_change_year



# extract relative change:
bmi_change_year_1 = extract_relative_change(data = bmi_change_year)

# extract pound benefit:
table_outputs[["england_adult_with_benefits"]] = extract_pound_benefit(data = bmi_change_year_1, duration = 5, cost = MODEL_CONSTANTS$COST_OF_OBESITY_IN_BILLIONS)




# extracting the reduction in obesity prevalence 
annual_obesity_prevalence_england = extract_relative_change(data = bmi_change_year)

# Relative reduction in obesity prevalence in England = 0.2%

# Adding to table outputs:
table_outputs[["annual_obesity_prevalence_eng"]] = annual_obesity_prevalence_england

# Estimating the annual value to government (benefit):
annual_benefit_to_gov = extract_pound_benefit_by_class(data = bmi_change_year,
                                                       total_cost = MODEL_CONSTANTS$COST_OF_OBESITY_IN_BILLIONS,
                                                       cost_30_40 = obesity_cost_30_40,
                                                       cost_over_40 = obesity_cost_over_40,
                                                       duration = MODEL_CONSTANTS$MODEL_DURATION,
                                                       option = "option_1")


write_xlsx(path = "outputs/new_policies/policy_38/method_1/sample_10K_sum.xlsx", x = table_outputs)
write.csv(post_df_adult, file = "outputs/new_policies/policy_38/method_1/sample_10K_bmi.csv")















