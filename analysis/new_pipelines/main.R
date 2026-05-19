#' ============================================================================
#' Obesity Impact Estimation
#' ============================================================================
#'
#' PURPOSE:
#' This script analyzes the impact of energy intake reduction on obesity
#' prevalence in England using the Hall et al. (2011) adult weight change model.
#' It estimates BMI trajectory changes over a 5-year implementation period and
#' calculates the associated reduction in obesity preavlence and economic
#' benefits to government.
#'
#' Constants and input parameters to the model can be modified in the config.R
#' file.
#'
#' METHOD:
#' - Uses HSE 2019 processed data as baseline populationby default
#' - Applies the input kcal daily intake reduction to individuals with BMI ≥ 25
#' - Incorporates compensation effects when set to TRUE
#' - Incorporates upweighting when set to TRUE
#' - Models weight changes using Hall's validated physiological model
#' - Calculates relative reduction in obesity prevalence
#' - Estimates annual economic benefits based on obesity cost set in config.R
#'
#' MAIN OUTPUTS:
#' 1. Dataframe of model parameters and annual economic benefits to government
#' 2. Dataframe of detailed individual-level BMI trajectories
#'
#' DATA SOURCES:
#' - Input: HSE 2019 processed data from S3 (hse/processed/hse_2019_16.csv)
#'
#' DEPENDENCIES:
#' - utils.R: Contains obesity modelling functions
#' - config.R: Model constants and parameters
#' ============================================================================

run_obesity_model <- function(
    cost_of_obesity_in_billions = NULL,
    model_duration = NULL,
    upweighting_by_bmi = NULL,
    compensation_factor = NULL,
    kcal_intake_change = NULL,
    sodium_intake_change = NULL,
    policy_name = NULL,
    survey_data = NULL,
    output_xlsx_object = NULL,
    output_csv_object = NULL) {
  # 1. setup
  gc()
  suppressMessages({
    library(tidyverse)
    library(here)
    library(bw)
    library(hrbrthemes)
    library(writexl)
  })


  # overwrite default parameters from config.R if not specified
  if (!is.null(cost_of_obesity_in_billions)) {
    MODEL_CONSTANTS$COST_OF_OBESITY_IN_BILLIONS <- cost_of_obesity_in_billions
  }
  if (!is.null(model_duration)) {
    MODEL_PARAMETERS$MODEL_DURATION <- model_duration
  }
  if (!is.null(upweighting_by_bmi)) {
    MODEL_PARAMETERS$UPWEIGHTING_BY_BMI <- upweighting_by_bmi
  }
  if (!is.null(compensation_factor)) {
    MODEL_PARAMETERS$COMPENSATION_FACTOR <- compensation_factor
  }
  if (!is.null(kcal_intake_change)) {
    MODEL_PARAMETERS$KCAL_INTAKE_CHANGE <- kcal_intake_change
  }
  if (!is.null(sodium_intake_change)) {
    MODEL_PARAMETERS$SODIUM_INTAKE_CHANGE <- sodium_intake_change
  }
  if (!is.null(policy_name)) {
    TEXT_PARAMETERS$POLICY_NAME <- policy_name
  }

  MODEL_CONSTANTS$COST_OF_OBESITY <- MODEL_CONSTANTS$COST_OF_OBESITY_IN_BILLIONS * 1e9

  # raise an error if kcal_intake_change is not specified
  if (is.null(MODEL_PARAMETERS$KCAL_INTAKE_CHANGE)) {
    stop("ERROR: kcal_intake_change parameter must be specified either as an argument to the function.")
  }

  table_outputs <- list() # creating a list of table outputs to be saved as an excel file

  # 2. Reading in required data:
  # read in the processed, cleaned file saved on S3:
  # hse_2019_processed <- s3read_using(
  #   FUN = read.csv,
  #   bucket = s3_bucket,
  #   object = hse_data_object,
  #   header = TRUE
  # )

  # 3. Impact on prevalence of obesity:
  # Estimating the impact of the policy:
  # input constants and parameters to the model can be updated in config.R file
  policy_impact_model <- calculate_bmi_from_eichange(
    df = survey_data,
    kcal_intake_change = MODEL_PARAMETERS$KCAL_INTAKE_CHANGE,
    implmentation_duration = MODEL_PARAMETERS$MODEL_DURATION,
    apply_upweighting_by_bmi_class = MODEL_PARAMETERS$UPWEIGHTING_BY_BMI,
    apply_compensation = TRUE,
    upweight_factor_col = "intake_change_upweighting_factor",
    compensation_effect = MODEL_PARAMETERS$COMPENSATION_FACTOR,
    na_intake_change = MODEL_PARAMETERS$SODIUM_INTAKE_CHANGE,
    tags = TEXT_PARAMETERS$POLICY_NAME
  )


  # 4. Summary of modelling parameters:
  # creating a single list of all parameters:
  all_parameters <- c(TEXT_PARAMETERS, MODEL_PARAMETERS, MODEL_CONSTANTS)

  # converting to dataframe
  model_params_table <- data.frame(
    Parameter = names(all_parameters),
    Value = unlist(all_parameters),
    row.names = NULL
  )

  table_outputs$model_params <- model_params_table

  # 5. Outputs

  # 5.1. Bar plot of change in year on year distribution of different BMI categories
  # bmp_category_plot <- policy_impact_model$bmi_category_plot

  # 5.2. Output table with year on year distribution of BMI categories
  bmi_category_change <- policy_impact_model$bmi_percent_prevalence

  # 5.3. Extracting the reduction in obesity prevalence
  annual_obesity_prevalence_england <- extract_relative_change(data = bmi_category_change, years = MODEL_PARAMETERS$MODEL_DURATION)

  # 5.4. Estimating the annual value to government (benefit):
  annual_benefit_to_gov <- extract_pound_benefit(
    data = annual_obesity_prevalence_england,
    cost = MODEL_CONSTANTS$COST_OF_OBESITY_IN_BILLIONS,
    duration = MODEL_PARAMETERS$MODEL_DURATION
  )

  table_outputs$annual_benefit_to_gov <- annual_benefit_to_gov

  # 5.5. Return the outputs as a list
  return(list(
    table_outputs = table_outputs,
    policy_impact_model = policy_impact_model
  ))
}
