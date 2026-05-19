library(tidyverse)
library(here)
library(bw)
library(survey)

#' Function - process_clean_save()
#' Process, clean, and save health survey data for specified nation and
#' population group
#'
#' Note: This function currently only works for England, Adult. All others need
#' to be updated.
#'
#' This function processes and cleans raw health survey data (e.g., HSE or SHeS)
#' for adults or children in England or Scotland. It filters out invalid or
#' missing observations, standardizes variable names, derives new variables
#' such as age, BMI class, energy intake, and income support status, and saves
#' the cleaned dataset to an AWS S3 bucket as a CSV file.
#'
#' @param df A data frame containing the raw health survey data.
#' @param nation Character string specifying the country; must be either
#' `"England"` or `"Scotland"`.
#' @param population_group Character string specifying the population group;
#' must be either `"Adult"` or `"Children"`.
#' @param file_name Character string specifying the name for the output CSV
#' file (Currently only implemented for England).
#'
#' @details
#' The function:
#' \itemize{
#'   \item Filters rows based on valid height, weight, and age values.
#'   \item Renames columns for consistency.
#'   \item Derives age group and mid-point age.
#'   \item Computes energy intake using PAL and resting metabolic rate (RMR).
#'   \item Classifies individuals into BMI categories.
#'   \item Calculates an intake up-weighting factor (adults in England only).
#'   \item Saves the cleaned data to an S3 bucket using `aws.s3::s3write_using`.
#' }
#'
#' Separate processing pipelines are implemented for each combination of
#' `nation` and `population_group`.
#'
#' @return No return value. The function saves the processed data as a `.csv`
#' file to S3 and prints a confirmation message.
#'
#' @import dplyr
#' @import aws.s3
#' @importFrom utils write.csv
#' @importFrom readr write_csv
#' @importFrom here here
#' @export

process_clean_save <- function(df, nation, population_group, file_name) {
  if (nation %in% c("England", "Scotland") && population_group %in% c("Adult", "Children")) {
    if (nation == "England" && population_group == "Adult") {
      # browser()
      df_2019_adult <- df %>%
        filter(.data$WtVal > 0 & .data$HtVal > 0 & .data$Age35g > 7) %>%
        mutate(age = case_when(
          Age35g == 7 ~ (16 + 19) / 2,
          Age35g == 8 ~ (20 + 24) / 2,
          Age35g == 9 ~ (25 + 29) / 2,
          Age35g == 10 ~ (30 + 34) / 2,
          Age35g == 11 ~ (35 + 39) / 2,
          Age35g == 12 ~ (40 + 44) / 2,
          Age35g == 13 ~ (45 + 49) / 2,
          Age35g == 14 ~ (50 + 54) / 2,
          Age35g == 15 ~ (55 + 59) / 2,
          Age35g == 16 ~ (60 + 64) / 2,
          Age35g == 17 ~ (65 + 69) / 2,
          Age35g == 18 ~ (70 + 74) / 2,
          Age35g == 19 ~ (75 + 79) / 2,
          Age35g == 20 ~ (80 + 84) / 2,
          Age35g == 21 ~ (85 + 89) / 2,
          Age35g == 22 ~ (90),
          TRUE ~ 0
        )) %>%
        mutate(age_grp = case_when(
          Age35g == 7 ~ "16-19",
          Age35g == 8 ~ "20-24",
          Age35g == 9 ~ "25-29",
          Age35g == 10 ~ "30-34",
          Age35g == 11 ~ "35-39",
          Age35g == 12 ~ "40-44",
          Age35g == 13 ~ "45-49",
          Age35g == 14 ~ "50-54",
          Age35g == 15 ~ "55-59",
          Age35g == 16 ~ "60-64",
          Age35g == 17 ~ "65-69",
          Age35g == 18 ~ "70-74",
          Age35g == 19 | Age35g == 20 | Age35g == 21 | Age35g == 22 ~ "75+",
          TRUE ~ "NA"
        )) %>%
        rename(
          weight = WtVal,
          height = HtVal,
          sex = Sex,
          bmi = BMIVal,
          qimd = qimd19,
          number_children = Nofch3,
          income_JSA = srcin05d, # Job Seekers Allowance
          income_IS = srcin07d, # Income Support
          income_PC = srcin08d, # Pension Credit
          income_CTC = srcin10d, # Child Tax Credit
          income_UC = srcin14d, # Universal Credit
          ethnicity = origin2,
          diabetes = diabete2,
          cardiovd = CardioTakg2,
          alcohol_overall = alcbase_19,
          diabetes_type = TypeD,
          hypertension = HyperTakg2,
          lipid = LipidTakg2,
          platlets = AntiPlaTakg2,
          ace_inhibitors = ACETAKg2,
          diuretics = DIURTAKg2,
          anti_diabetics = AntiDiabTakg2,
          metformin = METFORTAKg2,
          id = SerialA,
          psu = PSU_SCR,
          strata = cluster94
        ) %>%
        mutate(income_support_status = case_when(
          (income_JSA == 1 | income_IS == 1 | income_PC == 1 | income_CTC == 1 | income_UC == 1) ~ 1,
          TRUE ~ 0
        )) %>%
        mutate(children_updated = case_when(
          number_children == 0 ~ 0,
          TRUE ~ 1
        )) %>%
        mutate(diabetes = case_when(
          diabetes == 1 ~ 1,
          TRUE ~ 0
        )) %>%
        mutate(diabetes_type = case_when(
          diabetes_type == 2 ~ 1,
          TRUE ~ 0
        )) %>%
        mutate(cardiovd = case_when(
          cardiovd == 1 ~ 1,
          TRUE ~ 0
        )) %>%
        mutate(hypertension = case_when(
          hypertension == 1 ~ 1,
          TRUE ~ 0
        )) %>%
        mutate(lipid = case_when(
          lipid == 1 ~ 1,
          TRUE ~ 0
        )) %>%
        mutate(platlets = case_when(
          platlets == 1 ~ 1,
          TRUE ~ 0
        )) %>%
        mutate(ace_inhibitors = case_when(
          ace_inhibitors == 1 ~ 1,
          TRUE ~ 0
        )) %>%
        mutate(diuretics = case_when(
          diuretics == 1 ~ 1,
          TRUE ~ 0
        )) %>%
        mutate(anti_diabetics = case_when(
          anti_diabetics == 1 ~ 1,
          TRUE ~ 0
        )) %>%
        mutate(metformin = case_when(
          metformin == 1 ~ 1,
          TRUE ~ 0
        )) %>%
        dplyr::select(
          id, weight, height, age_grp, age, sex, bmi, qimd, alcohol_overall, children_updated, income_support_status,
          ethnicity, diabetes, diabetes_type, cardiovd, hypertension, lipid, platlets, ace_inhibitors,
          diuretics, anti_diabetics, metformin, wt_int, psu, strata
        ) %>% # select variables needed
        mutate(qimd_updated = case_when(
          (qimd == 4 | qimd == 5) ~ 1,
          TRUE ~ 0
        )) %>%
        mutate(
          pal = 1.6, # pal assumed to be 1.6 for the entire population to indicate a sendentary/ light active lifestyle
          rmr = case_when(
            sex == 1 ~ ((10 * weight) + (6.25 * height) - (5 * age) + 5),
            TRUE ~ ((10 * weight) + (6.25 * height) - (5 * age) - 161)
          )
        ) %>% # sex = 2 female; rmr is calculated using Mifflin St Jeor Equations from Mifflin et al (1990)
        mutate(bmi_class = case_when(
          bmi <= 18.5 ~ "underweight",
          bmi > 18.5 & bmi < 25 ~ "normal",
          bmi >= 25 & bmi < 30 ~ "overweight",
          bmi >= 30 & bmi < 40 ~ "obese",
          bmi >= 40 ~ "morbidly obese",
          TRUE ~ "NA"
        )) %>%
        mutate(excess_weight_class = case_when(
          bmi_class %in% c("overweight", "obese", "morbidly obese") ~ "excess_weight",
          TRUE ~ "not_excess_weight")) %>%
        mutate(intake = pal * rmr) # calculating value of energy intake

      # browser()
      up_weigting_factor_df <- df_2019_adult %>%
        group_by(excess_weight_class) %>%
        summarise(
          mean_intake = weighted.mean(w = wt_int, intake),
          count = sum(wt_int)
        ) %>%
        mutate(pop_mean_intake = weighted.mean(x = mean_intake, w = count)) %>%
        mutate(intake_change_upweighting_factor = mean_intake / pop_mean_intake)

      df_2019_adult <- df_2019_adult %>%
        left_join(up_weigting_factor_df %>%
          select(
            excess_weight_class,
            intake_change_upweighting_factor
          ))


      aws.s3::s3write_using(
        x = df_2019_adult,
        FUN = write.csv,
        bucket = "ahl-private-data",
        object = paste0("hse/processed/", file_name, ".csv"),
        row.names = FALSE
      )

      print(paste0(
        "Output csv with processed data is saved to S3 -",
        "hse/processed/", file_name, ".csv"
      ))
    } else if (nation == "Scotland" & population_group == "Adult") {
      # browser()
      # Scotland

      # select(CPSerialA, SYear,PSU, Strata, int19wt, cint19wt, bmival, htval,
      # wtval, BMIvg5, CBMIg5_new, age, Sex, SIMD20_SGa, Ethnic05, totinc,
      # eqv5_15, hedqul08)

      # The data used for this analysis is Scottish Health Survey 2019 available
      # on UK Data Service portal.
      # First, the dataset is filtered by weight, height and age to ensure that
      # all observations have a height and weight > 0 and is limited to Adults
      # (age>= 18). Subsequently, variables are renamed for ease and further
      # subset to include only the variables of interest for modelling obesity
      # prevalance.

      # The next step is to calculate the energy intake at baseline. This energy
      # intake can be calculated by multiplying an individuals physical activity
      # level (PAL) and basal metabolic rate (BMR). PAL values are available for
      # different levels of physical activity but on average for the population
      # we have assumed everyone to be engaged in light work (PAL = 1.6). BMR is
      # assumed to be equal to resting metabolic rate (RMR) which is the energy
      # cost of maintaining metabolic homeostasis.

      # At the baseline, in that point in time, we assume that people are
      # maintaining their weight, therefore, their energy intake equals the
      # energy requirement for physical activities and for metabolic homeostasis
      # which is given by the equation: PAL * BMR

      df_2019_adult <- df %>%
        filter(wtval > 0 & htval > 0 & age >= 18) %>% # exclude rows
        rename(
          weight = wtval,
          height = htval,
          sex = Sex,
          bmi_ur = bmi,
          bmi = bmival,
          simd = SIMD20_SGa,
          diabetes = diabete2,
          cardiovd = medtyp1B,
          ethnicity = Ethnic05,
          id = CPSerialA,
          psu = PSU,
          wt_int = int19wt,
          strata = Strata
        ) %>%
        mutate(age_grp = case_when(
          age >= 20 & age <= 24 ~ "20-24",
          age >= 25 & age <= 29 ~ "25-29",
          age >= 30 & age <= 34 ~ "30-34",
          age >= 35 & age <= 39 ~ "35-39",
          age >= 40 & age <= 44 ~ "40-44",
          age >= 45 & age <= 49 ~ "45-49",
          age >= 50 & age <= 54 ~ "50-54",
          age >= 55 & age <= 59 ~ "55-59",
          age >= 60 & age <= 64 ~ "60-64",
          age >= 65 & age <= 69 ~ "65-69",
          age >= 70 & age <= 74 ~ "70-74",
          age >= 75 ~ "75+",
          TRUE ~ "NA"
        )) %>%
        dplyr::select(
          id, weight, height, age_grp, age, sex, bmi, simd,
          diabetes, cardiovd, ethnicity, wt_int, psu, strata
        ) %>%
        mutate(simd_updated = case_when(
          (simd == 1 | simd == 2) ~ 1,
          TRUE ~ 0
        )) %>%
        mutate(
          pal = 1.6,
          rmr = case_when(
            sex == 1 ~ ((10 * weight) + (6.25 * height) - (5 * age) + 5),
            TRUE ~ ((10 * weight) + (6.25 * height) - (5 * age) - 161)
          )
        ) %>% # Calculating an individuals resting metabolic rate using equations published in Mifflin & St.Jeor (1990)
        mutate(intake = pal * rmr) %>% # calculating energy intake at baseline
        mutate(bmi_class = case_when(
          bmi <= 18.5 ~ "underweight",
          bmi > 18.5 & bmi < 25 ~ "normal",
          bmi >= 25 & bmi < 30 ~ "overweight",
          bmi >= 30 & bmi < 40 ~ "obese",
          bmi >= 40 ~ "morbidly obese",
          TRUE ~ "NA"
        )) %>% # classifying individuals into BMI categories based on BMI values at baseline
        mutate(excess_weight_class = case_when(
          bmi_class %in% c("overweight", "obese", "morbidly obese") ~ "excess_weight",
          TRUE ~ "not_excess_weight"))

      # browser()
      up_weigting_factor_df <- df_2019_adult %>%
        group_by(excess_weight_class) %>%
        summarise(
          mean_intake = weighted.mean(w = wt_int, intake),
          count = sum(wt_int)
        ) %>%
        mutate(pop_mean_intake = weighted.mean(x = mean_intake, w = count)) %>%
        mutate(intake_change_upweighting_factor = mean_intake / pop_mean_intake)
      
      df_2019_adult <- df_2019_adult %>%
        left_join(up_weigting_factor_df %>%
                    select(
                      excess_weight_class,
                      intake_change_upweighting_factor
                    ))

      aws.s3::s3write_using(
        x = df_2019_adult,
        FUN = write.csv,
        bucket = "ahl-private-data",
        object = paste0("shes/processed/", file_name, ".csv"),
        row.names = FALSE
      )

      print(paste0(
        "Output csv with processed data is saved to S3 -",
        "shes/processed/", file_name, ".csv"
      ))

    } else if (nation == "England" && population_group == "Children") {
      # Age35g is a categorical variable of 3 year age bands for 0-15 year olds, smallest possible grouping from HSE 2019
      # Age35g ==  indicates all those in age group 20-24 years
      df_2019_children <- read.table(here(file_path), sep = "\t", header = TRUE) %>%
        filter(WtVal > 0 & HtVal > 0 & Age35g > 2 & Age35g < 8) %>% # age is set to 5 - 18 years old as the Hall Model is only validted for that age group
        mutate(age = case_when(
          Age35g == 1 ~ (0 + 1) / 2, # 0.5
          Age35g == 2 ~ (2 + 4) / 2, # 3
          Age35g == 3 ~ (5 + 7) / 2, # 6
          Age35g == 4 ~ (8 + 10) / 2, # 9
          Age35g == 5 ~ (11 + 12) / 2, # 11.5
          Age35g == 6 ~ (13 + 15) / 2, # 14
          Age35g == 7 ~ (16 + 18) / 2, # 17
          TRUE ~ 0
        )) %>%
        mutate(age_grp = case_when( # Age35g == 7 ~ (16+19)/2,
          Age35g == 1 ~ "0-1",
          Age35g == 2 ~ "2-4",
          Age35g == 3 ~ "5-7",
          Age35g == 4 ~ "8-10",
          Age35g == 5 ~ "11-12",
          Age35g == 6 ~ "13-15",
          Age35g == 7 ~ "16-19",
          TRUE ~ "NA"
        ))


      df_2019_children <- df_2019_children %>%
        rename(
          weight = WtVal,
          height = HtVal,
          sex = Sex,
          bmi = BMIVal,
          father_bmi = fath_bmi2,
          mother_bmi = moth_bmi2,
          qimd = qimd19,
          id = SerialA,
          psu = PSU_SCR,
          strata = cluster94
        ) %>%
        dplyr::select(id, weight, height, age_grp, age, sex, bmi, father_bmi, mother_bmi, qimd, wt_int, psu, strata, origin2) %>% # select variables needed
        mutate(parent_bmi = case_when(
          (father_bmi == 2 | mother_bmi == 2) ~ 1,
          TRUE ~ 0
        )) %>%
        mutate(qimd_updated = case_when(
          (qimd == 4 | qimd == 5) ~ 1,
          TRUE ~ 0
        )) %>%
        mutate(
          pal = case_when(
            age < 3 ~ 1.40,
            age >= 3 & age < 10 ~ 1.58,
            age >= 10 & age < 18 ~ 1.75
          ), # pal set based on SACN (2011) guidelines
          rmr_hox = case_when(
            sex == 1 ~ (((66.9 * weight) + 2876) / 4.184),
            TRUE ~ (((47.9 * weight) + 3230) / 4.184)
          )
        ) %>% # rmr for children calculated using Henry Oxford equations
        mutate(intake_hox = pal * rmr_hox) # calculating value of energy intake and fat mass is calculated using Hudda et al (2019)


      # write_csv(df_2019_children, here("inputs/processed/hse_2019_children.csv"))
      # print("Output csv with processed data is saved here: inputs/processed/hse_2019_children.csv")

      aws.s3::s3write_using(
        x = df_2019_adult,
        FUN = write.csv,
        bucket = "ahl-private-data",
        object = "hse/processed/hse_2019_children.csv",
        row.names = FALSE
      )

      print("Output csv with processed data is saved to S3 - ahl-private-data/hse/processed/")
    } else if (nation == "Scotland" & population_group == "Children") {
      df_2019_child_sc <- read.table(here(file_path), sep = "\t", header = TRUE) %>%
        filter(wtval > 0 & htval > 0 & age >= 5 & age <= 18 & !is.na(cint19wt)) %>%
        rename(
          weight = wtval,
          height = htval,
          sex = Sex,
          bmi_ur = bmi,
          bmi = bmival,
          simd = SIMD20_SGa,
          id = CPSerialA,
          psu = PSU,
          wt_int = cint19wt,
          origin2 = Ethnic05,
          strata = Strata
        ) %>%
        mutate(age_grp = case_when(
          age >= 5 & age <= 7 ~ "5-7",
          age >= 8 & age <= 10 ~ "8-10",
          age >= 11 & age <= 12 ~ "11-12",
          age >= 13 & age <= 15 ~ "13-15",
          age >= 16 & age <= 19 ~ "16-19",
          TRUE ~ "NA"
        )) %>%
        dplyr::select(id, weight, height, age_grp, age, sex, bmi, simd, wt_int, psu, strata, origin2) %>% # select variables needed
        mutate(simd_updated = case_when(
          (simd == 1 | simd == 2) ~ 1,
          TRUE ~ 0
        )) %>%
        mutate(
          pal = case_when(
            age < 3 ~ 1.40,
            age >= 3 & age < 10 ~ 1.58,
            age >= 10 & age < 18 ~ 1.75
          ),
          rmr_hox = case_when(
            sex == 1 ~ (((66.9 * weight) + 2876) / 4.184),
            TRUE ~ (((47.9 * weight) + 3230) / 4.184)
          )
        ) %>%
        mutate(intake_hox = pal * rmr_hox) # calculating value of energy intake


      # write_csv(df_2019_child_sc, here("inputs/processed/shes_2019_children.csv"))
      # print("Output csv with processed data is saved here: inputs/processed/shes_2019_children.csv")

      aws.s3::s3write_using(
        x = df_2019_adult,
        FUN = write.csv,
        bucket = "ahl-private-data",
        object = "hse/processed/shes_2019_children.csv",
        row.names = FALSE
      )

      print("Output csv with processed data is saved to S3 - ahl-private-data/hse/processed/")
    }
  } else {
    # If the country is neither "England" nor "Scotland", print an error message
    stop("Invalid nation or population group specified! Please specify either 'England' or 'Scotland' for nation and 'Adult' or 'Children' for population_group.")
  }
}


#' Function - generate_cols_of_interest()
#' Generate columns of interest for time series analysis
#'
#' Creates a vector of column indices representing days, starting from n years
#' ago and ending at day 1. Each year is assumed to have 365 days.
#'
#' @param n Integer. The number of years to go back from the current time point.
#'
#' @return A numeric vector where the first element is 365*n (n years ago)
#'   and the last element is 1 (most recent day), in descending order.
#'
#' @export

generate_cols_of_interest <- function(n) {
  return(c(365 * (n:1), 1))
}


#' Function - classify_bmi()
#' Classify BMI values into standard weight categories
#'
#' Converts numeric BMI (Body Mass Index) values into standard weight status
#' categories based on WHO guidelines.
#'
#' @param bmi_value Numeric vector. BMI values to be classified. BMI is
#'   calculated as weight (kg) divided by height squared (m²).
#'
#' @return Character vector with BMI classifications:
#'   \itemize{
#'     \item "underweight" for BMI ≤ 18.5
#'     \item "normal" for BMI > 18.5 and < 25
#'     \item "overweight" for BMI ≥ 25 and < 30
#'     \item "obese" for BMI ≥ 30 and < 40
#'     \item "morbidly obese" for BMI ≥ 40
#'     \item "NA" for any other values (including actual NA values)
#'   }
#'
#' @export
classify_bmi <- function(bmi_value) {
  case_when(
    bmi_value <= 18.5 ~ "underweight",
    bmi_value > 18.5 & bmi_value < 25 ~ "normal",
    bmi_value >= 25 & bmi_value < 30 ~ "overweight",
    bmi_value >= 30 & bmi_value < 40 ~ "obese",
    bmi_value >= 40 ~ "morbidly obese",
    TRUE ~ "NA"
  )
}


#' Function - process_bmi_data()
#' Process BMI data from Hall equations output
#'
#' Takes a dataframe containing Hall equations output and processes it by
#' selecting relevant columns, renaming BMI time series columns to a
#' standardized format, and creating BMI classification columns for each time
#' point.
#'
#' @param df Data frame. The input dataframe containing Hall equations output
#' with BMI values at different time points and other metabolic variables.
#' @param n Integer. The number of years of BMI data to process. The function
#'   expects BMI columns for time points: 1 (day 1) and 365, 730, ..., 365*n
#'   (representing 1, 2, ..., n years).
#'
#' @return Data frame with:
#'   \itemize{
#'     \item All base columns: id, weight, height, age, sex, bmi, wt_int,
#'       intervention, pal, rmr, bmi_class, intake,
#'       intake_change_upweighting_factor, compensation_adjustment_factor,
#'       intake_diff
#'     \item Renamed BMI columns: bmi_0, bmi_1, ..., bmi_n (where bmi_0 is
#'     day 1, bmi_1 is 1 year, etc.)
#'     \item BMI classification columns: bmi_0_class, bmi_1_class, ...,
#'     bmi_n_class containing weight status categories for each time point
#'   }
#'
#'
#' @details
#' The function expects the input dataframe to contain columns named with numeric
#' values representing days (1, 365, 730, etc.) that contain BMI values from the
#' Hall equations model output. These are renamed to a more intuitive format
#' (bmi_0 for baseline, bmi_1 for 1 year, etc.).
#'
#' @seealso \code{\link{classify_bmi}} for the BMI classification function used
#'   to create the classification columns.
#'
#' @importFrom dplyr select all_of rename mutate
#' @importFrom rlang sym
#'
#' @export
process_bmi_data <- function(df, n) {
  # Generate the column names of the BMI columns in the Hall equations output
  time_cols <- as.character(c(1, 365 * (1:n)))

  # List of existing columns to select
  base_cols <- c(
    "id", "weight", "height", "age", "sex", "bmi", "wt_int",
    "intervention", "pal", "rmr", "bmi_class", "intake",
    "intake_change_upweighting_factor",
    "compensation_adjustment_factor", "intake_diff"
  )

  # Select required columns from the dataframe
  df_selected <- df %>%
    select(all_of(c(base_cols, time_cols)))

  # Update names of columns with numerical names
  rename_map <- setNames(time_cols, paste0("bmi_", 0:n))

  df_renamed <- df_selected %>%
    rename(!!!rename_map)

  # Create BMI classification columns
  for (i in 0:n) {
    col_name <- paste0("bmi_", i)
    class_col_name <- paste0("bmi_", i, "_class")

    # df_renamed[[class_col_name]] <- classify_bmi(df_renamed[[col]])
    df_renamed <- df_renamed %>%
      mutate(!!class_col_name := classify_bmi(!!sym(col_name)))
  }

  return(df_renamed)
}


#' Function - create_bmi_change()
#' Create BMI change summary data for visualization
#'
#' Transforms BMI classification data from wide to long format and calculates
#' weighted frequencies and percentages of BMI categories across different
#' time points for visualization purposes.
#'
#' @param df Data frame. Input dataframe containing BMI classification columns
#'   and weight intervention data. Must include columns: bmi_class,
#'   bmi_1_class, bmi_2_class, ..., bmi_n_class, and wt_int.
#' @param n Integer. The number of years of BMI data to process. Should match
#'   the number of bmi_*_class columns available in the dataframe.
#'
#' @return Data frame with columns:
#'   \itemize{
#'     \item type: Character. Time point labels ("Year 0", "Year 1", "Year 2", etc.)
#'     \item BMI: Factor. BMI categories ordered from underweight to morbidly obese
#'     \item n: Numeric. Weighted count of individuals in each BMI category
#'     \item freq: Numeric. Percentage frequency (0-100) of each BMI category within each time point
#'   }
#'
#' @details
#' The function performs the following transformations:
#' \enumerate{
#'   \item Selects BMI classification columns and weight intervention column
#'   \item Pivots data from wide to long format
#'   \item Creates readable time point labels (Year 0, Year 1, etc.)
#'   \item Calculates weighted counts using the weight intervention factor
#'   \item Computes percentage frequencies within each time point
#'   \item Orders BMI categories as a factor for consistent plotting
#' }
#'
#'
#' @seealso
#' \code{\link{process_bmi_data}} for preparing the input dataframe with BMI
#' classifications.
#'
#' @importFrom dplyr select all_of mutate case_when count group_by ungroup
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_extract
#'
#' @export
create_bmi_change <- function(df, n) {
  # Select BMI class columns
  bmi_cols <- c("bmi_class", paste0("bmi_", 1:n, "_class"))

  df %>%
    select(all_of(bmi_cols), wt_int) %>%
    pivot_longer(
      cols = all_of(bmi_cols),
      names_to = "year_col",
      values_to = "BMI"
    ) %>%
    mutate(type = case_when(
      year_col == "bmi_class" ~ "Year 0",
      TRUE ~ paste(
        "Year",
        str_extract(year_col, "\\d+")
      )
    )) %>%
    count(type, BMI, wt = wt_int) %>%
    group_by(type) %>%
    mutate(freq = n / sum(n) * 100) %>%
    ungroup() %>%
    mutate(BMI = factor(BMI, levels = c(
      "underweight",
      "normal",
      "overweight",
      "obese",
      "morbidly obese"
    ))) %>%
    as.data.frame()
}



#' Function - calculate_bmi_from_eichange()
#' Calculate BMI changes from energy intake interventions using the Hall model
#'
#' Applies the Hall et al. (2011) adult weight change model to estimate BMI
#' trajectories following energy intake interventions. The function processes
#' individual-level data, applies intervention effects with optional upweighting
#' and compensation adjustments, and returns comprehensive outputs including
#' plots and summary tables.
#'
#' @param df Data frame. Input dataset containing individual-level data with
#'   required columns: weight, height, age, sex (1 = male, 0 = female),
#'   bmi_class, and optionally the upweighting factor column.
#' @param kcal_intake_change Numeric. Daily energy intake change in kcal to be
#'   applied as intervention (negative values for reduction).
#' @param na_intake_change Numeric. Daily sodium intake change in mg.
#'  Default is 0.
#' @param implmentation_duration Integer. Duration of intervention
#' implementation in years. Must be non-zero. Default is 5.
#' @param apply_upweighting_by_bmi_class Logical. Whether to apply upweighting
#'   factors by BMI class. Default is TRUE.
#' @param upweight_factor_col Character. Name of column containing upweighting
#'   factors. Default is "intake_change_upweighting_factor".
#' @param apply_compensation Logical. Whether to apply compensation effects that
#'   reduce intervention effectiveness. Default is TRUE.
#' @param compensation_effect Numeric. Compensation factor (0-1) representing
#'   the proportion of intervention effect that is compensated. Default is 0.23.
#' @param tags Character. Tags or labels for plot titles.
#' Default is empty string.
#'
#' @return Named list containing:
#'   \itemize{
#'     \item hall_model_op: Raw output from the Hall adult weight model
#'     \item individual_full_bmi_matrix: Individual-level BMI trajectories for
#'      all days
#'     \item post_df: Processed dataframe with year-wise BMI values and
#'     classifications
#'     \item bmi_category_plot: ggplot object showing BMI distribution changes
#'     over time
#'     \item bmi_percent_prevalence: Summary table of BMI category prevalence
#'     by year
#'   }
#'
#' @details
#' The function implements the following workflow:
#' \enumerate{
#'   \item Applies interventions only to individuals with BMI ≥ 25
#'   (overweight/obese)
#'   \item Calculates effective intake change accounting for compensation and
#'    upweighting
#'   \item Runs the Hall adult weight model for the specified duration
#'   \item Processes results to extract year-wise BMI classifications
#'   \item Generates visualization and summary statistics
#' }
#'
#' Intervention targeting: Individuals classified as "underweight" or "normal"
#' weight receive no intervention (intake_diff = 0). Only those with BMI ≥ 25
#' receive the specified energy intake change.
#'
#' Compensation effects: When enabled, reduces the effective intervention by
#' the specified compensation factor to account for behavioral adaptation.
#'
#' @seealso
#' \code{\link{process_bmi_data}} for processing BMI time series data
#' \code{\link{create_bmi_change}} for creating BMI change summaries
#' \code{\link{classify_bmi}} for BMI classification
#'
#' @importFrom dplyr mutate case_when select pivot_wider
#' @importFrom rlang sym
#' @importFrom ggplot2 ggplot aes geom_bar labs theme
#' @importFrom hrbrthemes theme_ipsum
#' @importFrom tidyr pivot_wider
#'
#' @export
calculate_bmi_from_eichange <- function(df,
                                        kcal_intake_change,
                                        na_intake_change = 0,
                                        implmentation_duration = 5,
                                        apply_upweighting_by_bmi_class = TRUE,
                                        upweight_factor_col = "intake_change_upweighting_factor",
                                        apply_compensation = TRUE,
                                        compensation_effect = 0.23,
                                        tags = "") {
  # Updating variables based on inputs
  if (apply_upweighting_by_bmi_class == TRUE) {
    upweight_val <- sym(upweight_factor_col)
  } else {
    upweight_val <- 1
  }

  if (apply_compensation == TRUE) {
    compensation_val <- compensation_effect
  } else {
    compensation_val <- 0
  }

  if (implmentation_duration != 0) {
    n_days <- implmentation_duration * 365
  } else {
    message("Error, enter non-zero implementation duration!")
  }
  
  output_list <- list() # initializing an empty list to store outputs

  
  uptake_factor_excess_weight = df %>%
    filter(excess_weight_class == "excess_weight") %>%
    select(excess_weight_class, intake_change_upweighting_factor) %>%
    distinct() %>%
    pull(intake_change_upweighting_factor)
  
  
  kcal_input = kcal_intake_change
  kcal_upweighted = kcal_input * uptake_factor_excess_weight
  kcal_compensation_applied = kcal_upweighted * (1 - compensation_effect)
  print(paste0("Input kcal change: ", kcal_input,
               " | Upweighted kcal change: ", round(mean(kcal_upweighted),2),
               " | Compensation applied kcal change: ", round(mean(kcal_compensation_applied),2)
               ))

  df <- df %>% # processed dataset with variables such as height, weight, bmi, daily energy intake passed as input to the function
    mutate(sex = ifelse(sex == 1, "male", "female")) %>% # modifying the variable sex in a form expected by the Hall Model
    # mutate(intake_diff = ifelse(bmi_class %in% c("underweight", "normal"), 0, 99)) %>% # creating a variable to store the change in energy intake due to intervention and applying it only to those with BMI >= 25
    mutate(
      pop_intake_diff = kcal_intake_change,
      compensation_adjustment_factor = compensation_val
    ) %>%
    mutate(intake_diff = case_when(
      bmi_class %in% c("underweight", "normal") ~ 0,
      TRUE ~ pop_intake_diff * (1 - compensation_adjustment_factor) * !!upweight_val
    )) %>%
    mutate(intervention = ifelse(bmi_class %in% c("underweight", "normal"), "No", "Yes")) %>%
    mutate(na_intake_diff = ifelse(bmi_class %in% c("underweight", "normal"), 0, na_intake_change))

  eichange <- t(apply(df, 1, function(x) rep(as.numeric(x["intake_diff"]), n_days))) # creating the matrix (for the duration of implementation of the policy) with change in daily energy intake due to the policy

  nachange <- t(apply(df, 1, function(x) rep(as.numeric(x["na_intake_diff"]), n_days))) # creating the matrix (for the duration of implementation of the policy) with change in daily sodium intake due to the policy set to zero '0'

  # Implementing the Hall model described in Hall et al. (2011). Inputs to the model have self explanatory variable names
  model_weight <- adult_weight(
    bw = df$weight,
    ht = df$height / 100,
    age = df$age,
    sex = df$sex,
    EIchange = eichange,
    NAchange = nachange,
    days = n_days
  )

  output_list[["hall_model_op"]] <- model_weight # Hall Models output is added to the list

  bmi_model <- model_weight[["Body_Mass_Index"]] # Extracting the table with body mass index from Hall Model outputs
  post_df_adult <- cbind(df, bmi_model) # Combining the extracted body mass index table to the input dataframe
  output_list[["individual_full_bmi_matrix"]] <- post_df_adult


  year_wise_bmi_df <- process_bmi_data(post_df_adult, n = implmentation_duration)
  # Selecting variables of interest. The dataframe created in the previous line contains bmi values for each day of the implementation duration.
  # We are interested only in the BMI values at the end of each intervention year to estimate the distribution of bodyweights
  # the BMI values at the end of Year 1, Year 2, Year 3, Year 4 and Year 5 are collected and labelled with BMI categories.

  output_list[["post_df"]] <- year_wise_bmi_df # Dataframe added to outputs list


  # A new dataframe is created to capture population level prevalence of different BMI categories in each year and is saved as a dataframe
  bmi_change <- create_bmi_change(year_wise_bmi_df, implmentation_duration)

  # Converting BMI class to factor type:
  bmi_change <- bmi_change %>%
    mutate(BMI = factor(BMI, levels = c("underweight", "normal", "overweight", "obese", "morbidly obese"))) %>%
    as.data.frame()

  # Output 1: Plot of year on year BMI category distribution
  adult_bar_plot <- bmi_change %>%
    ggplot(., aes(y = freq, x = BMI, fill = type)) +
    geom_bar(stat = "identity", position = "dodge") +
    theme_ipsum() +
    labs(
      fill = "",
      title = paste(tags),
      y = "Prevalence - %",
      subtitle = "Adult | BMI Distribution"
    ) +
    theme_ipsum(base_size = 8, axis_title_size = 8) + # , base_family="Averta"
    theme(legend.position = "top")

  output_list[["bmi_category_plot"]] <- adult_bar_plot # BMI category distribution plot is added to the outputs list

  # Output 2: Table of year wise percentage prevalence of each BMI category
  bmi_change_year <- bmi_change %>%
    select(-c(n)) %>%
    pivot_wider(., names_from = BMI, values_from = freq) %>%
    select(type, underweight, normal, overweight, obese, `morbidly obese`)


  output_list[["bmi_percent_prevalence"]] <- bmi_change_year # Year wise BMI category percentage prevalance table is added to outputs list

  return(output_list) # outputs are returned
}




#' Function - extract_relative_change()
#' Extract relative change in obesity prevalence from BMI intervention data
#'
#' Calculates the relative change in total obesity prevalence (obese + morbidly obese)
#' compared to baseline (Year 0) and reports the reduction at Year 5. Adds calculated
#' absolute and relative change columns to the input data.
#'
#' @param data Data frame. Input dataset containing BMI prevalence data across
#'   different time points. Must include columns: "type" (time periods like "Year 0",
#'   "Year 5"), "obese" (obesity prevalence), and "morbidly obese" (morbid obesity
#'   prevalence).
#' @param year numeric value of the duration of modelling. Default is 5
#'
#' @return Data frame. The input dataframe with additional columns:
#'   \itemize{
#'     \item total_obesity: Numeric. Combined prevalence of obese and morbidly obese categories
#'     \item absolute_change: Numeric. Absolute difference between baseline and current
#'       year total obesity prevalence
#'     \item relative_change: Numeric. Percentage change in total obesity prevalence
#'       relative to baseline ((absolute_change/baseline_total_obesity) * 100)
#'   }
#'
#' @details
#' The function performs the following calculations:
#' \enumerate{
#'   \item Combines "obese" and "morbidly obese" categories into total_obesity
#'   \item Extracts baseline obesity prevalence from "Year 0" data
#'   \item Calculates absolute change as baseline minus current prevalence
#'   \item Computes relative change as percentage of baseline
#'   \item Prints the relative reduction at Year 5 as a summary statistic
#' }
#'
#' The function assumes that reductions in obesity are beneficial, so positive
#' relative_change values indicate successful intervention effects.
#'
#' @seealso
#' \code{\link{create_bmi_change}} for generating the input BMI prevalence data
#' \code{\link{extract_pound_benefit}} for converting relative changes to monetary benefits
#'
#' @importFrom dplyr mutate filter pull
#'
#' @export
extract_relative_change <- function(data, years = 5) {
  # browser()
  data_df <- data %>%
    mutate(total_obesity = obese + `morbidly obese`)

  baseline_total_obesity <- data_df %>%
    filter(type == "Year 0") %>%
    pull(total_obesity)

  data_df <- data_df %>%
    mutate(absolute_change = baseline_total_obesity - total_obesity) %>%
    mutate(relative_change = (absolute_change / baseline_total_obesity) * 100)

  relative_change_obesity_prevalence <- data_df %>%
    filter(type == paste0("Year ", years)) %>%
    pull(relative_change)

  print(paste0(
    "Relative reduction in obesity prevalence at ",
    years,
    " years = ",
    round(relative_change_obesity_prevalence, 1),
    "%"
  ))

  return(data_df)
}


#' Function - extract_pound_benefit()
#' Extract monetary benefits in pounds from intervention data
#'
#' Calculates the monetary value to government from health interventions by
#' applying cost data to relative changes and computing average annual benefits.
#'
#' @param data Data frame. Input dataset containing relative change data from
#'   health interventions. Must include a column named "relative_change" with
#'   percentage changes.
#' @param cost Numeric. Cost per unit or cost multiplier to be applied to the
#'   relative changes (e.g., cost per percentage point change).
#' @param duration Numeric. Duration period in years over which to calculate
#'   the average annual benefit.
#'
#' @return Data frame. The input dataframe with an additional column:
#'   \itemize{
#'     \item value_to_gov_per_year: Numeric. Calculated monetary value to
#'       government per year for each observation, derived from
#'       (relative_change * cost) / 100
#'   }
#'
#' @details
#' The function performs the following calculations:
#' \enumerate{
#'   \item Creates a new column calculating yearly government value from relative changes
#'   \item Sums all individual values and divides by duration to get average annual benefit
#'   \item Prints the average annual value in billions of pounds
#'   \item Returns the enhanced dataframe with value calculations
#' }
#'
#' The relative_change values are expected to be in percentage form and are
#' divided by 100 in the calculation.
#'
#' # Output will print: "Average annual value to government compared to baseline = £X.XX billions"
#' # Returns dataframe with value_to_gov_per_year column added
#' }
#'
#' @note
#' The function prints the average annual value in billions of pounds. Ensure
#' that the cost parameter is scaled appropriately (e.g., if cost represents
#' millions, the output will be in millions unless adjusted).
#'
#' @importFrom dplyr mutate select
#'
#' @export

extract_pound_benefit <- function(data, cost, duration) {
  # browser()
  data_df <- data %>%
    mutate(value_to_gov_per_year = (relative_change * cost) / 100)

  data_df <- data_df %>%
    mutate(year_in_order = as.numeric(gsub("Year", "", data_df$type))) %>%
    arrange(year_in_order) %>%
    select(-year_in_order)

  average_annual_value_to_gov <- data_df %>%
    select(value_to_gov_per_year) %>%
    sum() / duration

  print(paste0("Average annual value to government compared to baseline = £", round(average_annual_value_to_gov, 2), "billions"))

  return(data_df)
}
