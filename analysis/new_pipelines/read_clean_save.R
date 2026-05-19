#' ============================================================================
#' Health Survey Data Cleaning and Processing Script
#' ============================================================================
#'
#' PURPOSE:
#' This script processes and cleans raw health survey data for adults
#'  and children in England and Scotland (HSE/SHeS 2019). It filters
#'  data, derives new variables (e.g., age, BMI class, energy intake),
#'  and saves the cleaned dataset to AWS S3.
#'  It uses the function process_clean_save() from utils.
#'
#' MAIN OUTPUTS:
#' 1. CSV file with processed data stored on S3
#'
#' DATA SOURCES:
#' - Input: HSE 2019 raw data from S3 (hse/raw/hse_2019_eul_20211006.tab)
#'
#' DEPENDENCIES:
#' - utils.R: Contains data cleaning functions
#' ============================================================================


# setup
library(here)
library(tidyverse)
library(dplyr)

source("analysis/new_pipelines/utils.R")

# read raw data file from S3
hse_2019_raw <- s3read_using(
    FUN = read.table,
    bucket = "ahl-private-data",
    object = "hse/raw/hse_2019_eul_20211006.tab",
    sep = "\t", header = TRUE
)

# process, clean and save file for HSE 2019 to S3:
process_clean_save(
    df = hse_2019_raw,
    nation = "England",
    population_group = "Adult",
    file_name = "hse_2019_16_bp"
)

shes_2019_raw <- s3read_using(
  FUN = read.table,
  bucket = "ahl-private-data",
  object = "shes/raw/shes19i_eul.tab",
  sep = "\t", header = TRUE
)

# process, clean and save file for SHeS 2019 to S3:
process_clean_save(
  df = shes_2019_raw,
  nation = "Scotland",
  population_group = "Adult",
  file_name = "shes_2019_16_bp"
)


