

# Setup and function:
rm(list = ls())
library(tidyverse)
library(here)
library(writexl)
library(readxl)
library(aws.s3)
library(arrow)

source(file = "requirements.R")
source(file = "analysis/policy_analysis/mrt_child/mrt_child_utils.R")


# Datasets:

# Reading in the required datasets:

# 2022 datasets commented out for now, uncomment, if needed
hse_2022_child = s3read_using(FUN = arrow::read_parquet,
                              bucket = "ahl-private-data",
                              object = "hse/processed/hse_2022_child.parquet")

shes_2022_child = s3read_using(FUN = arrow::read_parquet,
                               bucket = "ahl-private-data",
                               object = "shes/processed/shes_2022_child.parquet")

# effect weighting updated - anchored on 16 year old instead of 18 year olds
effect_weighting_16 = s3read_using(FUN = read.csv,
                                    bucket = "ahl-private-data",
                                    object = "hse/ref_data/effect_weighting_updated_16.csv")

# effect weighting using new approach:
new_effect_weighting = s3read_using(FUN = read.csv,
                                    bucket = "ahl-private-data",
                                    object = "hse/ref_data/effect_weighting_copy.csv")

# effect weighting using old approach:
old_effect_weighting = s3read_using(FUN = read.csv,
                                    bucket = "ahl-private-data",
                                    object = "hse/ref_data/effect_weighting.csv")

# 2019 HSE and SHeS datasets:
hse_2019_child = s3read_using(FUN = read.csv,
                              bucket = "ahl-private-data",
                              object = "hse/processed/hse_2019_children_updated.csv")

shes_2019_child = s3read_using(FUN = read.csv,
                               bucket = "ahl-private-data",
                               object = "shes/processed/shes_2019_children_updated.csv")
