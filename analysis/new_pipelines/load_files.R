# Setup and function:
library(tidyverse)
library(here)
library(writexl)
library(readxl)
library(aws.s3)
library(arrow)

# source(file = "requirements.R")

# Datasets:

# Reading in the required datasets:


# effect weighting using new approach:
new_effect_weighting = s3read_using(FUN = read.csv,
                                    bucket = "ahl-private-data",
                                    object = "hse/ref_data/effect_weighting_copy.csv")

# effect weighting using old approach:
old_effect_weighting = s3read_using(FUN = read.csv,
                                    bucket = "ahl-private-data",
                                    object = "hse/ref_data/effect_weighting.csv")

# 2019 HSE and SHeS datasets:
bp_hse_2019_child = s3read_using(FUN = read.csv,
                              bucket = "ahl-private-data",
                              object = "hse/processed/blueprint/hse_2019_children.csv")

bp_shes_2019_child = s3read_using(FUN = read.csv,
                               bucket = "ahl-private-data",
                               object = "hse/processed/blueprint/shes_2019_children.csv")

bp_hse_2019_adult = s3read_using(FUN = read.csv,
                              bucket = "ahl-private-data",
                              object = "hse/processed/hse_2019_16.csv")

bp_shes_2019_adult = s3read_using(FUN = read.csv,
                               bucket = "ahl-private-data",
                               object = "shes/processed/shes_2019_16_bp.csv")
