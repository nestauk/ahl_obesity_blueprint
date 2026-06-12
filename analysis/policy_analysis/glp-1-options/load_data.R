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

# 2019 HSE and SHeS datasets:
bp_hse_2019_adult = s3read_using(FUN = read.csv,
                                 bucket = "ahl-private-data",
                                 object = "hse/processed/hse_2019_16.csv")
