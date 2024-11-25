# Packages
library(tidyverse)
library(lubridate)
library(acss)
library(brotli)
library(stringdist)
library(patchwork)

# SET ME
WD <- "."

RAW_DATA_PATH <- "Data/data_raw_2023-12-17.csv"
CLEAN_DATA_PATH <- "Data/data_clean.csv"
ANALYZED_DATA_PATH <- "Data/data_analyzed.csv"
AFTERCOP_DATA_PATH <- "Data/data_afterCop.csv"
SUMMARY_TEXT_OUTPUT_PATH <- "Output/REPORT.txt"

RANDOMIZATION_REPLICATES <- 100000

# Do you want to run random resampling tests for display characteristics and jaro distances?
RUN_RANDOM <- TRUE

# SET UP
setwd(WD)

# Source behavior element dictionaries 
# See Data/dictionary_behaviors.r for details
source("Data/dictionary_behaviors.r")

# Set random seed to replicate randomization results
set.seed(1973)

