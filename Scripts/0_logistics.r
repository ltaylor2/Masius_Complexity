# Packages
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(acss))
suppressPackageStartupMessages(library(brotli))
suppressPackageStartupMessages(library(stringdist))
suppressPackageStartupMessages(library(patchwork))

# SET ME

WD <- "C:/Users/Liam/Documents/Masius_Complexity"
# WD <- "C:/Users/ltayl/Documents/Masius_Complexity"
# WD <- "/Users/liamtaylor/Documents/Masius_Complexity"

RAW_DATA_PATH <- "Data/data_raw_2023-10-25.csv"
CLEAN_DATA_PATH <- "Data/data_clean.csv"
ANALYZED_DATA_PATH <- "Data/data_analyzed.csv"
AFTERCOP_DATA_PATH <- "Data/data_afterCop.csv"
SUMMARY_TEXT_OUTPUT_PATH <- "Output/REPORT.txt"

RANDOMIZATION_REPLICATES <- 100000
RUN_RANDOM <- FALSE

# SET UP
setwd(WD)

# Source behavior element dictionaries 
# See Data/dictionary_behaviors.r for details
source("Data/dictionary_behaviors.r")

# Set random seed to Tinbergen's birthday
#   To replicate randomizationr results
set.seed(1973)
