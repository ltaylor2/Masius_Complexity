# Packages
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(acss))
library(brotli)
library(patchwork)

# SET ME
WD <- "/mnt/c/Users/ltayl/Documents/Masius_Complexity"
RAW_DATA_PATH <- "Data/CoreColsMFC8Feb20.csv"
CLEAN_DATA_PATH <- "Data/data_clean.csv"
ANALYZED_DATA_PATH <- "Data/data_analyzed.csv"
SUMMARY_TEXT_OUTPUT_PATH <- "Output/display_summary_values.txt"

setwd(WD)