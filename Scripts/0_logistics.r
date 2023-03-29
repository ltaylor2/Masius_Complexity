# Packages
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(acss))
suppressPackageStartupMessages(library(brotli))
suppressPackageStartupMessages(library(stringdist))
suppressPackageStartupMessages(library(patchwork))

# SET ME
# WD <- "C:/Users/ltayl/Documents/Masius_Complexity"
WD <- "C://Users/ltayl/Documents/Masius_Complexity"
RAW_DATA_PATH <- "Data/data_raw_01_19_2019.csv"
CLEAN_DATA_PATH <- "Data/data_clean.csv"
ANALYZED_DATA_PATH <- "Data/data_analyzed.csv"
SUMMARY_TEXT_OUTPUT_PATH <- "Output/REPORT.txt"

setwd(WD)