# Female audiences shape male courtship displays in a lek-mating bird
## Data and code repository
## Last updated 2023-12-19

### About this repository
This repository contains all data and code necessary to generate manuscript results and figures. Execute the run.r script in the main directory.

Data (in the Data/ directory):
1. data_raw_2023-12-17.csv (raw behavioral data)
2. data_banding.csv (banding identification data)
3. dictionary_behaviors.r (behavioral element dictionaries)

Analysis scripts (in the Scripts/ directory):
1. 0_logistics.r (set runtime variables, file paths, load packages)
2. 1_parse_data.r (prepare dataset from indiivdual behavioral element records)
3. 2_analyze.r (calculate display characteristics and write analysis report file)
4. 3_plot.r (generate all figures and tables)

### Software requirements
Scripts are written for R v4.3.1. See scripts and manuscript for packages and software citations.

Required R packages can be installed in R with:
```R
install.packages(c("tidyverse",
                   "lubridate",
                   "acss",
                   "brotli",
                   "stringdist",
                   "patchwork"))
```               
