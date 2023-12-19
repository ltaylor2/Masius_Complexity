# Female audiences shape male courtship displays in a lek-mating bird
## Data and code repository
## Last updated 2023-12-19 (preprint submission version)

### ABSTRACT
Animal behavior research usually assumes that the characteristics of courtship displays, such as quality or complexity, are intrinsic traits of the organism performing the display. Here we provide evidence that the constituent behavioral elements (repertoire) and arrangement of elements (syntax) during courtship displays are shaped more by female audience context than male performer identity in lek-mating Golden-winged Manakins (*Masius chrysopterus*). We used video observation to analyze 422 male courtship displays in three audience contexts: Solo displays with no audience (SOLO, n = 307), unsuccessful displays for a female audience (AUDI, n = 108), and rare, successful displays for females ending in copulation (COP, n = 13). Using entropy and a novel metric we call compressibility, we find that the arrangement of behavioral elements, or syntax, is most complex (i.e., most unpredictable and varied) in SOLO displays, less complex in AUDI displays, and least complex in COP displays. Using Jaro string distance, a method from record-linkage theory, we directly quantify similarities in the syntax of display elements. Displays vary more by audience context than by individual male performer: COP displays were most similar to other displays in the COP context, regardless of male identity. Finally, we show that males perform different behaviors based on the location and reaction of their female audience. We found no evidence that males are intrinsically constrained or consistent producing a successful display repertoire or syntax. Our results suggest the courtship displays of Golden-winged Manakins are dynamic interactions between females and males, not controlled demonstrations of individual male characteristics. Studies of sexual selection and mate choice should distinguish intrinsic features of organisms (e.g., plumage ornaments) from interactive features of multiple individuals responding to one another at the moment of courtship (e.g., dances). 

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