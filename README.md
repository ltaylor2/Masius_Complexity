# Female audience shapes the complexity and syntax of male courtship displays in a lek-mating bird
## Data and code repository
## Last updated 2024-11-25

### Study summary
This repository contains all data and code necessary to generate results and figures for the manuscript, "Female audience shapes the complexity and syntax of male courtship displays in a lek-mating bird." This study analyzes the repetoires and syntax of courtship displays in a tropical lekking bird, the Golden-winged Manakin (*Masius chrysopterus*). By representing displays as strings of individual behavioral elements, we quantify the complexity these displays and directly compare the syntax of displays from similar, or different, males across three display contexts: SOLO (male-only display), AUDI (display for a female audience), and COP (display for a female audience that ends in copulation).

### About this repository
Execute the run.r script in the main directory.

Data (in the Data/ directory):
1. data_raw_2023-12-17.csv 
    (Each row is an individual behavioral element, a component of an overall display, output from BORIS https://www.boris.unito.it/)
    UID           (chr) - Unique ID for the display.
    ObsDate       (chr) - Date of the display (mm/dd/yyyy)
    Log           (chr) - The display log (territory) where the display took place.
    Observer      (chr) - Initials of the observer who transcribed display elements.
    Time          (dbl) - The video timestamp of the behavioral element (note decimal precision is constrained by BORIS software.) 
    Male1ID       (dbl) - Identification for the primary displaying male bird
    FemID         (dbl) - Identification for the primary female audience to the display (if present)
    Bird2ID       (dbl) - Identification for an addition bird, male or female (if present)
    FemOnOff      (chr) - Tag for female audience ON (="Fem On Log") or OFF (="Fem Off Log") log during display element
    FemUpDown     (chr) - Tag for female UP (="FemUp") or down (="FemDown") on the log, if the female is ON the log during a display 
    Behavior      (chr) - Behavioral element category 
    MaleOtherBeh1 (chr) - Additional specification for "Other" behaviors

2. data_banding.csv
    (Each row is the band identification information for an individual bird)
    Bands       (chr) - Field-readable color band combination
    Alum#       (chr) - Suffix to unique aluminum band number
    Date Banded (chr) - The date the bird was banded (dd-mm-yy)
    Sex         (chr) - Observed sex of the bird when banded, based on plumage, male (="M"), female (="F"), or unknown (="Unk")
    Age         (chr) - Observed age of the bird when banded, based on plumage, green (="G") or definitive (="Def")

3. dictionary_behaviors.r
    (R dictionaries for behavioral element abbreviations and filtering)
    behaviors_cut_partial - elements to cut becaue they represent just part of a behavioral element
    behaviors_cut_movement - elements to cut because they represent movement tracking data only
    behaviors_cut_tracking - elements to cut because they are used for meta tracking only
    behaviors_cut_other - elements to cut because they do not directly constitute dance display behaviors
    behavior_short - abbreviated (3-4 chars) names for BORIS display elements
    behavior_code - single-character codes for display elements 

Analysis scripts (in the Scripts/ directory):
1. 0_logistics.r (set runtime variables, file paths, load packages)
2. 1_parse_data.r (prepare dataset from individual behavioral element records)
3. 2_analyze.r (calculate display characteristics and write analysis report file)
4. 3_plot.r (generate all figures and tables)

### Software requirements
Scripts are written for R. See scripts and manuscript for packages and software citations.

R v4.4.1
tidyverse 2.0.0
lubridate 1.9.3
acss 0.2.5
brotli 1.3.0
stringdist 0.9.12
patchwork 1.2.0

### Motion Detection Software
The folder Masius_Movement.zip contains the full repository directory for the video motion-detection program used to flag clips featuring bird displays from the full, day-long video recordings. Motion segments are flagged from the full video file. The uesr can then manually confirm the clips, downloading them for subsequent behavioral coding with BORISsoftware. 

The program is built to run in Python 2.7.12 using OpenCV 3.3.0-dev.

This ZIP folder contains its own README with execution instructions and file descriptions.

These zipped files are drawn from GitHub (https://github.com/ltaylor2/Masius_Movement). They are provided here to associate versioned files with the DOI for this project.