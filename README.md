# Female audience shapes the complexity and syntax of male courtship displays in a lek-mating bird

## Data and code repository

### Study summary

This repository contains all data and code necessary to generate results and figures for the manuscript, "Female audience shapes the complexity and syntax of male courtship displays in a lek-mating bird." This study analyzes the repertoire and syntax of courtship displays in a tropical lekking bird, the Golden-winged Manakin (*Masius chrysopterus*). By representing displays as strings of individual behavioral elements, we quantify the complexity of these displays and directly compare the syntax of displays from the same males, or different males, across three display contexts: SOLO (male-only displays), AUDI (displays for a female audience), and COP (displays for a female audience that end in copulation).

### About this repository

Place the data in a Data/ directory. Place the four numbered .r scripts in a Scripts/ directory. Execute run.r in the main directory.

**Data (in the Data/ directory):**

data_raw_2024-12-05.csv (Each row is an individual behavioral element, a component of an overall display, output from BORIS [https://www.boris.unito.it/](https://www.boris.unito.it/))

| Column        | Datatype | Description                                                                                                        |
| :------------ | :------- | :----------------------------------------------------------------------------------------------------------------- |
| UID           | (chr)    | Unique ID for the display                                                                                          |
| Log           | (chr)    | The display log (territory) where the display took place                                                           |
| Observer      | (chr)    | Initials of the observer who transcribed display elements                                                          |
| Time          | (dbl)    | Video timestamp of the behavioral element (note decimal precision constrained by BORIS software)                   |
| Male1ID       | (dbl)    | Identification for primary displaying male bird                                                                    |
| FemID         | (dbl)    | Identification for primary female audience (if present)                                                            |
| Bird2ID       | (dbl)    | Identification for an additional bird, male or female (if present)                                                 |
| FemOnOff      | (chr)    | Tag for female audience ON (="Fem On Log") or OFF (="Fem Off Log") log during display element                      |
| FemUpDown     | (chr)    | Tag for female UP (="FemUp") or down (="FemDown") on the log, if the female is ON the log during a display element |
| Behavior      | (chr)    | Category of behavioral element                                                                                     |
| MaleOtherBeh1 | (chr)    | Additional specification for "Other" behaviors                                                                     |

data_banding.csv (Each row is the band identification information for an individual bird)

| Column      | Datatype | Description                                                                                             |
| :---------- | :------- | :------------------------------------------------------------------------------------------------------ |
| Bands       | (chr)    | Field-readable color band combination                                                                   |
| Alum#       | (chr)    | Suffix to unique aluminum band number                                                                   |
| Date Banded | (chr)    | The date the bird was banded (dd-mm-yy)                                                                 |
| Sex         | (chr)    | Observed sex of the bird when banded, based on plumage, male (="M"), female (="F"), or unknown (="Unk") |
| Age         | (chr)    | Observed age of the bird when banded, based on plumage, green (="G") or definitive (="Def")             |

dictionary_behaviors.r (R dictionaries for behavioral element abbreviations and filtering)

| Vector                   | Description                                                                     |
| :----------------------- | :------------------------------------------------------------------------------ |
| behaviors\_cut\_partial  | elements to cut because they represent just part of a behavioral element        |
| behaviors\_cut\_movement | elements to cut because they represent movement tracking data only              |
| behaviors\_cut\_tracking | elements to cut because they are used for meta tracking only                    |
| behaviors\_cut\_other    | elements to cut because they do not directly constitute dance display behaviors |
| behavior\_short          | abbreviated (3-4 chars) names for BORIS display elements                        |
| behavior\_code           | single-character codes for display elements                                     |

**Analysis scripts (in the Scripts/ directory):**

0_logistics.r (set runtime variables, file paths, load packages)

1_parse_data.r (prepare dataset from individual behavioral element records)

2_analyze.r (calculate display characteristics and write analysis report file)

3_plot.r (generate all figures and tables)

### Software requirements

Scripts are written for R. See scripts and manuscript for packages and software citations.

| Software   | Version |
| ---------- | ------- |
| R          | 4.4.1   |
| tidyverse  | 2.0.0   |
| lubridate  | 1.9.3   |
| acss       | 0.2.5   |
| brotli     | 1.3.0   |
| stringdist | 0.9.12  |
| patchwork  | 1.2.0   |

### Motion Detection Software

Masius_Movement.zip contains the files for the video motion-detection program used to flag clips featuring bird displays from the full, day-long video recordings taken in the field. The unzipped folder contains its own README with execution instructions and file descriptions.

The motion detection program program is built to run in Python 2.7.12 using OpenCV 3.3.0-dev. Motion segments are flagged from the full video file. The user can then manually confirm the clips, downloading them for subsequent behavioral coding with BORIS software. The annotated, post-BORIS data is already provided in this repository -- the motion detection software is provided for reference, and is not needed to replicate the results of the manuscript.

These files are drawn from GitHub ([https://github.com/ltaylor2/Masius_Movement](https://github.com/ltaylor2/Masius_Movement)).

### Example footage

Display_Footage_Examples.zip contains a sample of 12 displays from our dataset. There are 6 displays for a female audience  (AUDI), and 6 displays for a female audience that end in a copulation (COP).

Filenames are given as "Display_<AUDI/COP>_<UID>.mp4", where UID corresponds to the unique display identifier in our dataset.

All footage in .mp4 video format.