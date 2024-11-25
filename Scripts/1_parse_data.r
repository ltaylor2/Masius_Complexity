# Read data (raw)
# [Mutate] convert Date column to Datetime object
# [Mutate] add Month column
# [Mutate] add UniqueMale1ID column, where each unidentified male (8000)
#          is given a unique ID (i.e., assumed to be different)
# [Mutate] clean FemOnOff to NA=X, On=Y, Off=N
#          clean FemUpDown to NA=X, Up=U, Down=D
# [Mutate] mark FemOnOff and FemUpDown = A for AttC or C for Cop elements
#          NOTE this is used later to prune the FemOnOff and FemUpDown strings 
#               to match the DisplayString
data_raw <- read_csv(RAW_DATA_PATH, 
                     col_types = c("UID"='c',
                                   "ObsDate"='c',
                                   "Log"='c',
                                   "Observer"='c',
                                   "Time"='n',
                                   "Male1ID"='c',
                                   "FemID"='c',
                                   "Bird2ID"='c',
                                   "FemOnOff"='c',
                                   "Behavior"='c',
                                   "MaleOtherBeh1"='c')) |>
        mutate(ObsDate = mdy(ObsDate)) |>
        mutate(ObsMonth = as.character(month(ObsDate))) |>
        mutate(UniqueMale1ID = map2_chr(Male1ID, UID, ~ifelse(.x==8000, paste(.x, .y, sep="-"), .x))) |>
        mutate(FemOnOff = ifelse(is.na(FemOnOff), "X", FemOnOff),
               FemUpDown = ifelse(is.na(FemUpDown) | FemUpDown == "Na", "X", FemUpDown)) |>
        mutate(FemOnOff = c("X"="X", "Female On Log"="Y", "Female Off Log"="N")[FemOnOff],
               FemUpDown = c("X"="X", "FemUp"="U", "FemDown"="D")[FemUpDown]) |>
        mutate(FemOnOff = ifelse(Behavior=="Attempted Copulation", "A", FemOnOff),
               FemUpDown = ifelse(Behavior=="Attempted Copulation", "A", FemUpDown)) |>
        mutate(FemOnOff = ifelse(Behavior=="Copulation", "C", FemOnOff),
               FemUpDown = ifelse(Behavior=="Copulation", "C", FemUpDown)) |>
        mutate(FemUpDown = ifelse(FemOnOff == "N" | FemOnOff == "X", "X", FemUpDown))
 
# Write T S1, with raw element frequencies
table_s1 <- data_raw |>
         group_by(Behavior) |>
         tally() |>
         arrange(Behavior)
write_csv(table_s1, "Output/TABLE_S1.csv")

# Read in banding dataset
## Also note : 8200 (unknown, suspected females), 8300 (unknown sex), 8400 (unknown, suspected pre-def males)
# [Select] only ID (=Aluminum Band #), Date (=Date Banded), Sex, Age
# [Mutate] date column into Date object for sorting
# [Group by] ID
# [Slice Max] only the latest banding row for each ID, because some birds
#             were recapped and given updated band combos
#             in which case we want the most recent sex/age information
bands <- read_csv("Data/data_banding.csv", show_col_types=FALSE) |>
      select(Band_ID = "Alum#", Date="Date Banded", Sex, Age) |>
      mutate(Date = dmy(Date)) |>
      group_by(Band_ID) |>
      slice_max(order_by=Date, n=1, with_ties=FALSE)
      
# Custom function to categorize display types, 
#   Input: a UID for a display
#   Output: a dataframe mapping that UID to the display type
#           (SOLO, MULT, AUDI, COP)
# SOLO Displays: Performed by one male with no audience
# MULT Displays: Performance with actions from multiple birds
# AUDI Displays: Performance featuring female audience member(s) with no copulation
# COP Displays:  Performance featuring female audience member(s) with successful copulation
categorizeDisplayType <- function(uid) {
    display <- filter(data_raw, UID==uid)

    # Quick check to confirm there is only one ID for each ID column
    maleID <- unique(display$Male1ID)
    femID <- unique(display$FemID)

    if (is.na(femID)) {
        femSex <- "NA"
    } else if (as.numeric(femID) >= 8000) {
        femSex <- "Unknown"
    } else {
         femSex <- bands[bands$Band_ID == femID, "Sex"][[1]]
    }

    b2ID <- unique(display$Bird2ID)

    if (is.na(b2ID)) {
        b2Sex <- "NA"
    } else if (as.numeric(b2ID) >= 8000) {
        b2Sex <- "Unknown"
    } else {
         b2Sex <- bands[bands$Band_ID == b2ID, "Sex"][[1]]
    }

    if (length(maleID) > 1 | length(femID) > 1 | length(b2ID) > 1) { 
        cat(paste("\nERROR in Scripts/1_parse_data.r, categorizeDisplayType()\n",
                  "Display UID", uid, "has multiple IDs in one of the ID columns"))
    	quit(save="no", status=1)
    }

    # Now get various data from the display to use in coding the type of display
    hasFemOn <- "Female On Log" %in% display$FemOnOff
    hasFemAction <- any(grepl("Female", display$Behavior))
    hasAttCop <- "Attempted Copulation" %in% display$Behavior
    hasCop <- "Copulation" %in% display$Behavior
    hasMaleFem <- (femSex == "M")
    hasB2Action <- any(grepl("Bird2", display$Behavior))
    hasMaleB2 <- (b2Sex == "M")

    category <- "SOLO"
    if (!is.na(femID)) { category <- "AUDI"}
    if (is.na(femID) & (hasFemOn | hasFemAction) ) { category <- "ERROR"}
    if (hasCop) { category <- "COP"}
    if (hasMaleFem | hasB2Action) { category <- "MULT" }

    # TEMP data table with relevant info
    ret <- tibble(UID=uid, Category=category,
                  maleID, femID, b2ID, 
                  hasFemOn, hasFemAction, 
                  hasAttCop, hasCop, 
                  hasMaleFem, hasB2Action, hasMaleB2)
    ret[is.na(ret)] <- ""
    return(ret)
}

display_categories <- map_df(unique(data_raw$UID), categorizeDisplayType)
write_csv(display_categories, "Output/display_categories.csv")

# A function to specify "Other" behaviors using dataset notes
specifyOtherBehaviors <- function(behavior, details) {
    # If it's not an "Other" behavior, 
    #   return the original behavior
    if (!(grepl("Other Behavior", behavior))) {
        return(behavior)
    }

    # Otherwise, return Other Behavior <details>
    #   (if details are specified)
    if (is.na(details)) {
        return(paste(behavior, "Unspecified"))
    }
    return(paste(behavior, details))
}

# Organize raw data
# [Left join] Displays to get display categories
# [Filter] out MULTI displays (should retain SOLO, AUDI, COP only)
# [Filter] out partial behaviors
# [Filter] out movement-only behaviors not used in analyses
# [Filter] out tracking-type elements (Start, End, Female On, Female Off)
# [Mutate] specify "other" behaviors using dataset notes where applicable 
# [Filter] out "Other"-type behaviors when known or specified not to constitute direct dance display elements
#          NOTE these include vocalizations, gardening, and wing-flashing
# [Mutate] replace original behavioral descriptions with abbreviations 
# [Mutate] create single-column abbreviate col 
data_clean_long <- data_raw |>
                left_join(select(display_categories, UID, Category), by="UID") |>
                filter(Category != "MULT") |>
                filter(!(Behavior %in% behaviors_cut_partial)) |>
                filter(!(Behavior %in% behaviors_cut_movement)) |>
                filter(!(Behavior %in% behaviors_cut_tracking)) |>
                mutate(Behavior = map2_chr(Behavior, MaleOtherBeh1, specifyOtherBehaviors)) |>
                filter(!(Behavior %in% behaviors_cut_other)) |>
                mutate(BehaviorShort = behavior_short[Behavior]) |>
                mutate(BehaviorCode = behavior_code[BehaviorShort])

# Confirm that all the behavioral elements are in the dictionary
missingCodes <- data_clean_long |>
			 filter(is.na(BehaviorShort) | is.na(BehaviorCode)) |>
             select(UID, Time, Male1ID, Behavior, BehaviorShort, BehaviorCode)
if (nrow(missingCodes) > 0) {
	cat("\n\nERROR in Scripts/1_parse_data.r:\nYour behaviorCode dictionary does not include all behaviors.\n\nQUITTING\n\n")
    print(missingCodes)
    quit(save="no", status=1)
}

# Custom function to calculate display duration
#   NOTE calculated from Raw data, including all cut elements
#   NOTE cuts duration off at Cop, when relevant
displayDuration <- function(uid) {
    display <- data_raw |>
            filter(UID == uid) |>
            arrange(Time)

    startTime <- display$Time[1]
    duration <- 0
    for (r in 1:nrow(display)) {
        duration <- display$Time[r] - startTime
        behavior <- display$Behavior[r] 
        if (behavior == "Copulation") { return(duration) }
    }
    return(duration)
}

# Organize the final, wide version of the dataset
#   where each row is a Display String for one display
# [Group by] UID and all other info cols 
#            NOTE should still be one UID per row, but we add the extra info
#                 to check that UID's haven't been duplicated across different displays
# [Arrange] Element by Time within each UID to put elements in chronological order 
#           NOTE should be chronological order already, but just making sure! 
# [Summarize] the DisplayShort col as the collapsed set of elements, semicolon deliminted, across the display
#             the DisplayCode col as the collapsed set of single-character element for the final string
#             the FemOnOff col as the collapsed set of FemOnOff codes for the string
#             the FemUpDown col as the collapsed set of FemUpDown codes for the string
# [Separate] After-cop DisplayShort strings for each display (will be NA if no Cop is present in the display)
# [Separate] After-cop DisplayCode strings for each display (will be NA if no Cop is present)
# [Separate] Before-cop FemOnOff col for each display only
# [Separate] Before-cop FemUpDown col for each display only
# [Mutate] Total duration (s) of display
#          NOTE duration is calculated from raw data, including untracked elements
#               but cuts off duration at Cop element, when relevant
# [Ungroup] To avoid any errors
# [Filter] out displays <60s
# [Filter] out displays that are missing ALAD or Bows
# [Mutate] make a copy that does not cut Attempted Copulation or Copulation 
#          used in before vs. after cop comparisons
# [Mutate] cut Attempted Copulation ("AttC", should be code "M") from final display strings
# [Mutate] cut Copulation ("Cop", should be code "N") from final display strings
# [Mutate] cut Attempted Copulation from FemUpDown and FemOnOff columns (code "A" here)
# [Mutate] cut Copulation from FemUpDown and FemOnOff columns (code "C" here)
data_clean_wide <- data_clean_long |>
		        group_by(UID, Category, ObsDate, ObsMonth, Log, Male1ID, UniqueMale1ID, FemID, Bird2ID) |>
                arrange(UID, Time) |>
		        summarise(DisplayShort = paste(BehaviorShort, collapse=";"), 
                          DisplayCode = paste(BehaviorCode, collapse=""), 
                          FemOnOff = paste(FemOnOff, collapse=""),
                          FemUpDown = paste(FemUpDown, collapse=""),
                          .groups="keep") |>	
                separate(DisplayShort, into=c("DisplayShort", "DisplayShort_AfterCop"), 
                         sep="(?<=;Cop)", 
                         fill="right", extra="merge") |>
                separate(DisplayCode, into=c("DisplayCode", "DisplayCode_AfterCop"), 
                         sep=paste0("(?<=", behavior_code["Cop"], ")"),
                         fill="right", extra="merge") |>
                separate(FemOnOff, into=c("FemOnOff", "FemOnOff_AfterCop"), 
                         sep="(?<=C)", 
                         fill="right", extra="merge") |>
                separate(FemUpDown, into=c("FemUpDown", "FemUpDown_AfterCop"), 
                         sep="(?<=C)", 
                         fill="right", extra="merge") |>
                mutate(Duration = map_dbl(UID, displayDuration)) |>
                ungroup() |>
                filter(Duration >= 60) |>
                filter(grepl("ALAD", DisplayShort) | grepl("Bow", DisplayShort)) |>
                mutate(DisplayCode_withCops = DisplayCode) |>
                mutate(DisplayCode = str_replace(DisplayCode, behavior_code["AttC"], "")) |>
                mutate(DisplayCode = str_replace(DisplayCode, behavior_code["Cop"], "")) |>
                mutate(FemOnOff = str_replace(FemOnOff, "A", ""),
                       FemUpDown = str_replace(FemUpDown, "A", ""),
                       FemOnOff_AfterCop = str_replace(FemOnOff_AfterCop, "A", ""),
                       FemUpDown_AfterCop = str_replace(FemUpDown_AfterCop, "A", "")) |>
                mutate(FemOnOff = str_replace(FemOnOff, "C", ""),
                       FemUpDown = str_replace(FemUpDown, "C", ""),
                       FemOnOff_AfterCop = str_replace(FemOnOff_AfterCop, "C", ""),
                       FemUpDown_AfterCop = str_replace(FemUpDown_AfterCop, "C", ""))

# Write clean datasheet for analysis
write_csv(data_clean_wide, CLEAN_DATA_PATH)