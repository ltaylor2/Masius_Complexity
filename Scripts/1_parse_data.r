# Read data (raw)
# [Mutate] convert Date column to Datetime object
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
        mutate(ObsDate = mdy(ObsDate))

# Before filtering any elements,
#   categorize the type of each display
#  (SOLO, MULT, AUDI, or COP)

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
      mutate(Date = ymd(Date)) |>
      group_by(Band_ID) |>
      slice_max(order_by=Date, n=1, with_ties=FALSE)
      
# Custom function to categorize display types, 
#   Input: a UID for a display
#   Output: a dataframe mapping that UID to the display type
#           (SOLO, MULT, AUDI, COP)
# SOLO Displays: Performed by one male with no audience
# MULT Displays: Performance with actions from multiple birds
# AUDI Displays: Performance featuring female audience member(s) with no copulation
# COP Displays:  Performance featuring female audience member(s) with successful

categorizeDisplayType <- function(uid) {
    display <- filter(data_raw, UID==uid)

    # Quick check to confirm there is only one ID for each ID column
    maleID <- unique(display$Male1ID)
    femID <- unique(display$FemID)

    b2ID <- unique(display$Bird2ID)

    if (is.na(b2ID)) {
        b2Sex <- "NA"
    } else if (b2ID >= 8000) {
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
    hasB2Action <- any(grepl("Bird2", display$Behavior))
    hasMaleB2 <- (b2Sex == "M")

    category <- "SOLO"
    if (!is.na(femID)) { category <- "AUDI"}
    if (is.na(femID) & (hasFemOn | hasFemAction) ) { category <- "ERROR"}
    if (hasCop) { category <- "COP"}
    if (hasB2Action | (hasMaleB2)) { category <- "MULT" }
    # TEMP data table with relevant info
    ret <- tibble(UID=uid, Category=category,
                  maleID, femID, b2ID, 
                  hasFemOn, hasFemAction, 
                  hasAttCop, hasCop, 
                  hasB2Action, hasMaleB2)
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

# Source behavior element dictionaries 
# See Data/dictionary_behaviors.r for details
source("Data/dictionary_behaviors.r")

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

# Custom function to end uncoded behavior strings at cop
# Returns original string if no Cop element present
cutDisplayAtCop <- function(s, coded=FALSE) {
	# If the behaviors are coded,
	#	we're cutting at behaviorCods["Cop"] (should be "U")
	# If the behaviors are uncoded, 
	#	we're cutting at "Cop"
	if (coded) {
		copElement <- behavior_code["Cop"]
		ret <- strsplit(s, copElement)[[1]][1]
	} else {
		copElement <- ";Cop"
		ret <- strsplit(s, copElement)[[1]][1]
	}
	return(ret)
}

# Custom function to calculate display duration
#   NOTE calculated from Raw data, including all cut elements
#   NOTE cuts duration off at Cop, when relevant
displayDuration <- function(uid, coded=FALSE) {
    display <- data_raw |>
            filter(UID == uid)

    duration <- 0
    for (r in 1:nrow(display)) {
        duration <- display$Time[r]
        behavior <- display$Behavior[r]
        if (behavior == "Copulation") { return(duration) }
    }
    return(duration)
}

data_clean_wide <- data_clean_long |>
		        group_by(UID, Category, ObsDate, Log, Male1ID, FemID, Bird2ID) |>
		        summarise(DisplayShort = paste(BehaviorShort, collapse=";"), 
                          DisplayCode = paste(BehaviorCode, collapse=""), 
                          .groups="keep") |>	        
                mutate(DisplayShort = map_chr(DisplayShort, ~cutDisplayAtCop(., coded=FALSE)),
		   		       DisplayCode = map_chr(DisplayCode, ~cutDisplayAtCop(., coded=TRUE))) |>
                mutate(Duration = map_dbl(UID, displayDuration)) |>
                ungroup()

write_csv(data_clean_wide, "Data/data_clean.csv")
