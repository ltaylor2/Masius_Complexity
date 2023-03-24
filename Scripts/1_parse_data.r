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

# # A function to specify "Other" behaviors using dataset notes
# specifyOtherBehaviors <- function(behavior, details) {
#     # If it's not an "Other" behavior, 
#     #   return the original behavior
#     if (!(grepl("Other Behavior", behavior))) {
#         return(behavior)
#     }

#     # Otherwise, return BirdID Other Behavior <details>
#     #   (if details are specified)
#     if (is.na(details)) {
#         return(paste(behavior, "Unspecified"))
#     }
#     return(paste(behavior, details))
# }

# # Source behavior element dictionaries 
# # See Data/dictionary_behaviors.r for details
# source("Data/dictionary_behaviors.r")

# # Organize raw data
# # [Mutate] create col Duration (max element time) before cutting any elements 
# # [Filter] out partial behaviors
# # [Filter] out movement-only behaviors not used in analyses
# # [Filter] out tracking-type elements (Start, End, Female On, Female Off)
# # [Mutate] specify "other" behaviors using dataset notes where applicable 
# # [Filter] out "Other"-type behaviors when known or specified not to constitute direct dance display elements
# #          NOTE these include vocalizations, gardening, and wing-flashing
# # [Mutate] replace original behavioral descriptions with abbreviations 
# # [Mutate] create single-column abbreviate col 
# data_clean_long <- data_raw |>
#                 group_by(UID) |> mutate(Duration = max(Time)) |> ungroup() |>
#                 filter(!(Behavior %in% behaviors_cut_partial)) |>
#                 filter(!(Behavior %in% behaviors_cut_movement)) |>
#                 filter(!(Behavior %in% behaviors_cut_tracking)) |>
#                 mutate(Behavior = map2_chr(Behavior, MaleOtherBeh1, specifyOtherBehaviors)) |>
#                 filter(!(Behavior %in% behaviors_cut_other)) |>
#                 mutate(Behavior = behaviorCodes_original[Behavior]) |>
#                 mutate(BehaviorCode = behaviorCodes_short[Behavior])

# # Produce clean dataset with display element strings
# # These steps to include only male aesthetic displays
# #	in terms of the display
# # 1. FILTER out:
# #	 "Start", "End", "OthF", "FOn", and "Fff" elements
# # 2. MUTATE new bool column with coded display elements
# # 3. SUMMARIZE displays by collapsing elements to strings
# # 4. MUTATE new bool column for Multiple males present (i.e., has B2 display elements)
# # 5. MUTATE new bool column for Female present (i.e., FemID not NA)
# # 6. MUTATE new chr column for coding of display types:
# #		SOL bouts have one Male, no female present
# #		MUL bouts have multiple Males, no female present
# #		AUD bouts have female present, but no Cop 
# #		COP bouts have female present, and Cop
# # 7. MUTATE DisplayType column from Character -> Factors

# # Which elements are excluded?
# excludedElements <- c("Start", "End", "FOn", "Fff", "AttC")

# # Custom function to end uncoded behavior strings at cop
# # Returns original string if no Cop element present
# cutAtCop <- function(s, coded=FALSE) {
# 	# If the behaviors are coded,
# 	#	we're cutting at behaviorCods["Cop"] (should be "U")
# 	# If the behaviors are uncoded, 
# 	#	we're cutting at "Cop"
# 	if (coded) {
# 		copElement <- behaviorCodes["Cop"]
# 		ret <- strsplit(s, copElement)[[1]][1]
# 	} else {
# 		copElement <- ";Cop"
# 		ret <- strsplit(s, copElement)[[1]][1]
# 	}
# 	return(ret)
# }

# # Custom function to classify displays
# #		SOL bouts have one Male, no female present
# #		MUL bouts have multiple Males, no female present
# #		AUD bouts have female present, but no Cop 
# #		COP bouts have female present, and Cop
# getDisplayType <- function(hasMultipleMales, hasFemale, hasCopulation) {
# 	if (!hasMultipleMales & !hasFemale & !hasCopulation) {
# 		return("SOL")
# 	} else if (hasMultipleMales & !hasFemale & !hasCopulation) {
# 		return("MUL")
# 	} else if (!hasMultipleMales & hasFemale & !hasCopulation) {
# 		return("AUD")
# 	} else if (!hasMultipleMales & hasFemale & hasCopulation) {
# 		return("COP")
# 	}

# 	# TODO
# 	# all other combinations might erroneous -- code them as "CHECK" 
# 	return("CHECK")
# } 


# # ERROR CHECK does your dictionary contain codes for all behaviors?
# missingCodes <- data_raw |>
# 			 filter(!(Behavior %in% names(behaviorCodes)))
# if (nrow(missingCodes) > 0) {
# 	cat("\n\nERROR in Scripts/1_parse_data.r:\nYour behaviorCode dictionary does not include all behaviors.\n\nQUITTING\n\n")
# 	quit(save="no", status=1)
# }

# data_clean <- data_raw |>
# 		   filter(!(Behavior %in% excludedElements)) |>
# 		   mutate(BehaviorCode = map_chr(Behavior, ~ behaviorCodes[.])) |>
# 		   group_by(UID, ObsDate, Log, Male1ID, FemID, Bird2ID) |>
# 		   summarize(Duration = max(Time) - min(Time),
#                      Display = paste(Behavior, collapse=";"),
# 		   		  	 DisplayCode = paste(BehaviorCode, collapse=""),
# 		    		 .groups="keep") |>
# 		   mutate(hasMultipleMales = grepl("B2AL", Display) |
# 		   						     grepl("B2Bw", Display) |
# 		   							 grepl("B2Nk", Display)) |>
# 		   mutate(hasFemale = !is.na(FemID)) |>
# 		   mutate(hasCopulation = map_lgl(Display, ~grepl("Cop", .))) |>
# 		   mutate(Display = map_chr(Display, ~cutAtCop(., coded=FALSE)),
# 		   		  DisplayCode = map_chr(DisplayCode, ~cutAtCop(., coded=TRUE))) |>
# 		   mutate(DisplayType = pmap_chr(list(hasMultipleMales, hasFemale, hasCopulation), 
# 		   		  						 getDisplayType))
# write_csv(data_clean, "Data/data_clean.csv")
