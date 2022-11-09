#Read data (raw)
# 1. MUTATE ObsDate to <date> format
data_raw <- read_csv(RAW_DATA_PATH, show_col_types=FALSE) |>
		 mutate(ObsDate = lubridate::mdy(ObsDate))

# Produce clean dataset with display element strings
# These steps to include only male aesthetic displays
#	in terms of the display
# 1. FILTER out:
#	 "Start", "End", "OthF", "FOn", and "Fff" elements
# 2. MUTATE new bool column with coded display elements
# 3. SUMMARIZE displays by collapsing elements to strings
# 4. MUTATE new bool column for Multiple males present (i.e., has B2 display elements)
# 5. MUTATE new bool column for Female present (i.e., FemID not NA)
# 6. MUTATE new chr column for coding of display types:
#		SOL bouts have one Male, no female present
#		MUL bouts have multiple Males, no female present
#		AUD bouts have female present, but no Cop 
#		COP bouts have female present, and Cop
# 7. MUTATE DisplayType column from Character -> Factors

# Which elements are excluded?
excludedElements <- c("Start", "End", "FOn", "Fff", "OthF")

# A keyed dictionary of behavioral elements
# e.g., behaviorCodes["Start"] returns "A"
behaviorCodes <- c("Start" = "A",
				   "Zro" = "B",
				   "SLAD" = "C",
				   "OthM" = "D",
				   "HdBw" = "E",
				   "ALAD" = "F",
				   "HafB" = "G",
				   "Bow" = "H",
				   "End" = "I",
				   "Mix" = "J",
				   "Swtc" = "K",
				   "Neck" = "L",
				   "B2Nk" = "M",
				   "B2Bw" = "N",
				   "B2AL" = "O",
				   "Taf" = "P",
				   "FOn" = "Q",
				   "Fff" = "R",
				   "OthF" = "S",
				   "AttC" = "T",
				   "Cop" = "U",
				   "Metr" = "V",
				   "OthC" = "W")

# Custom function to end uncoded behavior strings at cop
# Returns original string if no Cop element present
cutAtCop <- function(s, coded=FALSE) {
	# If the behaviors are coded,
	#	we're cutting at behaviorCods["Cop"] (should be "U")
	# If the behaviors are uncoded, 
	#	we're cutting at "Cop"
	if (coded) {
		copElement <- behaviorCodes["Cop"]
		ret <- strsplit(s, copElement)[[1]][1]
	} else {
		copElement <- ";Cop"
		ret <- strsplit(s, copElement)[[1]][1]
	}
	return(ret)
}

# Custom function to classify displays
#		SOL bouts have one Male, no female present
#		MUL bouts have multiple Males, no female present
#		AUD bouts have female present, but no Cop 
#		COP bouts have female present, and Cop
getDisplayType <- function(hasMultipleMales, hasFemale, hasCopulation) {
	if (!hasMultipleMales & !hasFemale & !hasCopulation) {
		return("SOL")
	} else if (hasMultipleMales & !hasFemale & !hasCopulation) {
		return("MUL")
	} else if (!hasMultipleMales & hasFemale & !hasCopulation) {
		return("AUD")
	} else if (!hasMultipleMales & hasFemale & hasCopulation) {
		return("COP")
	}

	# TODO
	# all other combinations might erroneous -- code them as "CHECK" 
	return("CHECK")
} 


# ERROR CHECK does your dictionary contain codes for all behaviors?
missingCodes <- data_raw |>
			 filter(!(Behavior %in% names(behaviorCodes)))
if (nrow(missingCodes) > 0) {
	cat("\n\nERROR in Scripts/1_parse_data.r:\nYour behaviorCode dictionary does not include all behaviors.\n\nQUITTING\n\n")
	quit(save="no", status=1)
}

data_clean <- data_raw |>
		   filter(!(Behavior %in% excludedElements)) |>
		   mutate(BehaviorCode = map_chr(Behavior, ~ behaviorCodes[.])) |>
		   group_by(UID, ObsDate, Log, Male1ID, FemID, Bird2ID) |>
		   summarize(Display = paste(Behavior, collapse=";"),
		   		  	 DisplayCode = paste(BehaviorCode, collapse=""),
		    		 .groups="keep") |>
		   mutate(hasMultipleMales = grepl("B2AL", Display) |
		   						     grepl("B2Bw", Display) |
		   							 grepl("B2Nk", Display)) |>
		   mutate(hasFemale = !is.na(FemID)) |>
		   mutate(hasCopulation = map_lgl(Display, ~grepl("Cop", .))) |>
		   mutate(Display = map_chr(Display, ~cutAtCop(., coded=FALSE)),
		   		  DisplayCode = map_chr(DisplayCode, ~cutAtCop(., coded=TRUE))) |>
		   mutate(DisplayType = pmap_chr(list(hasMultipleMales, hasFemale, hasCopulation), 
		   		  						 getDisplayType))
write_csv(data_clean, "Data/data_clean.csv")