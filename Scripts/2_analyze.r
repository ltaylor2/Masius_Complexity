# Read data (cleaned up in Scripts/1_parse_data.r)
data_clean <- read_csv(CLEAN_DATA_PATH, show_col_types=FALSE)

# Function to count unique characters in a string  
countUniqueChars <- function(s) {
	return(length(unique(strsplit(s, "")[[1]])))
}
 
# Custom function to Brotli compress
compress <- function(raw) {
    maxWindow <- 22
    if (length(raw) < maxWindow) { maxWindow <- length(raw) }
    compressed <- brotli::brotli_compress(raw, quality=11, window=maxWindow)
    return(compressed)
}

# NOTE takes a few minutes because of call to acss::local_complexity()
# [Mutate] DisplayType into ordered factor
# [Mutate] DisplayCode_Raw format for compression
# [Mutate] DisplayCode_Compressed from brotli compression
#          NOTE default brotli::brotli_compress values of 
#               quality=11 (max) and window=22
# [Mutate] Calculate analysis values for each display
#           Duration (s) - already calculated
#           DisplayLength (#elements) - number of elements in display string
#           UniqueDisplayElements (#elements) - number of unique display elements
#           Entropy_Unscaled (numeric) - Second-order entropy from acss
#           Entropy_Scaled (numeric, 0-1) - Scaled entropy value by max entropy, where max is log2(UniqueElements) 
#           Compression_Length (numeric) - Length of raw compressed display string
#           Compression_Ratio (numeric, 0-1) - Relative length of raw compressed:uncompressed display string
# [Mutate] If unscaled entropy is 0, scaled entropy is 0
data_analyzed <- data_clean |>
              mutate(DisplayCode_Raw = map(DisplayCode, charToRaw)) |>
              mutate(DisplayCode_Compressed = map(DisplayCode_Raw, brotli::brotli_compress)) |>
			  mutate(DisplayLength         = nchar(DisplayCode),
			  		 UniqueDisplayElements = map_dbl(DisplayCode, countUniqueChars),
			  		 Entropy_Unscaled      = acss::entropy(DisplayCode),
                     Entropy_Scaled        = Entropy_Unscaled / log(UniqueDisplayElements, base=2),
                     Compression_Length    = map_dbl(DisplayCode_Compressed, length),
                     Compression_Ratio     = map2_dbl(DisplayCode_Raw, DisplayCode_Compressed, ~ length(.x) / length(.y))) |>
              mutate(Entropy_Scaled = ifelse(Entropy_Unscaled==0, 0, Entropy_Scaled))

# Write analyzed data file
write_csv(data_analyzed, ANALYZED_DATA_PATH)

# # Custom function to write a summary block
# # for output text files
writeSummaryBlock <- function(df, header, append=TRUE) {
	sink(SUMMARY_TEXT_OUTPUT_PATH, append=append)
	cat("------------------------------\n\n")
	cat(paste0("**",  header, "**\n\n"))
	print(df, n=nrow(df))
	cat("\n\n------------------------------\n\n")
	sink()
}


# Tally display categories 
data_analyzed |>
    group_by(Category) |>
    tally() |>
    writeSummaryBlock("Categories Tally", append=FALSE)

# Tally the number of logs
data_analyzed |>
    group_by(Log) |>
    tally() |>
    writeSummaryBlock("Logs Tally")

# Identifiable males
data_analyzed |>
    group_by(Male1ID < 8000) |>
    tally() |>
    writeSummaryBlock("Identifiable Male Tally")

# Male IDs
data_analyzed |>
    group_by(Male1ID) |>
    tally() |>
    writeSummaryBlock("Male ID Tally")

# Date tallies
data_analyzed |>
    mutate(ObsDate = format(ObsDate, format="%m-%d")) |>
    group_by(Category, ObsDate) |>
    tally() |>
    pivot_wider(names_from=Category, values_from=n) |>
    select(ObsDate, SOLO, AUDI, COP) |>
    arrange(ObsDate) |>
    writeSummaryBlock("Date Tally")

# Month tally
data_analyzed |>
    group_by(month(ObsDate)) |>
    tally() |>
    writeSummaryBlock("Month Tally")

# Category tally
data_analyzed |>
    group_by(Category) |>
    tally() |>
    writeSummaryBlock("Category Tally")

# Category tally
data_analyzed |>
    group_by(Category) |>
    tally() |>
    writeSummaryBlock("Category Tally")

# Female Identified Tally
data_analyzed |>
    group_by(FemID < 8000, Category) |>
    tally() |>
    writeSummaryBlock("Female Identified Tally")

# Female AUDI Attendance Tally
data_analyzed |>
    filter(Category == "AUDI") |>
    group_by(FemID) |>
    tally() |>
    writeSummaryBlock("Female AUDI Attendance Tally")

# Female COP Attendance Tally
data_analyzed |>
    filter(Category == "COP") |>
    group_by(FemID) |>
    tally() |>
    writeSummaryBlock("Female COP Attendance Tally")

# writeSummaryBlock(summary_displayLength,
# 				  "DISPLAY LENGTH summary",
# 				  append=TRUE)
# model_displayLength <- aov(DisplayLength ~ DisplayType, data=data_analyzed)
# tukey_displayLength <- TukeyHSD(model_displayLength)
# sink(SUMMARY_TEXT_OUTPUT_PATH, append=TRUE)
# print(summary(model_displayLength))
# print(tukey_displayLength)
# sink()

# # # Summarize unique display elements
# # # 	(appends to summary output file)
# summary_displayElements <- data_analyzed |>
# 			            group_by(DisplayType) |>
# 			            summarize(Mean = mean(UniqueDisplayElements),
# 			            		  Min = min(UniqueDisplayElements),
# 			            		  Max = max(UniqueDisplayElements),
# 			            		  SD = sd(UniqueDisplayElements)) |>
# 			            arrange(DisplayType)
        
# writeSummaryBlock(summary_displayElements,
# 				  "DISPLAY UNIQUE ELEMENTS summary",
# 				  append=TRUE)
# model_displayElements <- aov(UniqueDisplayElements ~ DisplayType, data=data_analyzed)
# tukey_displayElements <- TukeyHSD(model_displayElements)
# sink(SUMMARY_TEXT_OUTPUT_PATH, append=TRUE)
# print(summary(model_displayElements))
# print(tukey_displayElements)
# sink()

# # Summarize unique element ratio (Unique Elements / Display Length)
# # 	(appends to summary output file)
# summary_elementRatio <- data_analyzed |>
# 			         group_by(DisplayType) |>
# 			         summarize(Mean = mean(UniqueElementRatio),
# 			         		   Min = min(UniqueElementRatio),
# 			         		   Max = max(UniqueElementRatio),
# 			         		   SD = sd(UniqueElementRatio)) |>
# 			         arrange(DisplayType)

# writeSummaryBlock(summary_elementRatio,
# 				  "DISPLAY UNIQUE ELEMENTS-TO-LENGTH RATIO summary",
# 				  append=TRUE)
# model_elementRatio <- aov(UniqueElementRatio ~ DisplayType, data=data_analyzed)
# tukey_elementRatio <- TukeyHSD(model_elementRatio)
# sink(SUMMARY_TEXT_OUTPUT_PATH, append=TRUE)
# print(summary(model_elementRatio))
# print(tukey_elementRatio)
# sink()

# # Summarize entropy
# # 	(appends to summary output file)
# summary_entropy <- data_analyzed |>
# 			    group_by(DisplayType) |>
# 			    summarize(Mean = mean(Entropy),
# 			    		  Min = min(Entropy),
# 			    		  Max = max(Entropy),
# 			    		  SD = sd(Entropy)) |>
# 			    arrange(DisplayType)

# writeSummaryBlock(summary_entropy,
# 				  "DISPLAY ENTROPY summary",
# 				  append=TRUE)
# model_entropy <- aov(Entropy ~ DisplayType, data=data_analyzed)
# tukey_entropy <- TukeyHSD(model_entropy)
# sink(SUMMARY_TEXT_OUTPUT_PATH, append=TRUE)
# print(summary(model_entropy))
# print(tukey_entropy)
# sink()

# # Summarize compression ratio
# # 	(appends to summary output file)
# summary_compressionRatio <- data_analyzed |>
# 			    		 group_by(DisplayType) |>
# 			    		 summarize(Mean = mean(CompressionRatio),
# 			    		 		   Min = min(CompressionRatio),
# 			    		 		   Max = max(CompressionRatio),
# 			    		 		   SD = sd(CompressionRatio)) |>
# 			    		 arrange(DisplayType)

# writeSummaryBlock(summary_compressionRatio,
# 				  "DISPLAY COMPRESSION RATIO summary",
# 				  append=TRUE)
# model_compressionRatio <- aov(CompressionRatio ~ DisplayType, data=data_analyzed)
# tukey_compressionRatio <- TukeyHSD(model_compressionRatio)
# sink(SUMMARY_TEXT_OUTPUT_PATH, append=TRUE)
# print(summary(model_compressionRatio))
# print(tukey_compressionRatio)
# sink()

# # Summarize mean local complexity
# # 	(appends to summary output file)
# summary_localComplexity <- data_analyzed |>
# 			    		group_by(DisplayType) |>
# 			    		summarize(Mean = mean(LocalComplexity),
# 			    				  Min = min(LocalComplexity),
# 			    				  Max = max(LocalComplexity),
# 			    				  SD = sd(LocalComplexity)) |>
# 			    		arrange(DisplayType)

# writeSummaryBlock(summary_localComplexity,
# 				  "DISPLAY LOCAL COMPLEXITY summary (mean across rolling window)",
# 				  append=TRUE)
# model_localComplexity <- aov(LocalComplexity ~ DisplayType, data=data_analyzed)
# tukey_localComplexity <- TukeyHSD(model_localComplexity)
# sink(SUMMARY_TEXT_OUTPUT_PATH, append=TRUE)
# print(summary(model_localComplexity))
# print(tukey_localComplexity)
# sink()

# # Jaro string distances
# #
# uids <- data_clean |>
#      filter(Male1ID != 8000) |>
#      pull(UID)

# distMat <- matrix(data=NA, nrow=length(uids), ncol=length(uids),
#                   dimnames=list(uids, uids))

# for (r in 1:length(uids)) {
#     rID <- uids[r]
#     rD <- data_clean[data_clean$UID==rID,"DisplayCode"][[1]]
#     for (c in 1:length(uids)) {
#         cID <- uids[c]
#         cD <- data_clean[data_clean$UID==cID,"DisplayCode"][[1]]
#         dist <- stringdist(rD, cD, method="jw")
#         distMat[r,c] <- dist
#     }
# }

# getDistanceType <- function(d1_m, d1_type, d2_m, d2_type) {
#      distanceType <- "Diff Male / Diff Type"
#      if (d1_m == d2_m) {
#         if (d1_type == d2_type) {
#             distanceType <- "Same Male / Same Type"
#         } else {
#             distanceType <- "Same Male / Diff Type"
#         }
#      } else if (d1_type == d2_type) {
#         distanceType <- "Diff Male / Same Type"
#      }
#      return(distanceType)
# }

# distances <- distMat |>
#           as.data.frame() %>%
#           mutate(D1_UID=row.names(.)) |>
#           pivot_longer(cols=-D1_UID, names_to="D2_UID", values_to="Distance") |>
#           mutate(D1_Male1ID = map_chr(D1_UID, ~ data_clean[data_clean$UID==., "Male1ID"][[1]]),
#                  D1_DisplayType = map_chr(D1_UID, ~ data_clean[data_clean$UID==., "DisplayType"][[1]]),
#                  D2_Male1ID = map_chr(D2_UID, ~ data_clean[data_clean$UID==., "Male1ID"][[1]]),
#                  D2_DisplayType = map_chr(D2_UID, ~ data_clean[data_clean$UID==., "DisplayType"][[1]])) |>
#           mutate(DistanceType = pmap_chr(list(D1_Male1ID, D1_DisplayType, D2_Male1ID, D2_DisplayType), getDistanceType)) |>
#           filter(D1_UID != D2_UID)

# ggplot(distances) +
#     geom_boxplot(aes(x=DistanceType, y=Distance)) +
#     theme_bw() +
#     ylab("Jaro distance") +
#     theme(panel.grid=element_blank(),
#           axis.title.x=element_blank()) +
#     facet_grid(rows=vars(D1_DisplayType), 
#                cols=vars(D2_DisplayType))
