# Set random seed to Tinbergen's birthday
#   To replicate randomizationr results
set.seed(1973)

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

# Randomization tools (for small COP sample sizes) ---------------------------------------------
n_draws <- nrow(filter(data_analyzed, Category=="COP"))
# Custom function for a single random 
randomDraw <- function(i, variable) {
    if (i==1 | (i %% 5000) == 0) { cat(paste("Random draw", i, "for variable", variable, "\n")) }

    draw <- data_analyzed |>
         slice_sample(n=n_draws, replace=FALSE) |>
         pull(variable)
    return(tibble(Variable=variable, Mean=mean(draw)))
}

# Custom function to write a summary block
# for output text files
writeSummaryBlock <- function(df, header, append=TRUE) {
	sink(SUMMARY_TEXT_OUTPUT_PATH, append=append)
	cat("------------------------------\n\n")
	cat(paste0("**",  header, "**\n\n"))
	print(df, n=nrow(df))
	cat("\n\n------------------------------\n\n")
	while(sink.number()>0) { sink() }
}

# REPORT Tallies) ---------------------------------------------

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

# REPORT Duration ---------------------------------------------
data_analyzed |>
    group_by(Category) |>
    summarize(mean(Duration), sd(Duration), min(Duration), max(Duration)) |>
    writeSummaryBlock("DURATION -- Key values")

aov(Duration ~ Category, data=data_analyzed) |>
    summary() |>
    writeSummaryBlock("DURATION -- ANOVA")

aov(Duration ~ Category, data=data_analyzed) |>
    TukeyHSD() |>
    writeSummaryBlock("DURATION -- Tukey")

# REPORT Display Length ---------------------------------------------

data_analyzed |>
    group_by(Category) |>
    summarize(mean(DisplayLength), sd(DisplayLength), min(DisplayLength), max(DisplayLength)) |>
    writeSummaryBlock("DISPLAY LENGTH -- Key values")

aov(DisplayLength ~ Category, data=data_analyzed) |>
    summary() |>
    writeSummaryBlock("DISPLAY LENGTH -- ANOVA")

aov(DisplayLength ~ Category, data=data_analyzed) |>
    TukeyHSD() |>
    writeSummaryBlock("DISPLAY LENGTH -- Tukey")

# REPORT Unique elements ---------------------------------------------
data_analyzed |>
    group_by(Category) |>
    summarize(mean(UniqueDisplayElements), sd(UniqueDisplayElements), min(UniqueDisplayElements), max(UniqueDisplayElements)) |>
    writeSummaryBlock("UNIQUE ELEMENTS -- Key values")

aov(UniqueDisplayElements ~ Category, data=data_analyzed) |>
    summary() |>
    writeSummaryBlock("UNIQUE ELEMENTS -- ANOVA")

aov(UniqueDisplayElements ~ Category, data=data_analyzed) |>
    TukeyHSD() |>
    writeSummaryBlock("UNIQUE ELEMENTS -- Tukey")

# RANDOMIZATION Unique elements ---------------------------------------------

# Generate random distribution
randomDistribution_uniqueElements <- map_df(1:RANDOMIZATION_REPLICATES, ~randomDraw(., "UniqueDisplayElements"))

# Save for plotting later
saveRDS(randomDistribution_uniqueElements, file="Output/randomDistribution_uniqueElements.rds")

# Summarize for report
cop_UniqueDisplayElements <- data_analyzed |>
                          filter(Category == "COP") |>
                          pull(UniqueDisplayElements) |>
                          mean()

randomDistribution_uniqueElements |> 
    group_by(Mean < cop_UniqueDisplayElements) |>
    tally() |>
    writeSummaryBlock("RANDOMIZATION -- Unique elements")

# REPORT Entropy, scaled ---------------------------------------------
data_analyzed |>
    group_by(Category) |>
    summarize(mean(Entropy_Scaled), sd(Entropy_Scaled), min(Entropy_Scaled), max(Entropy_Scaled)) |>
    writeSummaryBlock("Entropy (scaled) -- Key values")

aov(Entropy_Scaled ~ Category, data=data_analyzed) |>
    summary() |>
    writeSummaryBlock("Entropy (scaled) -- ANOVA")

aov(Entropy_Scaled ~ Category, data=data_analyzed) |>
    TukeyHSD() |>
    writeSummaryBlock("Entropy (scaled) -- Tukey")

# RANDOMIZATION (Entropy, scaled) ---------------------------------------------
randomDistribution_entropy <- map_df(1:RANDOMIZATION_REPLICATES, ~randomDraw(., "Entropy_Scaled"))

# Save for plotting later
saveRDS(randomDistribution_entropy, file="Output/randomDistribution_entropy.rds")

# Summarize for report
cop_Entropy <- data_analyzed |>
            filter(Category == "COP") |>
            pull(Entropy_Scaled) |>
            mean()

randomDistribution_entropy |> 
    group_by(Mean < cop_Entropy) |>
    tally() |>
    writeSummaryBlock("RANDOMIZATION -- Entropy")

# REPORT Compression ratio ---------------------------------------------
data_analyzed |>
    group_by(Category) |>
    summarize(mean(Compression_Ratio), sd(Compression_Ratio), min(Compression_Ratio), max(Compression_Ratio)) |>
    writeSummaryBlock("Compression Ratio -- Key values")

aov(Compression_Ratio ~ Category, data=data_analyzed) |>
    summary() |>
    writeSummaryBlock("Compression Ratio -- ANOVA")

aov(Compression_Ratio ~ Category, data=data_analyzed) |>
    TukeyHSD() |>
    writeSummaryBlock("Compression Ratio -- Tukey")

# RANDOMIZATION (Compression ratio) ---------------------------------------------
randomDistribution_compressionRatio <- map_df(1:RANDOMIZATION_REPLICATES, ~randomDraw(., "Compression_Ratio"))

# Save for plotting later
saveRDS(randomDistribution_compressionRatio, file="Output/randomDistribution_compressionRatio.rds")

# Summarize for report
cop_compressionRatio <- data_analyzed |>
            filter(Category == "COP") |>
            pull(Compression_Ratio) |>
            mean()

randomDistribution_compressionRatio |> 
    group_by(Mean > cop_compressionRatio) |>
    tally() |>
    writeSummaryBlock("RANDOMIZATION -- Compression Ratio")

# Entropy vs. Compressibility correlation ---------------------------------------------
lm(Compression_Ratio ~ Entropy_Scaled, data=data_analyzed) |>
    summary() |>
    writeSummaryBlock("Entropy vs. Compressibility correlation")

# Most compressible string ---------------------------------------------
data_analyzed |>
    filter(Compression_Ratio == max(Compression_Ratio)) |>
    select(UID, Category, Entropy_Scaled, Compression_Ratio, DisplayLength, DisplayCode) |>
    writeSummaryBlock("Highest Compression Ratio")

# Compression ratio comparison string ---------------------------------------------
data_analyzed |>
    filter(UID == 453) |>
    select(UID, Category, Entropy_Scaled, Compression_Ratio, DisplayLength, DisplayCode) |>
    writeSummaryBlock("Compression rate comparison")

# Lowest entropy string ---------------------------------------------
data_analyzed |>
    filter(Entropy_Scaled == min(Entropy_Unscaled)) |>
    select(UID, Category, Entropy_Scaled, Compression_Ratio, DisplayLength, DisplayCode) |>
    writeSummaryBlock("Lowest Entropy display")

# Cop strings ---------------------------------------------
data_analyzed |>
    filter(Category == "COP") |>
    select(UID, Male1ID DisplayCode) |>
    writeSummaryBlock("COP display codes")


# JARO DISTANCES ---------------------------------------------
# Jaro string distances
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
