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

# Jaro distances ---------------------------------------------

# Custom function to classify Jaro comparison types
jaroComparisonType <- function(category_1, category_2,
                               male_1, male_2) {
    if ((category_1==category_2) & (male_1==male_2)) {
        return("Same Male/Same Context")        
    } else if ((category_1==category_2) & (male_1!=male_2)) {
        return("Diff Male/Same Context")
    } else if ((category_1!=category_2) & (male_1==male_2)) {
        return("Same Male/Diff Context")
    }
    return("Diff Male/Diff Context")
}

# Compute Jaro distances
# [Expand Grid] produce two columns, with all combinations
#               of display UID
# [Select] to rename col names
# [Filter] out rows with the same display
# [Left join] UID_1 information from full dataset
# [Select] Category, Male, and Display string for display 1
# [Left join] UID_2 information frm full dataset
# [Select] Category, Male, and Display string for display 2
# [Filter] to include only identified males
# [Filter] out redundant comparisons
#              NOTE we have to cut this because we don't want to double count 
#              e.g. M1-COP1 vs. M2-COP1 and M2-COP1 vs. M1-COP1
#                   or
#                   M1-COP1 vs. M1-COP2 and M1-COP2 vs. M1-COP1
#              HOWEVER we do want to retain these duplicates when they 
#                      are across different Category_1 contexts, because 
#                      they are used in different context comparisons
#              e.g.  M1-COP1 vs. M2-SOLO1 is used for the COP test
#              while M2-SOLO1 vs. M1-COP1 is used for the SOLO test
#              Because every unique combination of UID1 and UID2 are 
#                      in the table, we can do this by
#                      excluding the rows where 
#                       UID_1 < UID_2 and Category_1 == Category_2
#                      this will retain the row where
#                       UID_1 > UID_2 or Context_1 == Context_2
#                      and retains all combinations across Contexts
# [Mutate] Calculate Jaro distance between the two display strings
# [Mutate] Classify the Jaro comparison type
#          (see jaroComparisonType() function, above)
distances <- expand_grid(data_analyzed$UID, data_analyzed$UID) |>
          select(UID_1=1, UID_2=2) |>
          filter(UID_1 != UID_2) |>
          left_join(data_analyzed, by=c("UID_1"="UID")) |>
          select(UID_1, UID_2, 
                 Category_1=Category, Male_1=Male1ID, DisplayCode_1=DisplayCode) |>
          left_join(data_analyzed, by=c("UID_2"="UID")) |>
          select(UID_1, UID_2,
                 Category_1, Male_1, DisplayCode_1,
                 Category_2=Category, Male_2=Male1ID, DisplayCode_2=DisplayCode) |>
          filter(Male_1 < 8000 & Male_2 < 8000) |>    
          filter(!(UID_1 < UID_2 & Category_1 == Category_2)) |>    
          mutate(Jaro_Distance = map2_dbl(DisplayCode_1, DisplayCode_2, 
                                          ~ stringdist(.x, .y, method="jw")),
                 .after=UID_2) |>
          mutate(Comparison_Type = pmap_chr(list(Category_1, Category_2, Male_1, Male_2),
                                            jaroComparisonType),
                 .before=1)

# Save for plotting later
saveRDS(distances, file="Output/distances.rds")

# Randomization tools (for small COP sample sizes) ---------------------------------------------

n_draws <- nrow(filter(data_analyzed, Category=="COP"))
# Custom function for a single random draw of Display metric
randomDraw <- function(i, variable) {
    if (i==1 | (i %% 5000) == 0) { cat(paste("Random draw", i, "for variable", variable, "\n")) }

    draw <- data_analyzed |>
         slice_sample(n=n_draws, replace=FALSE) |>
         pull(variable)
    return(tibble(Variable=variable, Mean=mean(draw)))
}

# Custom function for a single random draw of Jaro distances
#   for two key COP-related distances
randomDrawJaro <- function(i, comparison=c("Diff Male/Same Context vs. Same Male/Diff Context COP",
                                           "Diff Male/Same Context COP vs. AUDI vs. SOLO")) {

    if (i==1 | (i %% 5000) == 0) { cat(paste("Random draw", i, "for Jaro distances", comparison, "\n")) }

    if (comparison == "Diff Male/Same Context vs. Same Male/Diff Context COP") {
        draw <- distances |>
             filter(Category_1 == "COP" & Comparison_Type == "Same Male/Diff Context") |>
             slice_sample(n=n_draws, replace=FALSE) |>
             pull(Jaro_Distance)
    } else if (comparison == "Diff Male/Same Context COP vs. AUDI + SOLO") {
        draw <- distances |>
             filter(Comparison_Type == "Diff Male/Same Context") |>
             slice_sample(n=n_draws, replace=FALSE) |>
             pull(Jaro_Distance)
    }
    return(tibble(Comparison = comparison, Mean=mean(draw)))
}

# REPORT Tallies ---------------------------------------------
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

# Generate random distribution (if needed)
#   or load random distribution saved from previous run
if (RUN_RANDOM) {
    randomDistribution_uniqueElements <- map_df(1:RANDOMIZATION_REPLICATES, ~randomDraw(., "UniqueDisplayElements"))
    saveRDS(randomDistribution_uniqueElements, file="Output/randomDistribution_uniqueElements.rds")
} else {
    randomDistribution_uniqueElements <- readRDS("Output/randomDistribution_uniqueElements.rds")
}
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
if (RUN_RANDOM) {
    randomDistribution_entropy <- map_df(1:RANDOMIZATION_REPLICATES, ~randomDraw(., "Entropy_Scaled"))
    saveRDS(randomDistribution_entropy, file="Output/randomDistribution_entropy.rds")
} else {
    randomDistribution_entropy <- readRDS("Output/randomDistribution_entropy.rds")
}

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
if (RUN_RANDOM) {
    randomDistribution_compressionRatio <- map_df(1:RANDOMIZATION_REPLICATES, ~randomDraw(., "Compression_Ratio"))
    saveRDS(randomDistribution_compressionRatio, file="Output/randomDistribution_compressionRatio.rds")
} else {
    randomDistribution_compressionRatio <- readRDS("Output/randomDistribution_compressionRatio.rds")   
}
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
    select(UID, Male1ID, DisplayCode) |>
    writeSummaryBlock("COP display codes")

# Jaro distances --  ---------------------------------------------
distances |>
    group_by(Comparison_Type) |>
    summarize(mean(Jaro_Distance), sd(Jaro_Distance)) |>
    writeSummaryBlock("Jaro distances by comparison type")    

distances |>
    filter(Comparison_Type == "Diff Male/Same Context") |>                        
    group_by(Category_1, Comparison_Type) |>
    summarize(mean(Jaro_Distance), sd(Jaro_Distance), .groups="keep") |>
    writeSummaryBlock("Diff Male/Same Context Jaro comparisons SOLO vs. AUDI vs. COP")

distances |>
    filter(Category_1 == "COP") |>
    group_by(UID_1) |>
    filter(Jaro_Distance == min(Jaro_Distance)) |>
    group_by(Category_1, Category_2, Comparison_Type) |>
    tally() |>
    writeSummaryBlock("COP closest partner")

# RANDOMIZATION Jaro distances ---------------------------------------------

# Random draws for COP, Diff Male/Same Context vs. Same Male/Diff Context
if (RUN_RANDOM) {
    randomDistribution_Jaro_SameDiffMaleCOP <- map_df(1:RANDOMIZATION_REPLICATES, 
                                                    ~randomDrawJaro(., "Diff Male/Same Context vs. Same Male/Diff Context COP"))
    saveRDS(randomDistribution_Jaro_SameDiffMaleCOP, file="Output/randomDistribution_Jaro_SameDiffMaleCOP.rds")
} else {
    randomDistribution_Jaro_SameDiffMaleCOP <- readRDS("Output/randomDistribution_Jaro_SameDiffMaleCOP.rds")
}

# Summarize for report
cop_Jaro_DiffMaleSameContext  <- distances |>
                              filter(Category_1 == "COP", Comparison_Type == "Diff Male/Same Context") |>
                              pull(Jaro_Distance) |>
                              mean()

randomDistribution_Jaro_SameDiffMaleCOP |> 
    group_by(Mean < cop_Jaro_DiffMaleSameContext) |>
    tally() |>
    writeSummaryBlock("RANDOMIZATION -- Jaro -- Diff Male/Same Context COP vs. Same Male/Diff Context COP")



# Random draws for COP, Diff Male/Same Context vs. Same Male/Diff Context
if (RUN_RANDOM) {
    randomDistribution_Jaro_DiffMaleAcrossContexts <- map_df(1:RANDOMIZATION_REPLICATES, 
                                                            ~randomDrawJaro(., "Diff Male/Same Context COP vs. AUDI + SOLO"))
    saveRDS(randomDistribution_Jaro_DiffMaleAcrossContexts, file="Output/randomDistribution_Jaro_DiffMaleAcrossContexts.rds")
} else {
    randomDistribution_Jaro_DiffMaleAcrossContexts <- readRDS("Output/randomDistribution_Jaro_DiffMaleAcrossContexts.rds")
}

# Summarize for report
randomDistribution_Jaro_DiffMaleAcrossContexts |> 
    group_by(Mean < cop_Jaro_DiffMaleSameContext) |>
    tally() |>
    writeSummaryBlock("RANDOMIZATION -- Jaro -- Diff Male/Same Context COP vs. AUDI + SOLO")

# SUPPLEMENTARY MATERIAL -- Before vs after copulation ---------------------------------------------

# Prepare the before- and after-copulation display code metrics
# [Filter] to include only COP displays
# [Select] only the UID, standard COP DisplayCode, and AfterCop DisplayCode strings
# [Mutate] exclude the Attempted copulation behavioral element from the AfterCop Display String
# [Mutate] exclude the Copulation behavioral element from the AfterCop display string
# [Pivot longer] to get one DisplayCode string per row, Before or After, retaining the UID to match them up
# [MUTATESx] Duplicate repertoire and syntax analyses (see LL 17-41 above)
# [Select] Only relevant cols for comparison
afterCop_comparison <- data_analyzed |>
                    filter(Category=="COP") |>
                    select(UID, Before=DisplayCode, After=DisplayCode_AfterCop) |>
                    mutate(After = str_replace(After, behavior_code["AttC"], "")) |>
                    mutate(After = str_replace(After, behavior_code["Cop"], "")) |>
                    pivot_longer(cols=c(Before, After), names_to="Section", values_to="DisplayCode") |>
                    mutate(DisplayCode_Raw = map(DisplayCode, charToRaw)) |>
                    mutate(DisplayCode_Compressed = map(DisplayCode_Raw, brotli::brotli_compress)) |>
			        mutate(DisplayLength = nchar(DisplayCode),
                           UniqueDisplayElements = map_dbl(DisplayCode, countUniqueChars), 
                           Entropy_Unscaled = acss::entropy(DisplayCode),
                           Entropy_Scaled = Entropy_Unscaled / log(UniqueDisplayElements, base=2),
                           Compression_Length = map_dbl(DisplayCode_Compressed, length),
                           Compression_Ratio = map2_dbl(DisplayCode_Raw, DisplayCode_Compressed, ~ length(.x) / length(.y))) |>
                    mutate(Entropy_Scaled = ifelse(Entropy_Unscaled==0, 0, Entropy_Scaled)) |>
                    select(UID, Section, DisplayCode, DisplayLength, UniqueDisplayElements, Entropy_Scaled, Compression_Ratio) |>
                    unite("UID_Section", UID, Section, sep="-", remove=FALSE)

# Save object to read in for plotting
saveRDS(afterCop_comparison, file="Output/afterCop_comparison.rds")

# Custom function to extract metric comparison values
beforeAfterCompare <- function(metric) {
    # Summarize mean and sd of metric
    means <- afterCop_comparison |>
          group_by(Section) |>
          summarize_at(.vars=metric, .funs=list(mean=mean, sd=sd))

    # Tally which BEFORE values are < AFTER values for given metric 
    tallies <- afterCop_comparison |>
            select(UID, Section, all_of(metric)) |>
            pivot_wider(id_cols=UID, names_from=Section, 
                        values_from=metric) |>
            group_by(Before<After) |>
            tally()

    # Widen by-UID comparison dataset for t.test 
    comp <- afterCop_comparison |>
         select(UID, Section, all_of(metric)) |>
         pivot_wider(id_cols=UID, names_from=Section, values_from=metric)

    # Paired t.test
    t <- t.test(x=comp$Before, y=comp$After, 
                alternative = "two.sided",
                paired=TRUE)
    return(list(metric, means, tallies, t))
}

# REPORT key values for supplementary report
afterCop_comparison |>
    filter(Section == "After") |>
    summarize(min(DisplayLength), max(DisplayLength)) |>
    writeSummaryBlock("AFTER-COP DISPLAYS -- LENGTH (elements) range")

beforeAfterCompare("UniqueDisplayElements") |>
    writeSummaryBlock("BEFORE- VS. AFTER-COP -- Unique Display Elements")

beforeAfterCompare("Entropy_Scaled") |>
    writeSummaryBlock("BEFORE- VS. AFTER-COP -- Entropy (scaled)")

beforeAfterCompare("Compression_Ratio") |>
    writeSummaryBlock("BEFORE- VS. AFTER-COP -- Compression Ratio")

# Custom function to classify before vs. after cop jaro distance 
#   comparison type
beforeAfterJaroType <- function(UID_1, UID_2, Section_1, Section_2) {
    levels <- c("Diff display/Same section", "Diff display/Diff section", "Same display/Diff section")
    if (UID_1 == UID_2) { ret <- "Same display/Diff section" } 
    else if (Section_1 == Section_2) { ret <- "Diff display/Same section" }
    else { ret <- "Diff display/Diff section" }
    return(factor(ret, levels=levels))
}

# TODO
#   Expand grid filter duplicate comparisons?
#   Compare same/diff to diff/same
# PLOT
# Jaro distances, before vs. after cop
beforeAfterDistances <- expand_grid(afterCop_comparison$UID_Section, 
                                    afterCop_comparison$UID_Section) |>
                     select(UID_Section_1=1, UID_Section_2=2) |>
                     filter(UID_Section_1 != UID_Section_2) |>
                     left_join(afterCop_comparison, by=c("UID_Section_1"="UID_Section")) |>
                     select(UID_Section_1, UID_Section_2, DisplayCode_1=DisplayCode) |>
                     left_join(afterCop_comparison, by=c("UID_Section_2"="UID_Section")) |>
                     select(UID_Section_1, UID_Section_2, DisplayCode_1, DisplayCode_2=DisplayCode) |>
                     separate(UID_Section_1, into=c("UID_1", "Section_1"), sep="-") |>
                     separate(UID_Section_2, into=c("UID_2", "Section_2"), sep="-") |>
                     filter(Section_1 == "Before") |>
                     mutate(Jaro_Distance = map2_dbl(DisplayCode_1, DisplayCode_2, 
                                          ~ stringdist(.x, .y, method="jw")),
                                          .after=UID_2) |>
                     mutate(Comparison_Type = pmap_chr(list(UID_1, UID_2, Section_1, Section_2), beforeAfterJaroType))


plot_beforeAfterJaro <- ggplot(beforeAfterDistances,
                               aes(x=Comparison_Type, y=Jaro_Distance)) +
                     geom_boxplot() +
                     facet_wrap(facets=vars(UID_1), ncol=3) +
                     scale_x_discrete(labels=c("Other\ndisplays\nbefore","Other\ndisplays\nafter", "Same\ndisplay\nafter")) +
                     scale_y_continuous(limits=c(0, 1), breaks=seq(0, 1, by=0.5)) +
                     xlab("Comparison type") +
                     ylab("Jaro distance") +
                     customTheme +
                     theme(strip.background=element_rect(colour="black", fill="#f3f3f3"),
                           axis.text.x=element_text(size=6),
                           axis.title.x=element_text(size=10, hjust=0.025))

ggsave(plot_beforeAfterJaro, file="Plots/FIGURE_S6.png", width=4, height=5)                    
