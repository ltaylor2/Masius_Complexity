# Custom plotting theme
customTheme <- theme_bw() +
            theme(panel.grid=element_blank(),
                  axis.text.x=element_text(size=8),
                  axis.text.y=element_text(size=8),
                  axis.title.x=element_text(size=12),
                  axis.title.y=element_text(size=12),
                  plot.title=element_text(size=12))
            
# 4-Class qualitative colors, colorblind safe
# from colorbrewer2.org
categoryColors <- c("SOLO" = "#a6cee3",
                    "AUDI" = "#b2df8a",
                    "COP" = "#2c4a11",
                    "Random" = "#d3d3d388")

# Read in analyzed data
data_analyzed <- read_csv(ANALYZED_DATA_PATH, show_col_types=FALSE)

# Load jaro distances
# [Mutate] Category_1 to Factor to order facets in plot
distances <- readRDS("Output/distances.rds") |>
          mutate(Category_1 = factor(Category_1, levels=c("SOLO", "AUDI", "COP")))

# TABLE 1 ------------------------------------------------------
# Table of core behavioral elements and descriptions, with category-specific frequencies
# [Select] UID, Category, and DisplayCode string cols
# [Separate] Display code into any number of single-column character columns
#            NOTE this gives each row the number of columns matching the max number
#                 of elements in the longest display, so we have trailing NA columns in most rows
# [Pivot longer] so each row has a UID, Category, and single character 
# [Filter] out missing codes (from Separate step, trailing NAs)
# [Group by] UID, Category, and Code
# [Tally] number of codes per UID, Category, and Code
#         this counts the number of times each character is in each DISPLAY,
#         i.e., each row is now a count of the number of times each 
#               behavior occurs in each display
#               while retaining Category type column
# [Group by] just Category and Code
# [Tally] the number of Codes per Category, because now each Code is only
#         represented by one row per Display UID where present
#         i.e., each row is now a count of the number of displays with each
#               behavioral code was present
# [Select] cols Category, Code, and renamed N_Displays_with_Element for tallys
# [Left join] these values with an inset-tallied dataset 
#             that contains one row per Category from the final dataset,
#             (this just matches each row with the total number of displays of each Category)
# [Mutate] the proportion of displays with each element as 
#            <Number of displays in the category with that element / Total displays of that category>
# [Mutate] Rounded percent from proportion
# [Select] Code, Column, Percent, and N
# [Pivot wider] to get one row per Behavioral Element/Code,
#               with columns now for each percentage and N from SOLO, AUDI, and COP displays
#               respectively,
# [Select] To reorder the columns
# [Mutate] an Element col with the shortened name of each behavioral code
table_1 <- data_analyzed |>
        select(UID, Category, DisplayCode) |>
        separate(DisplayCode, into=as.character(0:max(data_analyzed$DisplayLength)),
                 sep="", fill="right") |>
        pivot_longer(-c(UID, Category), names_to="Index", values_to="Code") |>
        filter(Code!="" & !is.na(Code)) |>
        group_by(UID, Category, Code) |>
        tally() |>
        group_by(Category, Code) |>
        tally() |>
        group_by(Category) |>
        select(Category, Code, N_Displays_with_Element=n) |>
        left_join(tally(group_by(data_analyzed, Category), name="Total_Displays_of_Category"), by="Category") |>
        mutate(Proportion_Displays_with_Element = N_Displays_with_Element / Total_Displays_of_Category) |>
        mutate(Percent_Displays_with_Element = round(Proportion_Displays_with_Element * 100,0)) |>
        select(Code, Perc=Percent_Displays_with_Element, N=N_Displays_with_Element) |>
        pivot_wider(id_cols=Code, names_from=Category, values_from=c(Perc, N)) |>
        select(Code, Perc_SOLO, Perc_AUDI, Perc_COP, N_SOLO, N_AUDI, N_COP) |>
        mutate(Element = map_chr(Code, ~ names(behavior_code)[behavior_code==.]),
               .after=Code)

# Write table to file            
write_csv(table_1, file="Output/TABLE_1.csv")

# FIGURE 1 ------------------------------------------------------
# Boxplot of Duration
plot_duration <- ggplot(data_analyzed, aes(x=Category, y=Duration, fill=Category)) +
              geom_jitter(width=0.15, height=0,
                          colour="black", size=0.5) +
                   geom_boxplot(alpha=0.5, outlier.shape=NA) +
              scale_x_discrete(limits=c("SOLO", "AUDI", "COP")) +
              scale_y_continuous(breaks=seq(0, 780, by=120)) +
              scale_fill_manual(values=categoryColors) +
              guides(fill="none") +
              ylab("Duration (s)") +
              customTheme +
              theme(axis.title.x = element_blank())

# Boxplot of Display Length                      
plot_displayLength <- ggplot(data_analyzed, aes(x=Category, y=DisplayLength, fill=Category)) +
                   geom_jitter(width=0.15, height=0,
                               colour="black", size=0.5) +
                   geom_boxplot(alpha=0.5, outlier.shape=NA) +
                   scale_x_discrete(limits=c("SOLO", "AUDI", "COP")) +   
                   scale_y_continuous(breaks=seq(0, 400, by=100), limits=c(0, 400)) +                
                   scale_fill_manual(values=categoryColors) +
                   guides(fill="none") +
                   xlab("Display context") +
                   ylab("Length (elements)") +
                   customTheme

# Boxplot of Unique Elements                      
plot_uniqueElements <- ggplot(data_analyzed, aes(x=Category, y=UniqueDisplayElements, fill=Category)) +
                    geom_jitter(width=0.15, height=0,
                                colour="black", size=0.5) +
                    geom_boxplot(alpha=0.5, outlier.shape=NA) +
                    scale_x_discrete(limits=c("SOLO", "AUDI", "COP")) +     
                    scale_y_continuous(breaks=seq(0, 10, by=2)) +              
                    scale_fill_manual(values=categoryColors) +
                    guides(fill="none") +
                    ylab("Unique elements") +
                    customTheme +
                    theme(axis.title.x = element_blank())

# Create combined plot and write to file              
plots_repertoire <- plot_duration + plot_displayLength + plot_uniqueElements +
                 plot_annotation(tag_levels="A", tag_prefix="(", tag_suffix=")") &
                 theme(plot.tag.position=c(0.89, 0.93)) 
ggsave(plots_repertoire, file="Plots/FIGURE_1.png", width=6, height=2) 


# FIGURE 2 ------------------------------------------------------
# Boxplot of scaled entropy
plot_entropy <- ggplot(data_analyzed, aes(x=Category, y=Entropy_Scaled, fill=Category)) +
             geom_jitter(width=0.15, height=0,
                         colour="black", size=0.5) +
                  geom_boxplot(alpha=0.5, outlier.shape=NA) +
             scale_x_discrete(limits=c("SOLO", "AUDI", "COP")) +
             scale_y_continuous(breaks=seq(0, 1, by=0.2), limits=c(0,1)) +
             scale_fill_manual(values=categoryColors) +
             guides(fill="none") +
             xlab("Display context") +
             ylab("Entropy (scaled)") +
             customTheme

# Boxplot of compression ratio                      
plot_compression <- ggplot(data_analyzed, aes(x=Category, y=Compression_Ratio, fill=Category)) +
                 geom_jitter(width=0.15, height=0,
                             colour="black", size=0.5) +
                 geom_boxplot(alpha=0.5, outlier.shape=NA) +
                 scale_x_discrete(limits=c("SOLO", "AUDI", "COP")) +   
                 scale_y_continuous(breaks=seq(0, 8, by=2), limits=c(0, 8)) +
                 scale_fill_manual(values=categoryColors) +
                 guides(fill="none") +
                 xlab("Display context") +
                 ylab("Compression ratio") +
                 customTheme

# Correlation plot of entropy and compression          

# Compute convex hulls
hulls <- data_analyzed |>
      group_by(Category) |>
      slice(chull(Entropy_Scaled, Compression_Ratio))

# Arrange hull labels
labels <- tibble(Category=c("SOLO", "AUDI", "COP"),
                 x       =c(0.88,    0.75,    0.15),
                 y       =c(0.19,    6.60,    2.75),
                 angle   =c(0,       -59,      -63))

plot_syntaxCorrelation <- ggplot(data_analyzed) +
                       geom_point(aes(x=Entropy_Scaled, y=Compression_Ratio, colour=Category), 
                                  size=0.6, alpha=0.9) +
                       geom_polygon(data=hulls, 
                                    aes(x=Entropy_Scaled, y=Compression_Ratio, fill=Category), alpha=0.4) +
                       geom_smooth(aes(x=Entropy_Scaled, y=Compression_Ratio), 
                                   formula="y~x", method="lm", 
                                   colour="black", se=FALSE) +
                       geom_text(data=labels, 
                                 aes(label=Category, colour=Category,
                                     x=x, y=y, angle=angle),
                                     size=3) + 
                       scale_colour_manual(values=categoryColors) +
                       scale_fill_manual(values=categoryColors) +
                       scale_x_continuous(breaks=seq(0, 1, by=0.2), limits=c(0,1)) +
                       scale_y_continuous(breaks=seq(0, 8, by=2), limits=c(0, 8)) +
                       guides(fill="none", colour="none") +
                       xlab("Entropy (scaled)") +
                       ylab("Compression ratio") +
                       customTheme


# Create combined plot and write to file              
plots_syntax <- plot_entropy + plot_compression + plot_syntaxCorrelation +
             plot_annotation(tag_levels="A", tag_prefix="(", tag_suffix=")") &
             theme(plot.tag.position=c(0.89, 0.93)) 
ggsave(plots_syntax, file="Plots/FIGURE_2.png", width=6, height=2) 

# FIGURE 3 ------------------------------------------------------
# Jaro distance comparison

comparisonSampleSizes <- distances |>
                      group_by(Category_1, Comparison_Type) |>
                      tally()
                      
comparisonOrder <- c("Same Male/Same Context",
                     "Diff Male/Same Context",
                     "Same Male/Diff Context",
                     "Diff Male/Diff Context")

comparisonLabels <- c("Same Male/Same Context" = "Same Male\nSame Context",
                     "Diff Male/Same Context"  = "Diff Male\nSame Context",
                     "Same Male/Diff Context"  = "Same Male\nDiff Context",
                     "Diff Male/Diff Context"  = "Diff Male\nDiff Context")

plot_jaro <- ggplot(distances, 
                    aes(x=Comparison_Type, y=Jaro_Distance,
                        fill=Category_1)) +
          geom_jitter(width=0.15, height=0,
                      colour="black", size=0.15, alpha=0.15) +
          geom_boxplot(alpha=0.5, outlier.shape=NA) +
          geom_text(data=comparisonSampleSizes,
                    aes(label=n, x=Comparison_Type, 
                        y=-Inf), vjust=-0.5, size=3) +
          xlab("Comparison display") +
          ylab("Jaro distance") +
          ggtitle("Focal display context") +
          facet_wrap(facet=vars(Category_1)) +
          scale_x_discrete(limits=comparisonOrder,
                           labels=comparisonLabels) +
          scale_y_continuous(limits=c(0,1), breaks=seq(0, 1, by=0.2)) +
          scale_fill_manual(values=categoryColors) +
          guides(fill="none") +
          customTheme +
          theme(plot.title = element_text(hjust=0.5, size=12),
                strip.text = element_text(size=12),
                axis.text.x = element_text(size=6, angle=30, vjust=0.87),
                strip.background = element_rect(colour="black", fill=NA))
      
ggsave(plot_jaro, file="Plots/FIGURE_3.png", width=7, height=4)

# TABLE S2 ---------------------------------------------
# Male performance patterns 

# Read in banding data
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

# Summarize Male performance patterns
# [Group by] main dataset by Male ID and display category
# [Tally] number of displays of each category per male ID
# [Pivot wider] for one row per MaleID
# [Mutate] Male1ID into character rather than numeric to match banding dataset
# [Left join] to banding dataset
# [Select] to rename / reorder columns
# [Mutate] to convert Age abbreviations from banding dataset into plumage types
# [Mutate] to get first and last display dates from full datasets
# [Mutate] to replace NA with 0 in display frequencies
table_s2 <- data_analyzed |>
         group_by(Male1ID, Category) |>
         tally() |>
         pivot_wider(id_cols=Male1ID, names_from="Category", values_from="n") |>
         mutate(Male1ID = as.character(Male1ID)) |>
         left_join(bands, by=c("Male1ID"="Band_ID")) |>
         select(Band_ID=Male1ID, Banding_Date=Date, Banding_Plumage=Age, SOLO, AUDI, COP) |>
         mutate(Banding_Plumage = c("Def"="Definitive", "G"="Predefinitive", "Pre-def"="Predefinitive")[Banding_Plumage]) |>
         mutate(First_Display = map_chr(Band_ID, ~as.character(min(filter(data_analyzed, Male1ID == .)$ObsDate))),
                Last_Display = map_chr(Band_ID, ~as.character(max(filter(data_analyzed, Male1ID == .)$ObsDate)))) |>
         mutate(across(c(SOLO, AUDI, COP), ~ ifelse(is.na(.x), 0, .x)))

# Write table to file
write_csv(table_s2, file="Output/TABLE_S2.csv")

# TABLE S3 ---------------------------------------------
# Audience attendance patterns 

# Custom function to return males attended by each audience member
getPerformersFromAudience <- function(audienceID, copulatorsOnly=FALSE) {
    data <- data_analyzed
    if(copulatorsOnly) { data <- filter(data_analyzed, Category=="COP") }
    performers <- data |>
               filter(FemID == audienceID | Bird2ID == audienceID) |>
               pull(Male1ID) |>
               unique() |>
               sort() |>
               paste(collapse=";")
    return(performers)
}

# Summarize audience attendance patterns
# [Select] only Category, FemID, and Bird2ID cols from full dataset
# [Pivot longer] to get one FemID OR Bird2ID per row,
#                whcih
#                retains Category column so, for example
#                   row AUDI FemID=834 Bird2ID=8200
#                becomes two rows
#                       AUDI ID=834
#                       AUDI ID=8200
# [Filter] out NA ID rows (i.e., missing FemID from SOLO or Bird2ID from any category display)
# [Group by] audience ID and Category
# [Tally] by ID and category
# [Pivot wider] to one row per ID, with columns for tallies of different display categories
# [Mutate] Audience ID into character rather than numeric to match banding dataset
# [Left join] to banding dataset
# [Select] to rename / reorder columns
# [Mutate] to convert Age abbreviations from banding dataset into plumage types
# [Mutate] to get first and last display attended dates from full dataset
# [Mutate] to get semicolon-concatenated list of MaleIDs for males seen or copulated with
#          see getPerformersFromAudience() above
# [Mutate] to replace NA with 0 in display frequencies
table_s3 <- data_analyzed |>
         select(Category, FemID, Bird2ID) |>
         pivot_longer(c(FemID, Bird2ID), 
                      names_to="Type", values_to="ID") |>
         filter(!is.na(ID)) |>
         group_by(ID, Category) |>
         tally() |>
         pivot_wider(id_cols=ID, names_from=Category, values_from=n) |>
         mutate(ID=as.character(ID)) |>
         left_join(bands, by=c("ID"="Band_ID")) |>
         select(Band_ID=ID, Banding_Date=Date, Banding_Plumage=Age, AUDI, COP) |>
         mutate(Banding_Plumage = c("G"="Green")[Banding_Plumage]) |>
         mutate(First_Display = map_chr(Band_ID, ~as.character(min(filter(data_analyzed, FemID == . | Bird2ID == .)$ObsDate))),
                Last_Display = map_chr(Band_ID, ~as.character(max(filter(data_analyzed, FemID == . | Bird2ID == .)$ObsDate)))) |>
         mutate(Males_Viewed = map_chr(Band_ID, getPerformersFromAudience), 
                Males_Copulated = map_chr(Band_ID, ~ getPerformersFromAudience(., copulatorsOnly=TRUE))) |>
         mutate(across(c(AUDI, COP), ~ ifelse(is.na(.x), 0, .x)))

# Write table to file
write_csv(table_s3, file="Output/TABLE_S3.csv")

# TABLE S4 ---------------------------------------------
# COP display strings
table_s4 <- data_analyzed |>
         filter(Category=="COP") |>
         select(UID, Male1ID, ObsDate, 
                Duration, DisplayLength, Entropy_Scaled, Compression_Ratio,
                DisplayCode) |>
         mutate(Duration = round(Duration, 0)) |>
         arrange(UID, Male1ID, ObsDate)

# Write table to file
write_csv(table_s4, file="Output/TABLE_S4.csv")

# TABLE S5 ---------------------------------------------
# Table of core behavioral elements and descriptions, with category-specific frequencies
# [Select] UID, Category, and DisplayCode string cols
# [Separate] Display code into any number of single-column character columns
#            NOTE this gives each row the number of columns matching the max number
#                 of elements in the longest display, so we have trailing NA columns in most rows
# [Pivot longer] so each row has a UID, Category, and single character 
# [Filter] out missing codes (from Separate step, trailing NAs)
# [Group by] Category and Code
# [Tally] so each row is a count of the Codes that appear in displays of each Category
# [Pivot wider] so each row is a Code, with columns for tallies of each display Category
# [Select] to reorder columns
# [Mutate] an Element col with the shortened name of each behavioral code
# [Mutate] to replace NAs with 0 for frequencies of each
table_s5 <- data_analyzed |>
         select(UID, Category, DisplayCode) |>
         separate(DisplayCode, into=as.character(0:max(data_analyzed$DisplayLength)),
                  sep="", fill="right") |>
         pivot_longer(-c(UID, Category), names_to="Index", values_to="Code") |>
         filter(Code!="" & !is.na(Code)) |>
         group_by(Category, Code) |>
         tally() |>
         pivot_wider(id_cols=Code, names_from=Category, values_from=n) |>
         select(Code, SOLO, AUDI, COP) |>
         mutate(Element = map_chr(Code, ~ names(behavior_code)[behavior_code==.]),
                .after=Code) |>
         mutate(across(c(SOLO, AUDI, COP), ~ ifelse(is.na(.x), 0, .x)))

# Write table to file
write_csv(table_s5, file="Output/TABLE_S5.csv")

# FIGURE S1 ---------------------------------------------

dateBreaks <- c("1999-01-01",
                "1999-02-01",
                "1999-06-01",
                "1999-07-01",
                "1999-08-01",
                "1999-09-01",
                "1999-10-01",
                "1999-11-01",
                "1999-12-01") |>
           ymd() |> yday()
dateLabels <- c("Jan", "Feb", "Jun",
                "Jul", "Aug", "Sep", 
                "Oct", "Nov", "Dec")

# Histogram date of displays separated by display category
figure_s1 <- ggplot(mutate(data_analyzed, Category=factor(Category, levels=c("COP", "AUDI", "SOLO")))) +
          geom_histogram(aes(x=yday(ObsDate), fill=Category),
                         alpha=0.5, colour="black", binwidth=5) +
          facet_wrap(facets=vars(Category), nrow=3, ncol=1,
                     strip.position="right") +
          scale_x_continuous(breaks=dateBreaks, labels=dateLabels) +
          scale_fill_manual(values=categoryColors) +
          xlab("Day of year") +
          ylab("Displays") +
          guides(fill="none") +
          customTheme +
          theme(strip.text = element_text(size=12, vjust=0.5),
                strip.background = element_rect(colour="NA", fill=NA))
          

ggsave(figure_s1, file="Plots/FIGURE_S1.png", width=5, height=3.5)


# FIGURE S2 ---------------------------------------------
# Randomization results

# Unique elements
empiricalCop_uniqueElements <- data_analyzed |>
                            filter(Category == "COP") |>
                            select(Category, UniqueDisplayElements)
empiricalCop_uniqueElements_MEAN <- mean(empiricalCop_uniqueElements$UniqueDisplayElements)

randomDistribution_uniqueElements <- readRDS("Output/randomDistribution_uniqueElements.rds") |>
                                  mutate(Category = "Random",
                                         UniqueDisplayElements=Mean) |>
                                  select(Category, UniqueDisplayElements)

plot_randComp_uniqueElements <- ggplot(randomDistribution_uniqueElements) +
                             geom_vline(xintercept=empiricalCop_uniqueElements_MEAN, 
                                        colour=categoryColors["COP"], linetype="dashed") +
                             geom_histogram(aes(x=UniqueDisplayElements, fill=Category),
                                            alpha=0.3, colour="black", bins=30) +
                             geom_dotplot(data=empiricalCop_uniqueElements,
                                          aes(x=UniqueDisplayElements, fill=Category), 
                                          alpha=0.75, dotsize=0.48) +
                             scale_x_continuous(breaks=seq(0, 10, by=1)) +              
                             scale_fill_manual(values=categoryColors) +
                             guides(colour="none", fill="none") +
                             xlab("Unique elements") +
                             ylab("Random sample mean") +
                             customTheme +
                             theme(axis.title.y=element_blank())


# Entropy
empiricalCop_entropy <- data_analyzed |>
                     filter(Category == "COP") |>
                     select(Category, Entropy_Scaled)
empiricalCop_entropy_MEAN <- mean(empiricalCop_entropy$Entropy_Scaled)

randomDistribution_entropy <- readRDS("Output/randomDistribution_entropy.rds") |>
                           mutate(Category = "Random",
                                  Entropy_Scaled=Mean) |>
                           select(Category, Entropy_Scaled)

plot_randComp_entropy <- ggplot(randomDistribution_entropy) +
                      geom_vline(xintercept=empiricalCop_entropy_MEAN, 
                                 colour=categoryColors["COP"], linetype="dashed") +
                      geom_histogram(aes(x=Entropy_Scaled, fill=Category),
                                     alpha=0.3, colour="black", bins=30) +
                      geom_point(data=empiricalCop_entropy,
                                 aes(x=Entropy_Scaled, fill=Category), 
                                 shape=21, alpha=0.75, y=105, size=2) +
                      scale_fill_manual(values=categoryColors) +
                      guides(colour="none", fill="none") +
                      xlab("Entropy (scaled)") +
                      ylab("Random sample mean") +
                      customTheme

# Compressibility
empiricalCop_compressionRatio <- data_analyzed |>
                              filter(Category == "COP") |>
                              select(Category, Compression_Ratio)
empiricalCop_compressionRatio_MEAN <- mean(empiricalCop_compressionRatio$Compression_Ratio)

randomDistribution_compressionRatio <- readRDS("Output/randomDistribution_compressionRatio.rds") |>
                                    mutate(Category = "Random",
                                           Compression_Ratio=Mean) |>
                                    select(Category, Compression_Ratio)

plot_randComp_compressionRatio <- ggplot(randomDistribution_compressionRatio) +
                               geom_vline(xintercept=empiricalCop_compressionRatio_MEAN, 
                                          colour=categoryColors["COP"], linetype="dashed") +
                               geom_histogram(aes(x=Compression_Ratio, fill=Category),
                                              alpha=0.3, colour="black", bins=30) +
                               geom_point(data=empiricalCop_compressionRatio,
                                          aes(x=Compression_Ratio, fill=Category), 
                                          shape=21, alpha=0.75, y=80, size=2) +
                               scale_fill_manual(values=categoryColors) +
                               guides(colour="none", fill="none") +
                               xlab("Compression ratio") +
                               ylab("Random sample mean") +
                               customTheme +
                               theme(axis.title.y=element_blank())

# Create combined plot and write to file              
plots_randComparions <- plot_randComp_uniqueElements /
                        plot_randComp_entropy /
                        plot_randComp_compressionRatio

ggsave(plots_randComparions, file="Plots/FIGURE_S2.png", width=6, height=5) 









