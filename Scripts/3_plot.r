# LOGISTICS ------------------------------------------------------
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
                    "Random" = "#d3d3d388",
                    "Before" = "#2c4a11",
                    "After" = NA)

# Read in analyzed data
data_analyzed <- read_csv(ANALYZED_DATA_PATH, show_col_types=FALSE)

# Load jaro distances
# [Mutate] Category_1 to Factor to order facets in plot
distances <- readRDS("Output/distances.rds") |>
          mutate(Focal_Label = ifelse(Category_1=="SOLO", "Focal display context: SOLO", Category_1)) |>
          mutate(Focal_Label = factor(Focal_Label, levels=c("Focal display context: SOLO", "AUDI", "COP")))

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

# TABLE 2 ------------------------------------------------------
elements_femOn <- map2_df(data_analyzed$Category, data_analyzed$DisplayCode_FemOn, 
                         ~ tibble(Element = str_split(.y, "")[[1]]) |> 
                                  group_by(Element) |> 
                                  tally() |> 
                                  mutate(Category = .x) |>
                                  filter(!is.na(Element))) |>
                group_by(Category, Element) |>
                summarize(.groups="keep", n = sum(n)) |>
                group_by(Category) |>
                mutate(Total = sum(n)) |>
                mutate(Perc = as.character(round((n / Total) * 100, 0))) |>
                mutate(Perc = ifelse(Perc < 1, "<1", Perc)) |>
                mutate(Category = paste0(Category, "_FemOn"))

elements_femOff <- map2_df(data_analyzed$Category, data_analyzed$DisplayCode_FemOff, 
                          ~ tibble(Element = str_split(.y, "")[[1]]) |> 
                                   group_by(Element) |> 
                                   tally() |> 
                                   mutate(Category = .x) |>
                                   filter(!is.na(Element))) |>
                 group_by(Category, Element) |>
                 summarize(.groups="keep", n = sum(n)) |>
                 group_by(Category) |>
                 mutate(Total = sum(n)) |>
                 mutate(Perc = as.character(round((n / Total) * 100, 0))) |>
                 mutate(Perc = ifelse(Perc < 1, "<1", Perc)) |>
                 mutate(Category = paste0(Category, "_FemOff"))

elements_femUp <- map2_df(data_analyzed$Category, data_analyzed$DisplayCode_FemUp, 
                          ~ tibble(Element = str_split(.y, "")[[1]]) |> 
                                   group_by(Element) |> 
                                   tally() |> 
                                   mutate(Category = .x) |>
                                   filter(!is.na(Element))) |>                                
                 group_by(Category, Element) |>
                 summarize(.groups="keep", n = sum(n)) |>
                 group_by(Category) |>
                 mutate(Total = sum(n)) |>
                 mutate(Perc = as.character(round((n / Total) * 100, 0))) |>
                 mutate(Perc = ifelse(Perc < 1, "<1", Perc)) |>
                 mutate(Category = paste0(Category, "_FemUp"))

elements_femDown <- map2_df(data_analyzed$Category, data_analyzed$DisplayCode_FemDown, 
                            ~ tibble(Element = str_split(.y, "")[[1]]) |> 
                                     group_by(Element) |> 
                                     tally() |> 
                                     mutate(Category = .x) |>
                                     filter(!is.na(Element))) |>                                
                    group_by(Category, Element) |>
                    summarize(.groups="keep", n = sum(n)) |>
                    group_by(Category) |>
                    mutate(Total = sum(n)) |>
                    mutate(Perc = as.character(round((n / Total) * 100, 0))) |>
                    mutate(Perc = ifelse(Perc < 1, "<1", Perc)) |>
                    mutate(Category = paste0(Category, "_FemDown"))

table_2 <- bind_rows(elements_femOn, elements_femOff, elements_femUp, elements_femDown) |>
        select(Element, Category, Perc) |>
        pivot_wider(id_cols = Element, names_from=Category, values_from = Perc) |>
        select(Code = Element, SOLO_FemOff, AUDI_FemOff, COP_FemOff, AUDI_FemOn, COP_FemOn, 
               AUDI_FemUp, COP_FemUp,AUDI_FemDown, COP_FemDown) |>
        mutate(Element = map_chr(Code, ~ names(behavior_code)[behavior_code==.]),
               .after=Code) |>
        arrange(Code)

# Write table to file            
write_csv(table_2, file="Output/TABLE_2.csv")

## Calculate total male behavioral elements corresponding to female behavior subsets     
table_2_totals <- bind_rows(elements_femOn, elements_femOff, elements_femUp, elements_femDown) |>
              group_by(Category, Total) |>
              tally() |>
              mutate(drop="drop") |>
              pivot_wider(id_cols = drop, names_from=Category, values_from=Total) |>
              select(SOLO_FemOff, AUDI_FemOff, COP_FemOff, AUDI_FemOn, COP_FemOn, 
                     AUDI_FemUp, COP_FemUp, AUDI_FemDown, COP_FemDown)

# Write table to file            
write_csv(table_2_totals, file="Output/TABLE_2_TOTALS.csv")

# FIGURE 1 ------------------------------------------------------

# Custom function to plot brackets with linear models for variable
geom_lmBracket <- function(category1="AUDI", category2=c("COP", "SOLO"), variable, min, max) {
    
    # Reconstruct linear model from dataset with target variable
    form <- paste(variable, "~ Category + as.character(ObsMonth) + UniqueMale1ID")
    model <- lm(form, data = data_analyzed) |>
          summary()

    # Extract p value for label will only work on COP or SOLO: AUDI is intercept variable)
    p <- model$coefficients[paste0("Category", category2), "Pr(>|t|)"]

    if (category1=="AUDI"&category2=="SOLO") {
        x1<-0.9;x2<-2.1
    } else if (category1=="AUDI"&category2=="COP") {
        x1<-1.9;x2<-3.1
    } else { 
        print("ERROR, category1 mispecified in geom_lmBracket, returning with probable error.")
        return()
    }

    range <- max-min
    offset <- max*-0.03
    
    label <- ""
    linetype <- "dashed"
    if (p <= 0.001) {label<-"***"; linetype<-"solid"}
    else if (p <= 0.01) {label<-"**"; linetype<-"solid"}
    else if (p <= 0.05) {label<-"*"; linetype<-"solid"}

    # Y axis 
    if (category1=="AUDI" & category2=="SOLO") {
        y = min + range * 1.00
    } else if (category1=="AUDI" & category2=="COP") {
        y = min + range * 0.90
    } else {
        print("ERROR, category1 mispecified in geom_lmBracket, returning with probable error.")
        return() 
    }

    bracketCorner <- range * 0.015
    g1 <- geom_segment(x=x1, xend=x2, 
                       y=y, yend=y, 
                       linetype=linetype, colour="gray",
                       linewidth=0.35)
    g2 <- geom_segment(x=x1, xend=x1, 
                       y=y+bracketCorner, yend=y-bracketCorner, 
                       colour="gray")
    g3 <- geom_segment(x=x2, xend=x2, 
                       y=y+bracketCorner, yend=y-bracketCorner, 
                       colour="gray")
    g4 <- geom_text(label=label, x=(x1+x2)/2, 
                    y=y+offset, colour="gray", vjust=0, size=3)
    
    return(list(g1, g2, g3, g4))
}

# Boxplot of Duration
plot_duration <- ggplot(data_analyzed, aes(x=Category, y=Duration, fill=Category)) +
              geom_lmBracket("AUDI", "SOLO", "Duration", 0, 720) +
              geom_lmBracket("AUDI", "COP", "Duration", 0, 720) +
              geom_jitter(width=0.15, height=0,
                          colour="black", size=0.5) +
              geom_boxplot(alpha=0.5, outlier.shape=NA) +
              scale_x_discrete(limits=c("SOLO", "AUDI", "COP")) +
              scale_y_continuous(breaks=seq(0, 720, by=120), limits=c(0, 740)) +
              scale_fill_manual(values=categoryColors) +
              guides(fill="none") +
              ylab("Duration (s)") +
              customTheme +
              theme(axis.title.x=element_blank())

# Boxplot of Display Length                      
plot_displayLength <- ggplot(data_analyzed, aes(x=Category, y=DisplayLength, fill=Category)) +
                   geom_lmBracket("AUDI", "SOLO", "DisplayLength", 0, 400) +
                   geom_lmBracket("AUDI", "COP", "DisplayLength", 0, 400) +
                   geom_jitter(width=0.15, height=0,
                               colour="black", size=0.5) +
                   geom_boxplot(alpha=0.5, outlier.shape=NA) +
                   scale_x_discrete(limits=c("SOLO", "AUDI", "COP")) +   
                   scale_y_continuous(breaks=seq(0, 400, by=100), limits=c(0, 420)) +                
                   scale_fill_manual(values=categoryColors) +
                   guides(fill="none") +
                   ylab("Length (# elements)") +
                   customTheme +
                   theme(axis.title.x=element_blank())

# Boxplot of Unique Elements                      
plot_uniqueElements <- ggplot(data_analyzed, aes(x=Category, y=UniqueDisplayElements, fill=Category)) +
                    geom_lmBracket("AUDI", "SOLO", "UniqueDisplayElements", 0, 10) +
                    geom_lmBracket("AUDI", "COP", "UniqueDisplayElements", 0, 10) +
                    geom_jitter(width=0.15, height=0,
                                colour="black", size=0.5) +
                    geom_boxplot(alpha=0.5, outlier.shape=NA) +
                    scale_x_discrete(limits=c("SOLO", "AUDI", "COP")) +     
                    scale_y_continuous(breaks=seq(0, 10, by=2), limits=c(0, 11)) +              
                    scale_fill_manual(values=categoryColors) +
                    guides(fill="none") +
                    xlab("Display context") +
                    ylab("Repertoire size") +
                    customTheme

# Boxplot of scaled Entropy                      
plot_entropy <- ggplot(data_analyzed, aes(x=Category, y=Entropy_Scaled, fill=Category)) +
             geom_lmBracket("AUDI", "SOLO", "Entropy_Scaled", 0, 1.1) +
             geom_lmBracket("AUDI", "COP", "Entropy_Scaled", 0, 1.1) +
             geom_jitter(width=0.15, height=0,
                         colour="black", size=0.5) +
             geom_boxplot(alpha=0.5, outlier.shape=NA) +
             scale_x_discrete(limits=c("SOLO", "AUDI", "COP")) +
             scale_y_continuous(breaks=seq(0, 1, by=0.2), limits=c(0,1.2)) +
             scale_fill_manual(values=categoryColors) +
             guides(fill="none") +
             ylab("Entropy (scaled)") +
             customTheme +
             theme(axis.title.x=element_blank())

# Boxplot of Unique Elements                      
plot_compression <- ggplot(data_analyzed, aes(x=Category, y=Compression_Ratio, fill=Category)) +
                 geom_lmBracket("AUDI", "SOLO", "Compression_Ratio", 0, 7) +
                 geom_lmBracket("AUDI", "COP", "Compression_Ratio", 0, 7) +
                 geom_jitter(width=0.15, height=0,
                             colour="black", size=0.5) +
                 geom_boxplot(alpha=0.5, outlier.shape=NA) +
                 scale_x_discrete(limits=c("SOLO", "AUDI", "COP")) +   
                 scale_y_continuous(breaks=seq(0, 8, by=2), limits=c(0, 8)) +
                 scale_fill_manual(values=categoryColors) +
                 guides(fill="none") +
                 ylab("Compression ratio") +
                 customTheme +
                 theme(axis.title.x=element_blank())

# Create combined plot and write to file              
plots_characteristics <- plot_duration + plot_displayLength + plot_uniqueElements +
                      plot_entropy + plot_compression +
                      plot_layout(nrow=1) +
                      plot_annotation(tag_levels="A", tag_prefix="(", tag_suffix=")") &
                      theme(plot.tag.position=c(0.89, 0.93)) 

ggsave(plots_characteristics, file="Plots/FIGURE_1.png", width=8, height=2) 

# FIGURE 2 ------------------------------------------------------
# Jaro distance comparison
comparisonSampleSizes <- distances |>
                      group_by(Category_1, Focal_Label, Comparison_Type) |>
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
                        y=-Inf), vjust=-0.5, size=2.5) +
          xlab("Comparison display") +
          ylab("Jaro distance") +
          facet_wrap(facet=vars(Focal_Label)) +
          scale_x_discrete(limits=comparisonOrder,
                           labels=comparisonLabels) +
          scale_y_continuous(limits=c(0,1), breaks=seq(0, 1, by=0.2)) +
          scale_fill_manual(values=categoryColors) +
          guides(fill="none") +
          customTheme +
          theme(strip.text = element_text(size=11, hjust=1),
                axis.text.x = element_text(size=6, angle=30, vjust=0.87),
                strip.background = element_rect(colour=NA, fill=NA))
      
ggsave(plot_jaro, file="Plots/FIGURE_2.png", width=8, height=3)

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
         filter(is.na(Sex) | Sex != "M") |>
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
# Custom function to add line breaks for consistent
#   display code table formatting
displayCodeLineBreak <- function(s) {
    chars <- strsplit(s, "")[[1]]
    newString <- ""
    for (i in 1:length(chars)) {
        newString <- paste0(newString, chars[i], collapse="")
        if (i %% 40 == 0) { newString <- paste0(newString, "\n", collapse="") }
    }
    return(newString)
}

# COP display code strings, with associated metrics
# [Filter] COP displays only
# [Select] Relevant metrics and DisplayCode string
# [Mutate] add line breaks to display code
#          see displayCodeLineBreak()
# [Mutate] Round metrics for display
# [Arrange] to sort
table_s4 <- data_analyzed |>
         filter(Category=="COP") |>
         select(UID, Male1ID, ObsDate, 
                Duration, DisplayLength, Entropy_Scaled, Compression_Ratio,
                DisplayCode) |>
         mutate(DisplayCode = map_chr(DisplayCode, displayCodeLineBreak)) |>
         mutate(Duration = round(Duration, 0),
                Entropy_Scaled = round(Entropy_Scaled, 2),
                Compression_Ratio = round(Compression_Ratio, 1)) |>
         arrange(UID, Male1ID, ObsDate)

# Write table to file
write_csv(table_s4, file="Output/TABLE_S4.csv")

# TABLE S5 ---------------------------------------------

# Overall counts core behavioral elements with category-specific frequencies
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

# FIGURE S3 ---------------------------------------------
# Correlation plot of entropy and compression

model_entropy_compressionRatio <- lm(Entropy_Scaled ~ Compression_Ratio, 
                                     data=data_analyzed) |>
                               summary()

# Compute convex hulls
hulls <- data_analyzed |>
      group_by(Category) |>
      slice(chull(Entropy_Scaled, Compression_Ratio))

# Arrange hull labels
labels <- tibble(Category=c("SOLO", "AUDI", "COP"),
                 x       =c(0.88,    0.75,    0.13),
                 y       =c(0.19,    6.60,    2.80),
                 angle   =c(0,       -63.5,    -60))

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

ggsave(plot_syntaxCorrelation, file="Plots/FIGURE_S3.png", width=4, height=4)

# FIGURE S4 ---------------------------------------------
# Correlation plot of display length and compression ratio          
# Linear regression
model_displayLength_compressionRatio <- lm(Compression_Ratio ~ DisplayLength, 
                                           data=data_analyzed) |>
                                     summary()

# Compute convex hulls
hulls <- data_analyzed |>
      group_by(Category) |>
      slice(chull(DisplayLength, Compression_Ratio))
                             
# # Arrange hull labels
labels <- tibble(Category=c("SOLO", "AUDI", "COP"),
                 x       =c(20,     300,     48),
                 y       =c(0.4,    3.5,    4.2),
                 angle   =c(0,      28,    75))

plot_lengthCompression <- ggplot(data_analyzed) +
                       geom_point(aes(x=DisplayLength, y=Compression_Ratio, colour=Category), 
                                  size=0.6, alpha=0.9) +
                       geom_polygon(data=hulls, 
                                    aes(x=DisplayLength, y=Compression_Ratio, fill=Category), alpha=0.4) +
                       geom_smooth(aes(x=DisplayLength, y=Compression_Ratio), 
                                   formula="y~x", method="lm", 
                                   colour="black", se=FALSE) +
                       geom_text(data=labels, 
                                 aes(label=Category, colour=Category,
                                     x=x, y=y, angle=angle),
                                     size=4) + 
                       scale_colour_manual(values=categoryColors) +
                       scale_fill_manual(values=categoryColors) +
                       scale_x_continuous(breaks=seq(0, 400, by=100), limits=c(0, 400)) +                
                       scale_y_continuous(breaks=seq(0, 8, by=2), limits=c(0, 8)) +
                       guides(fill="none", colour="none") +
                       xlab("Length (elements)") +
                       ylab("Compression ratio") +
                       customTheme

ggsave(plot_lengthCompression, file="Plots/FIGURE_S4.png", width=4, height=4)

# FIGURE S5 ---------------------------------------------
# Difference in Display Length vs. Jaro Distance
# Linear regressions
model_jaro_length <- lm(Jaro_Distance ~ Difference_DisplayLength, 
                        data=distances) |>
                  summary()

model_jaro_uniqueElements <- lm(Jaro_Distance ~ Difference_UniqueDisplayElements, 
                                data=distances) |>
                          summary()

# Dot plot with correlation line 
plot_jaroDisplayLength <- ggplot(distances,
                                 aes(x=Difference_DisplayLength, y=Jaro_Distance)) +
                       geom_point(size=0.5, alpha=0.1) +
                       geom_smooth(formula="y~x", method="lm", colour="red", se=FALSE) +
                       scale_y_continuous(limits=c(0, 1), breaks=seq(0, 1, by=0.2)) +
                       xlab("Difference in display length") +
                       ylab("Jaro distance") +
                       customTheme

# Difference in Unique Display Elements vs. Jaro Distance
# Dot plot with correlation line
plot_jaroDisplayElements <- ggplot(distances,
                                   aes(x=Difference_UniqueDisplayElements, y=Jaro_Distance)) +
                         geom_point(size=0.5, alpha=0.1) +
                         geom_smooth(formula="y~x", method="lm", colour="red", se=FALSE) +
                         scale_y_continuous(limits=c(0, 1), breaks=seq(0, 1, by=0.2)) +
                         xlab("Difference in distinct elements") +
                         customTheme +
                         theme(axis.title.y=element_blank())

# Combine plots and save to file
plots_jaroCorrelations <- plot_jaroDisplayLength +
                          plot_jaroDisplayElements

ggsave(plots_jaroCorrelations, file="Plots/FIGURE_S5.png",
       width=6, height=3)

# FIGURE S6 ---------------------------------------------
# Jaro Randomization Results       

# Empirical distances
#   Diff-Male/Same-Context COP displays
empiricalCop_Jaro_DiffMaleSameContext  <- distances |>
                                      filter(Category_1 == "COP" & Comparison_Type == "Diff Male/Same Context") |>
                                      select(Category_1, Comparison_Type, Jaro_Distance)
empiricalCop_Jaro_DiffMaleSameContext_MEAN <- mean(empiricalCop_Jaro_DiffMaleSameContext$Jaro_Distance)

# Randomly drawn Jaro distances, empirical Diff Male/Same Context COP vs. Same Male/Diff Context COP
randomDistribution_Jaro_SameDiffMaleCOP <- readRDS("Output/randomDistribution_Jaro_SameDiffMaleCOP.rds") |>
                                  mutate(Category = "Random",
                                         Jaro_Distance=Mean) |>
                                  select(Category, Jaro_Distance)

plot_randComp_jaroSameDiffMaleCOP <- ggplot(randomDistribution_Jaro_SameDiffMaleCOP) +
                                  geom_vline(xintercept=empiricalCop_Jaro_DiffMaleSameContext_MEAN, 
                                             colour="red", linetype="dashed") +
                                  geom_histogram(aes(x=Jaro_Distance, fill=Category),
                                                 alpha=0.3, colour="black", bins=40) +
                                  geom_point(data=empiricalCop_Jaro_DiffMaleSameContext,
                                             aes(x=Jaro_Distance), 
                                             fill="black", shape=21, alpha=0.75, y=50, size=2) +
                                  scale_x_continuous(limit = c(0, 1), oob = function(x, limits) x) +
                                #   scale_y_continuous(limits=c(0, 3500), breaks=seq(0, 3500, by=1000), oob = function(y, limits) y) +
                                  scale_fill_manual(values=categoryColors) +
                                  guides(colour="none", fill="none") +
                                  ggtitle(paste0("Empirical: COP Diff Male/Same Context (n = ", 
                                                  nrow(filter(distances, Category_1=="COP" & Comparison_Type=="Diff Male/Same Context")),
                                                  ")\n",
                                                  "Randomized: COP Same Male/Diff Context (n = ",
                                                  nrow(filter(distances, Category_1=="COP" & Comparison_Type=="Diff Male/Same Context")),
                                                  "/", nrow(filter(distances, Category_1=="COP" & Comparison_Type=="Same Male/Diff Context")),
                                                  ")")) +
                                  xlab("Jaro distance") +
                                  ylab("Random sample mean") +
                                  customTheme

# Randomly drawn Jaro distance, empirical Diff Male/Same Context COP vs. Diff Male/Same Context AUDI plus SOLO
randomDistribution_Jaro_DiffMaleAcrossContexts <- readRDS("Output/randomDistribution_Jaro_DiffMaleAcrossContexts.rds") |>
                                               mutate(Category = "Random",
                                                      Jaro_Distance=Mean) |>
                                               select(Category, Jaro_Distance)

plot_randComp_jaroDiffMaleAcrossContext <- ggplot(randomDistribution_Jaro_DiffMaleAcrossContexts) +
                                        geom_vline(xintercept=empiricalCop_Jaro_DiffMaleSameContext_MEAN, 
                                                   colour="red", linetype="dashed") +
                                        geom_histogram(aes(x=Jaro_Distance, fill=Category),
                                                       alpha=0.3, colour="black", bins=40) +
                                        geom_point(data=empiricalCop_Jaro_DiffMaleSameContext,
                                                   aes(x=Jaro_Distance), 
                                                   fill="black", shape=21, alpha=0.75, y=50, size=2) +
                                        scale_x_continuous(limit = c(0, 1), oob = function(x, limits) x) +
                                        # scale_y_continuous(limits=c(0, 3500), breaks=seq(0, 3500, by=1000), oob = function(y, limits) y) +
                                        scale_fill_manual(values=categoryColors) +
                                        guides(colour="none", fill="none") +
                                        ggtitle(paste0("Empirical: COP vs. Diff Male/Same Context COP (n = ", 
                                                      nrow(filter(distances, Category_1=="COP" & Comparison_Type=="Diff Male/Same Context")),
                                                      ")\n",
                                                      "Randomized: AUDI or SOLO vs. Diff Male/Same Context (n = ",
                                                      nrow(filter(distances, Category_1=="COP" & Comparison_Type=="Diff Male/Same Context")),
                                                      "/", nrow(filter(distances, Category_1%in%c("AUDI", "SOLO") & Comparison_Type=="Diff Male/Same Context")),
                                                      ")")) +
                                        xlab("Jaro distance") +
                                        ylab("Random sample mean") +
                                        customTheme +
                                        theme(plot.title = element_text(size=11.5))

# Create combined plot and write to file              
plots_randComparionsJaro <- plot_randComp_jaroSameDiffMaleCOP /
                            plot_randComp_jaroDiffMaleAcrossContext

ggsave(plots_randComparionsJaro, file="Plots/FIGURE_S6.png", width=6, height=5) 

# TABLE S6 ---------------------------------------------
# COP vs. after-COP displays, raw element frequencies
# [Filter] to include only COP displays
# [Select] the before- and after- display codes 
#          NOTE includes both "Attempted Copulation" and "Copulation" elements
#               which are excluded in the main analyses
# [Pivot longer] So each row is one DisplayCode (before or after)
# [Separate] DisplayCode into single-character elements
# [Pivot longer] So each row is on behavioral element 
# [Group by] Section (i.e., before or after first copulation) and Code
# [Tally] The frequency of elements for before- and after
# [Pivot wider] so each row is an individual element, 
#               including columns for Before- and After- copulation frequencies
# [Right join] to full-dataset behavioral element frequency table, which gives us
#              a row for elements even if they are missing from the COP or after-COP displays
#              NOTE also gives us the "Element" short name col
# [Select] Only Code, Element, Before, and After cols
# [Mutate] replace NAs with 0s in the Before and After cols
# [Arrange] alphabetical code order
table_s6 <- data_analyzed |>
         filter(Category=="COP") |>
         select(Before=DisplayCode_withCops, After=DisplayCode_AfterCop) |>
         pivot_longer(cols=everything(), names_to="Section", values_to="DisplayCode") |>
         separate(DisplayCode, into=as.character(0:400), sep="", fill="right") |>
         pivot_longer(-c(Section), names_to="Index", values_to="Code") |>
         filter(Code!="" & !is.na(Code)) |>
         group_by(Section, Code) |>
         tally() |>
         pivot_wider(id_cols=Code, names_from=Section, values_from=n) |>
         mutate(Element = map_chr(Code, ~ names(behavior_code)[behavior_code==.]),
                .after=Code) |>
         select(Code, Element, Before, After) |>
         mutate(across(c(Before, After), ~ifelse(is.na(.x), 0, .x))) |>
         arrange(Code)

# Write table to file
write_csv(table_s6, file="Output/TABLE_S6.csv")

# TABLE S7 ---------------------------------------------
# Full display strings of each before- and after-copulation displays
# [Filter] Copulation displays only
# [Select] Before and After display code columns 
#          (choosing the version of the main, before-COP displaycode that includes
#           Attempted copulation and Copulation elements)
# [Mutate] Mark whether After-copulation displays have attempted or succesful copulations
# [Mutate] Add line breaks to display strings
table_s7 <- data_analyzed |>
         filter(Category=="COP") |>
         select(UID, Male1ID, Before=DisplayCode_withCops, After=DisplayCode_AfterCop) |>
         mutate(After_HasAttC = grepl(behavior_code["AttC"], After),
                After_HasCop = grepl(behavior_code["Cop"], After)) |>
         mutate(Before = map_chr(Before, displayCodeLineBreak),
                After = map_chr(After, displayCodeLineBreak))

# Write table to file
write_csv(table_s7, file="Output/TABLE_S7.csv")

# FIGURE S7 ---------------------------------------------

# Read in before vs. after cop comparisons
afterCop_comparison <- readRDS("Output/afterCop_comparison.rds")

# Custom function to plot brackets with T-test for 
#   before and after cop comparisons
geom_tTestBracket <- function(metric, min, max) {
    # Widen by-UID comparison dataset for t.test 
    comp <- afterCop_comparison |>
         select(UID, Section, all_of(metric)) |>
         pivot_wider(id_cols=UID, names_from=Section, values_from=metric)

    # Pairwise t.test
    t <- t.test(x=comp$Before, y=comp$After, 
                alternative = "two.sided",
                paired=TRUE)

    p <- t$p.value

    label <- ""
    linetype <- "dashed"
    if (p <= 0.001) {label<-"***"; linetype<-"solid"}
    else if (p <= 0.01) {label<-"**"; linetype<-"solid"}
    else if (p <= 0.05) {label<-"*"; linetype<-"solid"}

    range <- max-min
    y = min + range * 0.90
    offset <- max*0
    bracketCorner <- range * 0.015
    
    g1 <- geom_segment(x=0.8, xend=2.2, 
                       y=y, yend=y, 
                       linetype=linetype, colour="gray",
                       linewidth=0.35)
    g2 <- geom_segment(x=0.8, xend=0.8, 
                       y=y+bracketCorner, yend=y-bracketCorner, 
                       colour="gray")
    g3 <- geom_segment(x=2.2, xend=2.2, 
                       y=y+bracketCorner, yend=y-bracketCorner, 
                       colour="gray")
    g4 <- geom_text(label=label, x=1.5, 
                    y=y+offset, colour="gray", vjust=0, size=3)
    
    return(list(g1, g2, g3, g4))
}

# Boxplot of Before vs. After copulation display length
plot_afterCop_uniqueElements <- ggplot(afterCop_comparison, 
                                       aes(x=Section, y=UniqueDisplayElements, fill=Section)) +
                             geom_tTestBracket("UniqueDisplayElements", 1, 6) +
                             geom_point(alpha=0.75, size=0.5, colour="black") +
                             geom_line(aes(group=UID)) +
                             geom_boxplot(alpha=0.5, outlier.shape=NA) +
                             scale_x_discrete(limits=c("Before", "After"), 
                                              labels=c("Before\ncopulation", "After\ncopulation")) +
                             scale_y_continuous(limits=c(1, 6)) +                                              
                             scale_fill_manual(values=categoryColors) +
                             ylab("Distinct elements") +
                             guides(fill="none") +
                             customTheme +
                             theme(axis.title.x=element_blank())

# Boxplot of Before vs. After copulation entropy
plot_afterCop_entropy <- ggplot(afterCop_comparison, 
                                aes(x=Section, y=Entropy_Scaled, fill=Section)) +
                      geom_tTestBracket("Entropy_Scaled", 0, 1.05) +                      
                      geom_point(alpha=0.75, size=0.5, colour="black") +
                      geom_line(aes(group=UID)) +
                      geom_boxplot(alpha=0.5, outlier.shape=NA) +
                      scale_x_discrete(limits=c("Before", "After"), 
                                       labels=c("Before\ncopulation", "After\ncopulation")) +
                      scale_y_continuous(limits=c(0, 1.05), breaks=seq(0, 1, by=0.2)) +                                       
                      scale_fill_manual(values=categoryColors) +
                      xlab("Display section") +
                      ylab("Entropy (scaled)") +
                      guides(fill="none") +
                      customTheme

# Boxplot of Before vs. After compression ratio
plot_afterCop_compression <- ggplot(afterCop_comparison, 
                                    aes(x=Section, y=Compression_Ratio, fill=Section)) +
                          geom_tTestBracket("Compression_Ratio", 0, 6.5) +
                          geom_point(alpha=0.75, size=0.5, colour="black") +
                          geom_line(aes(group=UID)) +
                          geom_boxplot(alpha=0.5, outlier.shape=NA) +
                          scale_x_discrete(limits=c("Before", "After"), 
                                           labels=c("Before\ncopulation", "After\ncopulation")) +
                          scale_y_continuous(limits=c(0, 6.5)) +                                           
                          scale_fill_manual(values=categoryColors) +
                          ylab("Compression ratio") +
                          guides(fill="none") +
                          customTheme +
                          theme(axis.title.x=element_blank())

# Combine plots and save to file
plots_afterCop <- plot_afterCop_uniqueElements +
                  plot_afterCop_entropy +
                  plot_afterCop_compression

ggsave(plots_afterCop, file="Plots/FIGURE_S7.png", width=6, height=2)

# Before vs. After Jaro distances

# Read in distances 
beforeAfterDistances <- readRDS("Output/beforeAfterDistances.rds")

# Separate boxplots with Before vs. (A) Other before, (B) Other after, (C) same after)
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

# Save to file
ggsave(plot_beforeAfterJaro, file="Plots/FIGURE_S8.png", width=4, height=5)                    

# TABLE S8 ---------------------------------------------

elements_femOn_beforeAfter <- map2_df(afterCop_comparison$Section, afterCop_comparison$DisplayCode_FemOn, 
                                      ~ tibble(Element = str_split(.y, "")[[1]]) |> 
                                               group_by(Element) |> 
                                               tally() |> 
                                               mutate(Section = .x) |>
                                               filter(!is.na(Element))) |>
                            group_by(Section, Element) |>
                            summarize(.groups="keep", n = sum(n)) |>
                            group_by(Section) |>
                            mutate(Total = sum(n)) |>
                            mutate(Perc = as.character(round((n / Total) * 100, 0))) |>
                            mutate(Perc = ifelse(Perc < 1, "<1", Perc)) |>
                            mutate(Section = paste0(Section, "_FemOn"))

elements_femOff_beforeAfter <- map2_df(afterCop_comparison$Section, afterCop_comparison$DisplayCode_FemOff, 
                                      ~ tibble(Element = str_split(.y, "")[[1]]) |> 
                                               group_by(Element) |> 
                                               tally() |> 
                                               mutate(Section = .x) |>
                                               filter(!is.na(Element))) |>
                            group_by(Section, Element) |>
                            summarize(.groups="keep", n = sum(n)) |>
                            group_by(Section) |>
                            mutate(Total = sum(n)) |>
                            mutate(Perc = as.character(round((n / Total) * 100, 0))) |>
                            mutate(Perc = ifelse(Perc < 1, "<1", Perc)) |>
                            mutate(Section = paste0(Section, "_FemOff"))

elements_femUp_beforeAfter <- map2_df(afterCop_comparison$Section, afterCop_comparison$DisplayCode_FemUp, 
                                      ~ tibble(Element = str_split(.y, "")[[1]]) |> 
                                               group_by(Element) |> 
                                               tally() |> 
                                               mutate(Section = .x) |>
                                               filter(!is.na(Element))) |>
                            group_by(Section, Element) |>
                            summarize(.groups="keep", n = sum(n)) |>
                            group_by(Section) |>
                            mutate(Total = sum(n)) |>
                            mutate(Perc = as.character(round((n / Total) * 100, 0))) |>
                            mutate(Perc = ifelse(Perc < 1, "<1", Perc)) |>
                            mutate(Section = paste0(Section, "_FemUp"))

elements_femDown_beforeAfter <- map2_df(afterCop_comparison$Section, afterCop_comparison$DisplayCode_FemDown, 
                                        ~ tibble(Element = str_split(.y, "")[[1]]) |> 
                                                 group_by(Element) |> 
                                                 tally() |> 
                                                 mutate(Section = .x) |>
                                                 filter(!is.na(Element))) |>
                             group_by(Section, Element) |>
                             summarize(.groups="keep", n = sum(n)) |>
                             group_by(Section) |>
                             mutate(Total = sum(n)) |>
                             mutate(Perc = as.character(round((n / Total) * 100, 0))) |>
                             mutate(Perc = ifelse(Perc < 1, "<1", Perc)) |>
                             mutate(Section = paste0(Section, "_FemDown"))

table_s8 <- bind_rows(elements_femOn_beforeAfter, elements_femOff_beforeAfter, elements_femUp_beforeAfter, elements_femDown_beforeAfter) |>
         select(Element, Section, Perc) |>
         pivot_wider(id_cols = Element, names_from = Section, values_from = Perc) |>
         select(Code = Element, 
                Before_FemOff, After_FemOff, 
                Before_FemOn, After_FemOn, 
                Before_FemUp, After_FemUp, 
                Before_FemDown, After_FemDown) |>
         mutate(Element = map_chr(Code, ~ names(behavior_code)[behavior_code==.]),
                .after=Code) |>
         arrange(Code)

# Write table to file            
write_csv(table_s8, file="Output/TABLE_S8.csv")

## Calculate total male behavioral elements corresponding to female behavior subsets     
table_s8_totals <- bind_rows(elements_femOn_beforeAfter, elements_femOff_beforeAfter, elements_femUp_beforeAfter, elements_femDown_beforeAfter) |>
                group_by(Section, Total) |>
                tally() |>
                mutate(drop="drop") |>
                pivot_wider(id_cols = drop, names_from = Section, values_from=Total) |>
                select(Before_FemOff, After_FemOff, 
                       Before_FemOn, After_FemOn, 
                       Before_FemUp, After_FemUp, 
                       Before_FemDown, After_FemDown)

# Write table to file            
write_csv(table_s8_totals, file="Output/TABLE_S8_TOTALS.csv")