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
                    "COP" = "#2c4a11")

# Read in analyzed data
data_analyzed <- read_csv(ANALYZED_DATA_PATH, show_col_types=FALSE)

# Load jaro distances
# [Mutate] Category_1 to Factor to order facets in plot
distances <- readRDS("Output/distances.rds") |>
          mutate(Category_1 = factor(Category_1, levels=c("SOLO", "AUDI", "COP")))

# TABLE 1 ------------------------------------------------------
# Table of core behavioral elements and descriptions, with category-specific frequencies
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













