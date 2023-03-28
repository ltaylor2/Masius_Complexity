# Custom plotting theme
customTheme <- theme_bw() +
            theme(panel.grid=element_blank(),
                  axis.text.x=element_text(size=6),
                  axis.text.y=element_text(size=6),
                  axis.title.x=element_text(size=9),
                  axis.title.y=element_text(size=9),
                  plot.title=element_text(size=9))
            
# 4-Class qualitative colors, colorblind safe
# from colorbrewer2.org
categoryColors <- c("SOLO" = "#a6cee3",
                    "AUDI" = "#b2df8a",
                    "COP" = "#2c4a11")

# Read in analyzed data
data_analyzed <- read_csv(ANALYZED_DATA_PATH, show_col_types=FALSE)

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

















