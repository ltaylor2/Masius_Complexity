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
customDisplayTypeColors <- c("SOL" = "#a6cee3",
							 "MUL" = "#1f78b4",
							 "AUD" = "#b2df8a",
							 "COP" = "#33a02c")

# Read in DATA
# TODO TODO TODO TODO TODO 
# *********TEMPORARY*********
# Right now we have 3 "CHECK" displays -- 
#	displays that appear to have multiple males and a female present
#	confirm how to code these
# 1. Factor display types to maintain order on plots
displayTypeFctLevels <- c("SOL", "MUL", "AUD", "COP", "CHECK")
data_analyzed <- read_csv(ANALYZED_DATA_PATH, show_col_types=FALSE) |>
			  filter(DisplayType != "CHECK") |>
			  mutate(DisplayType = factor(DisplayType, levels=displayTypeFctLevels))

# PLOT (boxplot) of display length
plot_displayLength <- ggplot(data_analyzed) +
				   geom_jitter(aes(x=DisplayType, y=DisplayLength),
				   		 	   colour="black",
				   			   width=0.15, height=0) +
				   geom_boxplot(aes(x=DisplayType, y=DisplayLength,
				   				 fill=DisplayType),
				   			    colour="black",
				   			    alpha=0.5) +
				   scale_fill_manual(values=customDisplayTypeColors) +
				   guides(fill="none") +
				   xlab("Display type") +
				   ggtitle("Display length (elements)") +
				   customTheme +
				   theme(axis.title.x=element_blank(),
				   		 axis.title.y=element_blank())

# PLOT (boxplot) of display elements
plot_displayElements <- ggplot(data_analyzed) +
				     geom_jitter(aes(x=DisplayType, y=UniqueDisplayElements),
				     		 	   colour="black",
				     			   width=0.15, height=0, alpha=0.5) +
				     geom_boxplot(aes(x=DisplayType, y=UniqueDisplayElements,
				     				 fill=DisplayType),
				     			    colour="black",
				     			    alpha=0.5) +
				     scale_fill_manual(values=customDisplayTypeColors) +
				     scale_y_continuous(breaks=seq(0, 12, by=2)) +
				     guides(fill="none") +
				     xlab("Display type") +
				     ggtitle("Unique elements") +
				     customTheme +
				     theme(axis.title.x=element_blank(),
				     	   axis.title.y=element_blank())

# PLOT (boxplot) of display element ratio
plot_elementRatio <- ggplot(data_analyzed) +
				  geom_jitter(aes(x=DisplayType, y=UniqueElementRatio),
				  		 	  colour="black",
				  			  width=0.15, height=0, alpha=0.5) +
				  geom_boxplot(aes(x=DisplayType, y=UniqueElementRatio,
				  				   fill=DisplayType),
				  			   colour="black",
				  			   alpha=0.5) +
				  scale_fill_manual(values=customDisplayTypeColors) +
				  guides(fill="none") +
				  xlab("Display type") +
				  ggtitle("Unique elements / length") +
				  customTheme +
				  theme(axis.title.x=element_blank(),
				  		axis.title.y=element_blank())

# PLOT (boxplot) of entropy
plot_entropy <- ggplot(data_analyzed) +
			 geom_jitter(aes(x=DisplayType, y=Entropy),
			 		 	 colour="black",
			 			 width=0.15, height=0, alpha=0.5) +
			 geom_boxplot(aes(x=DisplayType, y=Entropy,
			 				  fill=DisplayType),
			 			  colour="black",
			 			  alpha=0.5) +
			 scale_fill_manual(values=customDisplayTypeColors) +
			 guides(fill="none") +
			 xlab("Display type") +
			 ggtitle("Entropy") +
			 customTheme +
			 theme(axis.title.x=element_blank(),
			 		axis.title.y=element_blank())

# PLOT (boxplot) of compression ratio
plot_compressionRatio <- ggplot(data_analyzed) +
			 		  geom_jitter(aes(x=DisplayType, y=CompressionRatio),
			 		  		 	  colour="black",
			 		  			  width=0.15, height=0, alpha=0.5) +
			 		  geom_boxplot(aes(x=DisplayType, y=CompressionRatio,
			 		  				   fill=DisplayType),
			 		  			   colour="black",
			 		  			   alpha=0.5) +
			 		  scale_fill_manual(values=customDisplayTypeColors) +
			 		  guides(fill="none") +
			 		  xlab("Display type") +
			 		  ggtitle("Compression ratio") +
			 		  customTheme +
			 		  theme(axis.title.y=element_blank())

# PLOT (boxplot) of local complexity
plot_localComplexity <- ggplot(data_analyzed) +
			 		 geom_jitter(aes(x=DisplayType, y=LocalComplexity),
			 		 		 	 colour="black",
			 		 			 width=0.15, height=0, alpha=0.5) +
			 		 geom_boxplot(aes(x=DisplayType, y=LocalComplexity,
			 		 				  fill=DisplayType),
			 		 			  colour="black",
			 		 			  alpha=0.5) +
			 		 scale_fill_manual(values=customDisplayTypeColors) +
			 		 guides(fill="none") +
			 		 xlab("Display type") +
			 		 ggtitle("Local complexity (window mean)") +
			 		 customTheme +
			 		 theme(axis.title.x=element_blank(),
			 		 	   axis.title.y=element_blank())

# FIGURE 1 OUTPUT
# (assemble subplots via patchwork package)
FIGURE_1 <- (plot_displayLength + plot_displayElements + plot_elementRatio) /
			(plot_entropy + plot_compressionRatio + plot_localComplexity)
ggsave(FIGURE_1, file="Plots/FIGURE_1.png", width=7.5, height=6)