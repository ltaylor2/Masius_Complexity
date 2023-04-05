cat("Beginning Masius analysis.\n")

cat("Loading packages and local values.\n")
source("Scripts/0_logistics.r")

cat("Parsing data.\n")
source("Scripts/1_parse_data.r")

cat("Analyzing data.\n")
source("Scripts/2_analyze.r")

cat("Producing plots.\n")
source("Scripts/3_plot.r")

cat("Finished Masius analysis.\nSee 'Output/' and 'Plots/' folders in working directory.\nExiting.\n")