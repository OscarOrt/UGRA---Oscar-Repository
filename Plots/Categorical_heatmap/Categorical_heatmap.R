# This script is an example about how to plot a heatmap with categorical data

# Install packages (in case those haven't been installed before)
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("tidyR")
#install.packages("RColorBrewer")

# Load packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)

# Load data
setwd("~/GitHub/UGRA---Oscar-Repository/Plots/Categorical_heatmap") # Move to data location
data <- read.csv2("Categorical_heatmap_example.csv",stringsAsFactors = FALSE)
head(data)

# Transform to long format
data_tidy<- data %>% tidyr::gather(Variable, Category, 1:10) # Number of variables, always put the in ID the last column
head(data_tidy)

# Transform to factors
data_tidy$Sample <- factor(data_tidy$Sample, levels = rev(paste0("Patient_", 1:7))) # Samples to factors
data_tidy$Variable <- factor(data_tidy$Variable, levels = paste0("Gene_", 1:10)) # Variables to factors

colours_vector = c() # This loop create different categories for each column
for (i in 1:nrow(data_tidy)){
  temp = 0
  if (data_tidy[i,"Category"] > 0){
    temp = as.numeric(data_tidy[i,"Variable"])
  }
  colours_vector = c(colours_vector,temp)
}

data_tidy$Colours <- factor(colours_vector) # Number to categorical
head(data_tidy)

# Colour palette
par(mar=c(3,4,2,2))
display.brewer.all()
display.brewer.all(colorblindFriendly = TRUE)

colour_palette <- brewer.pal(12, "Paired") # Select the palette according to the number of variables
colour_palette

# Create plot
ggplot(data_tidy, aes(x=Variable, y=Sample, fill=Colours)) +
  geom_tile(color="black", size=0.5) + # Line colours and size
  coord_equal() +
  labs(x=NULL, y=NULL) +  # Remove labs (title="Mutation spectrum of nn cases")
  theme(axis.ticks = element_blank(), # Remove ticks in the axis
        axis.text = element_text(size=12), # Size labels text
        axis.text.x = element_text(angle = 90, hjust = 0), # X labels angle and aligned to the bottom
        legend.position = "none", # Remove legends
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) + # Remove background lines
  scale_x_discrete(position = "top") + # Move X labels to top
  scale_fill_manual(values = c("#FFFFFF", colour_palette))
