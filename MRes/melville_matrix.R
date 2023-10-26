# Load required packages
library(dplyr)
library(tidyr)
library(vegan)
library(tidyverse)
install.packages('ggvegan')
if (!require(devtools)) {
  install.packages("devtools")
}
devtools::install_github("gavinsimpson/ggvegan")
library(ggvegan)

# Load the CSV files of Melville dives

dive7c <- read.csv("/Users/user/Desktop/metadata_flow/completed_metadata_files/11056-dive7-file3.csv")
dive8c <- read.csv("/Users/user/Desktop/metadata_flow/completed_metadata_files/merged_DIVE8.csv")
dive9c<- read.csv("/Users/user/Desktop/metadata_flow/completed_metadata_files/merged_DIVE9.csv") 
dive10c <- read.csv("/Users/user/Desktop/metadata_flow/completed_metadata_files/merged_DIVE10.csv")

# Define your depth zones 
depth_zones <- c("Zone 1", "Zone 2", "Zone 3", "Zone 4", "Zone 5", "Zone 6", "Zone 7", "Zone 8", 'Zone 9', "Zone 10", "Zone 11", 'Zone 12')

# Define your depth breaks 
depth_breaks <- c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1200, 1400)

# Assign depth zone labels based on depth information 

dive7c$depth_zone<-cut(dive7$ROV.RovDepth,breaks = depth_breaks,labels=depth_zones,include.lowest = TRUE)
dive8c$depth_zone <- cut(dive8$ROV.RovDepth, breaks = depth_breaks, labels = depth_zones, include.lowest = TRUE)
dive9c$depth_zone <- cut(dive9$ROV.RovDepth, breaks = depth_breaks, labels = depth_zones, include.lowest = TRUE)
dive10c$depth_zone <- cut(dive10$ROV.RovDepth, breaks = depth_breaks, labels = depth_zones, include.lowest = TRUE)

colnames(dive7c) # check that new depth zone column has been included

# Now that each of the annotations have been assigned a depth zone, we can make a species abundance matrix 

# First combine the dataframes for all the Melville dives
melville_combined_data<-rbind(dive7c,dive8c,dive9c,dive10c)

# Count the occurrences of each species in each depth zone
species_abundance_matrix_100m_zones <- melville_combined_data %>%
  group_by(depth_zone, label_name) %>%
  summarise(count = n()) %>%
  spread(key = label_name, value = count, fill = 0)

view(species_abundance_matrix_100m_zones)


# Calculate the minimum count of species for each depth zone
min_species_per_zone <- apply(species_abundance_matrix_100m_zones[, -1], 1, min)

# Print the minimum count of species for each depth zone
print(min_species_per_zone)
# unequal sampling effort means we should standardize the data 

# rarefaction involves selecting a specified number of samples  that is equal
# to or less than the number of samples in the smallest sample, then randomly
# discarding reads from  larger samples until the number of remaining samples is 
# equal ro this threshold size.

# Based on these subsamples of equal size, diversity metrics can be calculated
# to compare sites 'fairly', independently of differences in sample sizes (Weiss et al. 2017)

# calculate total abundance for each site 
# 2) perform rarefaction 
# 3) calculate diversity metrics 

# Calculate diversity metrics 1) Species richness 2) Shannon and 3) Simpson 


# Assuming species_abundance_matrix_100m_zones is your abundance matrix
# Replace with your actual abundance matrix



# Convert the abundance matrix to numeric, handling non-numeric values
abundance_matrix_numeric <- as.numeric(as.matrix(species_abundance_matrix_100m_zones))
abundance_matrix_numeric

# Calculate observed richness for each depth zone
observed_richness <- apply(abundance_matrix_numeric, 1, function(x) sum(x > 0))

# Create a data frame with depth zones and observed richness
depth_richness_df <- data.frame(DepthZone = rownames(abundance_matrix_numeric), ObservedRichness = observed_richness)

# Calculate the accumulation curve
accum_curve <- specaccum(list(counts = abundance_matrix_numeric), method = "random", permutations = 100)

# Plot the accumulation curve
plot(accum_curve, xlab = "Number of Samples (Depth Zones)", ylab = "Number of Observed Species", main = "Species Accumulation Curve")

# Add observed richness to the plot
points(x = 1:length(observed_richness), y = observed_richness, col = "red", pch = 19)

# Add legend
legend("topleft", legend = c("Accumulation Curve", "Observed Richness"), col = c("black", "red"), pch = c(1, 19))

# Print the observed richness for each depth zone
print(depth_richness_df)

