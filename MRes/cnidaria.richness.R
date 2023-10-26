library(ggplot2)
library(dplyr)
library(tidyr)
library(vegan)

filtered_df_Cnidaria<- all_depth_zones %>%
  filter(grepl("Cnidaria", label_hierarchy))

cnidaria_abundance_matrix <- filtered_df_Cnidaria %>%
  group_by(depth_zone, label_name) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = label_name, values_from = count, values_fill = 0)


# order matrix into ascending depth zones 

desired_depth_order <- c("0-200m", "200-400m", "400-600m", "600-800m", "1000-1200m", "1200-1400m")

cnidaria_abundance_matrix $depth_zone <- factor(
  cnidaria_abundance_matrix $depth_zone,
  levels = desired_depth_order
)
cnidaria_abundance_matrix <- cnidaria_abundance_matrix %>%
  arrange(depth_zone)


cnidaria_abundance_matrix <- as.data.frame(cnidaria_abundance_matrix )


# Print the names of non-numeric columns
print(names(cnidaria_abundance_matrix )[non_numeric_columns])

# we need to convert the depth_zone names to numeric 
depth_zone_mapping <- c('0-200m' = 1, '200-400m' = 2, '400-600m' = 3, '600-800m' = 4, '1000-1200m' = 5, '1200-1300m' = 6)

# Replace depth zone labels with integers based on the mapping
cnidaria_abundance_matrix$depth_zone <- depth_zone_mapping[cnidaria_abundance_matrix$depth_zone]

# calculate morphospecies richness for each depth zone 

depth_zones <-cnidaria_abundance_matrix[, 1]

# Initialize an empty vector to store species richness for each depth zone
species_richness <- numeric(length(depth_zones))

# Loop through each depth zone
for (i in 1:length(depth_zones)) {
  # Calculate species richness by counting the number of non-zero values in the row
  species_richness[i] <- sum(cnidaria_abundance_matrix [i, -1] > 0)
}

# Create a data frame with depth zones and their corresponding species richness
depth_cnidaria_richness <- data.frame(DepthZone = depth_zones, SpeciesRichness = species_richness)

# Print or export the depth_species_richness data frame as needed
print(depth_cnidaria_richness)


depth_cnidaria_richness<- depth_cnidaria_richness %>% mutate(Transect = c("5", "9", "11", "5", "4", "6"))

depth_cnidaria_richness$SpeciesRichness <- as.numeric(depth_cnidaria_richness$SpeciesRichness)
depth_cnidaria_richness$Transect <- as.numeric(depth_cnidaria_richness$Transect)



depth_cnidaria_richness$StandardizedRichness <- depth_cnidaria_richness$SpeciesRichness / depth_cnidaria_richness$Transect

# Plot result
ggplot(data = depth_cnidaria_richness, aes(x = factor(DepthZone), y = StandardizedRichness)) +
  geom_bar(stat = "identity", width = 0.85, fill = "pink") +
  labs(x = "Depth Zone", y = "Standardized Richness") +
  ggtitle("") +
  theme_minimal()+
  theme(axis.text = element_text(size = 14),  # Adjust the size as needed
        axis.title = element_text(size = 16, face = "bold"))



# calculate richness from rarefied matrix 

#rarefy based on lowest abundance value in depth zone 
rarefied_cnidaria <- rrarefy(cnidaria_abundance_matrix, 32)


# generate repeats 

# Set the target rarefaction level
target_rarefaction_level <- 32

# Number of repeats
num_repeats <- 50

# Initialize a list to store the rarefied matrices
rarefied_matrices <- list()

# Loop to generate rarefied matrices
for (i in 1:num_repeats) {
  # Generate a rarefied matrix
  rarefied_matrix <- rrarefy(cnidaria_abundance_matrix, sample = target_rarefaction_level)
  
  # Store the rarefied matrix in the list
  rarefied_matrices[[i]] <- rarefied_matrix
}




# Initialize an empty vector to store the average richness for each depth zone
average_richness_per_depth <- numeric(nrow(rarefied_matrices[[1]]))

# Loop through each depth zone
for (depth_zone in 1:nrow(rarefied_matrices[[1]])) {
  # Extract the richness values for the current depth zone from all matrices
  richness_values <- sapply(rarefied_matrices, function(mat) sum(mat[depth_zone, ] > 0))
  
  # Calculate the average richness for the current depth zone
  average_richness <- mean(richness_values)
  
  # Store the average richness in the vector
  average_richness_per_depth[depth_zone] <- average_richness
}


cnidaria_richness_rarefied <- apply(rarefied_cnidaria, 1, function(row) {
  sum(row > 0)  # Count non-zero values in each row
})

cnidaria_richness_rarefied 
barplot(cnidaria_richness_rarefied)

df <- data.frame(DepthZone = DepthZone, SpeciesRichness = cnidaria_richness_rarefied)

ggplot(df, aes(x = factor(DepthZone), y = SpeciesRichness)) +
  geom_bar(stat = "identity", width = 0.7, fill = "darkseagreen3") +
  labs(x = "Depth Zone", y = "Rarefied Species Richness") +
  ggtitle("Rarefied Species Richness by Depth Zone") +
  theme_minimal()
