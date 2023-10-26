# richness of cnidaria vs porifera with depth on Melville Bank 

filtered_df_Porifera <- all_depth_zones %>%
  filter(grepl("Porifera", label_hierarchy))

porifera_abundance_matrix <- filtered_df_Porifera %>%
  group_by(depth_zone, label_name) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = label_name, values_from = count, values_fill = 0)

class(porifera_abundance_matrix)

# order matrix into ascending depth zones 

desired_depth_order <- c("0-200m", "200-400m", "400-600m", "600-800m", "1000-1200m", "1200-1400m")

porifera_abundance_matrix$depth_zone <- factor(
  porifera_abundance_matrix$depth_zone,
  levels = desired_depth_order
)
porifera_abundance_matrix<- porifera_abundance_matrix %>%
  arrange(depth_zone)


porifera_abundance_matrix<- as.data.frame(porifera_abundance_matrix)


# Print the names of non-numeric columns
print(names(porifera_abundance_matrix)[non_numeric_columns])

# we need to convert the depth_zone names to numeric 
depth_zone_mapping <- c('0-200m' = 1, '200-400m' = 2, '400-600m' = 3, '600-800m' = 4, '1000-1200m' = 5, '1200-1300m' = 6)

# Replace depth zone labels with integers based on the mapping
porifera_abundance_matrix$depth_zone <- depth_zone_mapping[porifera_abundance_matrix$depth_zone]


# calculate morphospecies richness for each depth zone 

depth_zones <-porifera_abundance_matrix[, 1]

# Initialize an empty vector to store species richness for each depth zone
species_richness <- numeric(length(depth_zones))

# Loop through each depth zone
for (i in 1:length(depth_zones)) {
  # Calculate species richness by counting the number of non-zero values in the row
  species_richness[i] <- sum(porifera_abundance_matrix[i, -1] > 0)
}

# Create a data frame with depth zones and their corresponding species richness
depth_porifera_richness <- data.frame(DepthZone = depth_zones, SpeciesRichness = species_richness)

# Print or export the depth_species_richness data frame as needed
print(depth_porifera_richness)

depth_porifera_richness$SpeciesRichness <- as.numeric(depth_porifera_richness$SpeciesRichness)
depth_porifera_richness$Transect <- as.numeric(depth_porifera_richness$Transect)


depth_porifera_richness<- depth_porifera_richness %>% mutate(Transect = c("5", "9", "11", "5", "4", "6"))

depth_porifera_richness$StandardizedRichness <- depth_porifera_richness$SpeciesRichness / depth_porifera_richness$Transect

# Plot result
ggplot(data = depth_porifera_richness, aes(x = factor(DepthZone), y = StandardizedRichness)) +
  geom_bar(stat = "identity", width = 0.85, fill = "darkseagreen3") +
  labs(x = "Depth Zone", y = "Standardized Richness") +
  ggtitle("") +
  theme_minimal()+
  theme(axis.text = element_text(size = 14),  # Adjust the size as needed
                          axis.title = element_text(size = 16, face = "bold"))




# grouped bar chart for porifera and cnidaria 

depth_cnidaria_richness$Group <- "Cnidaria"
depth_porifera_richness$Group <- "Porifera"

combined_data <- rbind(depth_cnidaria_richness, depth_porifera_richness)

# Create a grouped bar plot
p <- ggplot(data = combined_data, aes(x = factor(DepthZone), y = StandardizedRichness, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.85)) +
  labs(x = "Depth Zone", y = "Standardized Richness") +
  ggtitle("") +
  theme_minimal() +
  theme(axis.text = element_text(size = 14),  # Adjust the size as needed
        (axis.title = element_text(size = 16))face = "bold") + 
  theme(legend.key.size = unit(2, "cm"))

# Customize the legend
p + guides(fill = guide_legend(title = "Group"))






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




# trying to generate standard deviations for the plot 


combined_data <- list(dive8_nineteenth50,
                      dive8_twentieth50,
                      dive9_seventh50,
                      dive9_eighth50,
                      dive9_ninth50,
                      dive8_sixteenth50,
                      dive8_seventeenth50,
                      dive8_eighteenth50,
                      dive9_first50, 
                      dive9_second50,
                      dive9_third50, 
                      dive9_fourth50,
                      dive9_fifth50,
                      dive8_second50,
                      dive8_third50,
                      dive8_fourth50,
                      dive8_fifth50,
                      dive8_sixth50,
                      dive8_seventh50,
                      dive8_eighth50,
                      dive8_ninth50,
                      dive8_tenth50,
                      dive8_eleventh50,
                      dive8_twelfth50,
                      dive7_first50 ,
                      dive7_second50,
                      dive7_third50,
                      dive7_fourth50,
                      dive7_fifth50,
                      dive10_seventh50,
                      dive10_eighth50,
                      dive10_ninth50,
                      dive10_tenth50,
                      dive10_first50,
                      dive10_second50,
                      dive10_third50,
                      dive10_fourth50,
                      dive10_fifth50,
                      dive10_sixth50)



dive7_fifth50  <- dive7_fifth50 %>%
  mutate(color = NA)

# Create a sequence of transect labels from 1 to 39
transect_labels <- as.character(1:39)

# Add the "transect" column to each data frame
combined_data <- lapply(seq_along(combined_data), function(i) {
  combined_data[[i]]$transect <- transect_labels[i]
  return(combined_data[[i]])
})

# Combine the list of data frames into a single data frame
combined_dataframe <- bind_rows(combined_data)

# filter for VME taxa 
filtered_df_VME <- combined_dataframe%>%
  filter(grepl("Porifera", label_hierarchy))


porif_abundance_data <- filtered_df_VME %>%
  group_by(transect, label_name, depth_zone) %>%
  summarise(abundance = n())

porif_abundance_data<- porif_abundance_data %>%
  pivot_wider(names_from = label_name, values_from = abundance, values_fill = 0)

porif_abundance_data<- as.data.frame(porif_abundance_data)

View(porif_abundance_data)



# calculating mean richness and standard deviations for PORIFERA 


library(dplyr)

# Calculate species richness within each transect (row)
porif_abundance_data <- porif_abundance_data %>%
  mutate(Richness = rowSums(select(., -depth_zone, -transect) > 0))

# Calculate mean richness and standard deviation for each depth zone
richness_summary <- porif_abundance_data%>%
  group_by(depth_zone) %>%
  summarise(
    MeanRichness = mean(Richness),
    StdDevRichness = sd(Richness)
  )

# View the summary
print(richness_summary)

# re-order depth zones 

desired_order <- c("0-200m", "200-400m", "400-600m", "600-800m", "800-1000m", "1000-1200m", "1200-1400m")

# Convert DepthZone to a factor with the desired order
richness_summary$depth_zone <- factor(richness_summary$depth_zone, levels = desired_order)


ggplot(richness_summary, aes(x = depth_zone, y = MeanRichness)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_errorbar(aes(ymin = MeanRichness - StdDevRichness, ymax = MeanRichness + StdDevRichness), width = 0.2) +
  labs(title = "Mean Richness by Depth Zone with Standard Deviations",
       x = "Depth Zone",
       y = "Mean Richness") +
  theme_minimal()


# Calculating mean richness and standard deviation for CNIDARIA 


# filter for VME taxa 
filtered_df_cnid <- combined_dataframe%>%
  filter(grepl("Cnidaria", label_hierarchy))


cnid_abundance_data <- filtered_df_cnid %>%
  group_by(transect, label_name, depth_zone) %>%
  summarise(abundance = n())

cnid_abundance_data<- cnid_abundance_data %>%
  pivot_wider(names_from = label_name, values_from = abundance, values_fill = 0)



cnid_abundance_data<- as.data.frame(cnid_abundance_data)

View(cnid_abundance_data)


cnid<- numeric(length(depth_zones))
species_stddev <- numeric(length(depth_zones))


# Loop through each depth zone
species_richness <- numeric(length(depth_zones))
species_stddev <- numeric(length(depth_zones))

# Loop through each depth zone
for (i in 1:length(depth_zones)) {
  # Extract the abundance values for the current depth zone (excluding the first column)
  cnid_abundance_values <- cnid_abundance_data[i, -1]
  
  # Calculate species richness by counting non-zero values
  species_richness[i] <- sum(abundance_values > 0)
  
  # Calculate the standard deviation for the abundance values
  species_stddev[i] <- sd(abundance_values)
}
--------
# Calculate species richness within each transect (row)
cnid_abundance_data <- cnid_abundance_data %>%
  mutate(Richness = rowSums(select(., -depth_zone, -transect) > 0))

# Calculate mean richness and standard deviation for each depth zone
richness_summary_cnid <- cnid_abundance_data%>%
  group_by(depth_zone) %>%
  summarise(
    MeanRichness = mean(Richness),
    StdDevRichness = sd(Richness)
  )

# View the summary
print(richness_summary_cnid)

# re-order depth zones 

desired_order <- c("0-200m", "200-400m", "400-600m", "600-800m", "800-1000m", "1000-1200m", "1200-1400m")

# Convert DepthZone to a factor with the desired order
richness_summary_cnid$depth_zone <- factor(richness_summary_cnid$depth_zone, levels = desired_order)


ggplot(richness_summary_cnid, aes(x = depth_zone, y = MeanRichness)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_errorbar(aes(ymin = MeanRichness - StdDevRichness, ymax = MeanRichness + StdDevRichness), width = 0.2) +
  labs(title = "Mean Richness by Depth Zone with Standard Deviations",
       x = "Depth Zone",
       y = "Mean Richness") +
  theme_minimal()




# grouped bar chart for porifera and cnidaria 

richness_summary_cnid$Group <- "Cnidaria"
richness_summary$Group <- "Porifera"

combined_data <- rbind(richness_summary_cnid, richness_summary)

# Create a grouped bar plot
ggplot(combined_data, aes(x = depth_zone, y = MeanRichness, fill = depth_zone)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = MeanRichness - StdDevRichness, ymax = MeanRichness + StdDevRichness), position = position_dodge(width = 0.8), width = 0.2) +
  labs(title = "Richness by Depth Zone for Porifera and Cnidaria with Standard Deviations",
       x = "Depth Zone",
       y = "Mean Richness",
       fill = "Group") +
  theme_minimal()



