# SIMPER ANALYSIS 
# (similarity percentage analysis)
# used to identify and quantify the contributions of different species to the dissimilarity or similarity of different samples or sites 
# helps to see which species are most responsible for the observed differences or similarities in communities

library(vegan)
library(dplyr)
library(tidyr)

# make matrix from VME dataframe


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
  filter(grepl("Porifera|Cnidaria", label_hierarchy))


abundance_data <- filtered_df_VME %>%
  group_by(transect, label_name, depth_zone) %>%
  summarise(abundance = n())

abundance_matrix <- abundance_data %>%
  pivot_wider(names_from = label_name, values_from = abundance, values_fill = 0)

abundance_matrix <- as.data.frame(abundance_matrix)

am <- subset(abundance_matrix, select = -1)

View(am)
# Define a named vector mapping depth zone labels to numeric values
depth_zone_mapping <- c(
  "0-200m" = 1,
  "200-400m" = 2,
  "400-600m" = 3,
  "600-800m" = 4,
  "1000-1200m" = 5,
  "1200-1400m" = 6
)


am$depth_zone <- depth_zone_mapping[am$depth_zone]

simper_result <- simper(am, abundance_matrix$depth_zone, permutations = 100)

anresult<-anosim(am, am$depth_zone, permutations = 999, distance = "bray", strata = NULL,
       parallel = getOption("mc.cores"))




anresult
# Create a color palette for the boxes
box_colors <- c("red", "blue4", "blue3", "royalblue4", "steelblue3", "skyblue")  # Adjust colors as needed

# Plot the ANOSIM result with colored boxes
plot(anresult, col = box_colors)

plot(anresult, notch = FALSE)

anresult

summary(simper_result)

summary(simper_result ordered = TRUE,
        digits = max(3,getOption("digits") - 3), ...)

# visualise simper results 

simper_data<-as.data.frame(simper_result)
simper_data <- pivot_longer(simper_result, cols = -Morphospecies, names_to = "DepthZone", values_to = "Contribution")

simper_result_df <- data.frame(
  Morphospecies = rownames(simper_result$abund),
  Contribution = simper_result$avgcontrib,
  Abundance = simper_result$avgabund,
  Sd = simper_result$sdcontrib,
  Ratio = simper_result$ratio
)


______
abundance_matrix <- abundance_data %>%
  pivot_wider(names_from = label_name, values_from = abundance, values_fill = 0)

depth_zone_mapping <- data.frame(
  depth_zone = c("0-200m", "200-400m", "400-600m", "600-800m", "1000-1200m", "1200-1400m"),
  numeric_value = 1:6
)

# Create a named vector mapping between character labels and numeric values
depth_zone_mapping <- c(
  "0-200m" = 1,
  "200-400m" = 2,
  "400-600m" = 3,
  "600-800m" = 4,
  "1000-1200m" = 5,
  "1200-1400m" = 6
)


# Update the 'depth_zone' column with numeric values from the mapping
abundance_matrix$depth_zone <- depth_zone_mapping[abundance_matrix$depth_zone]

# Verify the result
head(abundance_matrix)

simper_result <- simper(abundance_matrix, grouping = abundance_matrix$depth_zone)

summary(simper_result)
print(simper_result)
# Preprocess data (you might need to extract species information)
# Example: combined_data <- combined_data %>%
#   mutate(Species = extract_species_from_observation_column)

# Aggregation (count occurrences of each species in each depth zone)
abundance_matrix <- combined_data %>%
  group_by(depth_zone, label_name) %>%
  summarise(label_name = n()) %>%
  pivot_wider(names_from = depth_zone, values_from = label_name, values_fill = 0)


#I want to creare rarefied communities to use in bray curtis and simper
# change order of rows to be descending depth zones 

desired_depth_order <- c("0-200m", "200-400m", "400-600m", "600-800m", "1000-1200m", "1200-1400m")

simper_abundance_matrix$depth_zone <- factor(
  simper_abundance_matrix$depth_zone,
  levels = desired_depth_order
)
simper_abundance_matrix<- simper_abundance_matrix%>%
  arrange(depth_zone)

# we need to convert the depth_zone names to numeric 
depth_zone_mapping <- c('0-200m' = 1, '200-400m' = 2, '400-600m' = 3, '600-800m' = 4,  '1000-1200m' = 5, '1200-1300m' = 6)

# Replace depth zone labels with integers based on the mapping
simper_abundance_matrix$depth_zone <- depth_zone_mapping[simper_abundance_matrix$depth_zone]


# the simper function performs a pairwise analysis of groups of sampling units 
# and finds the average contribution of each species to the average Bray-curtis dissimilarity
# the function displays the most importang species for each pair of groups 

simper_result <-simper(simper_abundance_matrix,simper_abundance_matrix$depth_zone, permutations = 0, dist.method = "jaccard")
rsummary(simper_result)



# Analysis of similarity 
# needs replicates between groups 


anosim(simper_abundance_matrix, simper_abundance_matrix$depth_zone, permutations = 100, distance = "jaccard", strata = NULL,
       parallel = getOption("mc.cores"))


# Create a dissimilarity matrix (e.g., Bray-Curtis)
# Replace this with your actual dissimilarity matrix
dissimilarity_matrix <- vegdist(simper_abundance_matrix, method = "jaccard")
plot(dissimilarity_matrix)


# Create a sampling effort matrix (e.g., number of transects)
sampling_effort <- c(5,  9, 11,  5 , 4 , 6)

# Combine dissimilarity and sampling effort
weighted_dissimilarity <- dissimilarity_matrix * sqrt(sampling_effort)

# Create a grouping factor
grouping_factor <- factor(weighted_dissimilarity$depth_zone)

# Perform ANOSIM with sampling effort
result <- anosim(weighted_dissimilarity, grouping_factor)

# Print the ANOSIM result
print(result)



# Extract the relevant information from the SIMPER object
library(dplyr)

# Initialize an empty data frame
simper_df <- data.frame()

# Loop through the list of 'simper' results and extract average values
for (depth_zone in names(simper_result)) {
  simper_group <- simper_result[[depth_zone]]
  species <- simper_group$species
  average <- simper_group$average
  group_df <- data.frame(Depth_Zone = depth_zone, Species = species, Average = average, P_value = p_values)
  simper_df <- bind_rows(simper_df, group_df)
  p_values <- simper_group$p
}

group_df
# View the resulting data frame
head(simper_df)

significant_species_df <- simper_df %>% filter(P_Value <= 0.05)

ggplot(significant_species_df, aes(x = Depth_Zone, y = Species, size = Average, color = P_Value)) +
  geom_point() +
  scale_size_continuous(range = c(2, 20)) +  # Adjust the size range
  scale_color_gradient(low = "blue", high = "red") +  # Choose a color gradient
  labs(
    x = "Depth Zone",
    y = "Species",
    size = "Average",
    color = "P-Value"
  ) +
  theme_minimal()

