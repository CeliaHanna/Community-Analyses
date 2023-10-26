# which VME species dominate each depth zone
library(dplyr)

most_abundant_species <- species_abundance_matrix %>%
  # Remove the "Depth_Zone" column for computation
  select(-depth_zone) %>%
  # Find the column (i.e., species) with the maximum abundance for each row
  mutate(Most_Abundant_Species = colnames(.)[apply(., 1, which.max)])

# Add back the "Depth_Zone" column
most_abundant_species <- cbind(Depth_Zone = species_abundance_matrix$depth_zone, most_abundant_species)

# Print the result
print(most_abundant_species)
