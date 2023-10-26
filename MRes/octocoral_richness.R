### Exploring octocoral distribution on Melville Bank ##
library(dplyr)
library(stringr)
library(ggplot2)
library(vegan)

#select all rows in the df that contain word octocorallia
# Filter rows that contain 'Octocorallia' in the label_hierarchy column
octocorallia_rows <- filtered_df_cnidaria%>%
  filter(str_detect(label_hierarchy, "Octocorallia"))

# View the selected rows
print(octocorallia_rows)
nrow(octocorallia_rows) # 1423 octocoral observations across whole seamount 

#now find number of different octocoral OTUs 


# Split the data frame by 'depth_zone' into a list of data frames
depth_zones_octocorals <- split(octocorallia_rows, octocorallia_rows$depth_zone)

# Now you have a list of data frames, each representing a different depth zone

# Access a specific depth zone data frame, for example, '0-200m'
depth_zone_0_200m <- depth_zones_octocorals$"0-200m"
depth_zone_200_400m <- depth_zones_octocorals$"200-400m"
depth_zone_400_600m <- depth_zones_octocorals$"400-600m"
depth_zone_600_800m <- depth_zones_octocorals$"600-800m"
depth_zone_1000_1200m <- depth_zones_octocorals$"1000-1200m"
depth_zone_1200_1400m <- depth_zones_octocorals$"1200-1400m"

# find out how many species present in each zone 

# Find the number of unique label_names

octocoral_richness_list <- c(richness1 <- depth_zone_0_200m %>%
  summarize(unique_label_names = n_distinct(label_name)), 
  richness2 <- depth_zone_200_400m %>%
  summarize(unique_label_names = n_distinct(label_name)),
  richness3 <- depth_zone_400_600m %>%
  summarize(unique_label_names = n_distinct(label_name)),
  richness4 <- depth_zone_600_800m %>%
  summarize(unique_label_names = n_distinct(label_name)),
  richness5 <- depth_zone_1000_1200m %>%
  summarize(unique_label_names = n_distinct(label_name)),
  richness6 <- depth_zone_1200_1400m %>%
  summarize(unique_label_names = n_distinct(label_name)))

transects <- c(5,9,11,5,4,6)
octo_depth_zones <- c("0-200m", "200-400m", "400-600m", "600-800m", "1000-1200m", 
                      "1200-1400m")
#combine into dataframe
octocoral_richness_list <- data.frame(
  depth_zone = octo_depth_zones,
  richness = c(richness1$unique_label_names, richness2$unique_label_names, 
               richness3$unique_label_names, richness4$unique_label_names, 
               richness5$unique_label_names, richness6$unique_label_names),
  transects = c(5, 9, 11, 5, 4, 6)
)

# add standardised richnesses for each zone

octocoral_richness_list$standardized_richness <- octocoral_richness_list$richness / octocoral_richness_list$transects

# Change the order of the depth zones
octocoral_richness_list$depth_zone <- factor(
  octocoral_richness_list$depth_zone,
  levels = c("0-200m", "200-400m", "400-600m", "600-800m", "1000-1200m", "1200-1400m")
)

# Create a bar chart of standardized richness by depth zone
ggplot(octocoral_richness_list, aes(x = depth_zone, y = standardized_richness)) +
  geom_bar(stat = "identity", fill = "pink") +
  labs(x = "Depth Zone", y = "Standardized Octocoral Richness", title = "Octocoral Richness by Depth Zone") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Now rarefy and see if same pattern holds 

octocorallia_rows
