###PLOTTING STANDARDISED ABUNDANCES OF DIFFERENT TAXONOMIC GROUPS AT EACH DEPTH ZONE ON MELVILLE BANK ####

library(ggplot2)
library(dplyr)
library(tidyr)
library(vegan)

##### CNIDARIA ######


# filter the dataframe containing observations form all transects to only contain 
# cnidaria 

filtered_df_VME <- all_depth_zones %>%
  filter(grepl("Porifera|Cnidaria", label_hierarchy))

filtered_df_cnidaria<- filtered_df_VME %>%
  filter(grepl("Octocorallia|Hexacorallia|Hydrozoa", label_hierarchy))
  

filtered_df_cnidaria$depth_zone <- factor(filtered_df_cnidaria$depth_zone, levels = c("0-200m", "200-400m", "400-600m", "600-800m","1000-1200m", "1200-1400m"))


transects_data <- data.frame(depth_zone = c("0-200m", "200-400m", "400-600m", "600-800m", "1000-1200m", "1200-1400m"),
                             transects = c(5, 9, 11, 5, 4, 6))


# Define a function to assign a group based on the label_hierarchy
# Assign a group to each row based on label_hierarchy

grouped_cnidaria <- filtered_df_cnidaria %>%
  mutate(group = sapply(label_hierarchy, assign_group))

# now sum total group from each depth zone 

group_counts_cnidaria <- grouped_cnidaria %>%
  group_by(depth_zone) %>%
  summarise(
    hexacorallia_count = sum(group == 'Hexacorallia'),
    octocorallia_count = sum(group == 'Octocorallia'),
    hydrozoa_count = sum(group == 'Hydrozoa')
  )

# now add column for number of transects at each depth zone 

# Merge 'group_counts' with 'transects_data' based on depth_zone
merged_data <- merge(group_counts_cnidaria, transects_data, by = "depth_zone")

merged_data <- merged_data %>%
  mutate(Hexacorallia = hexacorallia_count / transects,
         Octocorallia = octocorallia_count / transects,
         Hydrozoa = hydrozoa_count / transects)


merged_data

new_data <- merged_data %>%
  select(depth_zone,Hexacorallia,Hydrozoa,Octocorallia)


# Reshape data to long format for ggplot
data_long <- new_data %>%
  gather(key = "group", value = "standardized_abundance", -depth_zone)

# Create a stacked bar chart
ggplot(data_long, aes(x = depth_zone, y = standardized_abundance, fill = group)) +
  geom_bar(stat = "identity") +
  labs(x = "Depth Zone", y = "Standardized Abundance", fill = "Group") +
  ggtitle("Standardized Abundances of Cnidaria Classes at Each Depth Zone") +
  theme_minimal()


### PORIFRA ######


filtered_df_Porifera <- all_depth_zones %>%
  filter(grepl("Porifera", label_hierarchy))

filtered_df_Porifera<-filtered_df_Porifera %>%
  filter(grepl("Crust-like|Cup-like|Erect|Massive", label_hierarchy))


filtered_df_Porifera$depth_zone <- factor(filtered_df_Porifera$depth_zone, levels = c("0-200m", "200-400m", "400-600m", "600-800m","1000-1200m", "1200-1400m"))


transects_data <- data.frame(depth_zone = c("0-200m", "200-400m", "400-600m", "600-800m", "1000-1200m", "1200-1400m"),
                             transects = c(5, 9, 11, 5, 4, 6))


# Define a function to assign a group based on the label_hierarchy
# Assign a group to each row based on label_hierarchy


# Define funciton to assign group based on label hierarchy 

assign_group2<- function(label_hierarchy){ 
  if (grepl("Crust-like", label_hierarchy, ignore.case = TRUE)) { 
    return( 'Crust-like')
    } else if (grepl("Cup-like", label_hierarchy,ignore.case = TRUE)) { 
    return("Cup-like")
    } else if (grepl("Erect", label_hierarchy, ignore.case = TRUE)) { 
      return("Erect")
    } else if (grepl("Massive", label_hierarchy, ignore.case = TRUE)) { 
      return("Massive")}
    }

grouped_porifera <- filtered_df_Porifera %>%
  mutate(group = sapply(label_hierarchy, assign_group2))

# now sum total group from each depth zone 

group_counts_porifera <- grouped_porifera %>%
  group_by(depth_zone) %>%
  summarise(
    crust_like_count = sum(group == 'Crust-like'),
    cup_like_count = sum(group == 'Cup-like'),
    erect_count = sum(group == 'Erect'),
    massive_count = sum(group =="Massive")
  )

# now add column for number of transects at each depth zone 

# Merge 'group_counts' with 'transects_data' based on depth_zone
merged_data <- merge(group_counts_porifera, transects_data, by = "depth_zone")

merged_data <- merged_data %>%
  mutate(Crust_like = crust_like_count/transects,
         Cup_like =  cup_like_count/transects,
         Erect = erect_count/transects,
         Massive =  massive_count/transects)


merged_data

new_data <- merged_data %>%
  select(depth_zone, "Crust_like", "Cup_like", "Erect", "Massive")


# Reshape data to long format for ggplot
data_long <- new_data %>%
  gather(key = "group", value = "standardized_abundance", -depth_zone)

# Create a stacked bar chart
ggplot(data_long, aes(x = depth_zone, y = standardized_abundance, fill = group)) +
  geom_bar(stat = "identity") +
  labs(x = "Depth Zone", y = "Standardized Abundance", fill = "Group") +
  ggtitle("Standardized Abundances of Porifera Classes at Each Depth Zone") +
  theme_minimal()


# Now create rarefied dataset 

# Make an abundance matrix for the cnidaria counts

cnidaria_df <- all_depth_zones %>%
  filter(grepl("Cnidaria", label_hierarchy))

cnidaria_df

cnidaria_abundance_matrix <- cnidaria_df %>%
  group_by(depth_zone, label_name) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = label_name, values_from = count, values_fill = 0)


# order matrix depth zones 

desired_depth_order <- c("0-200m", "200-400m", "400-600m", "600-800m", "800-1000m", "1000-1200m", "1200-1400m")

cnidaria_abundance_matrix$depth_zone <- factor(
 cnidaria_abundance_matrix$depth_zone,
  levels = desired_depth_order
)
cnidaria_abundance_matrix <- cnidaria_abundance_matrix %>%
  arrange(depth_zone)

class(cnidaria_abundance_matrix)

cnidaria_abundance_matrix <- as.data.frame(cnidaria_abundance_matrix)

# we need to convert the depth_zone names to numeric 
depth_zone_mapping <- c('0-200m' = 1, '200-400m' = 2, '400-600m' = 3, '600-800m' = 4, '800-1000m' = 5, '1000-1200m' = 6, '1200-1300m' = 7)

# Replace depth zone labels with integers based on the mapping
cnidaria_abundance_matrix$depth_zone <- depth_zone_mapping[cnidaria_abundance_matrix$depth_zone]


# create a loop to generate rarefied datasets of cnidaria 
# and calculate their richness values 

plot(rarefy(cnidaria_abundance_matrix, 80))


list_richness <- vector(mode='list', length = n_rep)

n_rep = 100
cutoff = 33

plot(rarefy(cnidaria_abundance_matrix, 30))

for (i in 1:n_rep) { 
  print(i)
  rarefied_richness_temp <- rrarefy(cnidaria_abundance_matrix, cutoff)
  diversity_temp <- c(specnumber(rarefied_richness_temp))
  list_richness[[i]]=diversity_temp } 


df_richness <- do.call("cbind",list_richness)
df_richness <- df_richness %>% as.data.frame() %>%
  mutate(DepthZone = 1:n()) 

df_richness_cnidaria_long<-pivot_longer(df_richness, cols = 1:n_rep, values_to = "Richness")
