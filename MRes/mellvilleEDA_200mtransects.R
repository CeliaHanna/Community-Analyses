library(dplyr)
library(ggplot2)

# calculating basic diversity metrics for 200m transects 

dive7_first_200m

dive9_first_200m # depth zone 200-400m #normalised_length: 230.9432m # fishing gear + rubble

dive8_sixth_200m # depth zone 400-600m #normalised_length: 229.8959m #fishing gear + coral rubble

dive8_first_200m # depth zone 600-800m #normalised_length: 200.7654m coral rubble 

dive10_second_200m  # depth zone 1000-1200 #normalised_length: 223.7431m

dive10_first_200m # depth zone 1200-1400 #normalised_length: 204.8749m


View(dive7_first_200m)

# scale up so y axis is easier to read 
View(dive8_first_200m)

print(length(unique(dive9_first_200m[["label_name"]])) / 0.2309432)

print(length(unique(dive8_sixth_200m[["label_name"]])) / 0.2298959)

print(length(unique(dive8_first_200m[["label_name"]])) / 0.2007654)

print(length(unique(dive10_second_200m [["label_name"]])) / 0.2237431)

print(length(unique(dive10_first_200m[["label_name"]])) / 0.2048749)

print(length(unique(dive7_first_200m[["label_name"]]))/0.2114849)

dive7_first_200m_normalised_length

print(length(unique(dive8_first_200m[["label_name"]]))/ 0.2007654)



library(ggplot2)

# Dive names
# Dive names and normalized lengths
dive_names <- c("dive9_first_200m", "dive8_sixth_200m", "dive7_first_200m", 
                "dive10_second_200m", "dive10_first_200m")
normalized_richness <- c( length(unique(dive9_first_200m[["label_name"]])) / 0.2309432,
                         length(unique(dive8_sixth_200m[["label_name"]])) / 0.2298959,
                         107.2742, 
                         #length(unique(dive7_first_200m[["label_name"]])) / 0.2114849, 
                         #length(unique(dive8_first_200m[["label_name"]])) / 200.7654,
                         length(unique(dive10_second_200m[["label_name"]])) / 0.2237431,
                         length(unique(dive10_first_200m[["label_name"]])) / 0.2048749 )


normalized_abundance <- c( length(dive9_first_200m[["label_name"]]) / 0.2309432,
                           length(dive8_sixth_200m[["label_name"]]) / 0.2298959,
                           780.1735,
                           length(dive10_second_200m[["label_name"]]) / 0.2237431,
                           length(dive10_first_200m[["label_name"]]) / 0.2048749 )


print(length(dive8_first_200m[["label_name"]])/ 0.2007654) #278.9325
print(length(dive7_first_200m[["label_name"]])/ 0.21148489) 




# Create a data frame
data <- data.frame(dive_names, normalized_lengths)

# Reorder the dive names in the desired order
data$dive_names <- factor(data$dive_names, levels = c("dive9_first_200m", 
                                                      "dive8_sixth_200m", 
                                                      "dive8_first_200m", 
                                                      "dive10_second_200m", 
                                                      "dive10_first_200m"))

# Define custom labels for the x-axis
custom_labels <- c("200-400m", "400-600m", "600-800m", 
                   "1000-1200m", "1200-1400m")

# Create the plot for standardised richness
ggplot(data, aes(x = dive_names, y = normalized_richness)) +
  geom_bar(stat = "identity") +
  xlab("Depth Zones") +
  ylab("Standardised Richness") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_discrete(labels = custom_labels) 
  #ggtitle("Standardised Species Richness across 5 Depth Zones Melville Bank (all taxa)") 
#200m transects used 
  # Rotate x-axis labels for better visibility


# plot for standardised abundance 
ggplot(data, aes(x = dive_names, y = normalized_abundance)) +
  geom_bar(stat = "identity") +
  xlab("Depth Zones") +
  ylab("Standardised Abundance") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_discrete(labels = custom_labels) 
  #ggtitle("Standardised Species Abundance of all Annotated Taxa across 5 Depth Zones Melville Bank") 
# Rotate x-axis labels for better visibility



## Now make the same plots but only for VME taxa (Porifera & Cnidaria)

# Use grep function to make a new df containing only VME taxa 
dive7_first200m_VME <- dive7_first_200m %>%
  filter(grepl('Cnidaria', label_hierarchy)) # looks for rows in the 

dive9_first200m_VME <- dive9_first_200m %>%
  filter(grepl('Cnidaria', label_hierarchy)) # looks for rows in the 

dive8_sixth200m_VME <- dive8_sixth_200m  %>%
  filter(grepl('Cnidaria', label_hierarchy))

dive8_first200m_VME <- dive8_first_200m  %>%
  filter(grepl('Cnidaria', label_hierarchy))

dive10_second200m_VME <- dive10_second_200m %>%
  filter(grepl('Cnidaria', label_hierarchy))

dive10_first200m_VME <- dive10_first_200m %>%
  filter(grepl('Cnidaria', label_hierarchy))


print(length(unique(dive8_first200m_VME$label_id)))
print(length(unique(dive7_first200m_VME$label_id)))



standardised_richness_VMEs <- c( length(unique(dive9_first200m_VME [["label_name"]])) / 0.2309432,
                          length(unique(dive8_sixth200m_VME[["label_name"]])) / 0.2298959,
                          11.5,
                          length(unique(dive10_second200m_VME[["label_name"]])) / 0.2237431,
                          length(unique(dive10_first200m_VME[["label_name"]])) / 0.2048749 )

ggplot(data,aes(x=dive_names,y=standardised_richness_VMEs))+
  geom_bar(stat="identity")+
  xlab("Depth Zones") + 
  ylab("VME Species Richness")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_discrete(labels = custom_labels) 


standardised_abundance_VMEs <- c( length(dive9_first200m_VME [["label_name"]]) / 230.9432,
                                 length(dive8_sixth200m_VME[["label_name"]]) / 229.8959,
                                 length(dive8_first200m_VME[["label_name"]]) / 200.7654,
                                 length(dive10_second200m_VME[["label_name"]]) / 223.7431,
                                 length(dive10_first200m_VME[["label_name"]]) / 204.8749 )
  


ggplot(data,aes(x=dive_names,y=standardised_abundance_VMEs))+
  geom_bar(stat="identity")+
  xlab("Depth Zones") + 
  ylab("VME Species Abundance")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_discrete(labels = custom_labels) 




