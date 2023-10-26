# session 1 with Somali


library(dplyr)

dive7 <- read.csv("/Users/user/Desktop/metadata_flow/completed_metadata_files/11056-dive7-file3.csv") 
dive8 <- read.csv("/Users/user/Desktop/metadata_flow/completed_metadata_files/merged_DIVE8.csv")
dive9 <- read.csv("/Users/user/Desktop/metadata_flow/completed_metadata_files/merged_DIVE9.csv")
dive10 <- read.csv("/Users/user/Desktop/metadata_flow/completed_metadata_files/merged_DIVE10.csv")


plot3d(x,y,z)


dive7$n_dive = 7 
dive8$n_dive = 8
dive9$n_dive = 9 
dive10$n_dive = 10


# Create coordinate, depth, and distance variables 
coordinates <- dive8[, c("lat", "lon")]
depths <- dive8$ROV.RovDepth

coordinates <- dive7[, c("lat", "lon")]
depths <- dive7$ROV.RovDepth

coordinates <- dive9[, c("lat", "lon")]
depths <- dive9$ROV.RovDepth

coordinates <- dive10[, c("lat", "lon")]
depths <- dive10$ROV.RovDepth
----------

library(dplyr)

# Function to process the dataframe and create a new dataframe
process_dive_data <- function(dive_data, suffix) {
  new_data <- dive_data %>%
    mutate(
      depth_zone = case_when(
        ROV.RovDepth <= 100 ~ "1",
        ROV.RovDepth > 100 & ROV.RovDepth <= 200 ~ "2",
        ROV.RovDepth > 200 & ROV.RovDepth <= 300 ~ "3",
        ROV.RovDepth > 300 & ROV.RovDepth <= 400 ~ "4",
        ROV.RovDepth > 400 & ROV.RovDepth <= 500 ~ "5",
        ROV.RovDepth > 500 & ROV.RovDepth <= 600 ~ "6",
        ROV.RovDepth > 600 & ROV.RovDepth <= 700 ~ "7",
        ROV.RovDepth > 700 & ROV.RovDepth <= 800 ~ "8",
        ROV.RovDepth > 800 & ROV.RovDepth <= 900 ~ "9",
        ROV.RovDepth > 900 & ROV.RovDepth <= 1000 ~ "10",
        ROV.RovDepth > 1000 & ROV.RovDepth <= 1100 ~ "11",
        ROV.RovDepth > 1100 & ROV.RovDepth <= 1200 ~ "12",
        ROV.RovDepth > 1200 & ROV.RovDepth <= 1300 ~ "13",
        ROV.RovDepth > 1300 & ROV.RovDepth <= 1400 ~ "14"
      )
    ) %>%
    group_by(depth_zone) %>%
    summarize(
      abundance = n(),
      n_species = length(unique(label_name))
    )
  
  # Rename columns and create a new dataframe with the specified suffix
  new_data <- new_data %>%
    rename_with(~ paste0(., "_", suffix))
  
  return(new_data)
}

# Example usage
newdataframe7 <- process_dive_data(dive7, "7")
newdataframe8 <- process_dive_data(dive8, "8")
newdataframe9 <- process_dive_data(dive9, "9")
newdataframe10 <- process_dive_data(dive10, "10")

# View the resulting dataframes
View(newdataframe7)
newdataframe8
newdataframe9
newdataframe10


library(ggplot2)
ggplot(data = newdataframe, aes(x = distance_zone, y = depth_zone)) +
  geom_point(aes(size = n_species))

ggplot(data=newdataframe, aes(x=depth_zone, y=n_species)) +
  geom_bar(stat="identity")



--------------

# Initialize variables
total_distance <- 0
dive_distances <- numeric(length(depths))

# Calculate distances considering depth condition
for (i in 2:length(depths)) {
  depth_diff <- abs(depths[i] - depths[i-1])
  if (depth_diff <= 4) {
    distance <- geosphere::distHaversine(coordinates[i-1, , drop = FALSE], coordinates[i, , drop = FALSE])
    total_distance <- total_distance + distance
  }
  dive_distances[i] <- total_distance
}

# Create a data frame with distance and depth
dive8 <- cbind(dive8, dive_distances)


newdataframe<- dive8 %>% mutate(depth_zone = case_when(ROV.RovDepth<=200 ~ "Zone1",
                                                       ROV.RovDepth>200 & ROV.RovDepth<=400 ~ "Zone2",
                                                       ROV.RovDepth>400 & ROV.RovDepth<=600 ~ "Zone3",
                                                       ROV.RovDepth>600 & ROV.RovDepth<=800 ~ "Zone4",
                                                       ROV.RovDepth>800 & ROV.RovDepth<=1000 ~ "Zone5",
                                                       ROV.RovDepth>1000 & ROV.RovDepth<=1200 ~ "Zone6",
                                                       ROV.RovDepth>1200 & ROV.RovDepth<=1400 ~ "Zone7")) %>%
  group_by(depth_zone) %>%
  summarize(abundance = n(),
            n_species = length(unique(label_name)))

newdataframe<- dive8 %>% mutate(depth_zone = case_when(ROV.RovDepth<=100 ~ "1",
                                                       ROV.RovDepth>100 & ROV.RovDepth<=200 ~ "2",
                                                       ROV.RovDepth>200 & ROV.RovDepth<=300 ~ "3",
                                                       ROV.RovDepth>300 & ROV.RovDepth<=400 ~ "4",
                                                       ROV.RovDepth>400 & ROV.RovDepth<=500 ~ "5",
                                                       ROV.RovDepth>500 & ROV.RovDepth<=600 ~ "6",
                                                       ROV.RovDepth>600 & ROV.RovDepth<=700 ~ "7",
                                                       ROV.RovDepth>700 & ROV.RovDepth<=800 ~ "8",
                                                       ROV.RovDepth>800 & ROV.RovDepth<=900 ~ "9",
                                                       ROV.RovDepth>900 & ROV.RovDepth<=1000 ~ "10",
                                                       ROV.RovDepth>1000 & ROV.RovDepth<=1100 ~ "11",
                                                       ROV.RovDepth>1100 & ROV.RovDepth<=1200 ~ "12",
                                                       ROV.RovDepth>1200 & ROV.RovDepth<=1300 ~ "13",
                                                       ROV.RovDepth>1300 & ROV.RovDepth<=1400 ~ "14"),
                                distance_zone = case_when(dive_distances<=100 ~ "1",
                                                          dive_distances>100 & dive_distances<=200 ~ "2",
                                                          dive_distances>200 & dive_distances<=300 ~ "3",
                                                          dive_distances>300 & dive_distances<=400 ~ "4",
                                                          dive_distances>400 & dive_distances<=500 ~ "5",
                                                          dive_distances>500 & dive_distances<=600 ~ "6",
                                                          dive_distances>600 & dive_distances<=700 ~ "7",
                                                          dive_distances>700 & dive_distances<=800 ~ "8",
                                                          dive_distances>800 & dive_distances<=900 ~ "9",
                                                          dive_distances>900 & dive_distances<=1000 ~ "10",
                                                          dive_distances>1000 & dive_distances<=1100 ~ "11",
                                                          dive_distances>1100 & dive_distances<=1200 ~ "12")) %>%
  group_by(depth_zone, distance_zone) %>%
  mutate(distance_zone = as.numeric(distance_zone)) %>%
  summarize(abundance = n(),
            n_species = length(unique(label_name)))



newdataframe7<- dive7 %>% mutate(depth_zone = case_when(ROV.RovDepth<=100 ~ "1",
                                                       ROV.RovDepth>100 & ROV.RovDepth<=200 ~ "2",
                                                       ROV.RovDepth>200 & ROV.RovDepth<=300 ~ "3",
                                                       ROV.RovDepth>300 & ROV.RovDepth<=400 ~ "4",
                                                       ROV.RovDepth>400 & ROV.RovDepth<=500 ~ "5",
                                                       ROV.RovDepth>500 & ROV.RovDepth<=600 ~ "6",
                                                       ROV.RovDepth>600 & ROV.RovDepth<=700 ~ "7",
                                                       ROV.RovDepth>700 & ROV.RovDepth<=800 ~ "8",
                                                       ROV.RovDepth>800 & ROV.RovDepth<=900 ~ "9",
                                                       ROV.RovDepth>900 & ROV.RovDepth<=1000 ~ "10",
                                                       ROV.RovDepth>1000 & ROV.RovDepth<=1100 ~ "11",
                                                       ROV.RovDepth>1100 & ROV.RovDepth<=1200 ~ "12",
                                                       ROV.RovDepth>1200 & ROV.RovDepth<=1300 ~ "13",
                                                       ROV.RovDepth>1300 & ROV.RovDepth<=1400 ~ "14")) %>%
                                   group_by(depth_zone) %>%
                                   summarize(abundance = n(),
                                   n_species = length(unique(label_name)))



newdataframe8<- dive8 %>% mutate(depth_zone = case_when(ROV.RovDepth<=100 ~ "1",
                                                        ROV.RovDepth>100 & ROV.RovDepth<=200 ~ "2",
                                                        ROV.RovDepth>200 & ROV.RovDepth<=300 ~ "3",
                                                        ROV.RovDepth>300 & ROV.RovDepth<=400 ~ "4",
                                                        ROV.RovDepth>400 & ROV.RovDepth<=500 ~ "5",
                                                        ROV.RovDepth>500 & ROV.RovDepth<=600 ~ "6",
                                                        ROV.RovDepth>600 & ROV.RovDepth<=700 ~ "7",
                                                        ROV.RovDepth>700 & ROV.RovDepth<=800 ~ "8",
                                                        ROV.RovDepth>800 & ROV.RovDepth<=900 ~ "9",
                                                        ROV.RovDepth>900 & ROV.RovDepth<=1000 ~ "10",
                                                        ROV.RovDepth>1000 & ROV.RovDepth<=1100 ~ "11",
                                                        ROV.RovDepth>1100 & ROV.RovDepth<=1200 ~ "12",
                                                        ROV.RovDepth>1200 & ROV.RovDepth<=1300 ~ "13",
                                                        ROV.RovDepth>1300 & ROV.RovDepth<=1400 ~ "14")) %>%
  group_by(depth_zone) %>%
  summarize(abundance = n(),
            n_species = length(unique(label_name)))


view(newdataframe7)


newdataframe9<- dive9 %>% mutate(depth_zone = case_when(ROV.RovDepth<=100 ~ "1",
                                                        ROV.RovDepth>100 & ROV.RovDepth<=200 ~ "2",
                                                        ROV.RovDepth>200 & ROV.RovDepth<=300 ~ "3",
                                                        ROV.RovDepth>300 & ROV.RovDepth<=400 ~ "4",
                                                        ROV.RovDepth>400 & ROV.RovDepth<=500 ~ "5",
                                                        ROV.RovDepth>500 & ROV.RovDepth<=600 ~ "6",
                                                        ROV.RovDepth>600 & ROV.RovDepth<=700 ~ "7",
                                                        ROV.RovDepth>700 & ROV.RovDepth<=800 ~ "8",
                                                        ROV.RovDepth>800 & ROV.RovDepth<=900 ~ "9",
                                                        ROV.RovDepth>900 & ROV.RovDepth<=1000 ~ "10",
                                                        ROV.RovDepth>1000 & ROV.RovDepth<=1100 ~ "11",
                                                        ROV.RovDepth>1100 & ROV.RovDepth<=1200 ~ "12",
                                                        ROV.RovDepth>1200 & ROV.RovDepth<=1300 ~ "13",
                                                        ROV.RovDepth>1300 & ROV.RovDepth<=1400 ~ "14")) %>%
  group_by(depth_zone) %>%
  summarize(abundance = n(),
            n_species = length(unique(label_name)))

view(newdataframe9)

newdataframe10<- dive10 %>% mutate(depth_zone = case_when(ROV.RovDepth<=100 ~ "1",
                                                        ROV.RovDepth>100 & ROV.RovDepth<=200 ~ "2",
                                                        ROV.RovDepth>200 & ROV.RovDepth<=300 ~ "3",
                                                        ROV.RovDepth>300 & ROV.RovDepth<=400 ~ "4",
                                                        ROV.RovDepth>400 & ROV.RovDepth<=500 ~ "5",
                                                        ROV.RovDepth>500 & ROV.RovDepth<=600 ~ "6",
                                                        ROV.RovDepth>600 & ROV.RovDepth<=700 ~ "7",
                                                        ROV.RovDepth>700 & ROV.RovDepth<=800 ~ "8",
                                                        ROV.RovDepth>800 & ROV.RovDepth<=900 ~ "9",
                                                        ROV.RovDepth>900 & ROV.RovDepth<=1000 ~ "10",
                                                        ROV.RovDepth>1000 & ROV.RovDepth<=1100 ~ "11",
                                                        ROV.RovDepth>1100 & ROV.RovDepth<=1200 ~ "12",
                                                        ROV.RovDepth>1200 & ROV.RovDepth<=1300 ~ "13",
                                                        ROV.RovDepth>1300 & ROV.RovDepth<=1400 ~ "14")) %>%
  group_by(depth_zone) %>%
  summarize(abundance = n(),
            n_species = length(unique(label_name)))


view(newdataframe10)
# factor reorder level


combine_dataframes <- rbind(newdataframe7, newdataframe8,newdataframe9,newdataframe10)


ggplot(data = combine_dataframes, aes(x = reorder(depth_zone, -as.numeric(depth_zone)), y = abundance)) +
  geom_bar(stat = "identity") +
  xlab("Depth Zone") +
  ylab("Abundance") 

ggplot(data=combine_dataframes, aes(x=depth_zone, y=abundance))+
  geom_bar(stat="identity") + 
  scale_x_reverse()

library(ggplot2)
ggplot(data = newdataframe, aes(x = distance_zone, y = depth_zone)) +
  geom_point(aes(size = n_species)) + 


ggplot(data=newdataframe, aes(x=depth_zone, y=n_species)) +
  geom_bar(stat="identity") + 

# shows abundance highest at middle depths on dive 8, but this would make sense
# as most of the dive was in this region (sampling bias )

