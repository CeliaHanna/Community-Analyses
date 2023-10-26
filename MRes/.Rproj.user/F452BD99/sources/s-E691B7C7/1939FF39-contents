# Calculate richness for each depth zone using 50m transects and plot 
# Then Normalise by dividing species richness by 'along track' distance using pythag
library(ggplot2)
library(plotrix)
library(vegan)

#Writing a function to calculate richness 
calculate_species_richness <- function(data_frame, column_name) {
  unique_values <- unique(data_frame[[column_name]])
  richness <- length(unique_values)
  return(richness)
}
# First depth zone 0-200m # 5x 50m transects

# Create a list of data frames and corresponding labels
data_frames_list <- list(dive8_nineteenth50 = "d8_19_richness", 
                         dive8_twentieth50 = "d8_20_richness", 
                         dive9_seventh50 = "d9_7_richness",
                         dive9_eighth50 = "d9_8_richness", 
                         dive9_ninth50 = "d9_9_richness"
                         )

specnumber(dive8_nineteenth50)

# Create an empty list to store the species richness results
depth_zone_1_richness_results <- list()


# Create an empty list to store the species richness results
depth_zone_1_richness_results <-list()

#Iterate the species richness function over each dataframe
for (i in seq_along(data_frames_list)) {
  # Get the current data frame and label
  current_df <- get(names(data_frames_list)[i])  # Corrected to use get()
  label <- names(data_frames_list)[i]
  
  # Calculate species richness for the 'label_name' column
  richness_result <- calculate_species_richness(current_df, 'label_name')
  
  # Store the result in the depth_zone_1_richness_results list (corrected variable)
  depth_zone_1_richness_results[[i]] <- richness_result
}

# Print the results
for (i in seq_along(depth_zone_1_richness_results)) {
  print(paste("Species richness for", names(data_frames_list)[i], ":", depth_zone_1_richness_results[[i]]))
}

# Define 'along track' lengths for each transect
transect_lengths <- c(50.24938, 54.99464, 51.6124, 60.18513, 50.02559)  # Replace with actual transect lengths

normalised_richness_values<- c(
                              normalised_d8_19_richness <- d8_19_richness/0.5024938,
                              normalised_d8_20_richness <- d8_20_richness /0.5499464,
                              normalised_d9_7_richness <- d9_7_richness/0.516124,
                              normalised_d9_8_richness <- d9_8_richness /0.6018513,
                              normalised_d9_9_richness <- d9_9_richness/0.5002559)
                              
d8_19_richness/0.5

#sd
standard_deviation1<-sd(normalised_richness_values)

# standard error 
standard_error1<- ((standard_deviation1)/sqrt(5))
standard_error1 # 2.594366

#mean species richness for this group 
mean_richness_depth_zone1 <- sum(normalised_richness_values)/5

mean_richness_depth_zone1

####### Second depth zone 200-400m # 11x 50m transects ######

# create list of dataframes for the second depth zone (200:400m)

# Create a list of data frames and corresponding labels
data_frames_list_depth_zone2 <- list(dive8_fifteenth50 = "d8_15_richness", 
                                     dive8_sixteenth50 = "d8_16_richness", 
                                     dive8_seventeenth50 = "d8_17_richness",
                                     dive8_eighteenth50 = "d8_18_richness", 
                                     dive9_first50 = "d9_1_richness", 
                                     dive9_second50 = "d9_2_richness", 
                                     dive9_third50 = "d9_3_richness",
                                     dive9_fourth50  = "d9_4_richness", 
                                     dive9_fifth50 = "d9_5_richness", 
                                     dive9_sixth50= "d9_6_richness") 

# Create an empty list to store the species richness results
depth_zone_2_richness_results <-list()


#Iterate the species richness function over each dataframe
for (i in seq_along(data_frames_list_depth_zone2)) {
  # Get the current data frame and label
  current_df <- get(names(data_frames_list_depth_zone2 )[i])  # Corrected to use get()
  label <- names(data_frames_list_depth_zone2)[i]
  
  # Calculate species richness for the 'label_name' column
  richness_result <- calculate_species_richness(current_df, 'label_name')
  
  # Store the result in the depth_zone_1_richness_results list (corrected variable)
  depth_zone_2_richness_results[[i]] <- richness_result
}

# Print the results
for (i in seq_along(depth_zone_2_richness_results)) {
  print(paste("Species richness for", names(data_frames_list_depth_zone2)[i], ":", depth_zone_2_richness_results[[i]]))
}
                                     
# Along track values for later 
dive8_fifteenth50 #66.86823
dive8_sixteenth50 #53.24369
dive8_seventeenth50 #53.59562
dive8_eighteenth50 #53.04046
dive9_first50 # 54.42656
dive9_second50 #56.35601
dive9_third50 #55.67989
dive9_fourth50 #58.0031
dive9_fifth50 #70.35801
dive9_sixth50

normalised_depthzone2<- c(normalised_d8_15_richness <- d8_15_richness/0.6686823,
                          normalised_d8_16_richness <- d8_16_richness /0.5324369,
                          normalised_d8_17_richness <- d8_17_richness /0.5359562,
                          normalised_d8_18_richness <- 6 /0.5304046,
                          normalised_d9_1_richness <- d9_1_richness /0.5442656,
                          normalised_d9_2_richness <- d9_2_richness /0.5635601,
                          normalised_d9_3_richness <- d9_3_richness /0.5567989,
                          normalised_d9_4_richness <- 8 /0.580031,
                          normalised_d9_5_richness <- d9_5_richness /0.7035801)
                   
#mean species richness for this group 
mean_richness_depth_zone_2 <- sum(normalised_depthzone2/9)

#sd
standard_deviation2<-sd(normalised_depthzone2)

# standard error 
standard_error2<- ((standard_deviation2)/sqrt(9))
standard_error2 #1.824302


#Calculate the standard deviation (SD) of the mean richness
sd_mean_richness_depth_zone_2 <- sd(normalised_depthzone2) / sqrt(length(normalised_depthzone2))
sd_mean_richness_depth_zone_2 # 1.824302

print(sum(normalised_depthzone2)/9)


##### Third Depth Zone 400-600m ####

# Create a list of data frames and corresponding labels
data_frames_list_depth_zone3 <- list(dive8_second50 = "d8_2_richness", #53.77769
                                     dive8_third50 ="d8_3_richness",   #55.59218
                                     dive8_fourth50 = "d8_4_richness", #51.6124
                                     dive8_fifth50 = "d8_5_richness",  #50.20209
                                     dive8_sixth50 ="d8_6_richness",   #50.31262
                                     dive8_seventh50 = "d8_7_richness", #50.47385
                                     dive8_eighth50 = "d8_8_richness", #51.17431
                                     dive8_ninth50 = "d8_9_richness",  #71.56424
                                     dive8_tenth50 = "d8_10_richness", #50.0001
                                     dive8_eleventh50 = "d8_11_richness", #50.26967
                                     dive8_twelfth50 ="d8_12_richness") #50.87593

d8_2_richness

# Create an empty list to store the species richness results
depth_zone_3_richness_results <-list()

#Iterate the species richness function over each dataframe
for (i in seq_along(data_frames_list_depth_zone3)) {
  # Get the current data frame and label
  current_df <- get(names(data_frames_list_depth_zone3 )[i])  # Corrected to use get()
  label <- names(data_frames_list_depth_zone3)[i]
  
  # Calculate species richness for the 'label_name' column
  richness_result <- calculate_species_richness(current_df, 'label_name')
  
  # Store the result in the depth_zone_1_richness_results list (corrected variable)
  depth_zone_3_richness_results[[i]] <- richness_result
}

# Print the results
for (i in seq_along(depth_zone_3_richness_results)) {
  print(paste("Species richness for", names(data_frames_list_depth_zone3)[i], ":", depth_zone_3_richness_results[[i]]))
}


normalised_richness_depth_zone_3<- c(normalised_d8_2_richness <-  12/53.77769,
                                      normalised_d8_3_richness <- 11/55.59218,
                                      normalised_d8_4_richness <- 14/51.6124, 
                                      normalised_d8_5_richness <- 5/50.20209, 
                                      normalised_d8_6_richness <- 1/50.31262, 
                                      normalised_d8_7_richness <- 2/50.47385, 
                                      normalised_d8_8_richness <- 2/51.17431, 
                                      normalised_d8_9_richness <- 11/71.56424,
                                      normalised_d8_10_richness <- 4/50.0001,
                                      normalised_d8_11_richness <- 9/50.26967,
                                      normalised_d8_12_richness <-3/50.87593) 
                                      

# Mean richness for depth zone 3
mean_richness_depth_zone_2 <-sum(normalised_richness_depth_zone_3)/11

#sd
standard_deviation3<-sd(normalised_richness_depth_zone_3)

# standard error 
standard_error3<- ((standard_deviation3)/sqrt(11))
standard_error3 #0.02572811


##### Fourth Depth Zone 600-800m ####

# Create a list of data frames and corresponding labels
data_frames_list_depth_zone4 <- list(dive7_first50 = "d7_1_richness",
                                     dive7_second50 = "d7_2_richness",
                                     dive7_third50 = "d7_3_richness",
                                     dive7_fourth50 = "d7_4_richness",
                                     dive7_fifth50 = "d7_5_richness")

                                     
# Create an empty list to store the species richness results
depth_zone_4_richness_results <-list()

#Iterate the species richness function over each dataframe
for (i in seq_along(data_frames_list_depth_zone4)) {
  # Get the current data frame and label
  current_df <- get(names(data_frames_list_depth_zone4 )[i])  # Corrected to use get()
  label <- names(data_frames_list_depth_zone4)[i]
  
  # Calculate species richness for the 'label_name' column
  richness_result <- calculate_species_richness(current_df, 'label_name')
  
  # Store the result in the depth_zone_1_richness_results list (corrected variable)
  depth_zone_4_richness_results[[i]] <- richness_result
}

# Print the results
for (i in seq_along(depth_zone_4_richness_results)) {
  print(paste("Species richness for", names(data_frames_list_depth_zone4)[i], ":", depth_zone_4_richness_results[[i]]))
}


normalised_richness_depth_zone_4<- c(normalised_d7_1_richness <-  26/0.5268358,
                                     normalised_d7_2_richness <- 17/0.5075126,
                                     normalised_d7_3_richness<- 10/0.532094, 
                                     normalised_d7_4_richness <- 3/0.5017609, 
                                     normalised_d7_5_richness <- 8/0.5047385, 
                                     normalised_d8_1_richness<- 18/0.7049887)


mean_richness_depth_zone4 <- sum(normalised_richness_depth_zone_4)/6
mean_richness_depth_zone4


#sd
standard_deviation4<-sd(normalised_richness_depth_zone_4)

# standard error 
standard_error4<- ((standard_deviation4)/sqrt(6))
standard_error4 #6.189615


##### Fifth Depth Zone 1000-1200m ####

# Create a list of data frames and corresponding labels
data_frames_list_depth_zone5 <- list(dive10_seventh50 = "d10_7_richness", 
                                     dive10_eighth50 = "d10_8_richness",
                                     dive10_ninth50 = "d10_9_richness",
                                     dive10_tenth50 = "d10_10_richness")

# Create an empty list to store the species richness results
depth_zone_5_richness_results <-list()

#Iterate the species richness function over each dataframe
for (i in seq_along(data_frames_list_depth_zone5)) {
  # Get the current data frame and label
  current_df <- get(names(data_frames_list_depth_zone5 )[i])  # Corrected to use get()
  label <- names(data_frames_list_depth_zone5)[i]
  
  # Calculate species richness for the 'label_name' column
  richness_result <- calculate_species_richness(current_df, 'label_name')
  
  # Store the result in the depth_zone_1_richness_results list (corrected variable)
  depth_zone_5_richness_results[[i]] <- richness_result
}

# Print the results
for (i in seq_along(depth_zone_5_richness_results)) {
  print(paste("Species richness for", names(data_frames_list_depth_zone5)[i], ":", depth_zone_5_richness_results[[i]]))
}

normalised_richness_depth_zone_5<- c(normalised_d10_7_richness <-  14/0.5775128,
                                     normalised_d10_8_richness <- 8/0.5867274,
                                     normalised_d10_9_richness<- 14/0.5630995, 
                                     normalised_d10_10_richness <- 7/0.5649504)

mean_richness_depth_zone5 <- sum(normalised_richness_depth_zone_5)/4
# 18.782

mean_richness_depth_zone5
#sd
standard_deviation5<-sd(normalised_richness_depth_zone_5)

# standard error 
standard_error5<- ((standard_deviation5)/sqrt(4))
standard_error5 #3.343218


sd_mean_richness_depth_zone5<-sd(normalised_richness_depth_zone_5/sqrt(length(normalised_richness_depth_zone_5)))

sd_mean_richness_depth_zone5 #0.03343218

                         
##### Sixth Depth Zone 1000-1200m ####           

# Create a list of data frames and corresponding labels
data_frames_list_depth_zone6 <- list(dive10_first50 = "d10_1_richness",
                                     dive10_second50 = "d10_2_richness",
                                     dive10_third50 = "d10_3_richness",
                                     dive10_fourth50 = "d10_4_richness",
                                     dive10_fifth50 = "d10_5_richness",
                                     dive10_sixth50 = "d10_6_richness")

# Create an empty list to store the species richness results
depth_zone_6_richness_results <-list()

#Iterate the species richness function over each dataframe
for (i in seq_along(data_frames_list_depth_zone6)) {
  # Get the current data frame and label
  current_df <- get(names(data_frames_list_depth_zone6 )[i])  # Corrected to use get()
  label <- names(data_frames_list_depth_zone6)[i]
  
  # Calculate species richness for the 'label_name' column
  richness_result <- calculate_species_richness(current_df, 'label_name')
  
  # Store the result in the depth_zone_1_richness_results list (corrected variable)
  depth_zone_6_richness_results[[i]] <- richness_result
}

# Print the results
for (i in seq_along(depth_zone_6_richness_results)) {
  print(paste("Species richness for", names(data_frames_list_depth_zone6)[i], ":", depth_zone_6_richness_results[[i]]))
}


normalised_richness_depth_zone_6<- c(normalised_d10_1_richness <-  15/0.5189615,
                                     normalised_d10_2_richness <- 4/0.5002559,
                                     normalised_d10_3_richness<- 2/0.5008992, 
                                     normalised_d10_4_richness <- 6/0.5018456,
                                     normalised_d10_5_richness <- 11/5.581254,
                                     normalised_d10_6_richness <- 15/5.608173)
             
#sd
standard_deviation6<-sd(normalised_richness_depth_zone_6)

# standard error 
standard_error6<- ((standard_deviation6)/sqrt(6))

standard_error6

# calculate mean
total_richnesss<-sum(normalised_richness_depth_zone_6)/6

mean_richness_depth_zone6 <- sum(normalised_richness_depth_zone_6)/6


# Your data
Depth_zone <- c("0-200m", "200-400m", "400-600m", "600-800m","800-1000m", "1000-1200m", "1200-1400m")
Richness <- c(2.594366, 2.962344, 12.3832, 24.83378, 0, 18.78242, 16.55066)
StandardError<-c(2.336812,1.824302,2.572811,6.189615,0, 3.343218,4.156782)
data <- data.frame(Depth_zone, Richness, StandardError)

# Reorder the Depth_zone factor based on the sorted data
data$Depth_zone <- factor(data$Depth_zone, levels = Depth_zone)

# Create the plot with error bars representing standard error
ggplot(data, aes(x = Depth_zone, y = Richness)) + 
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = Richness - StandardError, ymax = Richness + StandardError), 
                width = 0.3, position = position_dodge(0.9)) +
  labs(x = "Depth Zone", y = "Mean Richness") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # this is species/10m

