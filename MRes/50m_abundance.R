# Calculate abundance for each depth zone using 50m transects and plot 
# Normalise by dividing species abundance by 'along track' distance 

library(ggplot2)
library(dplyr)

#Writing a function to calculate abundance
calculate_species_abundance <- function(data_frame) {
  abundance <-nrow(data_frame) 
  return(abundance)
}

# First depth zone 0-200m # 5x 50m transects

# Specify custom names for the results

d8_19_abundance<- calculate_species_abundance(dive8_nineteenth50)
d8_20_abundance<- calculate_species_abundance(dive8_twentieth50)
d9_7_abundance<-calculate_species_abundance(dive9_seventh50)
d9_8_abundance<-calculate_species_abundance(dive9_eighth50)
d9_9_abundance<-calculate_species_abundance(dive9_ninth50)

# Define 'along track' lengths for each transect
transect_lengths <- c(50.24938, 54.99464, 51.6124, 60.18513, 50.02559)  # Replace with actual transect lengths


normalised_abundance_depthzone_1 <- c(normalised_d8_19_abundance <- d8_19_abundance/0.5024938,
                                      normalised_d8_20_abundance <- d8_20_abundance/0.5499464,
                                      normalised_d9_7_abundance <- d9_7_abundance/0.516124,
                                      normalised_d9_8_abundance <- d9_8_abundance /0.6018513,
                                      normalised_d9_9_abundance <- d9_9_abundance/0.5002559)

mean_abundance_depthzone_1<-sum(normalised_d8_19_abundance,normalised_d8_20_abundance,normalised_d9_7_abundance,
          normalised_d9_8_abundance, normalised_d9_9_abundance)/5


# 231.4514

#sd
standard_deviation<-sd(normalised_abundance_depthzone_1)
standard_deviation

# standard error 
standard_error<- ((standard_deviation)/sqrt(5))
standard_error #32.77899



####### Second depth zone 200-400m # 11x 50m transects ######

# create list of dataframes for the second depth zone (200:400m)

abundance_depthzone_2<- c(d8_15_abundance<- calculate_species_abundance(dive8_fifteenth50),
                                     d8_16_abundance<- calculate_species_abundance(dive8_sixteenth50),
                                     d8_17_abundance<- calculate_species_abundance(dive8_seventeenth50),
                                     d8_18_abundance<- calculate_species_abundance(dive8_eighteenth50),
                                     d9_1_abundance<- calculate_species_abundance(dive9_first50),
                                     d9_2_abundance<- calculate_species_abundance(dive9_second50),
                                     d9_3_abundance<- calculate_species_abundance(dive9_third50),
                                     d9_4_abundance<- calculate_species_abundance(dive9_fourth50),
                                     d9_5_abundance<- calculate_species_abundance(dive9_fifth50),
                                     d9_6_abundance<- calculate_species_abundance(dive9_sixth50))



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

normalised_depthzone2<- c(normalised_d8_15_abundance <- d8_15_abundance/0.6686823,
                          normalised_d8_16_abundance <- d8_16_abundance /0.5324369,
                          normalised_d8_17_abundance <- d8_17_abundance /0.5359562,
                          normalised_d8_18_abundance <- 6 /0.5304046,
                          normalised_d9_1_abundance <- d9_1_abundance /0.5442656,
                          normalised_d9_2_abundance <- d9_2_abundance /0.5635601,
                          normalised_d9_3_abundance <- d9_3_abundance /0.5567989,
                          normalised_d9_4_abundance <- 8 /0.580031,
                          normalised_d9_5_abundance <- d9_5_abundance /0.7035801)



print(sum(normalised_depthzone2)/9)


#sd
standard_deviation2<-sd(normalised_depthzone2)
standard_deviation2

# standard error 
standard_error2<- ((standard_deviation2)/sqrt(9))
standard_error2 #1.824302


##### Third Depth Zone 400-600m ####

# Create a list of data frames and corresponding labels

abundance_depthzone_3<- c(d8_2_abundance<- calculate_species_abundance(dive8_second50),
                          d8_3_abundance<-calculate_species_abundance(dive8_third50),
                          d8_4_abundance<-calculate_species_abundance(dive8_fourth50),
                          d8_5_abundance<-calculate_species_abundance(dive8_fifth50),
                          d8_6_abundance<-calculate_species_abundance(dive8_sixth50),
                          d8_7_abundance<-calculate_species_abundance(dive8_seventh50),
                          d8_8_abundance<-calculate_species_abundance(dive8_eighth50),
                          d8_9_abundance<-calculate_species_abundance(dive8_ninth50),
                          d8_10_abundance<-calculate_species_abundance(dive8_tenth50),
                          d8_11_abundance<-calculate_species_abundance(dive8_eleventh50),
                          d8_12_abundance<-calculate_species_abundance(dive8_twelfth50))
                       
data_frames_list_depth_zone3 <- list(dive8_second50 = "d8_2_abundance", #53.77769
                                     dive8_third50 ="d8_3_abundance",   #55.59218
                                     dive8_fourth50 = "d8_4_abundance", #51.6124
                                     dive8_fifth50 = "d8_5_abundance",  #50.20209
                                     dive8_sixth50 ="d8_6_abundance",   #50.31262
                                     dive8_seventh50 = "d8_7_abundance", #50.47385
                                     dive8_eighth50 = "d8_8_abundance", #51.17431
                                     dive8_ninth50 = "d8_9_abundance",  #71.56424
                                     dive8_tenth50 = "d8_10_abundance", #50.0001
                                     
                                     dive8_eleventh50 = "d8_11_abundance", #50.26967
                                     dive8_twelfth50 ="d8_12_abundance") #50.87593

d8_2_abundance


normalised_abundance_depth_zone_3<- c(normalised_d8_2_abundance <-  12/0.5377769,
                                      normalised_d8_3_abundance <- 11/0.5559218,
                                      normalised_d8_4_abundance <- 14/0.516124, 
                                      normalised_d8_5_abundance <- 5/0.5020209, 
                                      normalised_d8_6_abundance <- 1/0.5031262, 
                                      normalised_d8_7_abundance <- 2/0.5047385, 
                                      normalised_d8_8_abundance <- 2/0.5117431, 
                                      normalised_d8_9_abundance <- 11/0.7156424,
                                      normalised_d8_10_abundance <- 4/0.500001,
                                      normalised_d8_11_abundance <- 9/0.5026967,
                                      normalised_d8_12_abundance <-3/0.5087593) 




print(sum(normalised_abundance_depth_zone_3)/11)

#sd
standard_deviation3<-sd(normalised_abundance_depth_zone_3)
standard_deviation3

# standard error 
standard_error3<- ((standard_deviation3)/sqrt(11))
standard_error3 #2.572811



##### Fourth Depth Zone 600-800m ####

# Create a list of data frames and corresponding labels
abundance_depth_zone4 <- c(d7_2_abundance<- calculate_species_abundance(dive7_second50),
                           d7_3_abundance<-calculate_species_abundance(dive7_third50),
                           d7_4_abundance<-calculate_species_abundance(dive7_fourth50),
                           d7_5_abundance<-calculate_species_abundance(dive7_fifth50),
                           d8_1_abundance<-calculate_species_abundance(dive8_sixth50))
 

normalised_abundance_depth_zone_4<- c(normalised_d7_1_abundance <-  26/0.5268358,
                                      normalised_d7_2_abundance <- 17/0.5075126,
                                      normalised_d7_3_abundance<- 10/0.532094, 
                                      normalised_d7_4_abundance <- 3/0.5017609, 
                                      normalised_d7_5_abundance <- 8/0.5047385, 
                                      normalised_d8_1_abundance<- 18/0.7049887) 

print(sum(normalised_abundance_depth_zone_4)/6)

#sd
standard_deviation4<-sd(normalised_abundance_depth_zone_4)
standard_deviation4

# standard error 
standard_error4<- ((standard_deviation4)/sqrt(6))
standard_error4 #6.189615


##### Fifth Depth Zone 1000-1200m ####

# Create a list of data frames and corresponding labels
data_frames_list_depth_zone5 <- c(d10_7_abundance<-calculate_species_abundance(dive10_seventh50), 
                                  d10_8_abundance<-calculate_species_abundance(dive10_eighth50),
                                  d10_9_abundance<-calculate_species_abundance(dive10_ninth50),
                                  d10_10_abundance<-calculate_species_abundance(dive10_tenth50))


normalised_abundance_depth_zone_5<- c(normalised_d10_7_abundance <-  14/0.5775128,
                                      normalised_d10_8_abundance <- 8/0.5867274,
                                      normalised_d10_9_abundance<- 14/0.5630995, 
                                      normalised_d10_10_abundance <- 7/0.5649504)

print(sum(normalised_abundance_depth_zone_5)/4)

#sd
standard_deviation5<-sd(normalised_abundance_depth_zone_5)
standard_deviation5

# standard error 
standard_error5<- ((standard_deviation5)/sqrt(4))
standard_error5 #3.343218



##### Sixth Depth Zone 1000-1200m ####           

# Create a list of data frames and corresponding labels
abundance_list_depth_zone6 <- list(dive10_first50 = "d10_1_abundance",
                                     dive10_second50 = "d10_2_abundance",
                                     dive10_third50 = "d10_3_abundance",
                                     dive10_fourth50 = "d10_4_abundance",
                                     dive10_fifth50 = "d10_5_abundance",
                                     dive10_sixth50 = "d10_6_abundance")


normalised_abundance_depth_zone_6<- c(normalised_d10_1_abundance <-  15/0.5189615,
                                      normalised_d10_2_abundance <- 4/0.5002559,
                                      normalised_d10_3_abundance<- 2/0.5008992, 
                                      normalised_d10_4_abundance <- 6/0.5018456,
                                      normalised_d10_5_abundance <- 11/5.581254,
                                      normalised_d10_6_abundance <- 15/5.608173)


print(sum(normalised_abundance_depth_zone_6)/6)

#sd
standard_deviation6<-sd(normalised_abundance_depth_zone_6)
standard_deviation6

# standard error 
standard_error6<- ((standard_deviation6)/sqrt(6))
standard_error6 #4.156782

# Your data
Depth_zone <- c("0:200m", "200:400m", "400:600m", "600:800m", "800:1000m", "1000:1200m", "1200:1400m")
Mean_abundance <- c(231.4514, 113.7222, 12.3832, 24.83378, 0, 18.78242, 16.55066)
StandardError <- c(32.8, 27.28, 2.57, 6.18, 0, 3.34, 4.15)
data_abundance <- data.frame(Depth_zone, Mean_abundance, StandardError)


# Reorder the Depth_zone factor based on the sorted data
data_abundance$Depth_zone <- factor(data_abundance$Depth_zone, levels = data_abundance$Depth_zone)

# Create the plot
ggplot(data_abundance, aes(x = Depth_zone, y = Mean_abundance)) + 
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = Mean_abundance - StandardError, ymax = Mean_abundance + StandardError), 
                width = 0.3, position = position_dodge(0.9)) +
  labs(x = "Depth Zone", y = "Mean Abundance") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

