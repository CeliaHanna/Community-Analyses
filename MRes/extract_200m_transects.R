# load necessary packages
library(dplyr)


# Extracting 200m transects from dive routes 

# Extract first 200m transect from all dives

dive7_first_200m <- subset(data_dive7, data_dive7$cumulative_distance <=204)
View(dive7_first_200m) #203.00091m
View(data_dive7)

# normalising transect length 

depthrange = (max(dive7_first_200m$ROV.RovDepth)-min(dive7_first_200m $ROV.RovDepth))
dive7_first_200m_normalised_length <-sqrt((203.00091^2) + (depthrange^2))
dive7_first_200m_normalised_length #211.4849m


##### DIVE 8 200m TRANSECTS ######
# taking the first and sixth 200m transects from dive 8 

View(data_dive8)

dive8_first_200m <- subset(data_dive8, data_dive8$cumulative_distance <=201)
View(dive8_first_200m) #length of transect: 200.76535m

dive8_sixth_200m <- data_dive8 %>%
  filter(cumulative_distance >= 995.4088 & cumulative_distance <= 1200) 
View(dive8_sixth_200m) #length of transect: 201.7625m

##normalise transects to account for gradient. 
# normalised distance = sqrt((transect_length^2) + (depth_range^2))

dive8_first_200m_normalised_length <- sqrt((200.76535^2) + (0^2))
dive8_first_200m_normalised_length # 200.7654

max <- print(max(dive8_sixth_200m$ROV.RovDepth))
min <- print(min(dive8_sixth_200m$ROV.RovDepth))
max-min # 110.2 depth difference 

dive8_sixth_200m_normalised_length<- sqrt((201.7625^2) + (110.2^2))
dive8_sixth_200m_normalised_length #229.8959m


#### DIVE 9 200m TRANSECTS ########
dive9_first_200m<-subset(data_dive9,data_dive9$cumulative_distance<=205)
View(dive9_first_200m) # 204.86873m
View(data_dive9)

dive9_second_200m<- data_dive9 %>%
  filter(cumulative_distance > 205 & cumulative_distance <= 400)
View(dive9_second_200m) # 192.2974

# Normalise distance by gradient
depth_range = (max(dive9_first_200m$ROV.RovDepth)-min(dive9_first_200m$ROV.RovDepth))
dive9_first_200m_normalised_length <-sqrt((204.86873^2) + (depth_range^2))
dive9_first_200m_normalised_length #230.9432

depth_range2 = (max(dive9_second_200m$ROV.RovDepth)-min(dive9_second_200m$ROV.RovDepth))
print(dive9_second_200m_normalised_length <-sqrt((192.2974^2) + (depth_range2^2))) #238.5702


####DIVE 10 200m TRANSECTS #####
dive10_first_200m<-subset(data_dive10,data_dive10$cumulative_distance<=204.402803)
View(dive10_first200m) # 204.402803m 
View(data_dive10)

dive10_second_200m <- data_dive10 %>%
  filter(cumulative_distance >= 450 & cumulative_distance <=655)
View(dive10_second_200m) # 191.3849m

# Normalising for gradient

depth_range3 = (max(dive10_first_200m$ROV.RovDepth)-min(dive10_first_200m$ROV.RovDepth))
print(depth_range3) # 13.9m
dive10_first_200m_normalised <- sqrt((204.402803^2) + (depth_range3^2)) # 204.8749

depth_range4 = (max(dive10_second_200m$ROV.RovDepth)-min(dive10_second_200m$ROV.RovDepth))
depth_range4
dive10_second_200m_normalised = sqrt((191.3849^2) + (depth_range4^2)) # 223.7431



# Normalising transects to account for gradient differences - then you get 
# lengths to divide species richness by 

# diversity metrics do not rely on the assumption of normality so can calculate
# these before checking distributions # use vegan cheat sheet 

# 1) number of species (species richness)
# 2) Shannon and simpson
# 3) Rank abundance plot (whittaker plot) this also considers eveness 

# 4) start looking for composition differences

# then check for normal distribution between depth zones 
## then can start General linear Model making 