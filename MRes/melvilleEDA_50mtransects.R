library(dplyr)
library(ggplot2)

data_dive7 <- read.csv("/Users/user/Desktop/metadata_flow/completed_metadata_files/11056-dive7-file3.csv")
data_dive8 <- read.csv("/Users/user/Desktop/metadata_flow/completed_metadata_files/merged_DIVE8.csv")
data_dive9 <- read.csv("/Users/user/Desktop/metadata_flow/completed_metadata_files/merged_DIVE9.csv") 
data_dive10<- read.csv("/Users/user/Desktop/metadata_flow/completed_metadata_files/merged_DIVE10.csv")


# Splitting dive routes up into 50m transects across 8 depth zones of 100m
# use objects that you have added cumulative distances to 
data_dive7
data_dive8
data_dive9
data_dive10


# split dive 7 into 100m transects # should split into 5 transects 

#### TRANSECT 1 ####
dive7_first50 <- data_dive7 %>%
  filter(cumulative_distance >= 0 & cumulative_distance <= 50) 

#DR stands for Depth Range
dive7_first50DR<-(max(dive7_first50$ROV.RovDepth)-min(dive7_first50$ROV.RovDepth))
dive7_first50DR # 16.6 metre depth range 
normalised_dive7_first50 <- sqrt((50^2) + (16.6^2)) # normalizes for gradient to get "along track" distance 
normalised_dive7_first50 #52.68358

dive7_first50<- dive7_first50 %>%
  mutate(depth_zone = '600-800m')

file_path <- "/Users/user/Desktop/dive7_transect2.csv"

# Export the data frame to a CSV file
write.csv(dive7_second50,file = file_path, row.names = FALSE)

#### TRANSECT 2 #####
dive7_second50 <- data_dive7 %>%
  filter(cumulative_distance >=50 & cumulative_distance <=100) 

View(dive7_second50)

print(dive7_second50DR<-(max(dive7_second50$ROV.RovDepth)-min(dive7_second50$ROV.RovDepth)))
normalised_second50_d7 <- sqrt((50^2) + (8.7^2))
normalised_second50_d7 #50.75126m 

dive7_second50<- dive7_second50 %>%
  mutate(depth_zone = '600-800m')

##### TRANSECT 3 ####

dive7_third50 <- data_dive7 %>%
  filter(cumulative_distance >=100 & cumulative_distance <=150) #length: 
View(dive7_third50)

print(dive7_third50DR<-(max(dive7_third50$ROV.RovDepth)-min(dive7_third50$ROV.RovDepth)))
View(dive7_third50)

normalised_third50_d7 <- sqrt((50^2) + (18.2^2))
normalised_third50_d7 #53.2094

dive7_third50<- dive7_third50 %>%
  mutate(depth_zone = '600-800m')

#### TRANSECT 4 ####

dive7_fourth50 <- data_dive7 %>%
  filter(cumulative_distance >=150 & cumulative_distance <=200) #length: 
print(dive7_fourth50DR<-(max(dive7_fourth50$ROV.RovDepth)-min(dive7_fourth50$ROV.RovDepth)))

normalised_fourth50_d7 <- sqrt((50^2) + (4.2^2))
normalised_fourth50_d7 #50.17609m 

dive7_fourth50<- dive7_fourth50 %>%
  mutate(depth_zone = '600-800m')

#### TRANSECT 5 ####

dive7_fifth50 <- data_dive7 %>%
  filter(cumulative_distance >=200 & cumulative_distance <=250) #length: 
print(dive7_fifth50DR<-(max(dive7_fifth50$ROV.RovDepth)-min(dive7_fifth50$ROV.RovDepth)))
normalised_fifth50_d7 <- sqrt((50^2) + (6.9^2))
normalised_fifth50_d7 #50.47385m

# adding depth zone column to transect

dive7_fifth50<- dive7_fifth50 %>%
  mutate(depth_zone = '600-800m')

#### SPLITTING DIVE 8 UP INTO 50M TRANSECTS ##### 

#### Transect 1 #####

dive8_first50 <- data_dive8 %>%
  filter(cumulative_distance >= 0 & cumulative_distance <= 50) 
print(dive8_first50DR<-(max(dive8_first50$ROV.RovDepth)-min(dive8_first50$ROV.RovDepth)))
normalised_first50_d8 <- sqrt((50^2) + (49.7^2))
normalised_first50_d8 #70.49887m

dive8_first50 <- dive8_first50%>%
  mutate(depth_zone = '600-800m')

##### Transect 2 #####

dive8_second50 <- data_dive8 %>%
  filter(cumulative_distance >= 100 & cumulative_distance <= 150) 
print(dive8_second50DR<-(max(dive8_second50$ROV.RovDepth)-min(dive8_second50$ROV.RovDepth)))
normalised_second50_d8 <- sqrt((50^2) + (19.8^2))
normalised_second50_d8 #53.77769

dive8_second50 <- dive8_second50%>%
  mutate(depth_zone = '400-600m')

###### Transect 3 #####

dive8_third50 <- data_dive8 %>%
  filter(cumulative_distance >= 150 & cumulative_distance <= 200) 

print(dive8_third50DR<-(max(dive8_third50$ROV.RovDepth)-min(dive8_third50$ROV.RovDepth)))
normalised_third50_d8 <- sqrt((50^2) + (24.3^2))
normalised_third50_d8 #55.59218

dive8_third50 <- dive8_third50%>%
  mutate(depth_zone = '400-600m')

#### Transect 4 #####

dive8_fourth50 <- data_dive8 %>%
  filter(cumulative_distance >= 650 & cumulative_distance <= 700) 

print(dive8_fourth50DR<-(max(dive8_fourth50$ROV.RovDepth)-min(dive8_fourth50$ROV.RovDepth)))

normalised_fourth50_d8 <- sqrt((50^2) + (12.8^2))
normalised_fourth50_d8 #51.6124

dive8_fourth50 <- dive8_fourth50%>%
  mutate(depth_zone = '400-600m')

#### Transect 5 #####

dive8_fifth50 <- data_dive8 %>%
  filter(cumulative_distance >= 700 & cumulative_distance <= 750) 
print(dive8_fifth50DR<-(max(dive8_fifth50$ROV.RovDepth)-min(dive8_fifth50$ROV.RovDepth)))

normalised_fifth50_d8 <- sqrt((50^2) + (4.5^2))
normalised_fifth50_d8 #50.20209

dive8_fifth50<- dive8_fifth50 %>%
  mutate(depth_zone = '400-600m')


#### Transect 6 #####

dive8_sixth50 <- data_dive8 %>%
  filter(cumulative_distance >= 750 & cumulative_distance <= 800) 
print(dive8_sixth50DR<-(max(dive8_sixth50 $ROV.RovDepth)-min(dive8_sixth50$ROV.RovDepth)))

normalised_sixth50_d8 <- sqrt((50^2) + (5.6^2))
normalised_sixth50_d8 #50.31262

dive8_sixth50 <-dive8_sixth50  %>%
  mutate(depth_zone = '400-600m')


#### Transect 7 ####

dive8_seventh50 <- data_dive8 %>%
  filter(cumulative_distance >= 800 & cumulative_distance <= 850) 
print(dive8_seventh50DR<-(max(dive8_seventh50 $ROV.RovDepth)-min(dive8_seventh50$ROV.RovDepth)))

normalised_seventh50_d8 <- sqrt((50^2) + (6.9^2))
normalised_seventh50_d8 #50.47385

dive8_seventh50 <-dive8_seventh50  %>%
  mutate(depth_zone = '400-600m')

### Transect 8 ####

dive8_eighth50 <- data_dive8 %>%
  filter(cumulative_distance >= 850 & cumulative_distance <= 900) 
print(dive8_eighth50DR<-(max(dive8_eighth50$ROV.RovDepth)-min(dive8_eighth50$ROV.RovDepth)))

normalised_eighth50_d8 <- sqrt((50^2) + (10.9^2))
normalised_eighth50_d8 #51.17431

dive8_eighth50 <-dive8_eighth50  %>%
  mutate(depth_zone = '400-600m')

#### Transect 9 ####

dive8_ninth50 <- data_dive8 %>%
  filter(cumulative_distance >= 900 & cumulative_distance <= 950) 
print(dive8_ninth50DR<-(max(dive8_ninth50$ROV.RovDepth)-min(dive8_ninth50$ROV.RovDepth)))

normalised_ninth50_d8<- sqrt((50^2) + (51.2^2))
normalised_ninth50_d8 #71.56424

dive8_ninth50  <-dive8_ninth50  %>%
  mutate(depth_zone = '400-600m')

#### Transect 10 ####

dive8_tenth50 <- data_dive8 %>%
  filter(cumulative_distance >= 950 & cumulative_distance <= 1000) 
print(dive8_tenth50DR<-(max(dive8_tenth50$ROV.RovDepth)-min(dive8_tenth50$ROV.RovDepth)))

normalised_tenth50_d8<- sqrt((50^2) + (0.1^2))
normalised_tenth50_d8 #50.0001

dive8_tenth50 <-dive8_tenth50  %>%
  mutate(depth_zone = '400-600m')


#### Transect 11 ####

dive8_eleventh50 <- data_dive8 %>%
  filter(cumulative_distance >= 1000 & cumulative_distance <= 1050) 
print(dive8_eleventh50DR<-(max(dive8_eleventh50$ROV.RovDepth)-min(dive8_eleventh50$ROV.RovDepth)))

normalised_eleventh50_d8<- sqrt((50^2) + (5.2^2))
normalised_eleventh50_d8 #50.26967

dive8_eleventh50 <-dive8_eleventh50 %>%
  mutate(depth_zone = '400-600m')

#### Transect 12 ####

dive8_twelfth50 <- data_dive8 %>%
  filter(cumulative_distance >= 1050 & cumulative_distance <= 1100) 
print(dive8_twelfth50DR<-(max(dive8_twelfth50$ROV.RovDepth)-min(dive8_twelfth50$ROV.RovDepth)))

normalised_twelfth50_d8<- sqrt((50^2) + (9.4^2))
normalised_twelfth50_d8 #50.87593

dive8_twelfth50<-dive8_twelfth50%>%
  mutate(depth_zone = '400-600m')


#### Transect 13 ####

dive8_thirteenth50 <- data_dive8 %>%
  filter(cumulative_distance >= 1100 & cumulative_distance <= 1150) 
print(dive8_thirteenth50DR<-(max(dive8_thirteenth50$ROV.RovDepth)-min(dive8_thirteenth50$ROV.RovDepth)))

normalised_thirteenth50_d8<- sqrt((50^2) + (15.8^2))
normalised_thirteenth50_d8 #52.43701

dive8_thirteenth50 <-dive8_thirteenth50 %>%
  mutate(depth_zone = '400-600m')

#### Transect 14 ####

dive8_fourteenth50 <- data_dive8 %>%
  filter(cumulative_distance >= 1150 & cumulative_distance <= 1200) 
print(dive8_fourteenth50DR<-(max(dive8_fourteenth50$ROV.RovDepth)-min(dive8_fourteenth50$ROV.RovDepth)))

normalised_fourteenth50_d8<- sqrt((50^2) + (35.3^2))
normalised_fourteenth50_d8 #61.20531

dive8_fourteenth50 <-dive8_fourteenth50 %>%
  mutate(depth_zone = '400-600m')


#### Transect 15 ####

dive8_fifteenth50 <- data_dive8 %>%
  filter(cumulative_distance >= 1250 & cumulative_distance <= 1300) 
print(dive8_fifteenth50DR<-(max(dive8_fifteenth50$ROV.RovDepth)-min(dive8_fifteenth50$ROV.RovDepth)))

normalised_fifteenth50_d8<- sqrt((50^2) + (44.4^2))
normalised_fifteenth50_d8 #66.86823

dive8_fifteenth50 <- dive8_fifteenth50 %>%
  mutate(depth_zone = '200-400m')

#### Transect 16 ####

dive8_sixteenth50 <- data_dive8 %>%
  filter(cumulative_distance >= 1300 & cumulative_distance <= 1350) 
print(dive8_sixteenth50DR<-(max(dive8_sixteenth50$ROV.RovDepth)-min(dive8_sixteenth50$ROV.RovDepth)))

normalised_sixteenth50_d8<- sqrt((50^2) + (18.3^2))
normalised_sixteenth50_d8 #53.24369

dive8_sixteenth50 <- dive8_sixteenth50 %>%
  mutate(depth_zone = '200-400m')


#### Transect 17 ####

dive8_seventeenth50 <- data_dive8 %>%
  filter(cumulative_distance >= 1400 & cumulative_distance <= 1450) 

View(dive8_seventeenth50)
print(dive8_seventeenth50DR<-(max(dive8_seventeenth50$ROV.RovDepth)-min(dive8_seventeenth50$ROV.RovDepth)))

normalised_seventeenth50_d8<- sqrt((50^2) + (19.3^2))
normalised_seventeenth50_d8 #53.59562

dive8_seventeenth50<- dive8_seventeenth50 %>%
  mutate(depth_zone = '200-400m')


#### Transect 18 ####

dive8_eighteenth50 <- data_dive8 %>%
  filter(cumulative_distance >= 1450 & cumulative_distance <= 1500) 

print(dive8_eighteenth50DR<-(max(dive8_eighteenth50$ROV.RovDepth)-min(dive8_eighteenth50$ROV.RovDepth)))

normalised_eighteenth50_d8<- sqrt((50^2) + (17.7^2))
normalised_eighteenth50_d8 # 53.04046

dive8_eighteenth50<- dive8_eighteenth50%>%
  mutate(depth_zone = '200-400m')

#### Transect 19 ####

dive8_nineteenth50<- data_dive8 %>%
  filter(cumulative_distance >= 1500 & cumulative_distance <= 1550) 

print(dive8_nineteenth50DR<-(max(dive8_nineteenth50$ROV.RovDepth)-min(dive8_nineteenth50$ROV.RovDepth)))

normalised_nineteenth50_d8<- sqrt((50^2) + (5^2))
normalised_nineteenth50_d8 # 50.24938

dive8_nineteenth50<- dive8_nineteenth50%>%
  mutate(depth_zone = '0-200m')

#### Transect 20 #####

dive8_twentieth50 <- data_dive8 %>%
  filter(cumulative_distance >= 1550 & cumulative_distance <= 1600) 

print(dive8_twentieth50DR<-(max(dive8_twentieth50$ROV.RovDepth)-min(dive8_twentieth50$ROV.RovDepth)))

normalised_twentieth50_d8<- sqrt((50^2) + (22.9^2))
normalised_twentieth50_d8 # 54.99464

dive8_twentieth50<- dive8_twentieth50%>%
  mutate(depth_zone = '0-200m')

####DIVE 10 ####

#### Transect 1 #####

dive10_first50 <- data_dive10 %>%
  filter(cumulative_distance >= 0 & cumulative_distance <= 50) 

print(dive10_first50DR<-(max(dive10_first50$ROV.RovDepth)-min(dive10_first50$ROV.RovDepth)))

normalised_first50_d10<- sqrt((50^2) + (13.9^2))
normalised_first50_d10 #51.89615

dive10_first50<- dive10_first50 %>%
  mutate(depth_zone = '1200-1400m')

####Transect 2 ###

View(data_dive10)

dive10_second50 <- data_dive10 %>%
  filter(cumulative_distance >= 50 & cumulative_distance <= 100) 

print(dive10_second50DR<-(max(dive10_second50$ROV.RovDepth)-min(dive10_second50$ROV.RovDepth)))

normalised_second50_d10<- sqrt((50^2) + (1.6^2))
normalised_second50_d10 #50.02559

dive10_second50 <- dive10_second50 %>%
  mutate(depth_zone = '1200-1400m')

#### Transect 3 ####

View(data_dive10)

dive10_third50 <- data_dive10 %>%
  filter(cumulative_distance >= 100 & cumulative_distance <= 150) 

print(dive10_third50DR<-(max(dive10_third50$ROV.RovDepth)-min(dive10_third50$ROV.RovDepth)))

normalised_third50_d10<- sqrt((50^2) + (3^2))
normalised_third50_d10 #50.08992

dive10_third50 <- dive10_third50 %>%
  mutate(depth_zone = '1200-1400m')

#### Transect 4 ####

dive10_fourth50 <- data_dive10 %>%
  filter(cumulative_distance >= 150 & cumulative_distance <= 200) 

print(dive10_fourth50DR<-(max(dive10_fourth50$ROV.RovDepth)-min(dive10_fourth50$ROV.RovDepth)))

normalised_fourth50_d10<- sqrt((50^2) + (4.3^2))
normalised_fourth50_d10 #50.18456

dive10_fourth50 <- dive10_fourth50 %>%
  mutate(depth_zone = '1200-1400m')

### Transect 5 ####

dive10_fifth50 <- data_dive10 %>%
  filter(cumulative_distance >= 200 & cumulative_distance <= 250) 

print(dive10_fifth50DR<-(max(dive10_fifth50$ROV.RovDepth)-min(dive10_fifth50$ROV.RovDepth)))

normalised_fifth50_d10<- sqrt((50^2) + (24.8^2))
normalised_fifth50_d10 # 55.81254

dive10_fifth50 <- dive10_fifth50 %>%
  mutate(depth_zone = '1200-1400m')

## Transect 6 ###

dive10_sixth50 <- data_dive10 %>%
  filter(cumulative_distance >= 250 & cumulative_distance <= 300) 

print(dive10_sixth50DR<-(max(dive10_sixth50$ROV.RovDepth)-min(dive10_sixth50$ROV.RovDepth)))

normalised_sixth50_d10<- sqrt((50^2) + (25.4^2))
normalised_sixth50_d10 # 56.08173

dive10_sixth50 <- dive10_sixth50 %>%
  mutate(depth_zone = '1200-1400m')


### Transect 7 ####

dive10_seventh50 <- data_dive10 %>%
  filter(cumulative_distance >= 500 & cumulative_distance <= 550) 

print(dive10_seventh50DR<-(max(dive10_seventh50 $ROV.RovDepth)-min(dive10_seventh50$ROV.RovDepth)))

normalised_seventh50_d10<- sqrt((50^2) + (28.9^2))
normalised_seventh50_d10# 57.75128

dive10_seventh50  <- dive10_seventh50 %>%
  mutate(depth_zone = '1000-1200m')

#### Transect 8 ####

dive10_eighth50 <- data_dive10 %>%
  filter(cumulative_distance >= 550 & cumulative_distance <= 600) 

print(dive10_eighth50DR<-(max(dive10_eighth50$ROV.RovDepth)-min(dive10_eighth50$ROV.RovDepth)))

normalised_eighth50_d10<- sqrt((50^2) + (30.7^2))
normalised_eighth50_d10 #58.67274

dive10_eighth50  <- dive10_eighth50 %>%
  mutate(depth_zone = '1000-1200m')

#### Transect 9 ####

dive10_ninth50 <- data_dive10 %>%
  filter(cumulative_distance >= 600 & cumulative_distance <= 650) 

print(dive10_ninth50DR<-(max(dive10_ninth50$ROV.RovDepth)-min(dive10_ninth50$ROV.RovDepth)))

normalised_ninth50_d10<- sqrt((50^2) + (25.9^2))
normalised_ninth50_d10 #56.30995

dive10_ninth50  <- dive10_ninth50 %>%
  mutate(depth_zone = '1000-1200m')



#### Transect 10 ####

dive10_tenth50 <- data_dive10 %>%
  filter(cumulative_distance >= 650 & cumulative_distance <= 700) 

print(dive10_tenth50DR<-(max(dive10_tenth50$ROV.RovDepth)-min(dive10_tenth50$ROV.RovDepth)))

normalised_tenth50_d10<- sqrt((50^2) + (26.3^2))
normalised_tenth50_d10 #56.49504

dive10_tenth50  <- dive10_tenth50 %>%
  mutate(depth_zone = '1000-1200m')


### DIVE 9 ### 

### Transect 1 D9 ### 

dive9_first50<- data_dive9 %>% 
  filter(cumulative_distance>=0 & cumulative_distance <=50)
print(dive9_first50DR<-(max(dive9_first50$ROV.RovDepth)-min(dive9_first50$ROV.RovDepth)))

normalised_dive9_first50<- sqrt((50^2) + (21.5^2))
normalised_dive9_first50 #54.42656

dive9_first50  <- dive9_first50%>%
  mutate(depth_zone = '200-400m')
         
#### Transect 2 D9 #####

dive9_second50<- data_dive9 %>% 
  filter(cumulative_distance>=50 & cumulative_distance <=100)
print(dive9_second50DR<-(max(dive9_second50$ROV.RovDepth)-min(dive9_second50$ROV.RovDepth)))

normalised_dive9_second50<- sqrt((50^2) + (26^2))
normalised_dive9_second50 #56.35601

dive9_second50  <- dive9_second50%>%
  mutate(depth_zone = '200-400m')

##### Transect 3 D9 #####

dive9_third50<- data_dive9 %>% 
  filter(cumulative_distance>=100 & cumulative_distance <=150)
print(dive9_third50DR<-(max(dive9_third50$ROV.RovDepth)-min(dive9_third50$ROV.RovDepth)))

normalised_dive9_third50<- sqrt((50^2) + (24.5^2))
normalised_dive9_third50 #55.67989

dive9_third50  <- dive9_third50%>%
  mutate(depth_zone = '200-400m')


####### Transect 4 D9 ######

dive9_fourth50<- data_dive9 %>% 
  filter(cumulative_distance>=150 & cumulative_distance <=200)
print(dive9_fourth50DR<-(max(dive9_fourth50$ROV.RovDepth)-min(dive9_fourth50$ROV.RovDepth)))

normalised_dive9_fourth50<- sqrt((50^2) + (29.4^2))
normalised_dive9_fourth50 #58.0031

dive9_fourth50  <- dive9_fourth50%>%
  mutate(depth_zone = '200-400m')


####### Transect 5 D9 ######

dive9_fifth50<- data_dive9 %>% 
  filter(cumulative_distance>=200 & cumulative_distance <=250)
print(dive9_fifth50DR<-(max(dive9_fifth50$ROV.RovDepth)-min(dive9_fifth50$ROV.RovDepth)))

normalised_dive9_fifth50<- sqrt((50^2) + (49.5^2))
normalised_dive9_fifth50 #70.35801

dive9_fifth50  <- dive9_fifth50%>%
  mutate(depth_zone = '200-400m')


##### Transect 6 D9 #####

dive9_sixth50<- data_dive9 %>% 
  filter(cumulative_distance>=250 & cumulative_distance <=300)
print(dive9_sixth50DR<-(max(dive9_sixth50$ROV.RovDepth)-min(dive9_sixth50$ROV.RovDepth)))

normalised_dive9_sixth50<- sqrt((50^2) + (37.7^2))
normalised_dive9_sixth50 #62.6202

dive9_sixth50  <- dive9_sixth50%>%
  mutate(depth_zone = '200-400m')


##### Transect 7 D9 ####

dive9_seventh50<- data_dive9 %>% 
  filter(cumulative_distance>=300 & cumulative_distance <=350)
print(dive9_seventh50DR<-(max(dive9_seventh50$ROV.RovDepth)-min(dive9_seventh50$ROV.RovDepth)))

normalised_dive9_seventh50<- sqrt((50^2) + (12.8^2))
normalised_dive9_seventh50 #51.6124

dive9_seventh50  <- dive9_seventh50%>%
  mutate(depth_zone = '0-200m')


##### Transect 8 D9 ####

dive9_eighth50<- data_dive9 %>% 
  filter(cumulative_distance>=350 & cumulative_distance <=400)
print(dive9_eighth50DR<-(max(dive9_eighth50$ROV.RovDepth)-min(dive9_eighth50$ROV.RovDepth)))

normalised_dive9_eighth50<- sqrt((50^2) + (33.5^2))
normalised_dive9_eighth50 #60.18513

dive9_eighth50  <- dive9_eighth50%>%
  mutate(depth_zone = '0-200m')

##### Transect 9 D9 ####

dive9_ninth50<- data_dive9 %>% 
  filter(cumulative_distance>=550 & cumulative_distance <=600)
print(dive9_ninth50DR<-(max(dive9_ninth50$ROV.RovDepth)-min(dive9_ninth50$ROV.RovDepth)))

normalised_dive9_ninth50<- sqrt((50^2) + (1.6^2))
normalised_dive9_ninth50 #50.02559

dive9_ninth50 <- dive9_ninth50%>%
  mutate(depth_zone = '0-200m')


View(data_dive10)
