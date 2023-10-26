library(geosphere)
library(ggplot2)
library(tidyr)
library(dplyr)
install.packages("plot3D")
library(plot3D)

# This code plots a dive profile for dive 8 
# and colours route by useable vs non-useable distances 

data_dive7<-read.csv("/Users/user/Desktop/metadata_flow/completed_metadata_files/11056-dive7-file3.csv")
data_dive8_original<-read.csv("/Users/user/Desktop/metadata_flow/completed_metadata_files/merged_DIVE8.csv")
data_dive9<-read.csv("/Users/user/Desktop/metadata_flow/completed_metadata_files/merged_DIVE9.csv")
data_dive10<- read.csv("/Users/user/Desktop/metadata_flow/completed_metadata_files/merged_DIVE10.csv")

# Remove first 54 rows of dive 8, there is a coordinate error. 
data_dive8 <- data_dive8_original[-(1:54), ]

# calculate the distance of the route using the Haversine formula 

plot3d(data_dive8$lat,data_dive8$lon, data_dive8$ROV.RovDepth)

haversine_distance<-function(lat1,lon1,lat2,lon2){
  R <- 6371000.0 # radius of the earth in metres 
  # Convert latitude and longitude from degrees to radians
  lat1 <- lat1 * pi / 180
  lon1 <- lon1 * pi / 180
  lat2 <- lat2 * pi / 180
  lon2 <- lon2 * pi / 180
  # Haversine formula
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  distance <- R * c
  return(distance)
}

# calculate the distances between consecutive coordinate pairs 
# sum these to get cumulative distance along the route
# save as a new column called cumulative_distances

data_dive8 <- data_dive8%>%
  mutate(
    distance = haversine_distance(
      lat,lon,
      lag(lat, default=first(lat)),
      lag(lon,default=first(lon))
    )
  ) %>%
  mutate(
    cumulative_distance=cumsum(distance)
  )


data_dive8_original <- data_dive8_original%>%
  mutate(
    distance = haversine_distance(
      lat,lon,
      lag(lat, default=first(lat)),
      lag(lon,default=first(lon))
    )
  ) %>%
  mutate(
    cumulative_distance=cumsum(distance)
  )


data_dive7 <- data_dive7 %>%
  mutate(
    distance = haversine_distance(
      lat,lon,
      lag(lat, default=first(lat)),
      lag(lon,default=first(lon))
    )
  ) %>%
  mutate(
    cumulative_distance=cumsum(distance)
  )


data_dive9 <- data_dive9 %>%
  mutate(
    distance = haversine_distance(
      lat,lon,
      lag(lat, default=first(lat)),
      lag(lon,default=first(lon))
    )
  ) %>%
  mutate(
    cumulative_distance=cumsum(distance)
  )


data_dive10 <- data_dive10 %>%
  mutate(
    distance = haversine_distance(
      lat,lon,
      lag(lat, default=first(lat)),
      lag(lon,default=first(lon))
    )
  ) %>%
  mutate(
    cumulative_distance=cumsum(distance)
  )



# Cumulative distance is now added as an extra column in dive8 dataset

# Specify start and end cumulative distances of non-usable footage 
# By referring to duration in video where visibility reduces
# There are 3 sections of this video which were not usable.
# Define the cumulative distances for the beginning and end of these sections


# dive 8 file 3 between 23 mins and 1hr 32 
# dive 8 file 4 between 1h 30 mins and 1hr 39 mins DOING



start_distance = 65.3828# (-38.46123, 46.75477)  #00:36:20 
end_distance = 72.24624 # (-38.46121, 46.75485) #00:40:01
#6.86344

start_distance2 = 192 # (-38.46173,46.75601) #00:57:12
end_distance2 = 345.5449  # (-38.46271, 46.75724) #01:12:14
#153.5449

start_distance3 = 370 #01:15:31
end_distance3 = 461.4307 #00:18:10 
#91.4307
  
start_distance4 = 461.8439 # (-38.46356, 46.75801) #00:22:16
end_distance4 = 650 # (-38.46513, 46.75882) #01:33:05
#188.1561

start_distance5 = 1204 # (-38.46826,46.76155) #01:28:41
end_distance5 = 1219.112 #(-38.46839,46.76157) #01:39:34
#15.112
 
View(data_dive8)

# so total length of dive 8 transect = 1577.700-455.1071 = 1122.593m

# Create a new variable to indicate the segment color
# Update color assignment to correctly represent the ranges


data_dive8 <- data_dive8 %>%
  mutate(
    color = case_when(
      cumulative_distance >= start_distance & cumulative_distance <= end_distance ~ "range1",
      cumulative_distance >= start_distance2 & cumulative_distance <= end_distance2 ~ "range2",
      cumulative_distance >= start_distance3 & cumulative_distance <= end_distance3 ~ "range3",
      cumulative_distance >= start_distance4 & cumulative_distance <= end_distance4 ~ "range4",
      cumulative_distance >= start_distance5 & cumulative_distance <= end_distance5 ~ "range5",
      TRUE ~ "other"
    ) )


View(data_dive8)


# Define dive 9 missing video sections

# 00:20 - 00:28 mins
start_distance9 = 446  # (-38.47342,46.74672)
end_distance9 = 542.7423  #  (-38.47265,46.74721)
#96.7423

# 1hr41 to 1hr49
start_distance9.2 = 619 # (-38.47242,46.74798)
end_distance9.2 = 691.9646 #  (-38.47238,46.74881)
#72.9646

# so total dive 9 distance: 709.4761 - (96.7423+72.9646) = 539.7692m

data_dive9 <- data_dive9 %>%
  mutate(
    color = case_when(
      cumulative_distance >= start_distance9 & cumulative_distance <= end_distance9 ~ "range1",
      cumulative_distance >= start_distance9.2 & cumulative_distance <= end_distance9.2 ~ "range2",
      TRUE ~ "other"
    ) )


View(data_dive9)


# dive 10 from 324.650222 to 460 distance should not be included 

start_distance10 = 324.650222
end_distance10 = 460.949140
#136.2989

# so total dive 10 length = 702.6366 - 136.2989 = 566.3377m


data_dive10 <- data_dive10 %>%
  mutate(
    color = case_when(
      cumulative_distance >= start_distance10 & cumulative_distance <= end_distance10 ~ "range1",
      TRUE ~ "other"
    ) )

View(data_dive10)


p <- ggplot() +
  geom_line(data = data_dive9, aes(x = cumulative_distance, y = ROV.RovDepth, color = color, group = 1), size = 1) +
  geom_line(data = data_dive10, aes(x = cumulative_distance, y = ROV.RovDepth, color = color, group = 1), size = 1) +
  geom_line(data = data_dive7, aes(x = cumulative_distance, y = ROV.RovDepth, group = 1), size = 1, color = "green") +
  geom_line(data = data_dive8, aes(x = cumulative_distance, y = ROV.RovDepth, color = color, group = 1), size = 1) +
  scale_colour_manual(values = c("range1" = "red", "range2" = "red", "range3" = "red", "range4"= "red", "range5"="red", "other" = "green"),
                      guide = guide_legend(title = NULL)) +
  labs(x = "Distance (m)", y = "Depth (m)", title = "Melville Bank Dives - Sampling Effort") + 
  scale_y_reverse(breaks = seq(0, 1200, by = 200)) + 
  scale_x_continuous(breaks=seq(0,1800,by=200)) + 
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white")) +
  theme(legend.position = "none")

# Modify x-axis text size
p <- p + theme(axis.text.x = element_text(size = 8))
p
melvilledives_useable_distances <- recordPlot()  # saves plot in R's global environment 
