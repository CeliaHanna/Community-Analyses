library(ggplot2)


# Read the CSV file into a data frame
data <- data_dive10

# Define a function to convert time format to total seconds with accumulation
convert_time_to_seconds <- function(time_str) {
  time_parts <- as.numeric(strsplit(time_str, ":")[[1]])
  
  # Convert hours, minutes, and seconds to total seconds
  total_seconds <- time_parts[1] * 3600 + time_parts[2] * 60 + time_parts[3]
  
  return(total_seconds)
}

# Initialize a variable to store cumulative seconds
cumulative_seconds <- 0

# Iterate through rows and calculate cumulative seconds
for (i in 1:nrow(data)) {
  # Convert the current row's video duration to total seconds
  seconds_in_row <- convert_time_to_seconds(data[i, "video_duration"])
  
  # Add the seconds to the cumulative seconds
  cumulative_seconds <- cumulative_seconds + seconds_in_row
  
  # Update the cumulative seconds in the data frame
  data[i, "cumulative_seconds"] <- cumulative_seconds
}


data$speed <- data$cumulative_distance/data$cumulative_seconds

ggplot(data, aes(x = cumulative_distance, y = speed)) +
  geom_line() +
  xlab("Cumulative Distance") +
  ylab("Speed") +
  ggtitle("Speed vs Cumulative Distance Dive 8")

View(data)
