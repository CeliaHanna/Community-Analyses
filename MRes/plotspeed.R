# Plots of ROV speed
# Calculate speed (distance per unit time)

# Modified function to process dive data with accurate cumulative duration
# Convert the time format (hh:mm:ss) to total seconds with accumulation
# Define a function to convert time format to total seconds with accumulation
# Define a function to convert time format to total seconds with accumulation
convert_time_to_seconds <- function(time_str) {
  time_parts <- as.numeric(strsplit(time_str, ":")[[1]])
  
  # Initialize total seconds and previous duration
  total_seconds <- 0
  prev_duration <- 0
  
  for (i in 1:3) {
    # If the current duration is less than or equal to the previous duration, accumulate seconds
    if (time_parts[i] <= prev_duration) {
      total_seconds <- total_seconds + prev_duration
    } else {
      total_seconds <- total_seconds + time_parts[i]
    }
    
    # Update the previous duration
    prev_duration <- time_parts[i]
  }
  
  return(total_seconds)
}

# Modified function to process dive data with accurate cumulative duration
process_dive_data <- function(data_dive) {
  data_dive$video_duration_seconds <- sapply(as.character(data_dive$video_duration), convert_time_to_seconds)
  
  # Calculate cumulative seconds considering resets and decreases in duration
  data_dive$total_seconds <- cumsum(data_dive$video_duration_seconds)
  for (i in 2:length(data_dive$total_seconds)) {
    if (data_dive$video_duration_seconds[i] <= data_dive$video_duration_seconds[i - 1]) {
      data_dive$total_seconds[i] <- data_dive$total_seconds[i - 1] + data_dive$video_duration_seconds[i]
    }
  }
  
  data_dive$speed <- data_dive$cumulative_distance / data_dive$total_seconds
  return(data_dive)
}

# Assuming you have 4 dataframes: data_dive1, data_dive2, data_dive3, data_dive4

# Custom names for the plots
custom_names <- c("Custom Name 1", "Custom Name 2", "Custom Name 3", "Custom Name 4")

# Process each dive data and store them in a list
dive_data_list <- list(
  process_dive_data(data_dive7),
  process_dive_data(data_dive8),
  process_dive_data(data_dive9),
  process_dive_data(data_dive10)
)

# Initialize variables to store the y-axis range
min_speed <- Inf
max_speed <- -Inf

# Find the overall y-axis range across all dives
for (i in 1:4) {
  min_speed <- min(min_speed, min(dive_data_list[[i]]$speed))
  max_speed <- max(max_speed, max(dive_data_list[[i]]$speed))
}

# Plot the data in a panel plot
par(mfrow=c(2, 2))  # Create a 2x2 panel plot

for (i in 1:4) {
  plot(
    dive_data_list[[i]]$total_seconds, 
    dive_data_list[[i]]$speed, 
    type = "l", 
    xlab = "Total Seconds", 
    ylab = "Speed",
    main = custom_names[i],  # Use custom names for titles
    ylim = c(min_speed, max_speed)  # Set y-axis limits to the overall range
  )
  
  # Optionally, add labels for clarity
  points(
    dive_data_list[[i]]$total_seconds, 
    dive_data_list[[i]]$speed, 
    col = "blue", 
    pch = 16
  )
  
  # Optionally, add a trendline
  fit <- lm(dive_data_list[[i]]$speed ~ dive_data_list[[i]]$total_seconds)
  abline(fit, col = "red")
}

# Reset plotting settings
par(mfrow=c(1, 1))  # Reset plotting to a single panel

}

# Reset plotting settings
par(mfrow=c(1, 1))  # Reset plotting to a single panel
View(data_dive9)

____________

# Convert video_duration to total seconds
data_dive10$video_duration_seconds <- sapply(as.character(data_dive10$video_duration), convert_time_to_seconds)

# Calculate speed
data_dive10$speed <- data_dive9$cumulative_distance / data_dive10$video_duration_seconds

# Plot the speed over time
plot(data_dive10$video_duration_seconds, data_dive10$speed, 
     type = "l", xlab = "Video Duration (seconds)", ylab = "Speed",
     main = "Speed vs Video Duration Dive 9")

# Optionally, add labels for clarity
points(data_dive7$video_duration_seconds, data_dive7$speed, col = "blue", pch = 16)

# Optionally, add a trendline
fit <- lm(data_dive7$speed ~ data_dive7$video_duration_seconds)
abline(fit, col = "red")
