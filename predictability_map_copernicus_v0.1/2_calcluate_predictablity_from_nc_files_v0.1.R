#This script will create a map of predictability
#it loads all .rds files in a 15x15° folder (see script before)
#then they are stacked
#then then this is transformed into a data.frame (rows are pixels and cols timepoints)
#predictability is then calculated for each row of the dataframe
#this is then written into a map

#current version takes about 40 minutes for one 15x15 folder of 10 years with 1 core.

#TODOs
#Parallell Processing
#Work with terra::app function to avoid transforming it into a data.frame
#select timeframe
#save the partial maps to .rds
#handle missing data - if there is one timepoint missing the predictability is not calculated for that pixel


# Load necessary packages
library(terra)

# Define the directory
dir_path <- "/Users/ben/Desktop/cop_break/lon_120_lat_-15/"

# List all .rds files in the directory
rds_files <- list.files(path = dir_path, pattern = "\\.rds$", full.names = TRUE)

# Extract dates from file names and sort the file list by date
dates <- as.Date(substr(basename(rds_files), 5, 12), format = "%Y%m%d")
rds_files_sorted <- rds_files[order(dates)]

# Plot the first file
first_raster <- readRDS(rds_files_sorted[1])
terra::plot(first_raster)


#remove
#rds_files_sorted = rds_files_sorted[1:60]


# Create an empty list to store the rasters
rasters <- list()

# Loop through the sorted .rds files and read them into Rasters
for (i in seq_along(rds_files_sorted)) {
  r_temp <- readRDS(rds_files_sorted[i])
  rasters[[i]] <- r_temp
}
rm(r_temp)


# Create a RasterStack from the list of Rasters
raster_stack <- terra::rast(rasters)
rm(rasters)
# Function to calculate the percentage of missing values in a vector
missing_perc <- function(x) {
  return(sum(is.na(x)) / length(x) * 100)
}

# Calculate percentage of missing data for each pixel
raster_missing_perc <- terra::app(raster_stack, fun = missing_perc,cores=5)

# Plot percentage of missing data
plot(raster_missing_perc)

# Check if all dates between the first and the last date are present
all_dates <- seq(from = min(dates), to = max(dates), by = "day")
missing_dates <- all_dates[!all_dates %in% dates]

if (length(missing_dates) > 0) {
  message("The following dates are missing:")
  print(missing_dates)
} else {
  message("All dates between the first and last date are present.")
}

# Count the number of days and years it spans
num_days <- length(dates)
num_years <- num_days / 365.25

message(paste("The data spans", num_days, "days, or approximately", round(num_years, 2), "years."))





library(envPred)

df <- terra::as.data.frame(raster_stack, xy = TRUE, na.rm = FALSE)

# Set the starting date and end date of the time series
start_date <- as.Date("2022-01-01")
end_date <- start_date + (ncol(df) - 3)  # Subtract 3 to exclude x, y columns

# Create a sequence of dates
dates <- seq(start_date, end_date, by = "day")

env_pred_results <- list()  # Create an empty list before the loop

df_len =nrow(df)

# Generate a dummy time series
dummy_ts <- rnorm(length(dates))
# Get the names of the variables that `env_stats()` returns
env_stats_names <- names(env_stats(dummy_ts, dates = dates, noise_method = "spectrum", delta = 1))


# Loop through each row of the dataframe
for (i in 1:nrow(df)) {
  # Get the time series data for this pixel
  pixel_ts <- as.numeric(df[i, -c(1, 2)])

  # Check if there are any NA values (land pixel)
  if (any(is.na(pixel_ts))) {
    # If it is a land pixel, append NA result
    env_pred_results[[i]] <- rep(NA, times = length(env_stats_names))
  } else {
    # If it is not a land pixel, perform the calculation
    pred <- data.frame(env_stats(pixel_ts, dates = dates, noise_method = "spectrum", delta = 1))
    # Add pred to the env_pred_results list
    env_pred_results[[i]] <- pred
  }
  
  # Print only every hundredth iteration
  if (i %% 100 == 0) {
    print(paste(i,"of",df_len,"=",100*i/df_len,"%",Sys.time()))
    
  }
}
# Combine the predictability results from the list into a single dataframe
predictability_res <- do.call(rbind, env_pred_results)

predictability_df = cbind(x=df$x, y = df$y, predictability_res)


# Create a list to store the rasters
raster_list <- list()

# Loop through each column (excluding x, y) of the predictability_df
for (i in 3:ncol(predictability_df)) {
  # Extract the values
  stat_values <- predictability_df[[i]]
  
  # Create a new raster and assign the values
  stat_raster <- terra::rast(first_raster)
  terra::values(stat_raster) <- stat_values
  
  # Add the raster to the list
  raster_list[[i-2]] <- stat_raster
  
  # Name the layers of the raster stack
  names(raster_list[[i-2]]) <- names(predictability_df)[i]
}

# Create a raster stack from the list
raster_stack_pred <- terra::rast(raster_list)


# Plot the raster stack
# Define the color palette
color_palette <- viridis::magma(200)
# Plot the raster stack
plot(raster_stack_pred, col = color_palette)

saveRDS(raster_stack_pred,file = "~/Desktop/lon_120_lat_-15.rds")
# 
# # Combine the predictability raster with the existing raster
# raster_combined <- app(raster_missing_perc, raster_pred)
# 
# 
# # Create a new raster layer with the same extent and resolution as the existing raster
# raster_pred <- raster(raster_missing_perc, nrow=nrow(raster_missing_perc), ncol=ncol(raster_missing_perc))
# 
# # Assign the predictability values to the new raster layer
# values(raster_missing_perc) <- predictability_df$raw_mean
# plot(raster_missing_perc)
# 
# # Combine the predictability raster with the existing raster
# raster_combined <- c(raster_missing_perc, raster_pred)
# 
# 
# raster_pred <- raster(raster_missing_perc, nrow=240, ncol=240)
# 



