library(terra)
library(data.table)
library(doParallel)
library(envPred)

# Function to process a tile
process_tile <- function(dir_path, start_date, end_date, results_path) {
  # Check if this tile has already been processed
  result_file <- file.path(results_path, paste0("pred_from_",start_date,"_to_",end_date, basename(dir_path), ".rds"))
  if (file.exists(result_file)) {
    message(paste("Skipping", dir_path, "- already processed."))
    return(NULL)
  }
  
  # List all .rds files in the directory
  rds_files <- list.files(path = dir_path, pattern = "\\.rds$", full.names = TRUE)
  
  # Extract dates from file names and filter the file list by date
  dates <- as.Date(substr(basename(rds_files), 5, 12), format = "%Y%m%d")
  rds_files_sorted <- rds_files[order(dates)]
  rds_files_filtered <- rds_files_sorted[dates >= start_date & dates <= end_date]

  dates <- dates[dates >= start_date & dates <= end_date]
  
  # Read the sorted and filtered .rds files into Rasters
  rasters <- lapply(rds_files_filtered, readRDS)
  

  # Create a RasterStack from the list of Rasters
  raster_stack <- terra::rast(rasters)
  rm(rasters)
  
  # Convert the RasterStack to a data.table
  df <- setDT(terra::as.data.frame(raster_stack, xy = TRUE, na.rm = FALSE))
  rm(raster_stack)
  
  # Generate a dummy time series
  dummy_ts <- rnorm(length(dates))
  
  # Get the names of the variables that `env_stats()` returns
  env_pred_colnames = names(env_stats(dummy_ts, dates = dates, noise_method = "spectrum", delta = 1))
  
  null_env_pred = as.numeric(rep(NA,19))
  
  #save coordinates into dataframe and remove from dataframe
  coords = df[, c(1, 2)]
  df = df[, -c(1, 2)]
  a=gc()
  # Parallelize the loop
  numCores <- detectCores()
  cl <- makeCluster(numCores)
  registerDoParallel(cl)
  
  Sys.time()
  env_pred_results <- foreach(i = 1:nrow(df), .packages = c("envPred")) %dopar% {
      pixel_ts <- as.numeric(df[i,])
    
    # Only calculate env_stats if there are no NA values
    if (any(is.na(pixel_ts))) {
      pred <- null_env_pred
      } else {
        pred <- tryCatch({
          as.numeric(env_stats(pixel_ts, dates = dates, noise_method = "spectrum", delta = 1))
        }, error = function(e) {
          message(paste("Error in env_stats at row ", i, ": ", e$message))
          pred = null_env_pred  # Return the default result in case of error
        })
    
     }
      rm(pixel_ts)
     pred  # This will return pred
  }
  
  Sys.time()
  stopCluster(cl)
  
  rm(df)
  a=gc()
  
  
  # Convert the list of numerics to a matrix, transpose it and then convert to data.table
  predictability_res <- data.table(do.call("rbind", env_pred_results))
  
  # Set column names
  setnames(predictability_res, env_pred_colnames)
  
  # Add coordinates
  predictability_df <- cbind(coords, predictability_res)
  
  # Save the results
  saveRDS(predictability_df, file = result_file)
  
  rm(predictability_res, predictability_df,env_pred_results, coords)
  a=gc()
}

# Define paths and dates
tile_paths <- list.dirs("/Users/ben/Desktop/cop_break", recursive = FALSE)  # replace with your actual path
start_date <- as.Date("2012-01-01")
end_date <- as.Date("2022-01-01")  # replace with your actual end date
results_path <- "/Users/ben/Desktop/cop_res"  # replace with your actual results path

for (idx in seq_along(tile_paths)) {
  
  tile_path <- tile_paths[idx]
  message(paste0("Processing tile ", idx, " of ", length(tile_paths), " (", tile_path, ")"))
  message(Sys.time())
  
  tryCatch({
    process_tile(tile_path, start_date, end_date, results_path)
  }, error = function(e) {
    message(paste0("Error processing tile ", idx, " (", tile_path, "): ", e$message))
  })
}

gc()