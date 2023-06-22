#this script needs a folder with .nc files from copernicus for the whole world
#it breaks up the .nc files in to 15°lat time 15°lon tiles
#The tiles are then saved as terra raster in an .rds file
#for each area a folder is created, e.g. "lon_-15_lat_-15"
#in each of those folders the tiles are saved with date in the filename sst_20120101_lon_-15_lat_-15.rds

# Packages
library(terra)
library(foreach)
library(doParallel)

# Directories
nc_dir <- "~/Desktop/copernicus/"
goal_dir <- "~/Desktop/cop_break/"

# Get the .nc file paths
nc_files <- list.files(nc_dir, pattern = "\\.nc$", full.names = TRUE)

# Set up the parallel backend
cl <- makeCluster(detectCores())  # Adjust the argument if you want to limit the number of cores used
registerDoParallel(cl)

# Iterate over the .nc files in parallel
results <- foreach(file_path = nc_files, .packages = "terra") %dopar% {
  # Extract the date from the file name
  file_name <- basename(file_path)
  date_str <- substr(file_name, 1, 8)
    
  # Load .nc file
  sst_nc <- terra::rast(file_path)
  
  # Select only the 'analysed_sst' layer
  analysed_sst <- sst_nc[["analysed_sst"]]
  
  # We know from your example that your map's extent is global (xmin: -180, xmax: 180, ymin: -90, ymax: 90)
  # We need to iterate through the extents with a 15° step
  for (xmin in seq(-180, 165, by = 15)) {
    for (ymin in seq(-90, 75, by = 15)) {
      # Define xmax and ymax based on xmin and ymin
      xmax <- xmin + 15
      ymax <- ymin + 15
      
      # Create an extent object
      e <- terra::ext(xmin, xmax, ymin, ymax)
      
      # Crop the raster based on the extent
      cropped_sst <- terra::crop(analysed_sst, e)
      
      # Create a directory for this extent if it doesn't already exist
      dir_path <- paste0(goal_dir, "lon_", xmin, "_lat_", ymin, "/")
      dir.create(dir_path, showWarnings = FALSE)
      
      # Save the cropped raster into an .rds file
      output_file_path <- paste0(dir_path, "sst_", date_str, "_lon_", xmin, "_lat_", ymin, ".rds")
      saveRDS(cropped_sst, output_file_path)
      
    }
  }
  
  # Return the name of the processed file for progress update
  return(paste(Sys.time(),file_name))
}

# Stop the cluster
stopCluster(cl)
Sys.time()
# Print the names of the processed files
print(results)

