# Load necessary libraries
library(terra) 
library(data.table)

#This script requires the computed predictibility datatables
#And scaffold maps, so we do not need to resample. This cript just replaces the values in the rasters with the predictibility values
#For the scaffolds i just used an SST map for each tile. (cop_break)
#It creates a raster stack for each tile and saves it as .rds

#then in the second part the raster stacks are merged to a 

#######Part 1 - Write the data onto maps and save them as stacked raster##########

# Define directories
cop_res_dir <- "~/Desktop/cop_res/" #Predictibility dir
map_scaffolds_dir <- "~/Desktop/map_scaffolds/"

cop_stack_dir <- "~/Desktop/cop_stack/" #output dir


# Get the list of .rds files in both directories
cop_res_files <- list.files(cop_res_dir, pattern = "\\.rds$")
map_scaffolds_files <- list.files(map_scaffolds_dir, pattern = "\\.rds$")

# Function to extract longitude and latitude from the filename
extract_lon_lat <- function(filename) {
  gsub(".*lon_([-0-9]*_lat_[-0-9]*).*", "\\1", filename)
}

# Extract lon_lat from cop_res_files and map_scaffolds_files
lon_lat_cop_res <- sapply(cop_res_files, extract_lon_lat)
lon_lat_map_scaffolds <- sapply(map_scaffolds_files, extract_lon_lat)

# Match lon_lat from 'cop_res' directory to those from 'map_scaffolds' directory
matched_files <- match(lon_lat_cop_res, lon_lat_map_scaffolds)

# Process matched files
for (i in seq_along(matched_files)) {
  
  # Check if there is a match for the current file
  if (!is.na(matched_files[i])) {
    
    # Define the file name for the raster stack
    file_name <- paste0(cop_stack_dir, "stack_", cop_res_files[i])
    
    # Check if the file already exists
    if (file.exists(file_name)) {
      # Skip this iteration of the loop and print a message
      message(paste(i, "file:", cop_res_files[i], "already exists. Skipping."))
      next
    }
    
    # Load data.table and raster
    predictability_df <- readRDS(paste0(cop_res_dir, cop_res_files[i]))
    scaffold_raster <- readRDS(paste0(map_scaffolds_dir, map_scaffolds_files[matched_files[i]]))
    
    message(paste(i,"Processing file:",cop_res_files[i]))
    
    # Create a list to store the rasters
    raster_list <- list()
    
    # Loop through each column (excluding x, y) of the predictability_df
    for (j in 3:ncol(predictability_df)) {
      
      # Extract the values
      stat_values <- predictability_df[[j]]
      
      # Create a new raster and assign the values
      stat_raster <- terra::rast(scaffold_raster)
      terra::values(stat_raster) <- stat_values
      
      # Add the raster to the list
      raster_list[[j-2]] <- stat_raster
      
      # Name the layers of the raster stack
      names(raster_list[[j-2]]) <- names(predictability_df)[j]
    }
    
    # Create a stack of rasters
    raster_stack <- terra::rast(raster_list)
    
    # Save the raster stack to a .rds file
    saveRDS(raster_stack, file_name)
  }
}

gc()



############ Part 2 - Read in the stacks and merge the maps #########

# List of raster files
rasters <- list.files(path = "~/Desktop/cop_stack", full.names = TRUE)

# Extract unique longitudes from the file names
longitudes <- unique(gsub(".*lon_(-?[0-9]+)_lat.*", "\\1", rasters))

# Initialize an empty list to store merged rasters for each longitude
merged_rasters <- vector("list", length(longitudes))
names(merged_rasters) <- longitudes

# Loop over each longitude
for (lon in longitudes) {
  message("merging longitude ",lon)
  # Subset the rasters for this longitude
  lon_rasters <- grep(paste0("lon_", lon), rasters, value = TRUE)
  
  # Load and merge rasters
  merged_rasters[[lon]] <- do.call(terra::merge, lapply(lon_rasters, terra::rast))
}

longitudes

# Convert the longitudes to numeric
numeric_lons <- as.numeric(longitudes)

# Define the global range of longitudes with a step size of 15
global_lons <- seq(-180, 180-15, by = 15)



# Identify the missing longitudes by set-difference
missing_lons <- setdiff(global_lons, numeric_lons)

# Add the missing longitudes to your numeric_lons vector
numeric_lons <- sort(c(numeric_lons, missing_lons))


# Convert back to character
all_lons <- as.character(numeric_lons)

# Initialize an empty list to store the longitude raster strips
raster_strips <- vector("list", length(all_lons))
names(raster_strips) <- all_lons



# Determine the dimensions and resolution of your existing rasters.
# Assuming all your rasters have the same dimensions and resolution,
# you can just read the first one to get this information
reference_raster <- merged_rasters[[1]]
ref_res <- terra::res(reference_raster)
ref_nrows <- terra::nrow(reference_raster)
ref_ncols <- terra::ncol(reference_raster)


# Load each longitude strip (or create a blank one if missing)
for (lon in all_lons) {
  if (lon %in% longitudes) {
    # This longitude strip exists, so use it from the list
    raster_strips[[lon]] <- merged_rasters[[which(longitudes == lon)]]
  } else {
    # This longitude strip is missing, so create a blank one
    empty_raster <- terra::rast(nrows=ref_nrows, ncols=ref_ncols, res=ref_res)
    values(empty_raster) <- as.numeric(NA)
    raster_strips[[lon]] <- empty_raster
  }
}

# Bind the raster strips together
# Initialize the merged raster with the first strip
worldmap <- raster_strips[[1]]
# Iterate over the rest of the raster_strips
for (i in 2:length(raster_strips)) {


  message(i, " of ", length(raster_strips))
  # Merge the current strip with the existing merged raster
  #worldmap <- terra::merge(worldmap, raster_strips[[i]]) 
  worldmap <- terra::merge(raster_strips[[i]],worldmap) 
  
}

worldmap

color_palette <- viridis::magma(200)
plot(worldmap, col = color_palette)


#Save each layer as .rds and .tif
# Define your directory
your_directory <- "~/Desktop/pred_maps"

# Get the names of the layers
layer_names <- names(worldmap)

# Loop over each layer
for (i in 1:length(layer_names)) {
  # Extract the layer
  layer <- worldmap[[i]]
  
  # Define the file names with full path
  rds_file_name <- file.path(your_directory, sprintf("%02d_%s.rds", i, layer_names[i]))
  tiff_file_name <- file.path(your_directory, sprintf("%02d_%s.tif", i, layer_names[i]))
  
  message(rds_file_name)
  # Save as .rds
  saveRDS(layer, file = rds_file_name)
  
  # Save as .tif
  terra::writeRaster(layer, tiff_file_name)
}




rm(ls())
gc()
