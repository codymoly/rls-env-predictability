library(terra)

# Generate file paths
file_paths <- list.files("/Users/ben/Desktop/cop_res/merged_stack",full.names = T)

# Use lapply to read the .rds files and store them in a list
layers <- lapply(file_paths, readRDS)

# Stack the layers
pred_maps <- terra::rast(layers)


# Assuming df is your dataframe with "longitude" and "latitude" columns
# df <- data.frame(longitude = c(...), latitude = c(...))

#Example
 df <- data.frame(longitude = c(-93.982973, -118.769320)
, latitude = c(-2.822728, 26.578800))


# Convert your dataframe to SpatVector
coordinates <- terra::vect(df, geom=c("longitude", "latitude"), crs=crs(pred_maps))

# Extract values
values <- terra::extract(pred_maps, coordinates)

# Convert to dataframe
df_values <- as.data.frame(values)
