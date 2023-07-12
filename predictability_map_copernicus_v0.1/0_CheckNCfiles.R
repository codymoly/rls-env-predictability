library(lubridate)


# Function to check missing files
check_missing_files <- function(directory) {
  # Get list of .nc files
  files <- list.files(path = directory, pattern = "\\.nc$", full.names = FALSE)
  
  str(files)
  # Extract dates from filenames
  dates <- as.character(sapply(files, function(x) substr(x, 1, 8)))
  
  dates <- as.Date(dates, format = "%Y%m%d")
  
  # Remove NA dates

  # Find the range of dates
  date_range <- range(dates)
  
  # Create a sequence of dates from the minimum to the maximum date
  expected_dates <- seq(from = date_range[1], to = date_range[2], by = "day")
  
  # Find missing dates
  missing_dates <- expected_dates[!expected_dates %in% dates]
  
  # Print first and last date
  cat("First date: ", as.character(date_range[1]), "\n")
  cat("Last date: ", as.character(date_range[2]), "\n")
  
  # Print missing dates
  if (length(missing_dates) > 0) {
    cat("Missing dates:\n")
    print(missing_dates)
  } else {
    cat("No missing dates.\n")
  }
}

directory=nc_dir

# Call the function
check_missing_files(nc_dir)
nc_dir <- "~/Desktop/copernicus/"

