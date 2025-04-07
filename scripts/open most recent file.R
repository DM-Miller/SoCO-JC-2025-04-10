# SUMMARY:
# This function `open_recent_file()` searches for the most recently modified file 
# in a specified directory with one of the allowed extensions (.rds, .xlsx, .csv).
# It excludes temporary files, identifies the most recent file, reads it based on 
# its format, and returns the data after removing empty columns.

# Load necessary libraries
library(tidyverse)  # Collection of packages for data manipulation and visualization
library(readxl)      # Enables reading Excel files

# Function to open the most recently modified file in a given directory
open_recent_file <- function(
    directory,  # Directory path where the files are located
    ext = c(".rds", ".xlsx", ".csv")  # Allowed file extensions
){
  
  # List all files in the specified directory with the given extensions
  # The pattern argument constructs a regex matching any of the provided extensions
  files <- list.files(path = directory, pattern = paste(ext, collapse = "|"), full.names = TRUE)
  
  # Exclude temporary files (e.g., Excel temp files starting with "~$")
  files <- files[!grepl("~\\$", basename(files))]  
  
  # Check if there are any matching files
  if(length(files) == 0) {
    cat("No files found with the specified extensions in the directory.") # Notify user if no files found
    return(NULL) # Exit function
  }
  
  # Get file information (including modification time)
  files_info <- file.info(files)
  
  # Identify the most recently modified file using the modification time
  most_recently_modified <- files[which.max(files_info$mtime)]
  
  # Extract the file extension from the most recent file
  file_ext <- tools::file_ext(most_recently_modified)
  
  # Initialize variable to store the loaded data
  most_recent_data <- NULL
  
  # Read the file based on its extension
  if (file_ext == "rds") {
    most_recent_data <- readRDS(most_recently_modified) # Load an RDS file
  } else if (file_ext == "xlsx") {
    most_recent_data <- readxl::read_xlsx(most_recently_modified) # Load an Excel file
  } else if (file_ext == "csv") {
    most_recent_data <- read_csv(most_recently_modified) # Load a CSV file
  } else {
    cat("The file extension is not supported.") # Notify user if the extension is unsupported
    return(NULL) # Exit function
  }
  
  # Remove columns where all values are NA (i.e., completely empty columns)
  most_recent_data <- most_recent_data[, !apply(is.na(most_recent_data), 2, all)]
  
  # Return the loaded data
  return(most_recent_data)
}
