# Load necessary libraries
library(readxl)
library(dplyr)
library(purrr)
library(fs)

# Define the directory path
dir_path <- "../four_factor_combined/primiss"

# List all .xlsx files in the directory
xlsx_files <- dir_ls(dir_path, glob = "*.xlsx")

# Read and concatenate all xlsx files into one data frame
combined_data <- xlsx_files %>%
  map_dfr(read_excel)

# Save the combined data to an .rds file
saveRDS(combined_data, "data/primiss.rds")
