# Define the base path for the project
if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here")
}
library(here)

# Adjust `base_path` to the appropriate root depending on the project (Journal Club or JoCO)
base_path <- here::here()  # For Journal Club, use project root

# For JoCO, it might be:
# base_path <- here::here("perspectives", "Vol_2_Issue_2", "nivo_rela_nivo_ipi_itc")

# Define directories relative to `base_path`
files_dir <- file.path(base_path, "files")
img_dir <- file.path(base_path, "img")
scripts_dir <- file.path(base_path, "scripts")

# Print paths to confirm
message("Files directory: ", files_dir)
message("Image directory: ", img_dir)
message("Scripts directory: ", scripts_dir)

if (interactive()) {
  source(file.path(scripts_dir, "load_packages.R"))
}
