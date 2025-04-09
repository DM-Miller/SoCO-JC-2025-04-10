# Load packages
# Define the base path for the project
base_path <- here::here()  # For Journal Club, use project root

# For JoCO, it might be:
# base_path <- here::here("perspectives", "Vol_2_Issue_2", "nivo_rela_nivo_ipi_itc")

# Define directories relative to `base_path`
files_dir <- file.path(base_path, "files")
img_dir <- file.path(base_path, "img")
scripts_dir <- file.path(base_path, "scripts")

source(file.path(scripts_dir, "load_packages.R"))



# Load Data

redcapr_dt <- REDCapR::redcap_read_oneshot(
  redcap_uri = "https://redcap.partners.org/redcap/api/",
  token = "3910D018AF3D663F7D1F2BA5266A7484",
)

dt2 <- redcapr_dt$data

save_files(
  directory = "",
  save_object = dt2,
  filename = "survey_results_post_test.rds",
  subD = file.path(
    files_dir,
    "Post_JC_survey_unprocessed")
)


dt <- open_recent_file(
  directory = file.path(
    files_dir,
    "Post_JC_survey_unprocessed")
)

# Apply formatting corrections to columns 3-
for (col in 1:5) {
  dt[, col] <- stringr::str_replace_all(
    string = dt[, col],
    pattern = regex("__"),
    replacement = "-"
  )
  
  dt[, col] <- stringr::str_replace_all(
    string = dt[, col],
    pattern = regex("_1"),
    replacement = "-1"
  )
  
  dt[, col] <- stringr::str_replace_all(
    string = dt[, col],
    pattern = regex("_"),
    replacement = " "
  )
  
  dt[, col] <- stringr::str_to_title(
    string = dt[, col]
  )
}
# Save processed data

save_files(
  directory = "",
  save_object = dt,
  filename = "survey_results_post_test_processed",
  subD = file.path(
    files_dir,
    "Post_JC_survey_processed")
)

