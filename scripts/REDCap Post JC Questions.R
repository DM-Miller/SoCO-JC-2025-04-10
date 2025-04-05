library(tidyverse)
library(REDCapR)
library(here)

# Load Data

redcapr_dt <- REDCapR::redcap_read_oneshot(
  redcap_uri = "https://redcap.partners.org/redcap/api/",
  token = "3910D018AF3D663F7D1F2BA5266A7484",
)

dt2 <- redcapr_dt$data

saveRDS(
  object = dt2,
  file = file.path(
    files_dir,
    "survey_results_post_test.rds")
  )


dt <- readRDS(
  file = file.path(
  files_dir,
  "survey_results_post_test.rds")
)


dt[,2] <- stringr::str_replace_all(
  string = dt[,2],
  pattern = regex("__"),
  replacement = "-"
)


dt[,2] <- stringr::str_replace_all(
  string = dt[,2],
  pattern = regex("_1"),
  replacement = ", "
)

dt[,2] <- stringr::str_replace_all(
  string = dt[,2],
  pattern = regex("_"),
  replacement = " "
)

dt[,2] <- stringr::str_to_title(
  string = dt[,2]
)
# Apply formatting corrections to columns 3-
for (col in 3:36) {
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

saveRDS(
  object = dt,
  file = file.path(
  files_dir,
  "survey_results_post_test_processed.rds")
)
