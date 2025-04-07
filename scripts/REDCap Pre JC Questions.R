# Load packages
source(here::here("scripts/load_packages.R"))
# Load Data

redcapr_dt <- REDCapR::redcap_read_oneshot(
  redcap_uri = "https://redcap.partners.org/redcap/api/",
  token = "95FCBA5C9E231A6CDE8C7886836042E2",
)

dt1 <- redcapr_dt$data

save_files(
  directory = "", # this is b/c files_dir has the relevent information
  save_object = dt1,
  filename = "survey_results_pre_test.rds",
  subD = file.path(
    files_dir,
    "Pre_JC_survey_unprocessed")
  )


dt <- open_recent_file(
  directory = file.path(
    files_dir,
    "Pre_JC_survey_unprocessed"
  )
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

dt[,3] <- stringr::str_replace_all(
  string = dt[,3],
  pattern = regex("__"),
  replacement = "-"
)

dt[,3] <- stringr::str_replace_all(
  string = dt[,3],
  pattern = regex("_1"),
  replacement = ", "
)

dt[,3] <- stringr::str_replace_all(
  string = dt[,3],
  pattern = regex("_"),
  replacement = " "
)

dt[,3] <- stringr::str_to_title(
  string = dt[,3]
)

dt[,4] <- stringr::str_replace_all(
  string = dt[,4],
  pattern = regex("__"),
  replacement = "-"
)

dt[,4] <- stringr::str_replace_all(
  string = dt[,4],
  pattern = regex("_1"),
  replacement = "-1"
)

dt[,4] <- stringr::str_replace_all(
  string = dt[,4],
  pattern = regex("_"),
  replacement = " "
)

dt[,4] <- stringr::str_to_title(
  string = dt[,4]
)

dt[,5] <- stringr::str_replace_all(
  string = dt[,5],
  pattern = regex("__"),
  replacement = "-"
)

dt[,5] <- stringr::str_replace_all(
  string = dt[,5],
  pattern = regex("_1"),
  replacement = ", "
)

dt[,5] <- stringr::str_replace_all(
  string = dt[,5],
  pattern = regex("_"),
  replacement = " "
)

dt[,5] <- stringr::str_to_title(
  string = dt[,5]
)

dt[,6] <- stringr::str_replace_all(
  string = dt[,6],
  pattern = regex("__"),
  replacement = "-"
)

dt[,6] <- stringr::str_replace_all(
  string = dt[,6],
  pattern = regex("_1"),
  replacement = ", "
)

dt[,6] <- stringr::str_replace_all(
  string = dt[,6],
  pattern = regex("_"),
  replacement = " "
)

dt[,6] <- stringr::str_to_title(
  string = dt[,6]
)
dt[,7] <- stringr::str_replace_all(
  string = dt[,7],
  pattern = regex("__"),
  replacement = "-"
)

dt[,7] <- stringr::str_replace_all(
  string = dt[,7],
  pattern = regex("_1"),
  replacement = "-1"
)

dt[,7] <- stringr::str_replace_all(
  string = dt[,7],
  pattern = regex("_"),
  replacement = " "
)

dt[,7] <- stringr::str_to_title(
  string = dt[,7]
)
dt[,8] <- stringr::str_replace_all(
  string = dt[,8],
  pattern = regex("__"),
  replacement = "-"
)

dt[,8] <- stringr::str_replace_all(
  string = dt[,8],
  pattern = regex("_1"),
  replacement = "-1"
)

dt[,8] <- stringr::str_replace_all(
  string = dt[,8],
  pattern = regex("_"),
  replacement = " "
)

dt[,8] <- stringr::str_to_title(
  string = dt[,8]
)

dt[,9] <- stringr::str_replace_all(
  string = dt[,9],
  pattern = regex("_"),
  replacement = " "
)

#dt[,9] <- stringr::str_replace_all(
#  string = dt[,9],
#  pattern = regex("_1"),
#  replacement = "-1"
#)

#dt[,9] <- stringr::str_replace_all(
#  string = dt[,9],
#  pattern = regex("_"),
#  replacement = " "
#)

dt[,9] <- stringr::str_to_title(
  string = dt[,9]
)

dt[,10] <- stringr::str_replace_all(
  string = dt[,10],
  pattern = regex("__"),
  replacement = "-"
)

#dt[,10] <- stringr::str_replace_all(
#  string = dt[,10],
#  pattern = regex("_1"),
#  replacement = "-1"
#)

dt[,10] <- stringr::str_replace_all(
  string = dt[,10],
  pattern = regex("_"),
  replacement = " "
)

dt[,10] <- stringr::str_to_title(
  string = dt[,10]
)
dt[,11] <- stringr::str_replace_all(
  string = dt[,11],
  pattern = regex("__"),
  replacement = "-"
)

#dt[,11] <- stringr::str_replace_all(
#  string = dt[,11],
#  pattern = regex("_1"),
#  replacement = "-1"
#)

dt[,11] <- stringr::str_replace_all(
  string = dt[,11],
  pattern = regex("_"),
  replacement = " "
)

dt[,11] <- stringr::str_to_title(
  string = dt[,11]
)

dt[,12] <- stringr::str_replace_all(
  string = dt[,12],
  pattern = regex("__"),
  replacement = "-"
)

#dt[,12] <- stringr::str_replace_all(
#  string = dt[,12],
#  pattern = regex("_1"),
#  replacement = "-1"
#)

dt[,12] <- stringr::str_replace_all(
  string = dt[,12],
  pattern = regex("_"),
  replacement = " "
)

dt[,12] <- stringr::str_to_title(
  string = dt[,12]
)

# Apply formatting corrections to columns 12 through 22
for (col in 12:22) {
  dt[, col] <- stringr::str_replace_all(
    string = dt[, col],
    pattern = regex("__"),
    replacement = "-"
  )
  
#  dt[, col] <- stringr::str_replace_all(
#    string = dt[, col],
#    pattern = regex("_1"),
#    replacement = "-1"
#  )
  
  dt[, col] <- stringr::str_replace_all(
    string = dt[, col],
    pattern = regex("_"),
    replacement = " "
  )
  
  dt[, col] <- stringr::str_to_title(
    string = dt[, col]
  )
}

save_files(
  directory = "", # this is b/c files_dir has the relevent information
  save_object = dt,
  filename = "survey_results_pre_test_processed",
  subD = file.path(
    files_dir,
    "Pre_JC_survey_processed")
)
