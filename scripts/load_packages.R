library(tidyverse)
library(plotly)
library(gt)
library(glue)
library(here)
# Define the base path for the project
base_path <- here::here()  # For Journal Club, use project root

# For JoCO, it might be:
# base_path <- here::here("perspectives", "Vol_2_Issue_2", "nivo_rela_nivo_ipi_itc")

# Define directories relative to `base_path`
files_dir <- file.path(base_path, "files")
img_dir <- file.path(base_path, "img")
scripts_dir <- file.path(base_path, "scripts")

source(file.path(scripts_dir, "save files.R"))
source(file.path(scripts_dir, "open most recent file.R"))
source(file.path(scripts_dir, "render_commit_push.R"))


