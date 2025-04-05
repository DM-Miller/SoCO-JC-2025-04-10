# Load necessary libraries
library(tidyverse)
library(here)

# Load Data
dt <- readRDS(file.path(files_dir, "survey_results_pre_test_processed.rds"))

dt$mcc_patients_per_month [is.na(dt$mcc_patients_per_month)] <- "Not Answered"

dt$mcc_patients_per_month[dt$mcc_patients_per_month == "I Am A Clinician But I Do Not Treat Mcc"] <- "I Am A Clinician But I Do Not Treat MCC"

# Define the correct order of response categories
ordered_levels <- rev(
  c(
  "Not Answered",
  "I Am Not A Clinician",
  "I Am A Clinician But I Do Not Treat MCC", 
  "1-2", 
  "3-5", 
  "6-10", 
  "11-20", 
  "Greater Than 20")
  )

# Ensure factor levels are set before counting
mcc_patients_per_month <- dt |> 
  drop_na(mcc_patients_per_month) |> 
  mutate(mcc_patients_per_month = factor(mcc_patients_per_month, levels = ordered_levels, ordered = TRUE)) |> 
  count(mcc_patients_per_month, .drop = FALSE) |>  # .drop = FALSE keeps missing factor levels
  mutate(prop = round(n / sum(n) * 100))  

# Debugging: Print to confirm all levels are there, even if n=0
#print(mcc_patients_per_month)

# Generate plot
mcc_patients_per_month_plot <- ggplot(
  mcc_patients_per_month,
  aes(x = mcc_patients_per_month, y = n)
) + 
  geom_col(fill = "steelblue4") + 
  geom_text(
    aes(label = paste0(prop, "%")),
    hjust = -0.1
  ) +
  ggtitle(str_wrap(
    "How many MCC patients do you see per month?",
    60)
  ) +
  xlab("") +
  ylab(paste0(
    "Number of Respondents (Total = ",
    sum(mcc_patients_per_month$n),
    ")")) +
  theme(
    plot.title = element_text(
      hjust = 0.5, face = "bold", size = 20,
      margin = margin(0, 130, 0, 0)),
    title = element_text(face = "bold", size = 18),
    axis.title.x = element_text(face = "bold", size = 16),
    axis.text.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 16),
    axis.text.y = element_text(face = "bold", size = 16),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(),
    plot.margin = margin(0.2, 0, 0.2, 0, "cm")
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  scale_y_continuous(
    breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))),
    limits = c(0, max(mcc_patients_per_month$n + 0.5))
  ) +
  coord_flip()

# Print the plot
mcc_patients_per_month_plot
