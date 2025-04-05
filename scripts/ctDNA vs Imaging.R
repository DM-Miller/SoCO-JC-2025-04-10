# Load necessary libraries
library(tidyverse)
library(here)

# Load Data
dt2 <- readRDS(file.path(files_dir, "survey_results_post_test_processed.rds"))

# Replace NA values with "Not Answered"
dt2$ctdna_vs_imaging[is.na(dt2$ctdna_vs_imaging)] <- "Not Answered"

# Standardize response categories
dt2 <- dt2 |> 
  select(ctdna_vs_imaging) |> 
  mutate(ctdna_vs_imaging = case_when(
    str_detect(ctdna_vs_imaging, "Very Likely") ~ "Very likely – favor ctDNA over imaging",
    str_detect(ctdna_vs_imaging, "Somewhat Likely") ~ "Somewhat likely – would use selectively with imaging",
    str_detect(ctdna_vs_imaging, "Uncertain") ~ "Uncertain – more evidence needed",
    str_detect(ctdna_vs_imaging, "Unlikely") ~ "Unlikely – I will continue to rely primarily on imaging",
    str_detect(ctdna_vs_imaging, "Not A Clinician") ~ "I am not a clinician",
    str_detect(ctdna_vs_imaging, "No Mcc Patients") ~ "I am a clinician but I don't see MCC",
    str_detect(ctdna_vs_imaging, "Not Answered") ~ "Not Answered",  # Ensure "Not Answered" remains
    TRUE ~ ctdna_vs_imaging  # Keep as is if no match is found
  ))

dt2 <- dt2 |> 
  select(ctdna_vs_imaging)

dt3 <- dt2 |> 
  filter(!ctdna_vs_imaging %in% c("Not Answered", "I am not a clinician", "I am a clinician but I don't see MCC"))

# Define the correct order of response categories
ordered_levels <- c(
  "Very likely – favor ctDNA over imaging",
  "Somewhat likely – would use selectively with imaging",
  "Uncertain – more evidence needed",
  "Unlikely – I will continue to rely primarily on imaging"
)

# Ensure factor levels are set before counting
ctdna_vs_imaging_counts <- dt3 |> 
  mutate(ctdna_vs_imaging = factor(ctdna_vs_imaging, levels = ordered_levels, ordered = TRUE)) |> 
  count(ctdna_vs_imaging, .drop = FALSE) |>  # .drop = FALSE keeps missing factor levels
  mutate(prop = round(n / sum(n) * 100))  

# Generate plot
ctdna_vs_imaging_plot <- ggplot(
  ctdna_vs_imaging_counts,
  aes(x = ctdna_vs_imaging, y = n)
) + 
  geom_col(fill = "steelblue4") + 
  geom_text(
    aes(label = paste0(prop, "%")),
    hjust = -0.1
  ) +
  ggtitle(str_wrap(
    "Based on today’s discussion, how likely are you to use ctDNA instead of imaging for surveillance in patients rendered NED after initial treatment?",
    50)
  ) +
  xlab("") +
  ylab(paste0(
    "Number of Respondents (Total = ", sum(ctdna_vs_imaging_counts$n), ")")) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18, margin = margin(0,160,0,0)),
    axis.title.x = element_text(face = "bold", size = 16),
    axis.text.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 16),
    axis.text.y = element_text(face = "bold", size = 14),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(),
    plot.margin = margin(0.2, 0, 0.2, 0, "cm")
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
  scale_y_continuous(
    breaks = seq(0, max(ctdna_vs_imaging_counts$n) + .05),
    limits = c(0, max(ctdna_vs_imaging_counts$n + 1))
  ) +
  coord_flip()

# Print the plot
ctdna_vs_imaging_plot
