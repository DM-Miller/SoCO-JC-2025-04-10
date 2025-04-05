# Load necessary libraries
library(tidyverse)
library(here)

# Load Data
dt2 <- readRDS(file.path(files_dir, "survey_results_post_test_processed.rds"))

# Replace NA values with "Not Answered"
dt2$comfort_with_ici[is.na(dt2$comfort_with_ici)] <- "Not Answered"

# Standardize response categories
dt2 <- dt2 |> 
  select(comfort_with_ici) |> 
  mutate(comfort_with_ici = case_when(
    str_detect(comfort_with_ici, "Very Comfortable") ~ "Very comfortable – strongly consider recommending ICI",
    str_detect(comfort_with_ici, "Somewhat Comfortable") ~ "Somewhat comfortable – for select high-risk cases",
    str_detect(comfort_with_ici, "Uncertain") ~ "Uncertain – need more evidence",
    str_detect(comfort_with_ici, "Uncomfortable") ~ "Uncomfortable – would not recommend based on ctDNA alone",
    str_detect(comfort_with_ici, "Not Applicable") ~ "Not applicable – I don't manage systemic therapy",
    str_detect(comfort_with_ici, "Not A Clinician") ~ "I am not a clinician",
    str_detect(comfort_with_ici, "No Mcc Patients") ~ "I am a clinician but I don't see MCC",
    str_detect(comfort_with_ici, "Not Answered") ~ "Not Answered",  # Ensure "Not Answered" remains
    TRUE ~ comfort_with_ici  # Keep as is if no match is found
  ))

dt2 <- dt2 |> 
  select(comfort_with_ici)

dt3 <- dt2 |> 
  filter(!comfort_with_ici %in% c(
    "Not Answered", 
    "I am a clinician but I don't see MCC", 
    "I am not a clinician",
    "Not applicable – I don't manage systemic therapy"))

# Define the correct order of response categories
ordered_levels <- c(
  "Very comfortable – strongly consider recommending ICI",
  "Somewhat comfortable – for select high-risk cases",
  "Uncertain – need more evidence",
  "Uncomfortable – would not recommend based on ctDNA alone"
)

# Ensure factor levels are set before counting
comfort_with_ici_counts <- dt3 |> 
  mutate(comfort_with_ici = factor(comfort_with_ici, levels = ordered_levels, ordered = TRUE)) |> 
  count(comfort_with_ici, .drop = FALSE) |>  # .drop = FALSE keeps missing factor levels
  mutate(prop = round(n / sum(n) * 100))  

# Generate plot
comfort_with_ici_plot <- ggplot(
  comfort_with_ici_counts,
  aes(x = comfort_with_ici, y = n)
) + 
  geom_col(fill = "steelblue4") + 
  geom_text(
    aes(label = paste0(prop, "%")),
    hjust = -0.1
  ) +
  ggtitle(str_wrap(
    "Comfort level recommending ICI for ctDNA-positive, clinically NED patients?",
    50)
  ) +
  xlab("") +
  ylab(paste0(
    "Number of Respondents (Total = ", sum(comfort_with_ici_counts$n), ")")) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18, margin = margin(0, 190, 0, 0)),
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
  scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) +
  scale_y_continuous(
    breaks = seq(0, max(comfort_with_ici_counts$n) + 0.5, by = 1),
    limits = c(0, max(comfort_with_ici_counts$n + 1))
  ) +
  coord_flip()

# Print the plot
comfort_with_ici_plot
