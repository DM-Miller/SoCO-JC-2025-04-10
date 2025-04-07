# Load necessary libraries
library(tidyverse)
# Load Data

dt <- open_recent_file(
  directory = file.path(
    files_dir,
    "Pre_JC_survey_processed"
  )
)

# Define the correct order of response categories
ordered_levels <- c(
  "Medical Oncologist",
  "Medical Dermatologist",
  "Surgical Oncologist",
  "Surgical Dermatologist",
  "Radiation Oncologist",
  "Advanced Practice Provider",
  "Non Clinician Researcher",
  "Student Trainee"
)

# Ensure factor levels are set before counting
who_are_you <- dt |> 
  drop_na(who_are_you) |> 
  mutate(who_are_you = factor(who_are_you, levels = ordered_levels, ordered = TRUE)) |> 
  count(who_are_you, .drop = FALSE) |>  # .drop = FALSE keeps missing factor levels
  mutate(prop = round(n / sum(n) * 100))  

# Generate plot
role_plot <- ggplot(
  who_are_you,
  aes(x = who_are_you, y = n)
) + 
  geom_col(fill = "steelblue4") + 
  geom_text(
    aes(label = paste0(prop, "%")),
    hjust = -0.1
  ) +
  ggtitle(str_wrap(
    "Which of the following best describes you?",
    60)
  ) +
  xlab("") +
  ylab(paste0(
    "Number of Respondents (Total = ", sum(who_are_you$n), ")")) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", 
                              size = 22,
                              margin = margin(0, 150, 0, 0)),
    title = element_text(face = "bold", size = 18),
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
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  scale_y_continuous(
    breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))),
    limits = c(0, max(who_are_you$n + 0.2))
  ) +
  coord_flip()

# Print the plot
role_plot

