# Load necessary libraries
library(tidyverse)
# Load Data

# Load Data
dt <- open_recent_file(
  directory = file.path(files_dir, "Pre_JC_survey_processed")
)

# Define variable and title
question_var <- "who_are_you"
question_title <- "Which of the following best describes you?"

# Define the correct order of response categories
ordered_levels <- c(
  "Medical Oncologist",
  "Medical Dermatologist",
  "Surgical Oncologist",
  "Surgical Dermatologist",
  "Radiation Oncologist",
  "Advanced Practice Provider",
  "Non Clinician Researcher",
  "Student Trainee",
  "Other",
  "Not Answered"
)
# Clean and prepare data
who_are_you_summary <- dt |>
  drop_na(all_of(question_var)) |> 
  mutate({{ question_var }} := factor(.data[[question_var]], levels = ordered_levels, ordered = TRUE)) |> 
  count(.data[[question_var]], name = "n", .drop = FALSE) |> 
  mutate(prop = round(n / sum(n) * 100))

# Generate plot
role_plot <- ggplot(who_are_you_summary, aes(x = .data[[question_var]], y = n)) +
  geom_col(fill = "steelblue4") +
  geom_text(aes(label = paste0(prop, "%")), hjust = -0.1) +
  ggtitle(str_wrap(question_title, width = 60)) +
  xlab("") +
  ylab(paste0("Number of Respondents (Total = ", sum(who_are_you_summary$n), ")")) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22,
                              margin = margin(0, 150, 0, 0)),
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
    limits = c(0, max(who_are_you_summary$n + 0.2))
  ) +
  coord_flip()

# Display the plot
role_plot