library(tidyverse)
library(here)

# Load Data
dt <- open_recent_file(
  directory = file.path(files_dir, "Pre_JC_survey_processed")
)

# Define the variable and plot title
question_var <- "used_ipi_nivo"
question_title <- "Have you recommended Ipi/Nivo for MCC before?"

# Optional recoding
dt[[question_var]] <- recode(
  dt[[question_var]],
  "I Am Not A Clinician" = "I am not a clinician",
  "Not Applicable Clinician" = "I am a clinician but do not manage MCC patients"
)

# Define the order of response levels
ordered_levels <- rev(c(
  "Not Answered", 
  "I am not a clinician",
  "I am a clinician but do not manage MCC patients",
  "No",
  "Yes"
))

# Clean and prepare the data
plot_data <- dt |>
  mutate({{ question_var }} := replace_na(.data[[question_var]], "Not Answered")) |>
  mutate({{ question_var }} := factor(.data[[question_var]], levels = ordered_levels, ordered = TRUE)) |>
  count(.data[[question_var]], name = "n", .drop = FALSE) |>
  mutate(prop = round(n / sum(n) * 100, 1))

# Generate the plot
used_ipi_nivo_plot <- ggplot(plot_data, aes(x = .data[[question_var]], y = n)) +
  geom_col(fill = "steelblue4") +
  geom_text(aes(label = paste0(prop, "%")), hjust = -0.1) +
  ggtitle(str_wrap(question_title, 60)) +
  xlab("") +
  ylab(paste0("Number of Respondents (Total = ", sum(plot_data$n), ")")) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
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
    breaks = seq(0, max(plot_data$n + 1), by = 2),
    limits = c(0, max(plot_data$n + 1))
  ) +
  coord_flip()

# Display the plot
used_ipi_nivo_plot
