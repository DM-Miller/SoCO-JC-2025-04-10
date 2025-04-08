# Load necessary libraries
library(tidyverse)

# Load Data
dt <- open_recent_file(
  directory = file.path(
    files_dir,
    "Pre_JC_survey_processed"
  )
)

# Define question variable and title
question_var <- "coverage_ipi_nivo"
title <- "Have you (or your MDC team) recommended Ipi/Nivo for a patient and had it ultimately denied by a payor (e.g. even after a peer-to-peer)?"

# Clean up missing or long-form answers if needed
dt[[question_var]][is.na(dt[[question_var]])] <- "Not Answered"
dt[[question_var]] <- recode(
  dt[[question_var]],
  "Not Applicable Clinician" = "I Am a Clinician But Have Not Recommended Nivo Plus Ipi",
  "Not Sure" = "I am not sure"
  )

# Define the correct order of response categories
ordered_levels <- c(
  "Yes",
  "No",
  "I am not sure",
  "I Am a Clinician But Have Not Recommended Nivo Plus Ipi",
  "I Am Not A Clinician",
  "Not Answered"
)

# Ensure factor levels are set before counting
coverage_plot_data <- dt |> 
  drop_na(!!sym(question_var)) |> 
  mutate(!!sym(question_var) := factor(.data[[question_var]], levels = ordered_levels, ordered = TRUE)) |> 
  count(!!sym(question_var), .drop = FALSE) |> 
  mutate(prop = round(n / sum(n) * 100))

# Create the plot
coverage_ipi_nivo_plot <- ggplot(
  coverage_plot_data,
  aes(x = .data[[question_var]], y = n)
) + 
  geom_col(fill = "steelblue4") + 
  geom_text(
    aes(label = paste0(prop, "%")),
    hjust = -0.1
  ) +
  ggtitle(str_wrap(title, 60)) +
  xlab("") +
  ylab(paste0(
    "Number of Respondents (Total = ",
    sum(coverage_plot_data$n),
    ")")) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20,
                              margin = margin(0, 130, 10, 0)),
    axis.title.x = element_text(face = "bold", size = 16),
    axis.text.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 16),
    axis.text.y = element_text(face = "bold", size = 14),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line()
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
  scale_y_continuous(
    breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))),
    limits = c(0, max(coverage_plot_data$n + 0.5))
  ) +
  coord_flip()

# Show the plot
coverage_ipi_nivo_plot
