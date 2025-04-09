# Load necessary libraries
library(tidyverse)

# Load Data
dt <- open_recent_file(
  directory = file.path(
    files_dir,
    "Pre_JC_survey_processed"
  )
)

# Recode NA if needed
dt$nccn_ipi_nivo[is.na(dt$nccn_ipi_nivo)] <- "Not Answered"

# QUESTION VARIABLE AND TITLE (modular for reuse)

question_var <- "nccn_ipi_nivo"
title <- "Are you aware that the NCCN guidelines include Ipi/Nivo in the category of 'Useful in Certain Circumstances'?"
dt[[question_var]][is.na(dt[[question_var]])] <- "Not Answered"

# Define order of responses
ordered_levels <- rev(c(
  "Not Answered",
  "I Am Not A Clinician",
  "Not Applicable Clinician",
  "No",
  "Yes"
))

# Prepare data
plot_data <- dt |>
  drop_na(!!sym(question_var)) |>
  mutate(!!question_var := factor(.data[[question_var]], levels = ordered_levels, ordered = TRUE)) |>
  count(!!sym(question_var), .drop = FALSE) |>
  mutate(prop = round(n / sum(n) * 100))

# Generate plot
nccn_ipi_nivo_plot <- ggplot(
  plot_data,
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
    "Number of Respondents (Total = ", sum(plot_data$n), ")"
  )) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20, margin = margin(0, 130, 0, 0)),
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
    breaks = seq(0, max(plot_data$n) + 1, by = 2),
    limits = c(0, max(plot_data$n + 1))
  ) +
  coord_flip()

# Show the plot
nccn_ipi_nivo_plot
