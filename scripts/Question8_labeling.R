# Load necessary libraries
library(tidyverse)
library(here)

# Load Data
dt <- open_recent_file(
  directory = file.path(
    files_dir,
    "Pre_JC_survey_processed"
  )
)

# Set variable
question_var <- "ipi_nivo_label"
title <- "Would it be helpful if MCC was added to the product label of Ipilimumab?"

# Recode NA if needed
dt[[question_var]][is.na(dt[[question_var]])] <- "Not Answered"
dt[[question_var]][dt[[question_var]] == "Not Applicable Clinician"] <- 
  "I Am A Clinician But I Do Not Manage MCC Patients"

# Set desired order of levels
ordered_levels <- rev(c(
  "Not Answered",
  "I Am Not A Clinician",
  "I Am A Clinician But I Do Not Manage MCC Patients",
  "No",
  "Yes"
  )
)

# Factor and summarize
plot_data <- dt |>
  mutate(!!question_var := factor(.data[[question_var]], levels = ordered_levels, ordered = TRUE)) |>
  count(!!sym(question_var), .drop = FALSE) |>
  rename(x = !!sym(question_var)) |>
  mutate(prop = round(n / sum(n) * 100))

# Generate plot
ipi_nivo_label_plot <- ggplot(plot_data, aes(x = x, y = n)) +
  geom_col(fill = "steelblue4") +
  geom_text(aes(label = paste0(prop, "%")), hjust = -0.1) +
  ggtitle(str_wrap(title, 65)) +
  xlab("") +
  ylab(paste0("Number of Respondents (Total = ", sum(plot_data$n), ")")) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20,
                              margin = margin(0, 130, 0, 0)),
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
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
  scale_y_continuous(
    breaks = seq(0, max(plot_data$n) + 1, by = 2),
    limits = c(0, max(plot_data$n + 1))
  ) +
  coord_flip()

# Display plot
ipi_nivo_label_plot
