library(tidyverse)

# Load data
dt <- open_recent_file(
  directory = file.path(files_dir, "Pre_JC_survey_processed")
)

# Set variable and question title
question_var <- "ipi_nivo_label_justified"
title_string <- "Based on currently available data, in which setting(s) do you believe Ipi/Nivo is justified for product labeling in MCC? (Select the best answer)"

# Recode values
dt[[question_var]][is.na(dt[[question_var]])] <- "Not Answered"
dt[[question_var]] <- recode(
  dt[[question_var]],
  "Pd1 Refractory Setting" = "Anti-PD-1-Refractory Setting",
  "Both 1l And 2l" = "Both First-Line and Second-Line Setting",
  "Not Applicable Clinician" = "I Am A Clinician But I Do Not Manage MCC Patients"
)

# Define the order of answer choices
ordered_levels <- c(
  "First-Line Setting",
  "Anti-PD-1-Refractory Setting",
  "Both First-Line and Second-Line Setting",
  "Data Does Not Yet Support Labeling",
  "I Am A Clinician But I Do Not Manage MCC Patients",
  "I Am Not A Clinician",
  "Not Answered"
)

# Tabulate counts and proportions
label_justified <- dt |> 
  mutate(!!question_var := factor(.data[[question_var]], levels = ordered_levels, ordered = TRUE)) |> 
  count(!!sym(question_var), .drop = FALSE) |> 
  mutate(prop = round(n / sum(n) * 100))

# Plot
label_justified_plot <- ggplot(label_justified, aes(x = .data[[question_var]], y = n)) +
  geom_col(fill = "steelblue4") +
  geom_text(aes(label = paste0(prop, "%")), hjust = -0.1) +
  ggtitle(str_wrap(title_string, 65)) +
  xlab("") +
  ylab(paste0("Number of Respondents (Total = ", sum(label_justified$n), ")")) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20,
                              margin = margin(0, 130, 0, 0)),
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
  coord_flip()

label_justified_plot
