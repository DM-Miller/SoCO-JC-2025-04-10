library(tidyverse)

# Load data
dt <- open_recent_file(
  directory = file.path(files_dir, "Pre_JC_survey_processed")
)

# Set variable and question title
question_var <- "ipi_nivo_dosing_1l"
title_string <- "If you were to use Nivo plus Ipi in the first-line setting, what dosing would you use?"

# Recode levels to match exact display order and tidy labels
dt[[question_var]][is.na(dt[[question_var]])] <- "Not Answered"
dt[[question_var]] <- recode(
  dt[[question_var]],
  "Nivo 1mg Per Kg Ipi 3mg Per Kg Q3 Weeks" = "Nivo 1 mg/kg + Ipi 3 mg/kg q3 weeks",
  "Nivo 3mg Per Kg Ipi 1mg Per Kg Q3 Weeks" = "Nivo 3 mg/kg + Ipi 1 mg/kg q3 weeks",
  "Nivo 240mg Q2 Weeks Ipi 1mg Per Kg Q6 Weeks" = "Nivo 240 mg q2 weeks + Ipi 1 mg/kg q6 weeks",
  "Nivo 360mg Q3 Weeks Ipi 1mg Per Kg Q6 Weeks" = "Nivo 360 mg q3 weeks + Ipi 1 mg/kg q6 weeks",
  "I Would Not Use In The First Line" = "I would not use Nivo plus Ipi in the first-line setting",
  "Not Applicable Clinician" = "I am a clinician but I do not manage MCC patients",
  "I Am Not A Clinician" = "I am not a clinician"
)

# Define the order of answer choices
ordered_levels <- c(
  "Nivo 1 mg/kg + Ipi 3 mg/kg q3 weeks",
  "Nivo 3 mg/kg + Ipi 1 mg/kg q3 weeks",
  "Nivo 240 mg q2 weeks + Ipi 1 mg/kg q6 weeks",
  "Nivo 360 mg q3 weeks + Ipi 1 mg/kg q6 weeks",
  "I would not use Nivo plus Ipi in the first-line setting",
  "I am a clinician but I do not manage MCC patients",
  "I am not a clinician",
  "Not Answered"
)

# Tabulate counts and proportions
dosing_plot_data <- dt |> 
  mutate(!!question_var := factor(.data[[question_var]], levels = ordered_levels, ordered = TRUE)) |> 
  count(!!sym(question_var), .drop = FALSE) |> 
  mutate(prop = round(n / sum(n) * 100))

# Plot
dosing_plot <- ggplot(dosing_plot_data, aes(x = .data[[question_var]], y = n)) +
  geom_col(fill = "steelblue4") +
  geom_text(aes(label = paste0(prop, "%")), hjust = -0.1) +
  ggtitle(str_wrap(title_string, 65)) +
  xlab("") +
  ylab(paste0("Number of Respondents (Total = ", sum(dosing_plot_data$n), ")")) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20,
                              margin = margin(0, 140, 0, 0)),
    axis.title.x = element_text(face = "bold", size = 16),
    axis.text.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 16),
    axis.text.y = element_text(face = "bold", size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line()
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35)) +
  coord_flip()

dosing_plot

