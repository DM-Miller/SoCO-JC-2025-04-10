library(tidyverse)

# Load the processed survey dataset
dt <- open_recent_file(
  directory = file.path(
    files_dir,
    "Pre_JC_survey_processed"
  )
)

# Define the variable name for this question
question_var <- "mgmt_case_1"
question_title <- "58M ECOG0, no PMH, mets to regional nodes (2) and solitary adrenal met. Not interested in trial."

# Define the ordered levels exactly as they appear in the form
ordered_levels <- c(
  "Pembrolizumab",
  "Avelumab",
  "Retifanlimab",
  "Nivolumab",
  "Nivolumab Ipilimumab",
  "Nivolumab Relatlimab",
  "Nivolumab Relatlimab Ipilimumab",
  "Other",
  "Not Applicable Clinician",
  "I Am Not A Clinician"
)

# Clean and prepare the data
mgmt_case_1_summary <- dt |> 
  drop_na(all_of(question_var)) |> 
  mutate(!!question_var := factor(.data[[question_var]], levels = ordered_levels, ordered = TRUE)) |> 
  count(.data[[question_var]], name = "n", .drop = FALSE) |> 
  mutate(prop = round(n / sum(n) * 100))

mgmt_1_plot <- ggplot(
  mgmt_case_1_summary,
  aes(x = .data[[question_var]], y = n)
) +
  geom_col(fill = "steelblue4") +
  geom_text(aes(label = paste0(prop, "%")), hjust = -0.1) +
  ggtitle(str_wrap(question_title, width = 60)) +
  xlab("") +
  ylab(paste0("Number of Respondents (Total = ", sum(mgmt_case_1_summary$n), ")")) +
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
    axis.line = element_line(),
    plot.margin = margin(0.2, 0, 0.2, 0, "cm")
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
  coord_flip()

# Display the plot
mgmt_1_plot