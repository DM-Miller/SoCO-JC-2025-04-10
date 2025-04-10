# Load necessary libraries
library(tidyverse)

# Load Data
dt <- open_recent_file(
  directory = file.path(
    files_dir,
    "Pre_JC_survey_processed"
  )
)

# Step 1: Identify checkbox columns
barriers_columns <- grep("^barriers_ipi_nivo___", names(dt), value = TRUE)

# Step 2: Ensure all checkbox columns are numeric
dt[barriers_columns] <- lapply(dt[barriers_columns], function(x) as.numeric(as.character(x)))

# Step 3: Create 'answered' status based on sum across all barrier checkboxes
dt <- dt |> 
  mutate(barriers_ipi_nivo___answered = rowSums(across(all_of(barriers_columns))) > 0)


# Step 4: Pivot into long format
barriers_long <- dt |> 
  dplyr::select(all_of(barriers_columns)) |> 
  pivot_longer(cols = everything(), 
               names_to = "barriers_ipi_nivo", 
               values_to = "selected") |> 
  dplyr::filter(selected == 1) |> 
  mutate(barriers_ipi_nivo = str_replace(barriers_ipi_nivo, "barriers_ipi_nivo___", ""))

# Step 5: Add non-respondents ("Not Answered")
not_answered_n <- dt |> 
  dplyr::filter(barriers_ipi_nivo___answered == 0) |> 
  summarise(n = n()) |> 
  mutate(
    barriers_ipi_nivo = "not_answered",
    selected = 1
  )

# Step 6: Combine with actual responses
barriers_long <- barriers_long |> 
  bind_rows(not_answered_n |> select(barriers_ipi_nivo, selected))

# Step 7: Apply human-readable labels
barriers_labels <- c(
  "toxicity_concerns"        = "Toxicity concerns",
  "lack_of_data"             = "Lack of high-level data in MCC",
  "institutional_policy"     = "Institutional restrictions or guidelines",
  "payer_barriers"           = "Insurance or Medicare denial",
  "patient_preference"       = "Patient preference",
  "other"                    = "Other",
  "no_barriers"              = "No significant barriers",
  "i_am_not_a_clinician"     = "I am not a clinician",
  "not_applicable_clinician" = "I am a clinician, but I do not manage MCC",
  "not_answered"             = "Not Answered"
)

barriers_long <- barriers_long |> 
  mutate(barriers_ipi_nivo = recode(barriers_ipi_nivo, !!!barriers_labels))

# Step 8: Define ordered levels (include non-response)
ordered_levels <- c(
  "Toxicity concerns",
  "Lack of high-level data in MCC",
  "Institutional restrictions or guidelines",
  "Insurance or Medicare denial",
  "Patient preference",
  "Other",
  "No significant barriers",
  "I am a clinician, but I do not manage MCC",
  "I am not a clinician",
  "Not Answered"
  )

# Step 5: Count occurrences & normalize by total **filtered** responses
plot_data <- barriers_long |> 
  count(barriers_ipi_nivo) |> 
  mutate(prop = round(n / nrow(dt) * 100)) |>  # Normalize by filtered responses
  mutate(barriers_ipi_nivo = factor(barriers_ipi_nivo, levels = ordered_levels, ordered = TRUE)) 

# Step 6: Create Plot
barriers_ipi_nivo_plot <- plot_data |> 
  ggplot(
    aes(x = barriers_ipi_nivo,
        y = n)
  ) + 
  geom_col(fill = "steelblue4") + 
  geom_text(
    aes(label = paste(prop, "%")),
    hjust = -0.1
  ) +
  ggtitle(str_wrap(
    "Barriers to Using Ipi/Nivo in Practice",
    55)
  ) +
  xlab("") +
  ylab(paste0(
    "Number of Respondents (Total = ", nrow(dt), ")")) +  # Use filtered total responses
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(0, 200, 0, 0)),
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
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
  scale_y_continuous(
    breaks = seq(0, max(plot_data$n + 1), by = 2), # Sets breaks at intervals of 2
    limits = c(0, max(plot_data$n + 1)) # Adjusts upper limit for spacing
  ) +
  coord_flip() 

# Display the plot
barriers_ipi_nivo_plot
