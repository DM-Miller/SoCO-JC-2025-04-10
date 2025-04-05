# Load necessary libraries
library(tidyverse)
library(here)

# Load Data
dt2 <- readRDS(here("files", "survey_results_post_test_processed.rds"))

# Identify columns related to the multi-checkbox field
ctdna_confidence_columns <- grep("^confidence_in_ctdna_use___", names(dt2), value = TRUE)

# Ensure all checkbox columns are numeric (0/1)
dt2[ctdna_confidence_columns] <- lapply(dt2[ctdna_confidence_columns], function(x) as.numeric(as.character(x)))

# Pivot from wide to long format
confidence_long <- dt2 |> 
  select(all_of(ctdna_confidence_columns)) |> 
  pivot_longer(cols = everything(), 
               names_to = "confidence_in_ctdna_use", 
               values_to = "selected") |> 
  filter(selected == 1) |>  # Keep only checked responses
  mutate(confidence_in_ctdna_use = str_replace(confidence_in_ctdna_use, "confidence_in_ctdna_use___", ""))

# Map coded values to human-readable labels using the REDCap data dictionary
confidence_labels <- c(
  "none_needed" = "No further research needed—ready for clinical implementation",
  "more_data" = "More clinical data on outcomes",
  "clearer_guidelines" = "Clearer guidelines on how to use results",
  "consensus" = "Institutional or specialty group consensus",
  "insurance" = "Insurance coverage for testing",
  "not_a_clinician" = "I am not a clinician",
  "no_mcc_patients" = "I am a clinician but I don't see MCC"
)

# Replace coded values with human-readable labels
confidence_long <- confidence_long |> 
  mutate(confidence_in_ctdna_use = recode(confidence_in_ctdna_use, !!!confidence_labels))

# Define explicit ordering of categories
ordered_levels <- c(
  "No further research needed—ready for clinical implementation",
  "More clinical data on outcomes",
  "Clearer guidelines on how to use results",
  "Institutional or specialty group consensus",
  "Insurance coverage for testing",
  "I am not a clinician",
  "I am a clinician but I don't see MCC"
)

# Count occurrences & normalize by total responses (not participants)
confidence_count <- confidence_long |> 
  count(confidence_in_ctdna_use) |> 
  mutate(prop = round(n / nrow(dt2) * 100)) |>  # Normalize by total responses
  mutate(confidence_in_ctdna_use = factor(confidence_in_ctdna_use, levels = ordered_levels, ordered = TRUE))

# Generate Plot
confidence_plot <- confidence_count |> 
  ggplot(aes(x = confidence_in_ctdna_use, y = n)) + 
  geom_col(fill = "steelblue4") + 
  geom_text(aes(label = paste(prop, "%")), hjust = -0.1) +
  ggtitle(str_wrap(
    "What would most help you feel more confident incorporating ctDNA into patient care?",
    50)
  ) +
  xlab("") +
  ylab(paste0(
    "Number of Responses (Total = ", nrow(dt2), ")")) +  # Normalize by total responses
 # theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.title.x = element_text(face = "bold", size = 16),
    axis.text.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 16),
    axis.text.y = element_text(face = "bold", size = 14),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(),
    plot.margin = margin(t = 0.2, r = 0, b = 0.2, l = 0, "cm")
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) +
  scale_y_continuous(
    breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))),
    limits = c(0, max(confidence_count$n + 0.5))
  ) +
  coord_flip()

# Print the plot
confidence_plot
