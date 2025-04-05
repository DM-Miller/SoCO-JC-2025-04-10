# Load necessary libraries
library(tidyverse)
library(here)

# Load Data
dt2 <- readRDS(file.path(files_dir, "survey_results_post_test_processed.rds"))

# Identify columns related to the multi-checkbox field
ici_info_columns <- grep("^additional_info_for_ici___", names(dt2), value = TRUE)

# Replace NA values with 0 (unchecked)
dt2[ici_info_columns][is.na(dt2[ici_info_columns])] <- 0

# Pivot from wide to long format
ici_info_long <- dt2 |> 
  select(all_of(ici_info_columns)) |> 
  pivot_longer(cols = everything(), 
               names_to = "additional_info_for_ici", 
               values_to = "selected") |> 
  filter(selected == 1) |>  # Keep only checked responses
  mutate(additional_info_for_ici = str_replace(additional_info_for_ici, "additional_info_for_ici___", ""))

# Map coded values to their corresponding answer choices
ici_info_labels <- c(
  "not_needed" = "No further info needed – would recommend ICI",
  "prospective_trial" = "Prospective clinical trial supporting benefit",
  "retrospective_data" = "Retrospective data showing improved outcomes",
  "validation" = "Further validation of ctDNA assays",
  "guidelines" = "Consensus guidelines or expert recommendations",
  "real_world_evidence" = "Additional real-world evidence",
  "not_a_clinician" = "I am not a clinician",
  "no_mcc_patients" = "I am a clinician but I don't see MCC"
)

# Replace coded values with human-readable labels
ici_info_long <- ici_info_long |> 
  mutate(additional_info_for_ici = recode(additional_info_for_ici, !!!ici_info_labels))

dt3 <- dt2 |> 
  select(contains("additional_info_for_ici"))

# Filter for MCC clinicians only
mcc_clinicians <- dt3 |> 
  filter(additional_info_for_ici___not_a_clinician != 1 &
           additional_info_for_ici___no_mcc_patients != 1)

# Use MCC clinician subset for calculations
ici_info_long_2 <- ici_info_long |> 
  filter(!ici_info_long$additional_info_for_ici %in% c(
    "I am not a clinician", "I am a clinician but I don't see MCC"
  ))

# Define explicit ordering of categories
ordered_levels <- c(
  "No further info needed – would recommend ICI",
  "Prospective clinical trial supporting benefit",
  "Retrospective data showing improved outcomes",
  "Further validation of ctDNA assays",
  "Consensus guidelines or expert recommendations",
  "Additional real-world evidence"
)

# Count occurrences & normalize by MCC clinician responses
ici_info_counts <- ici_info_long_2 |> 
  count(additional_info_for_ici) |> 
  mutate(prop = round(n / nrow(mcc_clinicians) * 100)) |>  # Normalize by MCC clinicians (n=18)
  mutate(additional_info_for_ici = factor(additional_info_for_ici, levels = ordered_levels, ordered = TRUE)) 

# Generate Plot
ici_info_plot <- ggplot(
  ici_info_counts,
  aes(x = additional_info_for_ici, y = n)
) + 
  geom_col(fill = "steelblue4") + 
  geom_text(
    aes(label = paste(prop, "%")),
    hjust = -0.1
  ) +
  ggtitle(str_wrap(
    "What would help you feel more confident recommending an ICI for ctDNA-positive, NED patients?",
    52)
  ) +
  xlab("") +
  ylab(paste0(
    "Number of Responses (Total = ", nrow(mcc_clinicians), ")")) +  # Use MCC clinicians (n=18)
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18, margin = margin(0,170,0,0)),
    axis.title.x = element_text(face = "bold", size = 16),
    axis.text.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 16),
    axis.text.y = element_text(face = "bold", size = 12),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(),
    plot.margin = margin(0.2, 0, 0.2, 0, "cm")
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) +
  scale_y_continuous(
    breaks = seq(0, max(ici_info_counts$n) + 0.5, by = 1),
    limits = c(0, max(ici_info_counts$n + 1))
  ) +
  coord_flip()

# Print the plot
ici_info_plot
