# Load necessary libraries
library(tidyverse)
library(here)

# Load Data
dt2 <- readRDS(here("files", "survey_results_post_test_processed.rds"))

# Identify columns related to the multi-checkbox field
future_role_columns <- grep("^future_role_ctdna___", names(dt2), value = TRUE)

# Ensure all checkbox columns are numeric (0/1)
dt2[future_role_columns] <- lapply(dt2[future_role_columns], function(x) as.numeric(as.character(x)))

# Pivot from wide to long format
future_role_long <- dt2 |> 
  select(all_of(future_role_columns)) |> 
  pivot_longer(cols = everything(), 
               names_to = "future_role_ctdna", 
               values_to = "selected") |> 
  filter(selected == 1) |>  # Keep only checked responses
  mutate(future_role_ctdna = str_replace(future_role_ctdna, "future_role_ctdna___", ""))

# Map coded values to human-readable labels
future_role_labels <- c(
  "routine_surveillance" = "Routine surveillance after treatment",
  "high_risk_id" = "Identifying high-risk patients for early intervention",
  "systemic_therapy" = "Guiding systemic therapy choices",
  "no_significant_role" = "I do not see a significant future role",
  "not_a_clinician" = "I am not a clinician",
  "no_mcc_patients" = "I am a clinician but I don't see MCC"
)

# Replace coded values with human-readable labels
future_role_long <- future_role_long |> 
  mutate(future_role_ctdna = recode(future_role_ctdna, !!!future_role_labels))

# Define explicit ordering of categories
ordered_levels <- c(
  "Routine surveillance after treatment",
  "Identifying high-risk patients for early intervention",
  "Guiding systemic therapy choices",
  "I do not see a significant future role",
  "I am not a clinician",
  "I am a clinician but I don't see MCC"
)

# Add "Not Answered" category
answered_ids <- dt2 |> select(all_of(future_role_columns)) |> rowSums() > 0
dt2$future_role_ctdna[!answered_ids] <- "Not Answered"

# Count occurrences & normalize by total responses (not participants)
future_role_count <- future_role_long |> 
  count(future_role_ctdna) |> 
  mutate(prop = round(n / nrow(dt2) * 100)) |>  # Normalize by total responses
  mutate(future_role_ctdna = factor(future_role_ctdna, levels = c(ordered_levels, "Not Answered"), ordered = TRUE))

# Generate Plot
future_role_plot <- future_role_count |> 
  ggplot(aes(x = future_role_ctdna, y = n)) + 
  geom_col(fill = "steelblue4") + 
  geom_text(aes(label = paste(prop, "%")), hjust = -0.1) +
  ggtitle(str_wrap(
    "What potential future role do you see for ctDNA in MCC management?",
    50)
  ) +
  xlab("") +
  ylab(paste0(
    "Number of Responses (Total = ", nrow(dt2), ")")) +  # Normalize by total responses
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
    limits = c(0, max(future_role_count$n + 0.2))
  ) +
  coord_flip()

# Print the plot
future_role_plot
