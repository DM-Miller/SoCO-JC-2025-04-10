# Load necessary libraries
library(tidyverse)
library(here)

# Load Data
dt2 <- readRDS(file.path(files_dir, "survey_results_post_test_processed.rds"))

# Identify columns related to the multi-checkbox field
ctdna_columns <- grep("^ctdna_research_needs___", names(dt2), value = TRUE)

# Ensure all checkbox columns are numeric (0/1)
dt2[ctdna_columns] <- lapply(dt2[ctdna_columns], function(x) as.numeric(as.character(x)))

# Pivot from wide to long format
ctdna_long <- dt2 |> 
  select(all_of(ctdna_columns)) |> 
  pivot_longer(cols = everything(), 
               names_to = "ctdna_research_needs", 
               values_to = "selected") |> 
  filter(selected == 1) |>  # Keep only checked responses
  mutate(ctdna_research_needs = str_replace(ctdna_research_needs, "ctdna_research_needs___", ""))

# Map numeric values to their corresponding answer choices using the REDCap data dictionary
ctdna_labels <- c(
  "surveillance" = "Utility in surveillance",
  "treatment_decision" = "Impact on treatment decision-making",
  "standardization" = "Standardization of assays and interpretation",
  "cost" = "Cost-effectiveness and insurance coverage",
  "none_needed" = "No further research needed—ready for clinical implementation",
  "not_a_clinician" = "I am not a clinician",
  "no_mcc_patients" = "I am a clinician but I don't see MCC"
)

# Replace coded values with human-readable labels
ctdna_long <- ctdna_long |> 
  mutate(ctdna_research_needs = recode(ctdna_research_needs, !!!ctdna_labels))

# Define explicit ordering of categories
ordered_levels <- c(
  "No further research needed—ready for clinical implementation",
  "Utility in surveillance",
  "Impact on treatment decision-making",
  "Standardization of assays and interpretation",
  "Cost-effectiveness and insurance coverage",
  "I am not a clinician",
  "I am a clinician but I don't see MCC"
)

# Count occurrences & normalize by total responses (not participants)
ctdna_research_needs_count <- ctdna_long |> 
  count(ctdna_research_needs) |> 
  mutate(prop = round(n / nrow(dt2) * 100)) |>  # Normalize by total responses
  mutate(ctdna_research_needs = factor(ctdna_research_needs, levels = ordered_levels, ordered = TRUE)) 

# Create Plot
ctdna_research_plot <- ctdna_research_needs_count |> 
  ggplot(
    aes(x = ctdna_research_needs,
        y = n)
  ) + 
  geom_col(fill = "steelblue4") + 
  geom_text(
    aes(label = paste(prop, "%")),
    hjust = -0.1
  ) +
  ggtitle(str_wrap(
    "Which aspect of ctDNA in MCC needs the most research?",
    40)
  ) +
  xlab("") +
  ylab(paste0(
    "Number of Responses (Total = ", nrow(dt2), ")")) +  # Use total responses, not participants
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
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
    breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))),
    limits = c(0, max(ctdna_research_needs_count$n + 0.2))
  ) +
  coord_flip() 

# Display the plot
#ctdna_research_plot
