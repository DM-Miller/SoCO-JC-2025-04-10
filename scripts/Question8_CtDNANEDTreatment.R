# Load necessary libraries
library(tidyverse)
library(here)

# Load Data
dt <- readRDS(file.path(files_dir, "survey_results_pre_test_processed.rds"))

dt$ctdna_ned_treatment[is.na(dt$ctdna_ned_treatment)] <- "Not Answered"

dt1 <- dt |> 
  select(ctdna_ned_treatment)

# Remove "Not Answered" and "I Am Not A Clinician" responses
dt_filtered <- dt1 |> 
  filter(!ctdna_ned_treatment %in% c("Not Answered", "I Am Not A Clinician", "Not Applicable"))

# Define the correct order of response categories
ordered_levels <- c(
  "No",
  "Yes"
)

# Process Data with explicit ordering
ctdna_ned_treatment_count <- dt_filtered |> 
  mutate(ctdna_ned_treatment = factor(ctdna_ned_treatment, levels = ordered_levels, ordered = TRUE)) |> 
  count(ctdna_ned_treatment, .drop = FALSE) |>  # .drop = FALSE ensures all levels appear even if count is 0
  mutate(prop = round(n / sum(n) * 100))  


# Debugging: Print to confirm ordering
#print(ctdna_ned_treatment_count)

# Create Plot
ctdna_ned_treatment_plot <- ggplot(ctdna_ned_treatment_count, aes(x = ctdna_ned_treatment, y = n)) +
  geom_col(fill = "steelblue4") +
  geom_text(aes(label = paste0(prop, "%")), hjust = -0.1) +
  ggtitle(str_wrap("Have you initiated systemic therapy in NED patients based on ctDNA?", 40)) +
  xlab("") +
  ylab(paste0("Number of Respondents (Total = ", sum(ctdna_ned_treatment_count$n), ")")) +
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
  scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) +
  scale_y_continuous(
    breaks = seq(0, max(ctdna_ned_treatment_count$n + 1), by = 2), # Sets breaks at intervals of 2
    limits = c(0, max(ctdna_ned_treatment_count$n + 0.4))
  ) +
  coord_flip()

ctdna_ned_treatment_plot
