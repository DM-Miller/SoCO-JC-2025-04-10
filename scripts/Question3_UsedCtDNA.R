# Load necessary libraries
library(tidyverse)
library(here)

# Load Data
dt <- readRDS(file.path(files_dir, "survey_results_pre_test_processed.rds"))

# Replace NA values with "Not Answered" before filtering
dt_used_ctdna <- dt %>%
  select(used_ctdna) |> 
  mutate(used_ctdna = replace_na(used_ctdna, "Not Answered"))

# Filter out "Not Answered" and "I Am Not A Clinician"
dt_used_ctdna_filtered <- dt_used_ctdna %>%
  filter(!(used_ctdna %in% c("Not Answered", "I Am Not A Clinician")))

# Define the correct order of response categories
ordered_levels <- c("Yes", "No")

# Convert to factor with defined order
dt_used_ctdna_filtered <- dt_used_ctdna_filtered %>%
  mutate(used_ctdna = factor(used_ctdna, levels = ordered_levels, ordered = TRUE))

# Count occurrences of each response
used_ctdna <- dt_used_ctdna_filtered %>%
  count(used_ctdna, .drop = FALSE) %>%
  mutate(prop = round(n / sum(n) * 100, 1))  # Ensuring proportions are calculated correctly


# Generate plot
used_ctdna_plot <- ggplot(
  used_ctdna,
  aes(x = used_ctdna, y = n)
) + 
  geom_col(fill = "steelblue4") + 
  geom_text(
    aes(label = paste0(prop, "%")),
    hjust = -0.1
  ) +
  ggtitle(str_wrap(
    "Have you used ctDNA in clinical practice?",
    50)
  ) +
  xlab("") +
  ylab(paste0(
    "Number of Respondents (Total = ",
    sum(used_ctdna$n),
    ")")) +
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
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  scale_y_continuous(
    breaks = seq(0, max(used_ctdna$n + 1), by = 2), # Sets breaks at intervals of 2
    limits = c(0, max(used_ctdna$n + 1))
  ) +
  coord_flip()

# Print the plot
used_ctdna_plot
