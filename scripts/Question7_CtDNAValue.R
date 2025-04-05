# Load necessary libraries
library(tidyverse)
library(here)

# Load Data
dt <- readRDS(file.path(files_dir, "survey_results_pre_test_processed.rds"))

dt$ctdna_value[is.na(dt$ctdna_value)] <- "Not Answered"

# Define the correct order of response categories
ordered_levels <- rev(
  c(
    "Not Answered",
    "Not Familiar",
    "No Value",
    "Not Very Valuable",
    "Uncertain",
    "Somewhat Valuable",
    "Extremely Valuable"
  )
)

# Ensure factor levels are set before counting
ctdna_value <- dt |> 
  drop_na(ctdna_value) |> 
  mutate(ctdna_value = factor(ctdna_value, levels = ordered_levels, ordered = TRUE)) |> 
  count(ctdna_value, .drop = FALSE) |>  # .drop = FALSE ensures all levels appear even if count is 0
  mutate(prop = round(n / sum(n) * 100))  

# Debugging: Print to confirm all levels are there, even if n=0
#print(ctdna_value)

# Generate plot
ctdna_value_plot <- ggplot(
  ctdna_value,
  aes(x = ctdna_value, y = n)
) + 
  geom_col(fill = "steelblue4") + 
  geom_text(
    aes(label = paste0(prop, "%")),
    hjust = -0.1
  ) +
  ggtitle(str_wrap(
    "What is your perception of ctDNA value?",
    50)
  ) +
  xlab("") +
  ylab(paste0(
    "Number of Respondents (Total = ",
    sum(ctdna_value$n),
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
    breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))),
    limits = c(0, max(ctdna_value$n + 0.2))
  ) +
  coord_flip()

# Print the plot
ctdna_value_plot
