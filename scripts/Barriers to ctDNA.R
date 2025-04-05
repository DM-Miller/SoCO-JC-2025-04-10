# Load necessary libraries
library(tidyverse)
library(here)

# Load Data
dt2 <- readRDS(file.path(files_dir, "survey_results_post_test_processed.rds"))

# Standardize response categories by mapping them correctly
dt2 <- dt2 |> 
  mutate(barriers_to_ctdna = case_when(
    str_detect(barriers_to_ctdna, "No Barriers") ~ "I do not see significant barriers to using ctDNA",
    str_detect(barriers_to_ctdna, "Guidelines") ~ "Limited clinical guidelines",
    str_detect(barriers_to_ctdna, "Unclear Outcomes") ~ "Impact on outcomes unclear",
    str_detect(barriers_to_ctdna, "Cost Issues") ~ "Cost/coverage issues",
    str_detect(barriers_to_ctdna, "Logistical Challenges") ~ "Logistical challenges",
    str_detect(barriers_to_ctdna, "No Mcc Patients") ~ "I am a clinician but I don't see MCC",
    str_detect(barriers_to_ctdna, "Not A Clinician") ~ "I am not a clinician",
    TRUE ~ barriers_to_ctdna  # Keep as is if no match is found
  ))

# Replace NA values with "Not Answered"
dt2$barriers_to_ctdna[is.na(dt2$barriers_to_ctdna)] <- "Not Answered"

# Define the correct order of response categories
ordered_levels <- c(
  "I do not see significant barriers to using ctDNA",
  "Limited clinical guidelines",
  "Impact on outcomes unclear",
  "Cost/coverage issues",
  "Logistical challenges",
  "I am not a clinician",
  "I am a clinician but I don't see MCC",
  "Not Answered"
)

# Ensure factor levels are set before counting
barriers_ctdna <- dt2 |> 
  drop_na(barriers_to_ctdna) |> 
  mutate(barriers_to_ctdna = factor(barriers_to_ctdna, levels = ordered_levels, ordered = TRUE)) |> 
  count(barriers_to_ctdna, .drop = FALSE) |>  
  mutate(prop = round(n / sum(n) * 100))  

# Generate plot
barriers_ctdna_plot <- ggplot(
  barriers_ctdna,
  aes(x = barriers_to_ctdna, y = n)
) + 
  geom_col(fill = "steelblue4") + 
  geom_text(
    aes(label = paste0(prop, "%")),
    hjust = -0.1
  ) +
  ggtitle(str_wrap(
    "What do you see as the biggest barrier to using ctDNA in clinical practice?",
    50)
  ) +
  xlab("") +
  ylab(paste0(
    "Number of Respondents (Total = ", sum(barriers_ctdna$n), ")")) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    title = element_text(face = "bold", size = 18),
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
  scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) +
  scale_y_continuous(
    breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))),
    limits = c(0, max(barriers_ctdna$n + 0.2))
  ) +
  coord_flip()

# Print the plot
barriers_ctdna_plot
