# Load necessary libraries
library(tidyverse)
library(here)

# Load Data
dt2 <- readRDS(file.path(files_dir, "survey_results_post_test_processed.rds"))

# Standardize response categories by mapping them correctly
dt2 <- dt2 |> 
  mutate(comfort_interpreting_ctdna = case_when(
    str_detect(comfort_interpreting_ctdna, "Fully Confident") ~ "Fully confident in interpreting ctDNA results",
    str_detect(comfort_interpreting_ctdna, "More Comfortable") ~ "Yes, much more comfortable",
    str_detect(comfort_interpreting_ctdna, "Somewhat Comfortable") ~ "Somewhat, but still have questions",
    str_detect(comfort_interpreting_ctdna, "Still Challenging") ~ "No, I still find it challenging",
    str_detect(comfort_interpreting_ctdna, "No Mcc Patients") ~ "I am a clinician but I don't see MCC",
    str_detect(comfort_interpreting_ctdna, "Not A Clinician") ~ "I am not a clinician",
    TRUE ~ comfort_interpreting_ctdna  # Keep as is if no match is found
  ))

# Replace NA values with "Not Answered"
dt2$comfort_interpreting_ctdna[is.na(dt2$comfort_interpreting_ctdna)] <- "Not Answered"

# Define the correct order of response categories
ordered_levels <- c(
  "Fully confident in interpreting ctDNA results",
  "Yes, much more comfortable",
  "Somewhat, but still have questions",
  "No, I still find it challenging",
  "I am not a clinician",
  "I am a clinician but I don't see MCC",
  "Not Answered"
  )

# Ensure factor levels are set before counting
comfort_ctdna <- dt2 |> 
  mutate(comfort_interpreting_ctdna = factor(comfort_interpreting_ctdna, levels = ordered_levels, ordered = TRUE)) |> 
  count(comfort_interpreting_ctdna, .drop = FALSE) |>  # .drop = FALSE keeps missing factor levels
  mutate(prop = round(n / sum(n) * 100))  

# Generate plot
comfort_ctdna_plot <- ggplot(
  comfort_ctdna,
  aes(x = comfort_interpreting_ctdna, y = n)
) + 
  geom_col(fill = "steelblue4") + 
  geom_text(
    aes(label = paste0(prop, "%")),
    hjust = -0.1
  ) +
  ggtitle(str_wrap(
    "After this discussion, do you feel more comfortable interpreting ctDNA results in the context of MCC?",
    55)
  ) +
  xlab("") +
  ylab(paste0(
    "Number of Respondents (Total = ", sum(comfort_ctdna$n), ")")) +
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
    plot.margin = margin(0.2, 0, 0.2, 0, "cm")
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) +
  scale_y_continuous(
    breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))),
    limits = c(0, max(comfort_ctdna$n + 0.2))
  ) +
  coord_flip()

# Print the plot
comfort_ctdna_plot
