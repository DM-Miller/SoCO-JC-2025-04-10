# Load necessary libraries
library(tidyverse)
library(here)


dt2 <- readRDS(
  here(
    "files",
    "/survey_results_post_test_processed.rds")
)

# Replace NA values with "Not Answered"
dt2$integrate_ctdna[is.na(dt2$integrate_ctdna)] <- "Not Answered"

#Standardize response categories by mapping them correctly
dt2 <- dt2 |> 
  mutate(integrate_ctdna = case_when(
    str_detect(integrate_ctdna, "Already Using") ~ "Already using it routinely",
    str_detect(integrate_ctdna, "More Likely") ~ "More likely to consider using it in select cases",
    str_detect(integrate_ctdna, "Uncertain") ~ "Uncertain, need more evidence",
    str_detect(integrate_ctdna, "Unlikely") ~ "Unlikely to change current practice",
    str_detect(integrate_ctdna, "Not A Clinician") ~ "I am not a clinician",
    str_detect(integrate_ctdna, "No Mcc Patients") ~ "I am a clinician but I don't see MCC",
    str_detect(integrate_ctdna, "Not Answered") ~ "Not Answered",  # Ensure "Not Answered" remains
    TRUE ~ integrate_ctdna  # Keep as is if no match is found
  ))

# Define the correct order of response categories
ordered_levels <- c(
  "Already using it routinely",
  "More likely to consider using it in select cases",
  "Uncertain, need more evidence",
  "Unlikely to change current practice",
  "I am not a clinician",
  "I am a clinician but I don't see MCC",
  "Not Answered"
)

# Ensure factor levels are set before counting
integrate_ctdna <- dt2 |> 
  drop_na(integrate_ctdna) |> 
  mutate(integrate_ctdna = factor(integrate_ctdna, levels = ordered_levels, ordered = TRUE)) |> 
  count(integrate_ctdna, .drop = FALSE) |>  # .drop = FALSE keeps missing factor levels
  mutate(prop = round(n / sum(n) * 100))  

# Generate plot
integrate_ctdna_plot <- ggplot(
  integrate_ctdna,
  aes(x = integrate_ctdna, y = n)
) + 
  geom_col(fill = "steelblue4") + 
  geom_text(
    aes(label = paste0(prop, "%")),
    hjust = -0.1
  ) +
  ggtitle(str_wrap(
    "Based on today’s discussion, how likely are you to integrate ctDNA into your clinical practice?",
    50)
  ) +
  xlab("") +
  ylab(paste0(
    "Number of Respondents (Total = ", sum(integrate_ctdna$n), ")")) +
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
    breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))),
    limits = c(0, max(integrate_ctdna$n + 0.2))
  ) +
  coord_flip()

# Print the plot
#integrate_ctdna_plot
