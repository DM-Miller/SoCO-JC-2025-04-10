# Load Data
dt <- readRDS(file.path(files_dir, "survey_results_pre_test_processed.rds"))

# Identify multi-checkbox columns for ctDNA influence
ctdna_influence_columns <- grep("^ctdna_influence___", names(dt), value = TRUE)

# Convert checkbox fields to numeric (0/1)
dt[ctdna_influence_columns] <- lapply(dt[ctdna_influence_columns], function(x) as.numeric(as.character(x)))

dt1 <- dt |> 
  select(contains("ctdna_influence___")) 

# Remove rows where respondents marked "Not Applicable" or "I Am Not A Clinician"
dt_filtered <- dt1 |>  
  filter(ctdna_influence___not_applicable != 1, 
         ctdna_influence___i_am_not_a_clinician != 1)

# Pivot from wide to long format
ctdna_influence_long <- dt_filtered |> 
  select(all_of(ctdna_influence_columns)) |> 
  pivot_longer(cols = everything(), 
               names_to = "ctdna_influence", 
               values_to = "selected") |> 
  filter(selected == 1) |>  # Keep only checked responses
  mutate(ctdna_influence = str_replace(ctdna_influence, "ctdna_influence___", ""))

# Map numeric codes to human-readable labels
ctdna_labels <- c(
  "yes_influenced_treatment_decision" = "Yes, influenced treatment decisions",
  "yes_influenced_surveillance" = "Yes, influenced surveillance",
  "no_influence" = "No influence"
)

# Replace coded values with text labels
ctdna_influence_long <- 
  ctdna_influence_long |> 
  mutate(ctdna_influence = recode(ctdna_influence, !!!ctdna_labels))

# Define explicit ordering of categories
ordered_levels <- c(
  "Yes, influenced treatment decisions",
  "Yes, influenced surveillance",
  "No influence"
)

# Count occurrences & normalize by filtered responses
ctdna_influence_count <- ctdna_influence_long |> 
  count(ctdna_influence) |> 
  mutate(prop = round(n / nrow(dt_filtered) * 100)) |>  # Normalize by filtered responses
  mutate(ctdna_influence = factor(ctdna_influence, levels = ordered_levels, ordered = TRUE)) 

# Create Plot
ctdna_influence_plot <- ctdna_influence_count |> 
  ggplot(
    aes(x = ctdna_influence, y = n)
  ) + 
  geom_col(fill = "steelblue4") + 
  geom_text(
    aes(label = paste(prop, "%")),
    hjust = -0.1
  ) +
  ggtitle(str_wrap(
    "Has ctDNA influenced your decision-making?",
    50)
  ) +
  xlab("") +
  ylab(paste0(
    "Number of Responses (Total = ", nrow(dt_filtered), ")")) +  # Use total responses after filtering
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(0, 140, 0, 0)),
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
    limits = c(0, max(ctdna_influence_count$n + 1))
  ) +
  coord_flip() 

# Display the plot
ctdna_influence_plot
