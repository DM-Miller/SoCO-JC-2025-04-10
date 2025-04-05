# Load Data
dt <- readRDS(file.path(files_dir, "survey_results_pre_test_processed.rds"))

# Identify columns related to the multi-checkbox field
ctdna_columns <- grep("^ctdna_use_settings___", names(dt), value = TRUE)

# Ensure all checkbox columns are numeric (0/1)
dt[ctdna_columns] <- lapply(dt[ctdna_columns], function(x) as.numeric(as.character(x)))

# Step 1: Remove respondents who selected "Not a clinician" or "I have not used ctDNA"
filtered_dt <- dt |> 
  filter(ctdna_use_settings___not_a_clinician != 1, ctdna_use_settings___have_not_used != 1)

# Step 2: Pivot from wide to long format
ctdna_long <- filtered_dt |> 
  select(all_of(ctdna_columns)) |> 
  pivot_longer(cols = everything(), 
               names_to = "ctdna_use_settings", 
               values_to = "selected") |> 
  filter(selected == 1) |>  # Keep only checked responses
  mutate(ctdna_use_settings = str_replace(ctdna_use_settings, "ctdna_use_settings___", ""))

# Step 3: Map numeric values to their corresponding answer choices
ctdna_labels <- c(
  "surveillance_recurrence" = "Monitoring recurrence (surveillance after treatment)",
  "monitor_response" = "Monitoring disease response during therapy",
  "diagnostic_workup" = "Part of initial diagnostic evaluation",
  "research_only" = "Only as part of a clinical trial or research"
)

# Replace coded values with human-readable labels
ctdna_long <- ctdna_long |> 
  mutate(ctdna_use_settings = recode(ctdna_use_settings, !!!ctdna_labels))

# Step 4: Define explicit ordering of categories
ordered_levels <- c(
  "Monitoring recurrence (surveillance after treatment)",
  "Monitoring disease response during therapy",
  "Part of initial diagnostic evaluation",
  "Only as part of a clinical trial or research"
)

# Step 5: Count occurrences & normalize by total **filtered** responses
ctdna_use_settings_count <- ctdna_long |> 
  count(ctdna_use_settings) |> 
  mutate(prop = round(n / nrow(filtered_dt) * 100)) |>  # Normalize by filtered responses
  mutate(ctdna_use_settings = factor(ctdna_use_settings, levels = ordered_levels, ordered = TRUE)) 

# Step 6: Create Plot
ctdna_use_settings_plot <- ctdna_use_settings_count |> 
  ggplot(
    aes(x = ctdna_use_settings,
        y = n)
  ) + 
  geom_col(fill = "steelblue4") + 
  geom_text(
    aes(label = paste(prop, "%")),
    hjust = -0.1
  ) +
  ggtitle(str_wrap(
    "Settings where ctDNA was used",
    35)
  ) +
  xlab("") +
  ylab(paste0(
    "Number of Respondents (Total = ", nrow(filtered_dt), ")")) +  # Use filtered total responses
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(0, 200, 0, 0)),
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
    breaks = seq(0, max(ctdna_use_settings_count$n + 1), by = 2), # Sets breaks at intervals of 2
    limits = c(0, max(ctdna_use_settings_count$n + 1)) # Adjusts upper limit for spacing
  ) +
  coord_flip() 

# Display the plot
ctdna_use_settings_plot
