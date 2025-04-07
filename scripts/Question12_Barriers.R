# Load necessary libraries
library(tidyverse)

# Load Data
dt <- open_recent_file(
  directory = file.path(
    files_dir,
    "Pre_JC_survey_processed"
  )
)

# Identify columns related to the multi-checkbox field
barriers_columns <- grep("^barriers_ipi_nivo___", names(dt), value = TRUE)

# Ensure all checkbox columns are numeric (0/1)
dt[barriers_columns] <- lapply(dt[barriers_columns], function(x) as.numeric(as.character(x)))

# Step 1: Remove respondents who selected "Not a clinician"
filtered_dt <- dt |> 
  dplyr::filter(
    barriers_ipi_nivo___i_am_not_a_clinician != 1
    )

# Step 2: Pivot from wide to long format
barriers_long <- filtered_dt |> 
  dplyr::select(all_of(barriers_columns)) |> 
  pivot_longer(cols = everything(), 
               names_to = "barriers_ipi_nivo", 
               values_to = "selected") |> 
  dplyr::filter(selected == 1) |>  # Keep only checked responses
  mutate(barriers_ipi_nivo = str_replace(barriers_ipi_nivo, "barriers_ipi_nivo___", ""))

# Step 3: Map numeric values to their corresponding answer choices
# Label mapping
barriers_labels <- c(
  "toxicity_concerns"     = "Toxicity concerns",
  "lack_of_data"          = "Lack of high-level data in MCC",
  "institutional_policy"  = "Institutional restrictions or guidelines",
  "payer_barriers"        = "Insurance or Medicare denial",
  "patient_preference"    = "Patient preference",
  "other"                 = "Other",
  "no_barriers"           = "No significant barriers"
)

# Replace coded values with human-readable labels
# Recode and order
barriers_long <- barriers_long |> 
  dplyr::filter(barriers_ipi_nivo %in% names(barriers_labels)) |> 
  mutate(barriers_ipi_nivo = recode(barriers_ipi_nivo, !!!barriers_labels))

# Step 4: Define explicit ordering of categories
ordered_levels <- c(
  "Toxicity concerns",
  "Lack of high-level data in MCC",
  "Institutional restrictions or guidelines",
  "Insurance or Medicare denial",
  "Patient preference",
  "Other",
  "No significant barriers"
)

# Step 5: Count occurrences & normalize by total **filtered** responses
barriers_ipi_nivo_count <- barriers_long |> 
  count(barriers_ipi_nivo) |> 
  mutate(prop = round(n / nrow(filtered_dt) * 100)) |>  # Normalize by filtered responses
  mutate(barriers_ipi_nivo = factor(barriers_ipi_nivo, levels = ordered_levels, ordered = TRUE)) 

# Step 6: Create Plot
barriers_ipi_nivo_plot <- barriers_ipi_nivo_count |> 
  ggplot(
    aes(x = barriers_ipi_nivo,
        y = n)
  ) + 
  geom_col(fill = "steelblue4") + 
  geom_text(
    aes(label = paste(prop, "%")),
    hjust = -0.1
  ) +
  ggtitle(str_wrap(
    "Barriers to Using Ipi/Nivo in Practice",
    55)
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
    breaks = seq(0, max(barriers_ipi_nivo_count$n + 1), by = 2), # Sets breaks at intervals of 2
    limits = c(0, max(barriers_ipi_nivo_count$n + 1)) # Adjusts upper limit for spacing
  ) +
  coord_flip() 

# Display the plot
barriers_ipi_nivo_plot
