
# Load Data

# Load data using the files_dir variable from path.R
dt <- readRDS(file.path(files_dir, "survey_results_pre_test_processed.rds"))

surveillance_lables <- c(
  "I Am Not A Clinician" = "I am not a clinician",
  "Not Applicable Clinician" = "Not applicable to my practice",
  "Not Currently" = "Not currently",
  "Would Use" = "No, but would if recommended",
  "Yes Selective" = "Yes, but only in select high-risk cases",
  "Yes Routinely" = "Yes, I use it regularly"
)

# Replace labels and handle NAs
dt <- dt |> 
  mutate(
    ctdna_surveillance = recode(ctdna_surveillance, !!!surveillance_lables),
    ctdna_surveillance = replace_na(ctdna_surveillance, "Not Answered") # Handle NA values
  )


# Define the correct order of response categories
ordered_levels <- rev(c(
  "Not Answered",
  "I am not a clinician",
  "Not applicable to my practice",
  "Not currently",
  "No, but would if recommended",
  "Yes, but only in select high-risk cases",
  "Yes, I use it regularly"
))

# Ensure factor levels are set before counting
ctdna_surveillance <- dt |> 
  drop_na(ctdna_surveillance) |> 
  mutate(ctdna_surveillance = factor(ctdna_surveillance, levels = ordered_levels, ordered = TRUE)) 

ctdna_surveillance <- 
  ctdna_surveillance |> 
  count(ctdna_surveillance, .drop = FALSE) |>  # .drop = FALSE ensures missing factor levels appear
  mutate(prop = round(n / sum(n) * 100))  

# Generate plot
ctdna_surveillance_plot <- ggplot(
  ctdna_surveillance,
  aes(x = ctdna_surveillance, y = n)
) + 
  geom_col(fill = "steelblue4") + 
  geom_text(
    aes(label = paste0(prop, "%")),
    hjust = -0.1
  ) +
  ggtitle(str_wrap(
    "Do you use ctDNA for MCC surveillance?",
    50)
  ) +
  xlab("") +
  ylab(paste0(
    "Number of Respondents (Total = ",
    sum(ctdna_surveillance$n),
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
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
  scale_y_continuous(
    breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))),
    limits = c(0, max(ctdna_surveillance$n + 0.2))
  ) +
  coord_flip()

# Print the plot
ctdna_surveillance_plot