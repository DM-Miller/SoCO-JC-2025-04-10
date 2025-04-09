# Load the processed survey dataset
dt2 <- open_recent_file(
  directory = file.path(
    files_dir,
    "Post_JC_survey_processed"
  )
)

# Standardize response categories
dt2 <- dt2 |> 
  mutate(change_practice_yn = case_when(
    str_detect(change_practice_yn, "Yes") ~ "Yes",
    str_detect(change_practice_yn, "No Not Persuasive Enough") ~ "No – Not persuasive enough",
    str_detect(change_practice_yn, "No My Practice Is Already Aligned") ~ "No – My practice is already aligned",
    str_detect(change_practice_yn, "Topic Not Relevant To My Trade") ~ "Topic not relevant to my trade",
    TRUE ~ change_practice_yn  # Keep as is if no match is found
  ))

# Replace NA values with "Not Answered"
dt2$change_practice_yn[is.na(dt2$change_practice_yn)] <- "Not Answered"

# Define the correct order of response categories
ordered_levels <- c(
  "Yes",
  "No – My practice is already aligned",
  "No – Not persuasive enough",
  "Topic not relevant to my trade",
  "Not Answered"
)

# Ensure factor levels are set before counting
practice <- dt2 |> 
  count(change_practice_yn) |> 
  mutate(prop = round(n / sum(n) * 100)) |> 
  mutate(change_practice_yn = factor(change_practice_yn, levels = ordered_levels, ordered = TRUE))

# Generate plot
practice_plot <- practice |> 
  ggplot(aes(x = change_practice_yn, y = n)) + 
  geom_col(fill = "steelblue4") + 
  geom_text(aes(label = paste0(prop, "%")), hjust = -0.1) +
  ggtitle(str_wrap("Will this paper change your practice?", 50)) +
  xlab("") +
  ylab(paste0("Number of Respondents (Total = ", sum(practice$n), ")")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 24),
    axis.title.x = element_text(face = "bold", size = 16),
    axis.text.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 16),
    axis.text.y = element_text(face = "bold", size = 16),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.line = element_line(),
    plot.margin = margin(t = 0.2, r = 0, b = 0.2, l = 0, "cm")
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  scale_y_continuous(
    breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))),
    limits = c(0, max(practice$n + 0.2))
  ) +
  coord_flip()

# Print the plot
practice_plot
