# Load necessary libraries
library(tidyverse)
library(scales) # For better label formatting

df <- read_csv("ResponsesDma_preprocessed_R.csv")

-------------------------------------

plot_primary_usage <- df %>%
  count(`Primary Screen Usage`) %>%
  mutate(
    Percentage = n / sum(n),
    Label_Text = scales::percent(Percentage) # Create percentage label
  ) %>%
  # Order by percentage
  mutate(`Primary Screen Usage` = fct_reorder(`Primary Screen Usage`, Percentage)) %>%
  ggplot(aes(x = Percentage, y = `Primary Screen Usage`)) +
  geom_col(fill = "#1ABC9C") +
  # Add data labels
  geom_text(aes(label = Label_Text), hjust = -0.1, size = 4, color = "#2C3E50") +
  labs(
    title = "What Occupies Most of Your Screen Time?",
    x = "Percentage of Respondents",
    y = NULL
  ) +
  scale_x_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    axis.text.y = element_text(size = 10)
  )

print(plot_primary_usage)

plot_distraction <- df %>%
  count(`Most Distracting Activity`, sort = TRUE) %>%
  mutate(`Most Distracting Activity` = fct_reorder(`Most Distracting Activity`, n)) %>%
  ggplot(aes(x = n, y = `Most Distracting Activity`)) +
  geom_col(fill = "#3498DB") +
  # Add count labels
  geom_text(aes(label = n), hjust = -0.3, size = 4) +
  labs(
    title = "Most Distracting Screen Activities During Work/Study",
    x = "Count of Respondents",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14)
  )

print(plot_distraction)

# ----------------------------------------------------------------------
# 3. Screen Time vs. Sleep Quality Impact (Violin Plot with Median Labels)
# Insight: Relationship between screen time (distribution) and sleep issues.
# ----------------------------------------------------------------------

# 1. Create the ordered factor for sleep quality labels
df <- df %>%
  mutate(`Sleep Quality Factor` = factor(`Sleep Quality Impact`,
                                         levels = c(0, 1, 2, 3),
                                         labels = c("No effect (0)", "Slightly affects (1)", "Moderately affects (2)", "Significantly affects (3)"),
                                         ordered = TRUE)) %>%
  # 2. Calculate medians for labels
  group_by(`Sleep Quality Factor`) %>%
  mutate(median_time = median(`Screen Time (Hours)`, na.rm = TRUE)) %>%
  ungroup()

plot_sleep_impact <- df %>%
  ggplot(aes(x = `Sleep Quality Factor`, y = `Screen Time (Hours)`, fill = `Sleep Quality Factor`)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.1, outlier.shape = NA, fill = "white") +
  # Add median labels
  geom_text(aes(y = median_time, label = round(median_time, 1)),
            color = "black",
            fontface = "bold",
            size = 4,
            vjust = -1.5,
            position = position_dodge(width = 0.1)) +
  labs(
    title = "Daily Screen Time Distribution by Sleep Impact",
    x = "Impact on Sleep Quality (Numerical Scale in Brackets)",
    y = "Screen Time (Hours per day)"
  ) +
  scale_fill_manual(values = c("#2ECC71", "#F1C40F", "#E67E22", "#E74C3C")) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    axis.text.x = element_text(angle = 15, hjust = 1)
  )

print(plot_sleep_impact)


# ----------------------------------------------------------------------
# 4. App/Tool Usage vs. Concentration Impact (Box Plot with Labels)
# Insight: Compares concentration scores for users vs. non-users of focus apps.
# ----------------------------------------------------------------------

# 1. Create factor labels for App Usage
df <- df %>%
  mutate(`App/Tool Usage Factor` = factor(`App/Tool Usage`,
                                          levels = c(0, 1),
                                          labels = c("No App/Tool Used", "App/Tool Used"))) %>%
  # 2. Calculate mean concentration for labels
  group_by(`App/Tool Usage Factor`) %>%
  mutate(mean_concentration = mean(`Concentration Impact (1-5)`, na.rm = TRUE)) %>%
  ungroup()

plot_app_concentration <- df %>%
  ggplot(aes(x = `App/Tool Usage Factor`, y = `Concentration Impact (1-5)`, fill = `App/Tool Usage Factor`)) +
  geom_boxplot(width = 0.4, alpha = 0.8) +
  # Add mean concentration labels
  geom_text(aes(y = mean_concentration, label = paste("Mean:", round(mean_concentration, 2))),
            color = "#34495E",
            fontface = "bold",
            size = 4,
            vjust = -1.5) +
  labs(
    title = "Concentration Impact by App/Tool Usage",
    x = NULL,
    y = "Concentration Impact (1=Worst, 5=Best)"
  ) +
  scale_y_continuous(breaks = 1:5, limits = c(1, 5.5)) +
  scale_fill_manual(values = c("#95A5A6", "#8E44AD")) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14)
  )

print(plot_app_concentration)

# ----------------------------------------------------------------------
# 5. Productivity Time Distribution (Lollipop Chart)
# Insight: Clear visualization of when respondents feel most productive.
# ----------------------------------------------------------------------

plot_productivity_time <- df %>%
  count(`Most Productive Time`) %>%
  drop_na() %>%
  mutate(`Most Productive Time` = fct_reorder(`Most Productive Time`, n)) %>%
  ggplot(aes(x = `Most Productive Time`, y = n)) +
  geom_segment(aes(xend = `Most Productive Time`, yend = 0), color = "#E67E22", linewidth = 1) +
  geom_point(size = 4, color = "#D35400") +
  # Add count labels
  geom_text(aes(label = n), vjust = -1, size = 4, color = "#2C3E50", fontface = "bold") +
  labs(
    title = "Most Productive Time of Day Using Screens",
    x = NULL,
    y = "Count of Respondents"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    axis.text.x = element_text(angle = 20, hjust = 1)
  )

print(plot_productivity_time)

# ----------------------------------------------------------------------
# 6. Likert Scale Agreement Heatmap (Visualizing Three Agreement Columns)
# Insight: How responses are distributed across agreement statements.
# ----------------------------------------------------------------------

# 1. Gather the three Likert columns into a long format
likert_df <- df %>%
  select(`Screens help me learn new skills. (Likert)`,
         `I feel addicted to my screen. (Likert)`,
         `I experience eye strain or headaches from screen use. (Likert)`) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Statement",
    values_to = "Agreement_Score"
  ) %>%
  drop_na() %>%
  # 2. Count the frequency for each combination
  count(Statement, Agreement_Score) %>%
  group_by(Statement) %>%
  mutate(Percentage = n / sum(n)) %>%
  ungroup() %>%
  # 3. Create descriptive labels for the score
  mutate(
    Agreement_Label = case_when(
      Agreement_Score == 5 ~ "Strongly Agree (5)",
      Agreement_Score == 4 ~ "Agree (4)",
      Agreement_Score == 3 ~ "Neutral (3)",
      Agreement_Score == 2 ~ "Disagree (2)",
      Agreement_Score == 1 ~ "Strongly Disagree (1)",
      TRUE ~ "NA"
    ),
    # 4. Convert score to a factor for correct ordering
    Agreement_Label = factor(Agreement_Label, levels = c("Strongly Disagree (1)", "Disagree (2)", "Neutral (3)", "Agree (4)", "Strongly Agree (5)"))
  )

plot_heatmap <- likert_df %>%
  ggplot(aes(x = Agreement_Label, y = Statement, fill = Percentage)) +
  geom_tile(color = "white", linewidth = 1) +
  # Add percentage labels (legends) to the tiles
  geom_text(aes(label = scales::percent(Percentage, accuracy = 1)), color = "white", size = 4) +
  labs(
    title = "Agreement Distribution for Key Statements",
    x = "Agreement Score",
    y = NULL,
    fill = "Percentage of Responses"
  ) +
  scale_fill_gradient(low = "#ECF0F1", high = "#C0392B", labels = scales::percent) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    axis.text.x = element_text(angle = 25, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 9)
  )

print(plot_heatmap)