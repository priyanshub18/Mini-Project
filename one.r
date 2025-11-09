# Load necessary libraries
library(tidyverse)

# Define the file name
file_name <- "ResponsesDma.csv"

# Load the dataset (using read_csv from readr for better performance and handling)
df <- read_csv(file_name)

# --- Define Mappings for Numerical Conversions ---

# 2. Screen Time (Mean of the range)
screen_time_map <- c(
  "Less than 2 hours" = 1,
  "2–4 hours" = 3,
  "4–6 hours" = 5,
  "6–8 hours" = 7,
  "More than 8 hours" = 9
)

# 7/9. Break Frequency Scale (Every 30 min as 1, etc.)
break_frequency_map <- c(
  "Every 30 minutes" = 1,
  "Every hour" = 2,
  "Every 2–3 hours" = 3,
  "Rarely" = 4
)

# 8. Sleep Quality Impact (No effect=0, up to Significant=3)
sleep_impact_map <- c(
  "No effect" = 0,
  "Slightly affects" = 1,
  "Moderately affects" = 2,
  "Significantly affects" = 3
)

# 11. Likert Scale Mapping (Strongly Agree=5 to Strongly Disagree=1)
likert_map <- c(
  "Strongly Agree" = 5,
  "Agree" = 4,
  "Neutral" = 3,
  "Disagree" = 2,
  "Strongly Disagree" = 1
)


# --- Data Preprocessing Pipeline ---

df_preprocessed <- df %>%
  # Drop Timestamp and Email address for cleaner data
  select(-Timestamp, -`Email address`) %>%
  
  # 1. make score column all na as it is useless
  mutate(Score = NA_real_) %>%
  
  # 2. Convert Screen Time to numerical mean
  mutate(`Screen Time (Hours)` = recode(`How many hours per day do you spend in front of a screen (phone, laptop, tablet, TV)?`, !!!screen_time_map)) %>%
  select(-`How many hours per day do you spend in front of a screen (phone, laptop, tablet, TV)?`) %>%
  
  # 6. Convert App/Tool Usage to binary (1 for Yes, 0 for No)
  mutate(`App/Tool Usage` = if_else(str_detect(`Do you use any apps/tools to manage screen time or improve focus?`, "Yes"), 1, 0)) %>%
  select(-`Do you use any apps/tools to manage screen time or improve focus?`) %>%
  
  # 7/9. Convert Break Frequency to numerical scale
  mutate(`Break Frequency Scale` = recode(`How often do you take breaks from screens while working/studying?`, !!!break_frequency_map)) %>%
  select(-`How often do you take breaks from screens while working/studying?`) %>%
  
  # 8. Convert Sleep Quality Impact to numerical scale (0 to 3)
  mutate(`Sleep Quality Impact` = recode(`What impact do screens have on your sleep quality?`, !!!sleep_impact_map)) %>%
  select(-`What impact do screens have on your sleep quality?`) %>%
  
  # 11. Convert Likert scale columns to numerical (5 to 1)
  mutate(
    `Screens help me learn new skills. (Likert)` = recode(`How much do you agree with the following statements regarding screen usage? [Screens help me learn new skills.]`, !!!likert_map),
    `I feel addicted to my screen. (Likert)` = recode(`How much do you agree with the following statements regarding screen usage? [I feel addicted to my screen.]`, !!!likert_map),
    `I experience eye strain or headaches from screen use. (Likert)` = recode(`How much do you agree with the following statements regarding screen usage? [I experience eye strain or headaches from screen use.]`, !!!likert_map)
  ) %>%
  select(
    -`How much do you agree with the following statements regarding screen usage? [Screens help me learn new skills.]`,
    -`How much do you agree with the following statements regarding screen usage? [I feel addicted to my screen.]`,
    -`How much do you agree with the following statements regarding screen usage? [I experience eye strain or headaches from screen use.]`
  ) %>%
  
  # 3, 4, 5, 10, and existing 1-5 scales: Rename the remaining columns for clarity
  rename(
    `Primary Screen Usage` = `What type of screen usage occupies most of your time?`,                 # Categorical (3)
    `Most Distracting Activity` = `Which of the following screen activities distracts you the most during work/study?`, # Categorical (4)
    `Most Productive Time` = `At what time of the day do you feel most productive while using screens?`,             # Categorical (5)
    `Balance Strategy` = `In your opinion, what’s one strategy that helps balance screen use and productivity?`,   # Categorical (10)
    `Concentration Impact (1-5)` = `How would you rate the impact of screens on your ability to concentrate?`,   # Numerical (1-5 scale)
    `Importance of Reduction (1-5)` = `On a scale of 1-5, how important is it for you to reduce your daily screen time?` # Numerical (1-5 scale)
  )

# Save the preprocessed data
write_csv(df_preprocessed, "ResponsesDma_preprocessed_R.csv")

# Display the first few rows and the structure of the final dataframe
print(head(df_preprocessed))
print(glimpse(df_preprocessed))