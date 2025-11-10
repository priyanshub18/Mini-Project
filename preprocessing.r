
library(tidyverse)


file_name <- "ResponsesDma.csv"


df <- read_csv(file_name)




screen_time_map <- c(
  "Less than 2 hours" = 1,
  "2–4 hours" = 3,
  "4–6 hours" = 5,
  "6–8 hours" = 7,
  "More than 8 hours" = 9
)


break_frequency_map <- c(
  "Every 30 minutes" = 1,
  "Every hour" = 2,
  "Every 2–3 hours" = 3,
  "Rarely" = 4
)


sleep_impact_map <- c(
  "No effect" = 0,
  "Slightly affects" = 1,
  "Moderately affects" = 2,
  "Significantly affects" = 3
)


likert_map <- c(
  "Strongly Agree" = 5,
  "Agree" = 4,
  "Neutral" = 3,
  "Disagree" = 2,
  "Strongly Disagree" = 1
)




df_preprocessed <- df %>%
  
  select(-Timestamp, -`Email address`) %>%
  
  
  mutate(Score = NA_real_) %>%
  
  
  mutate(`Screen Time (Hours)` = recode(`How many hours per day do you spend in front of a screen (phone, laptop, tablet, TV)?`, !!!screen_time_map)) %>%
  select(-`How many hours per day do you spend in front of a screen (phone, laptop, tablet, TV)?`) %>%
  
  
  mutate(`App/Tool Usage` = if_else(str_detect(`Do you use any apps/tools to manage screen time or improve focus?`, "Yes"), 1, 0)) %>%
  select(-`Do you use any apps/tools to manage screen time or improve focus?`) %>%
  
  
  mutate(`Break Frequency Scale` = recode(`How often do you take breaks from screens while working/studying?`, !!!break_frequency_map)) %>%
  select(-`How often do you take breaks from screens while working/studying?`) %>%
  
  
  mutate(`Sleep Quality Impact` = recode(`What impact do screens have on your sleep quality?`, !!!sleep_impact_map)) %>%
  select(-`What impact do screens have on your sleep quality?`) %>%
  
  
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
  
  
  rename(
    `Primary Screen Usage` = `What type of screen usage occupies most of your time?`,                 
    `Most Distracting Activity` = `Which of the following screen activities distracts you the most during work/study?`, 
    `Most Productive Time` = `At what time of the day do you feel most productive while using screens?`,             
    `Balance Strategy` = `In your opinion, what’s one strategy that helps balance screen use and productivity?`,   
    `Concentration Impact (1-5)` = `How would you rate the impact of screens on your ability to concentrate?`,   
    `Importance of Reduction (1-5)` = `On a scale of 1-5, how important is it for you to reduce your daily screen time?` 
  )


write_csv(df_preprocessed, "ResponsesDma_preprocessed_R.csv")


print(head(df_preprocessed))
print(glimpse(df_preprocessed))