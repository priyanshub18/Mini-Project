# --- 1. Load Libraries ---
library(tidyverse)
library(rpart)    # Base Decision Tree package
library(rpart.plot) 
# Note: We are no longer using 'caret', which caused the stability issues.

# Load the preprocessed CSV file (Corrected file name)
file_name <- "ResponsesDma_preprocessed_R.csv"
df <- read_csv(file_name)

# --- 2. Define Target Variable and Select/Clean Features ---

# Target Definition: Productive (1) if Concentration Impact >= 4, else Not Productive (0)
df <- df %>%
  mutate(Is_Productive = if_else(`Concentration Impact (1-5)` >= 4, 1, 0)) %>%
  mutate(Is_Productive = factor(Is_Productive, levels = c(0, 1), labels = c("Not_Productive", "Productive")))

# Simplified Feature Selection
df_model <- df %>%
  select(
    # Features
    `Screen Time (Hours)`,
    `App/Tool Usage`,
    `Break Frequency Scale`,
    `I feel addicted to my screen. (Likert)`,
    `I experience eye strain or headaches from screen use. (Likert)`,
    `Most Distracting Activity`,
    `Most Productive Time`,
    
    # Target
    Is_Productive
  )

# Convert character columns to factors
df_model <- df_model %>%
  mutate_if(is.character, as.factor)

# Impute Numerical/Ordinal NAs with Median
numerical_to_impute <- c("Break Frequency Scale", "I feel addicted to my screen. (Likert)", 
                         "I experience eye strain or headaches from screen use. (Likert)")

df_model <- df_model %>%
  mutate_at(vars(one_of(numerical_to_impute)), ~replace_na(., median(., na.rm = TRUE)))

# **CRITICAL FIX**: Remove any row that still contains NA (Safest approach for rpart)
df_processed <- na.omit(df_model)


# --- 3. Split Data into Training and Testing Sets ---
set.seed(42) # for reproducibility
train_index <- sample(nrow(df_processed), size = 0.7 * nrow(df_processed))
train_data <- df_processed[train_index, ]
test_data  <- df_processed[-train_index, ]

# --- 4. Train the Decision Tree Model (Base rpart) ---

dt_model_base <- rpart(
  Is_Productive ~ ., 
  data = train_data, 
  method = "class", 
  control = rpart.control(cp = 0.01) # Set a basic complexity parameter
)

# --- 5. Prediction and Manual Metric Calculation (Guaranteed Output) ---

predictions <- predict(dt_model_base, newdata = test_data, type = "class")
actual <- test_data$Is_Productive

# Create the Confusion Matrix
conf_matrix_table <- table(Predicted = predictions, Actual = actual)

# Calculate Metrics Manually
TP <- conf_matrix_table["Productive", "Productive"]
TN <- conf_matrix_table["Not_Productive", "Not_Productive"]
FP <- conf_matrix_table["Productive", "Not_Productive"]
FN <- conf_matrix_table["Not_Productive", "Productive"]

Accuracy <- (TP + TN) / sum(conf_matrix_table)
Precision_Prod <- TP / (TP + FP)
Recall_Prod <- TP / (TP + FN)

cat("\n============================================\n")
cat("  DECISION TREE RESULTS (BASE RPARD METHOD)\n")
cat("============================================\n")

cat("Confusion Matrix:\n")
print(conf_matrix_table)

cat("\n\nAccuracy (Overall Correct Predictions):", round(Accuracy, 4), "\n")
cat("Precision (Positive Predictive Value for 'Productive'):", round(Precision_Prod, 4), "\n")
cat("Recall/Sensitivity (True Positive Rate for 'Productive'):", round(Recall_Prod, 4), "\n")


# --- 6. Plot the Decision Tree ---
rpart.plot(dt_model_base,
           type = 2,
           extra = 104, # show count and percentage
           branch.lty = 3,
           shadow.col = "gray",
           main = "Decision Tree for Predicting Productivity (Base Rpart)",
           cex = 0.8)