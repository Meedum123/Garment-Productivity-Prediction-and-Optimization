# Load necessary libraries
library(randomForest)
library(caret)
library(dplyr)
library(ggplot2)

set.seed(123)

# Read data
Train_set <- read.csv("train_cleandata.csv")
Test_set <- read.csv("test_cleandata.csv")

#---------- Data Preparation -----------------------------------------------
# Convert categorical variables to factors
convert_to_factors <- function(df) {
  df$quarter <- as.factor(df$quarter)
  df$day <- as.factor(df$day)
  df$team <- as.factor(df$team)
  df$department <- as.factor(df$department)
  return(df)
}

Train_set <- convert_to_factors(Train_set)
Test_set <- convert_to_factors(Test_set)

# Define common predictor names (excluding actual_productivity)
predictors <- c("targeted_productivity", "smv", "wip", "over_time",
                "incentive", "idle_time", "idle_men", "no_of_style_change",
                "no_of_workers", "quarter", "day", "team")

# Function to calculate R-squared
calc_r2 <- function(y_true, y_pred) {
  1 - sum((y_true - y_pred)^2) / sum((y_true - mean(y_true))^2)
}

#------------------ Model Training & Evaluation ----------------------------
train_evaluate_rf <- function(train_data, test_data, department_name) {
  
  # Automatically tune `mtry`
  best_mtry <- tuneRF(
    x = train_data[, predictors], 
    y = train_data$actual_productivity, 
    stepFactor = 1.5,  # Increase `mtry` by this factor
    improve = 0.01,  # Stop if no improvement by at least 1%
    ntreeTry = 100,  # Number of trees for each test
    trace = TRUE  # Show progress
  )
  
  optimal_mtry <- best_mtry[which.min(best_mtry[, 2]), 1]  # Select best `mtry`
  
  # Train Random Forest model
  rf_model <- randomForest(
    actual_productivity ~ ., 
    data = train_data[, c(predictors, "actual_productivity")],
    ntree = 500,  # Number of trees
    mtry = optimal_mtry,  # Best `mtry`
    importance = TRUE
  )
  
  # Feature importance
  importance_data <- data.frame(Feature = rownames(importance(rf_model)), 
                                Importance = importance(rf_model)[, 1])
  
  feature_plot <- ggplot(importance_data, aes(x = reorder(Feature, Importance), y = Importance)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    theme_minimal() +
    labs(title = paste(department_name, "- Feature Importance Random Forest"), 
         x = "Features", y = "Importance")
  ggsave(paste0(department_name, "_feature_importance_Rand_F.png"), plot = feature_plot)
  
  # Generate predictions
  train_pred <- predict(rf_model, newdata = train_data)
  test_pred <- predict(rf_model, newdata = test_data)
  
  # Calculate metrics
  metrics <- list(
    best_mtry = optimal_mtry,
    train_mse = mean((train_data$actual_productivity - train_pred)^2),
    test_mse = mean((test_data$actual_productivity - test_pred)^2),
    train_r2 = calc_r2(train_data$actual_productivity, train_pred),
    test_r2 = calc_r2(test_data$actual_productivity, test_pred)
  )
  
  return(metrics)
}

#------------------ Department-specific Analysis ---------------------------
# Prepare department-specific data
sewing_train <- Train_set[Train_set$department == "sewing", ]
sewing_test <- Test_set[Test_set$department == "sewing", ]

finishing_train <- Train_set[Train_set$department == "finishing", ]
finishing_test <- Test_set[Test_set$department == "finishing", ]

# Train and evaluate models
set.seed(123)
sewing_metrics <- train_evaluate_rf(sewing_train, sewing_test, "Sewing")
finishing_metrics <- train_evaluate_rf(finishing_train, finishing_test, "Finishing")

# Create results table
results <- data.frame(
  Department = c("Sewing", "Finishing"),
  Best_Mtry = c(sewing_metrics$best_mtry, finishing_metrics$best_mtry),
  Train_MSE = c(sewing_metrics$train_mse, finishing_metrics$train_mse),
  Test_MSE = c(sewing_metrics$test_mse, finishing_metrics$test_mse),
  Train_R2 = c(sewing_metrics$train_r2, finishing_metrics$train_r2),
  Test_R2 = c(sewing_metrics$test_r2, finishing_metrics$test_r2)
)

print(results)



# Train and evaluate model on the full dataset
set.seed(123)
overall_metrics <- train_evaluate_rf(Train_set, Test_set, "Overall")

# Create a results table for the entire dataset
results <- data.frame(
  Department = "Overall",
  Best_Mtry = overall_metrics$best_mtry,
  Train_MSE = overall_metrics$train_mse,
  Test_MSE = overall_metrics$test_mse,
  Train_R2 = overall_metrics$train_r2,
  Test_R2 = overall_metrics$test_r2
)

# Print results
print(results)

