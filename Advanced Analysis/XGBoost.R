# Load necessary libraries
library(xgboost)
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

# Define predictors (excluding target)
predictors <- c("targeted_productivity", "smv", "wip", "over_time",
                "incentive", "idle_time", "idle_men", "no_of_style_change",
                "no_of_workers", "quarter", "day", "team")

# Function to calculate R²
calc_r2 <- function(y_true, y_pred) {
  1 - sum((y_true - y_pred)^2) / sum((y_true - mean(y_true))^2)
}

#------------------ Model Training & Evaluation ----------------------------
train_evaluate_xgb <- function(train_data, test_data, department_name) {
  
  # Convert categorical variables to one-hot encoding
  train_matrix <- model.matrix(~ . -1, data = train_data[, predictors])
  test_matrix <- model.matrix(~ . -1, data = test_data[, predictors])
  
  # Convert to xgb.DMatrix
  dtrain <- xgb.DMatrix(data = train_matrix, label = train_data$actual_productivity)
  dtest <- xgb.DMatrix(data = test_matrix, label = test_data$actual_productivity)
  
  # Define tuning grid with required parameters
  xgb_grid <- expand.grid(
    eta = c(0.01, 0.05, 0.1),  # Learning rate
    max_depth = c(3, 6, 9),  # Tree depth
    nrounds = seq(50, 200, 50),  # Number of boosting iterations
    gamma = c(0, 0.1, 0.2),  # Minimum loss reduction
    colsample_bytree = c(0.6, 0.8, 1),  # Feature subsampling
    subsample = c(0.7, 0.9, 1),  # Row subsampling
    min_child_weight = c(1, 3, 5)  # Minimum child weight
  )
  
  
  # Define training control
  train_control <- trainControl(method = "cv", number = 5, verboseIter = TRUE)
  
  # Train XGBoost model using caret
  xgb_model <- train(
    x = train_matrix, y = train_data$actual_productivity,
    method = "xgbTree",
    trControl = train_control,
    tuneGrid = xgb_grid
  )
  
  # Best parameters
  best_params <- xgb_model$bestTune
  
  # Train final XGBoost model with best parameters
  final_model <- xgboost(
    data = dtrain, nrounds = best_params$nrounds,
    eta = best_params$eta, max_depth = best_params$max_depth,
    gamma = best_params$gamma, colsample_bytree = best_params$colsample_bytree,
    subsample = best_params$subsample, objective = "reg:squarederror",
    verbose = 0
  )
  
  # Feature importance
  importance_matrix <- xgb.importance(feature_names = colnames(train_matrix), model = final_model)
  feature_plot <- ggplot(importance_matrix, aes(x = reorder(Feature, Gain), y = Gain)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    theme_minimal() +
    labs(title = paste(department_name, "- Feature Importance XGBoost"), x = "Features", y = "Gain")
  
  ggsave(paste0(department_name, "_feature_importance_XGB.png"), plot = feature_plot)
  
  # Predictions
  train_pred <- predict(final_model, dtrain)
  test_pred <- predict(final_model, dtest)
  
  # Calculate metrics
  metrics <- list(
    best_params = best_params,
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

# Train and evaluate models per department
set.seed(123)
sewing_metrics <- train_evaluate_xgb(sewing_train, sewing_test, "Sewing")
finishing_metrics <- train_evaluate_xgb(finishing_train, finishing_test, "Finishing")

# Train and evaluate model on full dataset
set.seed(123)
overall_metrics <- train_evaluate_xgb(Train_set, Test_set, "Overall")

# Create results table
results <- data.frame(
  Department = c("Sewing", "Finishing", "Overall"),
  Best_Eta = c(sewing_metrics$best_params$eta, finishing_metrics$best_params$eta, overall_metrics$best_params$eta),
  Best_Depth = c(sewing_metrics$best_params$max_depth, finishing_metrics$best_params$max_depth, overall_metrics$best_params$max_depth),
  Best_Nrounds = c(sewing_metrics$best_params$nrounds, finishing_metrics$best_params$nrounds, overall_metrics$best_params$nrounds),
  Train_MSE = c(sewing_metrics$train_mse, finishing_metrics$train_mse, overall_metrics$train_mse),
  Test_MSE = c(sewing_metrics$test_mse, finishing_metrics$test_mse, overall_metrics$test_mse),
  Train_R2 = c(sewing_metrics$train_r2, finishing_metrics$train_r2, overall_metrics$train_r2),
  Test_R2 = c(sewing_metrics$test_r2, finishing_metrics$test_r2, overall_metrics$test_r2)
)

# Print results
print(results)
