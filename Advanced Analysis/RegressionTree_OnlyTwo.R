# Load necessary libraries
library(rpart)
library(caret)
library(dplyr) 
library(rpart.plot)  
library(ggplot2)

set.seed(123)

# Read data
Train_set <- read.csv("train_cleandata.csv")
Test_set <- read.csv("test_cleandata.csv")

#---------- Data Preparation -----------------------------------------------
# Convert categorical variables to factors directly
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
predictors <- c("targeted_productivity", "incentive")

# Function to calculate R-squared
calc_r2 <- function(y_true, y_pred) {
  1 - sum((y_true - y_pred)^2) / sum((y_true - mean(y_true))^2)
}

#------------------ Model Training & Evaluation ----------------------------
train_evaluate_tree <- function(train_data, test_data, department_name) {
  # Train full tree
  tree_model <- rpart(
    actual_productivity ~ .,
    data = train_data[, c(predictors, "actual_productivity")],
    method = "anova",
    control = rpart.control(minsplit = 10, cp = 0.001)
  )
  
  # Save initial tree plot
  png(paste0(department_name, "_initial_tree.png"))
  rpart.plot(tree_model, main = paste(department_name, " - Initial Decision Tree"))
  dev.off()
  
  # Find optimal CP using 1-SE rule
  cp_table <- as.data.frame(tree_model$cptable)
  best_cp <- cp_table %>% 
    filter(xerror <= min(xerror) + xstd[which.min(xerror)]) %>% 
    pull(CP) %>% 
    max()
  
  # Prune tree
  pruned_tree <- prune(tree_model, cp = best_cp)
  
  # Save pruned tree plot
  pdf(paste0(department_name, "_pruned_tree.pdf"), width = 12, height = 8)
  rpart.plot(pruned_tree, main = paste(department_name, " - Pruned Decision Tree"), cex = 1)
  dev.off()
  
  # Generate feature importance graph
  if (!is.null(pruned_tree$variable.importance)) {  # Check if importance exists
    importance <- data.frame(Feature = names(pruned_tree$variable.importance),
                             Importance = pruned_tree$variable.importance)
    
    # Create the feature importance plot
    feature_plot <- ggplot(importance, aes(x = reorder(Feature, Importance), y = Importance)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      theme_minimal() +
      labs(title = paste(department_name, " - Feature Importance Regression Tree"), x = "Features", y = "Importance")
    
    # Save the plot
    ggsave(paste0(department_name, "_feature_importance_(incentive_tp).png"), plot = feature_plot)
  } else {
    message(paste("No feature importance found for", department_name))
  }
  
  # Generate predictions
  train_pred <- predict(pruned_tree, newdata = train_data)
  test_pred <- predict(pruned_tree, newdata = test_data)
  
  # Calculate metrics
  metrics <- list(
    best_cp = best_cp,
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

# Train and evaluate models for both departments
set.seed(123)
sewing_metrics <- train_evaluate_tree(sewing_train, sewing_test, "Sewing")
finishing_metrics <- train_evaluate_tree(finishing_train, finishing_test, "Finishing")

# Create results table
results <- data.frame(
  Department = c("Sewing", "Finishing"),
  Best_CP = c(sewing_metrics$best_cp, finishing_metrics$best_cp),
  Train_MSE = c(sewing_metrics$train_mse, finishing_metrics$train_mse),
  Test_MSE = c(sewing_metrics$test_mse, finishing_metrics$test_mse),
  Train_R2 = c(sewing_metrics$train_r2, finishing_metrics$train_r2),
  Test_R2 = c(sewing_metrics$test_r2, finishing_metrics$test_r2)
)

print(results)
