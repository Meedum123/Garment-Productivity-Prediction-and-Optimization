# Load necessary libraries
library(glmnet)


# Read data
Train_set <- read.csv("train_cleandata.csv")
Test_set <- read.csv("test_cleandata.csv")

#---------------------pre train-------------------------------------------------
# Define numeric and categorical variables
numeric_vars_Train <- Train_set[, c("targeted_productivity", "smv", "wip", "over_time", "incentive", "idle_time", "idle_men", "no_of_style_change", "no_of_workers", "actual_productivity")]
categorical_vars_Train <- Train_set[, c("quarter", "day", "team")]
categorical_vars_Train <- data.frame(lapply(categorical_vars_Train, as.factor))

# One-hot encoding for categorical variables
categorical_encoded_Train <- model.matrix(~ . - 1, data = categorical_vars_Train)

# Ensure x is a numeric matrix
x_Train <- as.matrix(cbind(numeric_vars_Train[, !names(numeric_vars_Train) %in% "actual_productivity"], categorical_encoded_Train))
y_Train <- numeric_vars_Train$actual_productivity  # Response variable

#---------------------pre test--------------------------------------------------
# Define numeric and categorical variables for Test set
numeric_vars_Test <- Test_set[, c("targeted_productivity", "smv", "wip", "over_time", "incentive", "idle_time", "idle_men", "no_of_style_change", "no_of_workers", "actual_productivity")]
categorical_vars_Test <- Test_set[, c("quarter", "day", "team")]
categorical_vars_Test <- data.frame(lapply(categorical_vars_Test, as.factor))

# One-hot encoding for categorical variables
categorical_encoded_Test <- model.matrix(~ . - 1, data = categorical_vars_Test)

# Ensure x is a numeric matrix
x_Test <- as.matrix(cbind(numeric_vars_Test[, !names(numeric_vars_Test) %in% "actual_productivity"], categorical_encoded_Test))
y_Test <- numeric_vars_Test$actual_productivity  # Response variable

# Separate indices for sewing and finishing departments
sewing_Train <- which(Train_set$department == "sewing")
finishing_Train <- which(Train_set$department == "finishing")
sewing_Test <- which(Test_set$department == "sewing")
finishing_Test <- which(Test_set$department == "finishing")

# Function to perform Elastic Net with Grid Search
elastic_net_tuning <- function(x_train, y_train, x_test, y_test, seed=42) {
  set.seed(seed)
  
  # Define alpha sequence
  alpha_seq <- seq(0, 1, by = 0.1)
  
  # Initialize best parameters
  best_alpha <- NULL
  best_lambda <- NULL
  best_mse <- Inf
  best_model <- NULL
  
  # Loop over alpha values to find the best one
  for (a in alpha_seq) {
    set.seed(seed)
    cv_model <- cv.glmnet(x_train, y_train, alpha = a)
    lambda_min <- cv_model$lambda.min
    mse_min <- min(cv_model$cvm)
    
    if (mse_min < best_mse) {
      best_alpha <- a
      best_lambda <- lambda_min
      best_mse <- mse_min
      best_model <- cv_model
    }
  }
  
  # Final model using best alpha and lambda
  final_model <- glmnet(x_train, y_train, alpha = best_alpha, lambda = best_lambda)
  
  # Predictions and test MSE
  pred_train <- predict(final_model, s = best_lambda, newx = x_train)
  pred_test <- predict(final_model, s = best_lambda, newx = x_test)
  mse_train <- mean((y_train - pred_train)^2)
  mse_test <- mean((y_test - pred_test)^2)
  
  # Compute R-squared
  r2_train <- 1 - (sum((y_train - pred_train)^2) / sum((y_train - mean(y_train))^2))
  r2_test <- 1 - (sum((y_test - pred_test)^2) / sum((y_test - mean(y_test))^2))
  
  # Return results
  list(
    best_alpha = best_alpha,
    best_lambda = best_lambda,
    train_mse = mse_train,
    test_mse = mse_test,
    train_r2 = r2_train,
    test_r2 = r2_test,
    coefficients = coef(final_model, s = best_lambda)
  )
}

# Fit Elastic Net for Sewing
sewing_results <- elastic_net_tuning(x_Train[sewing_Train, ], y_Train[sewing_Train], x_Test[sewing_Test, ], y_Test[sewing_Test])
print(sewing_results)

# Fit Elastic Net for Finishing
finishing_results <- elastic_net_tuning(x_Train[finishing_Train, ], y_Train[finishing_Train], x_Test[finishing_Test, ], y_Test[finishing_Test])
print(finishing_results)

# Create summary table
results_table <- data.frame(
  Department = c("Sewing", "Finishing"),
  Best_Alpha = c(sewing_results$best_alpha, finishing_results$best_alpha),
  Best_Lambda = c(sewing_results$best_lambda, finishing_results$best_lambda),
  Train_MSE = c(sewing_results$train_mse, finishing_results$train_mse),
  Test_MSE = c(sewing_results$test_mse, finishing_results$test_mse),
  Train_R2 = c(sewing_results$train_r2, finishing_results$train_r2),
  Test_R2 = c(sewing_results$test_r2, finishing_results$test_r2)
)

print(results_table)
