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


# Initialize a list to store results
results <- list()

set.seed(123)
# Process each department
for (dept in c("sewing", "finishing")) {
  # Get indices
  if (dept == "sewing") {
    train_idx <- sewing_Train
    test_idx <- sewing_Test
  } else {
    train_idx <- finishing_Train
    test_idx <- finishing_Test
  }
  
  # Subset the data
  x_train <- x_Train[train_idx, ]
  y_train <- y_Train[train_idx]
  x_test <- x_Test[test_idx, ]
  y_test <- y_Test[test_idx]
  
  # Define alpha grid
  alphas <- seq(0, 1, by = 0.1)
  
  # Initialize best parameters
  best_alpha <- 0
  best_lambda <- NULL
  best_cvm <- Inf
  
  # Tune alpha and lambda using cross-validation
  for (alpha in alphas) {
    cv_model <- cv.glmnet(x_train, y_train, alpha = alpha, 
                          type.measure = "mse", nfolds = 10)
    min_cvm <- min(cv_model$cvm)
    
    if (min_cvm < best_cvm) {
      best_cvm <- min_cvm
      best_alpha <- alpha
      best_lambda <- cv_model$lambda.min
    }
  }
  
  # Train final model with best parameters
  final_model <- glmnet(x_train, y_train, alpha = best_alpha, 
                        lambda = best_lambda)
  
  # Calculate predictions and metrics
  train_pred <- predict(final_model, newx = x_train)
  test_pred <- predict(final_model, newx = x_test)
  
  train_mse <- mean((y_train - train_pred)^2)
  train_r2 <- 1 - sum((y_train - train_pred)^2)/sum((y_train - mean(y_train))^2)
  
  test_mse <- mean((y_test - test_pred)^2)
  test_r2 <- 1 - sum((y_test - test_pred)^2)/sum((y_test - mean(y_test))^2)
  
  # Store results
  results[[dept]] <- data.frame(
    Department = dept,
    Best_Alpha = best_alpha,
    Best_Lambda = best_lambda,
    Train_MSE = train_mse,
    Test_MSE = test_mse,
    Train_R2 = train_r2,
    Test_R2 = test_r2
  )
}

# Combine results into a table
result_table <- do.call(rbind, results)
rownames(result_table) <- NULL

# Print formatted table
print(knitr::kable(result_table, digits = 6))