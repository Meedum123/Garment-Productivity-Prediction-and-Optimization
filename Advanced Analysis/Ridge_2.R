# Load necessary libraries
library(ISLR)
library(glmnet)
library(pls)
library(leaps)

# Read data
Train_set <- read.csv("train_cleandata.csv")
Test_set <- read.csv("test_cleandata.csv")

#----------Pre Train set---------------------------------------------------
# Define numeric and categorical variables
numeric_vars_Train <- Train_set[, c("targeted_productivity", "smv", "wip",
                                    "over_time", "incentive", "idle_time",
                                    "idle_men", "no_of_style_change",
                                    "no_of_workers", "actual_productivity")]

#numeric_vars_Train <- c("targeted_productivity", "smv", "over_time", "no_of_workers", "actual_productivity")

categorical_vars_Train <- Train_set[, c("quarter", "day", "team")]
categorical_vars_Train <- data.frame(lapply(categorical_vars_Train, as.factor))

# One-hot encoding for categorical variables
categorical_encoded_Train <- model.matrix(~ . - 1, data = categorical_vars_Train)  # Removing intercept

# Ensure x is a numeric matrix
x_Train <- as.matrix(cbind(numeric_vars_Train[ , !names(numeric_vars_Train) %in% "actual_productivity"], categorical_encoded_Train))
y_Train <- numeric_vars_Train$actual_productivity  # Response variable

# Separate indices for sewing and finishing departments
sewing_Train = which(Train_set$department == "sewing")
finishing_Train = which(Train_set$department == "finishing")

set.seed(123)
#-------------Pre test set------------------------------------------------------
numeric_vars_Test <- Test_set[, c("targeted_productivity", "smv", "wip",
                                  "over_time", "incentive", "idle_time",
                                  "idle_men", "no_of_style_change",
                                  "no_of_workers", "actual_productivity")]
numeric_vars_Train <- c("targeted_productivity", "smv", "over_time", "no_of_workers")

categorical_vars_Test <- Test_set[, c("quarter", "day", "team")]
categorical_vars_Test <- data.frame(lapply(categorical_vars_Test, as.factor))

# One-hot encoding for categorical variables
categorical_encoded_Test <- model.matrix(~ . - 1, data = categorical_vars_Test)  # Removing intercept

# Ensure x is a numeric matrix
x_Test <- as.matrix(cbind(numeric_vars_Test[ , !names(numeric_vars_Test) %in% "actual_productivity"], categorical_encoded_Test))
y_Test <- numeric_vars_Test$actual_productivity  # Response variable

# Separate indices for sewing and finishing departments
sewing_Test = which(Test_set$department == "sewing")
finishing_Test = which(Test_set$department == "finishing")

# Function to calculate R-squared
calc_r2 <- function(y_true, y_pred) {
  ss_total <- sum((y_true - mean(y_true))^2)
  ss_residual <- sum((y_true - y_pred)^2)
  return(1 - (ss_residual / ss_total))
}

#------------------ Fit Ridge Regression for Sewing -----------------------------
x_s = x_Train[sewing_Train, ]
y_s = y_Train[sewing_Train]

fit.ridge.s <- glmnet(x_s, y_s, alpha = 0)
cv.ridge.s <- cv.glmnet(x_s, y_s, alpha = 0,
                        type.measure = "mse", nfolds = 10)

bestlam.s <- cv.ridge.s$lambda.min 

# Predictions and accuracy check
pred_s_train = predict(fit.ridge.s, s = bestlam.s, newx = x_s)
mse_s_train = mean((y_s - pred_s_train)^2)
r2_s_train = calc_r2(y_s, pred_s_train)

pred_s_test = predict(fit.ridge.s, s = bestlam.s, newx = x_Test[sewing_Test, ])
mse_s_test = mean((y_Test[sewing_Test] - pred_s_test)^2)
r2_s_test = calc_r2(y_Test[sewing_Test], pred_s_test)

#------------------ Fit Ridge Regression for Finishing -----------------------------
x_f = x_Train[finishing_Train, ]
y_f = y_Train[finishing_Train]

fit.ridge.f <- glmnet(x_f, y_f, alpha = 0)
cv.ridge.f <- cv.glmnet(x_f, y_f, alpha = 0,
                        type.measure = "mse", nfolds = 10)
bestlam.f <- cv.ridge.f$lambda.min 

# Predictions and accuracy check
pred_f_train = predict(fit.ridge.f, s = bestlam.f, newx = x_f)
mse_f_train = mean((y_f - pred_f_train)^2)
r2_f_train = calc_r2(y_f, pred_f_train)

pred_f_test = predict(fit.ridge.f, s = bestlam.f, newx = x_Test[finishing_Test, ])
mse_f_test = mean((y_Test[finishing_Test] - pred_f_test)^2)
r2_f_test = calc_r2(y_Test[finishing_Test], pred_f_test)

# Summary table
results <- data.frame(
  Category = c("Sewing", "Finishing"),
  Best_Lambda = c(bestlam.s, bestlam.f),
  Train_MSE = c(mse_s_train, mse_f_train),
  Test_MSE = c(mse_s_test, mse_f_test),
  Train_R2 = c(r2_s_train, r2_f_train),
  Test_R2 = c(r2_s_test, r2_f_test)
)
print(results)
