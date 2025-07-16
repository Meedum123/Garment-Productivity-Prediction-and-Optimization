# Load necessary libraries
library(ISLR)
library(glmnet)
library(pls)
library(leaps)

# Read data
data <- read.csv("train_cleandata.csv")
nrow(data)

# Define numeric and categorical variables
numeric_vars <- data[, c("targeted_productivity", "smv", "wip",
                         "over_time", "incentive", "idle_time",
                         "idle_men", "no_of_style_change",
                         "no_of_workers", "actual_productivity")]

categorical_vars <- data[, c("quarter", "department", "day", "team")]
categorical_vars <- data.frame(lapply(categorical_vars, as.factor))

# One-hot encoding for categorical variables
categorical_encoded <- model.matrix(~ . - 1, data = categorical_vars)  # Removing intercept

# Ensure x is a numeric matrix
x <- as.matrix(cbind(numeric_vars[ , !names(numeric_vars) %in% "actual_productivity"], categorical_encoded))
y <- numeric_vars$actual_productivity  # Response variable

# Separate indices for sewing and finishing departments
sewing = which(data$department == "sewing")
finishing = which(data$department == "finishing")

#------------------ Fit Ridge Regression for Sewing -----------------------------
x_s = x[sewing, ]
y_s = y[sewing]

set.seed(42)
train_s = sample(1:length(y_s), size = floor(0.7 * length(y_s)))  # 70% for training
test_s = setdiff(1:length(y_s), train_s)

fit.ridge.s <- glmnet(x_s[train_s, ], y_s[train_s], alpha = 0)
plot(fit.ridge.s, xvar = "lambda", label = TRUE, lw = 2)

# Cross-validation to find best lambda
cv.ridge.s <- cv.glmnet(x_s[train_s, ], y_s[train_s], alpha = 0)
plot(cv.ridge.s)    
bestlam.s <- cv.ridge.s$lambda.min 
print(bestlam.s)

# Get coefficients for best lambda
coef(fit.ridge.s, s = bestlam.s)

# Accuracy check using test set
pred_s = predict(fit.ridge.s, s = bestlam.s, newx = x_s[test_s, ])
mse_s = mean((y_s[test_s] - pred_s)^2)
print(paste("MSE for sewing:", mse_s))

#------------------ Fit Ridge Regression for Finishing -----------------------------
x_f = x[finishing, ]
y_f = y[finishing]

set.seed(42)
train_f = sample(1:length(y_f), size = floor(0.7 * length(y_f)))  # 70% for training
test_f = setdiff(1:length(y_f), train_f)

fit.ridge.f <- glmnet(x_f[train_f, ], y_f[train_f], alpha = 0)
plot(fit.ridge.f, xvar = "lambda", label = TRUE, lw = 2)

# Cross-validation to find best lambda
cv.ridge.f <- cv.glmnet(x_f[train_f, ], y_f[train_f], alpha = 0)
plot(cv.ridge.f)    
bestlam.f <- cv.ridge.f$lambda.min 
print(bestlam.f)

# Get coefficients for best lambda
coef(fit.ridge.f, s = bestlam.f)

# Accuracy check using test set
pred_f = predict(fit.ridge.f, s = bestlam.f, newx = x_f[test_f, ])
mse_f = mean((y_f[test_f] - pred_f)^2)
print(paste("MSE for finishing:", mse_f))
