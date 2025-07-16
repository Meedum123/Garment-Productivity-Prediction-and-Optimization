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

categorical_vars <- data[, c("quarter", "department", "day", "team")]  # Adjust column names if necessary
categorical_vars <- data.frame(lapply(categorical_vars, as.factor))

# One-hot encoding for categorical variables
categorical_encoded <- model.matrix(~ . - 1, data = categorical_vars)  # Removing intercept

# Ensure x is a numeric matrix
x <- as.matrix(cbind(numeric_vars[ , !names(numeric_vars) %in% "actual_productivity"], categorical_encoded))
y <- numeric_vars$actual_productivity  # Response variable


#############################################################
#     Principal components Regression(PCR)                  #
#############################################################
library(pls)

set.seed(2)
pcr.fit<-pcr(y~x ,scale=TRUE, validation="CV")
summary(pcr.fit)
validationplot(pcr.fit ,val.type="RMSEP")

pcr.fit2<-pcr(y~x ,scale=TRUE,ncomp=5) 
summary(pcr.fit2)
coef(pcr.fit2)
