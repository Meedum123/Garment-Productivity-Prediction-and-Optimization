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



############################################################
#               PLSR                                       #
############################################################
set.seed(1) 
pls.fit<-plsr(y~x, data=Hitters,scale=TRUE, 
              validation="CV")
summary(pls.fit)
validationplot(pls.fit ,val.type="RMSEP")

pls.fit2<-plsr(y~x, data=Hitters ,scale=TRUE,ncomp=8)
summary(pls.fit2)
coef(pls.fit2)
