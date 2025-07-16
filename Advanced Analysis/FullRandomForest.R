# Read CSV files
df_perf <- read.csv("PerformanceRating.csv", stringsAsFactors = FALSE)
library(skimr)
skim(df_perf)
df_emp <- read.csv("Employee.csv", stringsAsFactors = FALSE)
head(df_emp)
nrow(df_emp)

# EducationField Marketing 
df_emp[df_emp$EducationField=="Marketing ","EducationField"]="Marketing"

#------------------------- feature engineering
# JobRole---------
df_emp$JobRole_Grouped <- df_emp$JobRole  # Create a new column

# Group roles using domain knowledge
df_emp$JobRole_Grouped[df_emp$JobRole %in% c("Software Engineer", "Senior Software Engineer")] <- "Software Engineer"
df_emp$JobRole_Grouped[df_emp$JobRole %in% c("Machine Learning Engineer", "Data Scientist")] <- "AI/ML"
df_emp$JobRole_Grouped[df_emp$JobRole %in% c("Engineering Manager", "Analytics Manager", "Manager")] <- "Management"
df_emp$JobRole_Grouped[df_emp$JobRole %in% c("HR Manager", "HR Executive", "HR Business Partner", "Recruiter")] <- "HR"
df_emp$JobRole_Grouped[df_emp$JobRole %in% c("Sales Executive", "Sales Representative")] <- "Sales"

# Verify results
table(df_emp$JobRole, df_emp$JobRole_Grouped)

# EducationField---------
table(df_emp$EducationField)
df_emp$EducationField_Grouped <- df_emp$EducationField  # Create a new column

# Group fields based on domain knowledge
df_emp$EducationField_Grouped[df_emp$EducationField %in% c("Computer Science", "Information Systems", "Technical Degree")] <- "STEM"
df_emp$EducationField_Grouped[df_emp$EducationField %in% c("Economics", "Business Studies", "Marketing")] <- "Business"
df_emp$EducationField_Grouped[df_emp$EducationField %in% c("Human Resources")] <- "HR"
df_emp$EducationField_Grouped[df_emp$EducationField %in% c("Other")] <- "Other"

# Verify results
table(df_emp$EducationField, df_emp$EducationField_Grouped)

# Ethnicity-----------
table(df_emp$Ethnicity)
df_emp$Ethnicity_Grouped <- df_emp$Ethnicity  # Create a new column

# Group ethnicities based on domain knowledge
df_emp$Ethnicity_Grouped[df_emp$Ethnicity %in% c("Asian or Asian American")] <- "Asian"
df_emp$Ethnicity_Grouped[df_emp$Ethnicity %in% c("Black or African American")] <- "Black"
df_emp$Ethnicity_Grouped[df_emp$Ethnicity %in% c("White")] <- "White"
#df_emp$Ethnicity_Grouped[df_emp$Ethnicity %in% c("American Indian or Alaska Native", "Native Hawaiian ")] <- "Indigenous"
df_emp$Ethnicity_Grouped[df_emp$Ethnicity %in% c("American Indian or Alaska Native", "Native Hawaiian ", "Mixed or multiple ethnic groups", "Other ")] <- "Mixed/Other"

# Verify the new grouping
table(df_emp$Ethnicity, df_emp$Ethnicity_Grouped)

# Gender----------------
df_emp$Gender_Grouped <- df_emp$Gender  # Create a new column

# Merge Non-Binary & Prefer Not To Say into "Other"
df_emp$Gender_Grouped[df_emp$Gender %in% c("Non-Binary", "Prefer Not To Say")] <- "Other"

# Verify new category distribution
table(df_emp$Gender, df_emp$Gender_Grouped)

#-----------------------
correlation_matrix <- cor(df_emp[, c("YearsAtCompany", "YearsInMostRecentRole", "YearsSinceLastPromotion", "YearsWithCurrManager")], use = "complete.obs")
print(correlation_matrix)

pca_result <- prcomp(df_emp[, c("YearsAtCompany", "YearsInMostRecentRole", "YearsSinceLastPromotion", "YearsWithCurrManager")], scale = TRUE)
#df_emp$Tenure_PC1 <- pca_result$x[,1]  # First principal component


#-----------------------
# Convert ReviewDate to Date format
df_perf$ReviewDate <- as.Date(df_perf$ReviewDate, format="%m/%d/%Y")

# Order the dataset by EmployeeID and ReviewDate (latest first)
df_perf <- df_perf[order(df_perf$EmployeeID, -as.numeric(df_perf$ReviewDate)), ]
head(df_perf)
nrow(df_perf)

# Keep only the latest performance record for each EmployeeID
latest_perf <- df_perf[!duplicated(df_perf$EmployeeID), ]
head(latest_perf)
nrow(latest_perf)

# Perform the merge (inner join)
merged_df <- merge(df_emp, latest_perf, by = "EmployeeID", all = FALSE)

# View result
skim(merged_df)
print(dim(merged_df))  # Check dimensions
head(merged_df)  # Show first few rows
nrow(merged_df)
ncol(merged_df)
#write.csv(merged_df, "merged_df.csv")

# Convert nominal variables to factor
nominal_vars <- c("EmployeeID", "FirstName", "LastName", "Gender", "BusinessTravel",
                  "Department", "State", "Ethnicity", "EducationField", "JobRole",
                  "MaritalStatus", "PerformanceID", "Attrition", "OverTime",
                  "EducationField_Grouped", "Ethnicity_Grouped", "Gender_Grouped",
                  "JobRole_Grouped")
merged_df[nominal_vars] <- lapply(merged_df[nominal_vars], factor)

# Convert ordinal variables to ordered factors
ordinal_vars <- c("Education", "StockOptionLevel", "EnvironmentSatisfaction",
                  "JobSatisfaction", "RelationshipSatisfaction", "WorkLifeBalance",
                  "SelfRating", "ManagerRating")
merged_df[ordinal_vars] <- lapply(merged_df[ordinal_vars], function(x) {
  ordered(x, levels = sort(unique(x)))
})

# Convert HireDate to Date type (optional)
merged_df$HireDate <- as.Date(merged_df$HireDate)

# Ensure ratio variables are numeric
ratio_vars <- c("Age", "DistanceFromHome..KM.", "Salary", "YearsAtCompany",
                "YearsInMostRecentRole", "YearsSinceLastPromotion",
                "YearsWithCurrManager","TrainingOpportunitiesWithinYear",
                "TrainingOpportunitiesTaken")
merged_df[ratio_vars] <- lapply(merged_df[ratio_vars], as.numeric)


# Verify structure
str(merged_df)
ncol(merged_df)

nominal_vars_keep <- c("BusinessTravel", "Department", "State",
                       "MaritalStatus", "Attrition", "OverTime",
                       "EducationField_Grouped", "Ethnicity_Grouped",
                       "Gender_Grouped", "JobRole_Grouped")
ordinal_vars_keep <- c("Education", "StockOptionLevel", "EnvironmentSatisfaction",
                       "JobSatisfaction", "RelationshipSatisfaction", "WorkLifeBalance",
                       "ManagerRating")
#.self ratning removed
ratio_vars_keep <- c("Age", "DistanceFromHome..KM.", "Salary",
                     "YearsInMostRecentRole", "YearsSinceLastPromotion",
                     "TrainingOpportunitiesWithinYear","TrainingOpportunitiesTaken")
# YearsAtCompany, YearsWithCurrManager removed
vars_keep <- c(nominal_vars_keep, ordinal_vars_keep, ratio_vars_keep)

df <- merged_df[, vars_keep]
head(df)
str(df)

#----------------- Random forest----------------------------------------------------------------
# Load necessary libraries
library(randomForest)
library(caret)
library(ROSE)
library(pROC)  # For AUC calculation

# Set seed for reproducibility
set.seed(123)

# Ensure the target variable is a factor
df$Attrition <- as.factor(df$Attrition)

# Split data into training (80%) and testing (20%)
train_index <- createDataPartition(df$Attrition, p = 0.8, list = FALSE)
train_data <- df[train_index, ]
test_data <- df[-train_index, ]
# Set seed for reproducibility
set.seed(123)

# 2. Define control with SMOTE and cross-validation
ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  sampling = "smote",
  savePredictions = "final"
)

# 3. Expanded tuning grid for deeper parameter search
tune_grid <- expand.grid(
  mtry = c(2, 4, 6, 8, 10, 12),          # Features per split
  splitrule = "gini",            # Splitting criterion
  min.node.size = c(1, 5, 10, 20)  # Controls tree depth (critical for overfitting)
)

# 4. Train model with extended parameters
rf_model <- train(
  Attrition ~ .,
  data = train_data,
  method = "ranger",
  metric = "ROC",
  tuneGrid = tune_grid,
  trControl = ctrl,
  num.trees = 1000,             # Increased number of trees
  importance = "permutation"    # More reliable importance
)

# 5. Overfitting check: Compare train vs test performance
train_pred <- predict(rf_model, train_data, type = "prob")
test_pred <- predict(rf_model, test_data, type = "prob")

train_roc <- roc(train_data$Attrition, train_pred$Yes)
test_roc <- roc(test_data$Attrition, test_pred$Yes)

cat("Training AUC:", auc(train_roc), "\n")
cat(" Test AUC:", auc(test_roc), "\n")

# 6. Visual overfitting check
performance_df <- data.frame(
  Dataset = rep(c("Train", "Test"), each = 100),
  Sensitivity = c(train_roc$sensitivities, test_roc$sensitivities),
  Specificity = c(train_roc$specificities, test_roc$specificities)
)

ggplot(performance_df, aes(x = 1 - Specificity, y = Sensitivity, color = Dataset)) +
  geom_line(linewidth = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  ggtitle("ROC Curve Comparison") +
  theme_minimal()

# 7. Final model evaluation
print(rf_model)
plot(rf_model)  # Shows parameter tuning performance

# 8. Variable importance check
var_imp <- varImp(rf_model)
plot(var_imp, top = 15)