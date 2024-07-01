# Load required libraries
library(tidyverse)
library(caret)
library(pROC)
library(rpart)
library(rpart.plot)

# Load the dataset
file_path <- 'C:/Users/anjel/Downloads/pima-diabetes.csv'
data <- read.csv(file_path)

# Display the first few rows of the dataset
print(head(data))

# Check for missing values
print(colSums(is.na(data)))

# Split the data into features and target
X <- data %>% select(-Outcome)
y <- data$Outcome

# Split the data into training and test sets
set.seed(42)
train_index <- createDataPartition(y, p = 0.7, list = FALSE)
X_train <- X[train_index, ]
X_test <- X[-train_index, ]
y_train <- y[train_index]
y_test <- y[-train_index]

# Standardize the features
preprocess_params <- preProcess(X_train, method = c("center", "scale"))
X_train <- predict(preprocess_params, X_train)
X_test <- predict(preprocess_params, X_test)

# Logistic Regression
log_reg <- glm(y_train ~ ., data = as.data.frame(X_train), family = binomial)

# Predictions
y_pred_log_reg <- predict(log_reg, newdata = as.data.frame(X_test), type = "response")
y_pred_log_reg_class <- ifelse(y_pred_log_reg > 0.5, 1, 0)

# Confusion Matrix for Logistic Regression
conf_matrix_log_reg <- confusionMatrix(factor(y_pred_log_reg_class), factor(y_test))
print('Confusion Matrix for Logistic Regression:')
print(conf_matrix_log_reg$table)

# ROC Curve for Logistic Regression
roc_log_reg <- roc(y_test, y_pred_log_reg)
auc_log_reg <- auc(roc_log_reg)
print(paste('ROC AUC for Logistic Regression:', round(auc_log_reg, 2)))

# Plot ROC Curve for Logistic Regression
plot(roc_log_reg, main = "ROC Curve - Logistic Regression")
abline(a = 0, b = 1, lty = 2, col = "gray")

# Classification Report for Logistic Regression
print('Classification Report for Logistic Regression:')
print(conf_matrix_log_reg)

# Decision Tree
decision_tree <- rpart(y_train ~ ., data = as.data.frame(X_train), method = "class")

# Predictions
y_pred_tree <- predict(decision_tree, newdata = as.data.frame(X_test), type = "prob")[,2]
y_pred_tree_class <- ifelse(y_pred_tree > 0.5, 1, 0)

# Confusion Matrix for Decision Tree
conf_matrix_tree <- confusionMatrix(factor(y_pred_tree_class), factor(y_test))
print('Confusion Matrix for Decision Tree:')
print(conf_matrix_tree$table)

# ROC Curve for Decision Tree
roc_tree <- roc(y_test, y_pred_tree)
auc_tree <- auc(roc_tree)
print(paste('ROC AUC for Decision Tree:', round(auc_tree, 2)))

# Plot ROC Curve for Decision Tree
plot(roc_tree, main = "ROC Curve - Decision Tree")
abline(a = 0, b = 1, lty = 2, col = "gray")

# Classification Report for Decision Tree
print('Classification Report for Decision Tree:')
print(conf_matrix_tree)

# Compare the models
print('Logistic Regression vs Decision Tree')
print('-----------------------------------')
print(paste('Logistic Regression ROC AUC:', round(auc_log_reg, 2)))
print(paste('Decision Tree ROC AUC:', round(auc_tree, 2)))

# Probit Regression on "NSSO68.csv"

# Load required libraries
library(tidyverse)
library(MASS)

# Load the dataset
file_path <- 'C:/Users/anjel/Downloads/SCMA/NSSO68.csv'
data <- read.csv(file_path)

# Display all column names
print(colnames(data))

# Create the binary target variable for non-vegetarians
non_veg_items <- c('eggsno_q', 'fishprawn_q', 'goatmeat_q', 'beef_q', 'pork_q', 'chicken_q', 'othrbirds_q')
data$non_vegetarian <- as.integer(rowSums(data[non_veg_items]) > 0)

# Select relevant features for the model
features <- c('hhdsz', 'Religion', 'Social_Group', 'Whether_owns_any_land', 'Regular_salary_earner', 'MPCE_URP', 'Sex', 'Age', 'Education')
df <- data[c(features, 'non_vegetarian')]

# Drop rows with missing values for simplicity
df <- na.omit(df)

# Convert categorical variables to factors
categorical_vars <- c('Religion', 'Social_Group', 'Whether_owns_any_land', 'Regular_salary_earner', 'Sex', 'Education')
df[categorical_vars] <- lapply(df[categorical_vars], factor)

# Fit the probit model
probit_model <- glm(non_vegetarian ~ ., data = df, family = binomial(link = "probit"))

# Print the model summary
summary(probit_model)

#TOBIT MODEL


install.packages('ggplot2')

# Load necessary libraries
library(AER)
library(ggplot2)


# Read the dataset
df <- read.csv('C:\\Users\\anjel\\Downloads\\NSSO68.csv', header=TRUE)

# Filter the data for 'MH' instead of 'AP'
df_mh <- df[df$state_1 == 'MH',]

# Define variables
vars <- c("Sector", "hhdsz", "Religion", "Social_Group", "MPCE_URP", "Sex", "Age", "Marital_Status", "Education", "chicken_q", "chicken_v")
df_mh_p <- df_mh[vars]

# Calculate price
df_mh_p$price <- df_mh_p$chicken_v / df_mh_p$chicken_q
df_mh_p$price[is.na(df_mh_p$price)] <- 0

# Summary statistics
summary(df_mh_p)

# Fit a Multiple Linear Regression Model
fit <- lm(chicken_q ~ hhdsz + Religion + MPCE_URP + Sex + Age + Marital_Status + Education + price, data = df_mh_p)
summary(fit)

# Plot pairs
install.packages("GGally")
library(GGally)

ggpairs(df_mh_p[, c("chicken_q", "MPCE_URP", "price")])

# Fit a Tobit Model
install.packages("censReg")
library(censReg)

model <- censReg(chicken_q ~ hhdsz + Religion + MPCE_URP + Sex + Age + Marital_Status + Education + price, 
                 data = df_mh_p)
summary(model)


