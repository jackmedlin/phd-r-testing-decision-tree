![image](https://github.com/user-attachments/assets/af26a8ff-8d47-4719-82b3-c7dbba0fa7f4)# phd-r-testing-decision-tree

```
# Load necessary libraries
library(tidyverse)
library(tidymodels)
library(janitor)
library(rpart.plot)
library(vip)

# Set working directory
setwd("C:\\RPhD")

# Read data from CSV
df <- read_csv("JM_PhD_DATA.csv", show_col_types = FALSE)

# Clean column names
df <- clean_names(df)

# Selecting numeric variables for the decision tree model
# Exclude specific columns that are redundant or categorical
# Keep getting error with categorical variables
numeric_vars <- df %>%
  select(where(is.numeric)) %>%
  select(-outcomes_value, -completion_denom, -continuation_denom,
         -outcomes_denom, -teaching_denom, -assessment_denom, -support_denom,
         -res_denom, -voice_denom) %>%
  names()

# Create a formula for the decision tree model
# The response variable is 'outcomes_value'
# Predictors are the selected numeric variables
tree_formula <- as.formula(
  paste("outcomes_value ~", paste(numeric_vars, collapse = " + "))
)

# Split the data into training and testing sets
# Setting a seed for reproducibility
set.seed(123)
data_split <- initial_split(df, prop = 0.75)
train_data <- training(data_split)
test_data <- testing(data_split)

# Create and fit the decision tree model
# Using the rpart engine for regression
tree_spec <- decision_tree(mode = "regression") %>%
  set_engine("rpart")

tree_fit <- tree_spec %>%
  fit(tree_formula, data = train_data)

# Print the fitted model
rpart.plot(tree_fit$fit, type = 4, extra = 101, under = TRUE,
           cex = 0.8, box.palette = "auto", roundint = FALSE)

# Visualise importance of variables
var_importance <- vip::vip(tree_fit, num_features = 10) +
  labs(title = "Variable Importance for Decision Tree Model")
print(var_importance)

```
