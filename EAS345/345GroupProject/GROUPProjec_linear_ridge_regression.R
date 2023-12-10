packages <- c("readr", "nnet", "caret", "keras", "glmnet")

# Loop through the list, check if each package is installed, install if necessary, and then load the package
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# For keras, you might need to install it via keras::install_keras() after the package is installed
if (!require("keras", character.only = TRUE)) {
  install.packages("keras")
  library(keras)
  keras::install_keras()
}

#------------------------------------------------------------------

# Reading the file
data <- read_csv("/Users/zaidsaleh/Desktop/EAS345/345GroupProject/insurance.csv")

# Display the data
print(data)


#------------------------------------------------------------------

# First function: describe() give overview of data
describe <- summary(data)
print(describe)


#------------------------------------------------------------------

# Second function: head() to see how the dataset looks like
Head <- head(data)
print(Head)

#------------------------------------------------------------------

# Third function: variance to check the variance of the dataset
ageVariance <- var(data$age)
bmiVariance <- var(data$bmi)
childrenVariance <- var(data$children)
chargesVariance <- var(data$charges)
cat("Variance for age: ", ageVariance, "\n")
cat("BMI variance: ", bmiVariance, "\n")
cat("Children variance: ", childrenVariance, "\n")
cat("Charges variance: ", chargesVariance, "\n")

#------------------------------------------------------------------

# Fourth function: nunique() to check the number of unique values in each column
for (col in names(data)) {
  cat(col, "---", length(unique(data[[col]])), "\n")
}
uniqueAge <- length(unique(data$age))
uniqueNumberOfChildren <- length(unique(data$children))
print(unique(data$region))



#------------------------------------------------------------------

# changing the (male to 0), (female to 1), .. changing string to categorical
data$sex <- ifelse(data$sex == "male", 0, 1)
data$smoker <- ifelse(data$smoker == "yes", 1, 0)
levels(data$region) <- c("southwest", "southeast", "northwest", "northeast")
data$region <- as.integer(factor(data$region)) - 1

# to show the all string value changed to numercial actually worked
Head <- head(data)
print(Head)


#------------------------------------------------------------------

# creating a correlation matrix and heatmap to get a better idea 
# which features are contributing more to the insurance charges
correlation_matrix <- cor(data, use = "complete.obs")
print("Correlation Matrix:")
print(correlation_matrix)
library(ggplot2)
if (!require(reshape2)) {
  install.packages("reshape2")
}

# Load the reshape2 package
library(reshape2)

# Convert the correlation matrix to a long format for ggplot
correlation_matrix_long <- melt(correlation_matrix)

# Create the heatmap
ggplot(correlation_matrix_long, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = '', y = '') +
  geom_text(aes(label = sprintf("%.2f", value)), vjust = 1)




#------------------------------------------------------------------

# to see the ditribution of male and female in the data set
data$sex <- as.factor(data$sex)

# Create the bar plot using ggplot2
ggplot(data, aes(x = sex)) +
  geom_bar(aes(fill = sex)) +
  geom_text(stat = 'count', aes(label = ..count.., vjust = -0.5)) +
  labs(title = "Bar Plot for Sex", x = "Sex", y = "Count") +
  theme_minimal()
cat("Male: ", 0 ,'\n')
cat("Female: ", 1)

#------------------------------------------------------------------

# Scatter plot of smoker to charges ( to have an idea how many people smoke or dont in dataset)
ggplot(data, aes(x = smoker, y = charges)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Scatter Plot of Smoker vs Charges", x = "Smoker", y = "Charges")

#------------------------------------------------------------------
# Pie chart for region distribution
cat("southwest: 0",
    "southeast: 1",
    "northwest: 2",
    "northeast: 3", sep = "\n")
region_counts <- table(data$region)
pie(region_counts, main = "Distribution of Clients by Region",
    labels = paste(names(region_counts), ": ", round(100 * prop.table(region_counts), 1), "%", sep=""))

#------------------------------------------------------------------
print("Number of Unique values:")
print("Column Name      No. of Unique values")
for (column in names(data)) {
  print(sprintf("%-18s %s", column, length(unique(data[[column]]))))
}

#------------------------------------------------------------------
# Sum of missing entries
sum1 <- colSums(is.na(data))
print(sum1)


#------------------------------------------------------------------
normalize <- function(data, features) {
  for (feature in features) {
    feature_values <- data[[feature]]
    min_val <- min(feature_values, na.rm = TRUE)
    max_val <- max(feature_values, na.rm = TRUE)
    data[[feature]] <- (feature_values - min_val) / (max_val - min_val)
  }
  return(data)
}

# Normalize 'bmi' and 'age'
data <- normalize(data, c('bmi', 'age'))

# Print the first 4 rows of the data
head(data, 4)

#------------------------------------------------------------------

# Histogram of 'charges' using ggplot2
ggplot(data, aes(x = charges)) +
  geom_histogram(binwidth = 1000, fill = "blue", color = "black") + # Adjust binwidth as needed
  theme_minimal() +
  labs(title = "Histogram of Charges", x = "Charges", y = "Count")

#------------------------------------------------------------------
#LINEAR REGRESSION
# Helper function to check and install packages
check_and_install <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Check for required packages and install if needed
packages <- c("readr", "caret", "glmnet")
sapply(packages, check_and_install)

# Read the dataset
file_path <- "/Users/zaidsaleh/Desktop/EAS345/345GroupProject/insurance.csv" # Replace with the correct file path
insurance_data <- read_csv(file_path)

# Convert categorical variables to factors
insurance_data$sex <- as.factor(insurance_data$sex)
insurance_data$smoker <- as.factor(insurance_data$smoker)
insurance_data$region <- as.factor(insurance_data$region)

# Split the data into training and testing sets
set.seed(123) # for reproducibility
index <- createDataPartition(insurance_data$charges, p = 0.8, list = FALSE)
train_data <- insurance_data[index, ]
test_data <- insurance_data[-index, ]

# Train a linear regression model using caret package
model <- train(charges ~ ., data = train_data, method = "lm")

# Evaluate the model on the test set
predictions <- predict(model, test_data)

# Calculate RMSE
rmse_value <- RMSE(predictions, test_data$charges)
print(paste("RMSE on test set:", rmse_value))

# Calculate R-squared value
rss <- sum((predictions - test_data$charges)^2)
tss <- sum((mean(train_data$charges) - test_data$charges)^2)
r_squared <- 1 - rss/tss
print(paste("R-squared on test set:", r_squared))

# Function to predict charges for new input data
predict_charges <- function(new_data, model, training_data) {
  # Ensure that the factor levels in the new data match those in the training data
  for (factor_col in names(which(sapply(training_data, class) == "factor"))) {
    levels_in_train <- levels(training_data[[factor_col]])
    if (!(new_data[[factor_col]] %in% levels_in_train)) {
      stop(paste("The factor level in", factor_col, "is not present in the training data"))
    }
    new_data[[factor_col]] <- factor(new_data[[factor_col]], levels = levels_in_train)
  }
  
  # Convert new data to dataframe if it is a list
  if (is.list(new_data)) {
    new_data <- as.data.frame(new_data)
  }
  
  # Predict charges
  predict(model, new_data)
}

# Example usage:
new_input <- data.frame(
  age = 30,
  sex = factor("female", levels = levels(train_data$sex)),
  bmi = 30,
  children = 2,
  smoker = factor("no", levels = levels(train_data$smoker)),
  region = factor("northeast", levels = levels(train_data$region))
)

predicted_charge <- predict_charges(new_input, model, train_data)
print(predicted_charge)

library(ggplot2)

# Create a data frame for plotting
comparison_df <- data.frame(Actual = test_data$charges, Predicted = predictions)

# Plot actual vs predicted charges
ggplot(comparison_df, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", size = 1) +
  labs(x = "Actual Charges", y = "Predicted Charges", title = "Actual vs Predicted Charges") +
  theme_minimal()




#------------------------------------------------------------------
#RIDGE REGRESSION
# Helper function to check and install packages
check_and_install <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Check for required packages and install if needed
packages <- c("readr", "caret", "glmnet", "ggplot2")
sapply(packages, check_and_install)

# Read the dataset
file_path <- "/Users/zaidsaleh/Desktop/EAS345/345GroupProject/insurance.csv" # Replace with the correct file path
insurance_data <- readr::read_csv(file_path)

# Convert categorical variables to factors
insurance_data$sex <- as.factor(insurance_data$sex)
insurance_data$smoker <- as.factor(insurance_data$smoker)
insurance_data$region <- as.factor(insurance_data$region)

# Create a model matrix with dummy variables for categorical features
x <- model.matrix(charges ~ . - 1, data = insurance_data) # -1 to exclude the intercept
y <- insurance_data$charges

# Split the data into training and testing sets
set.seed(123) # for reproducibility
index <- caret::createDataPartition(y, p = 0.8, list = FALSE)
x_train <- x[index, ]
y_train <- y[index]
x_test <- x[-index, ]
y_test <- y[-index]

# Train the ridge regression model with cross-validation to find optimal lambda
cv_model <- glmnet::cv.glmnet(x_train, y_train, alpha = 0, # alpha=0 for ridge regression
                              standardize = TRUE, # Standardizing predictors
                              nfolds = 10) # Number of folds in cross-validation

# The best lambda value
best_lambda <- cv_model$lambda.min

# Train the final model using the best lambda
final_model <- glmnet::glmnet(x_train, y_train, alpha = 0, lambda = best_lambda)

# Predict on test data
predictions <- predict(final_model, s = best_lambda, newx = x_test)

# Calculate RMSE
rmse_value <- sqrt(mean((y_test - predictions)^2))
print(paste("RMSE on test set:", rmse_value))

# Function to predict charges for new input data
predict_charges <- function(new_data, final_model, training_data) {
  # Convert new data to dataframe if it is a list
  if (is.list(new_data)) {
    new_data <- as.data.frame(new_data)
  }
  
  # Convert factors to match training data levels
  for (colname in names(new_data)) {
    if (is.factor(training_data[[colname]])) {
      new_data[[colname]] <- factor(new_data[[colname]], levels = levels(training_data[[colname]]))
    }
  }
  
  # Create the model matrix for the new data
  new_matrix <- model.matrix(~ . - 1, data = new_data, reference = training_data)
  
  # Predict charges
  prediction <- predict(final_model, newx = new_matrix, s = best_lambda)
  return(prediction)
}

# Example usage of the prediction function
new_input <- data.frame(
  age = 35,
  sex = "female",
  bmi = 21,
  children = 5,
  smoker = "no",
  region = "northeast"
)

# Convert factors to match training data levels
new_input$sex <- factor(new_input$sex, levels = levels(insurance_data$sex))
new_input$smoker <- factor(new_input$smoker, levels = levels(insurance_data$smoker))
new_input$region <- factor(new_input$region, levels = levels(insurance_data$region))

predicted_charge <- predict_charges(new_input, final_model, insurance_data)
print(paste("Predicted Charge: ", predicted_charge))

# Plot actual vs predicted charges
if (is.matrix(predictions)) {
  predictions <- predictions[,1]
}

# Check dimensions
print(paste("Length of y_test:", length(y_test)))
print(paste("Length of predictions:", length(predictions)))

# Create the comparison data frame
comparison_df <- data.frame(Actual = y_test, Predicted = predictions)

# Inspect the first few rows of the comparison data frame
print(head(comparison_df))

# Plot actual vs predicted charges
ggplot2::ggplot(comparison_df, ggplot2::aes(x = Actual, y = Predicted)) +
  ggplot2::geom_point(alpha = 0.5) + 
  ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", size = 1) +
  ggplot2::labs(x = "Actual Charges", y = "Predicted Charges", title = "Actual vs Predicted Charges") +
  ggplot2::theme_minimal()
