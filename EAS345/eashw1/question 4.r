# Load necessary library
library(ggplot2)

# Part 4(a)i - Linear Regression
x <- c(0, 1, 2, 3, 4, 5)
y <- c(-0.01, 1.2, 1.97, 3.15, 4.02, 5.01)

# Perform linear regression
linear_model <- lm(y ~ x)

# Compute and display confidence intervals and p-values for linear regression
cat("Linear Regression Coefficients (95% CI and p-values):\n")
print(summary(linear_model)$coefficients)
print(confint(linear_model, level = 0.95))

# Plot the data and the linear regression line
ggplot() + 
  geom_point(aes(x, y)) +
  geom_smooth(method = "lm", aes(x, y), se = FALSE, color = "blue") +
  ggtitle("Linear Regression")

# Part 4(a)ii - Quadratic Regression
quadratic_model <- lm(y ~ x + I(x^2))

# Compute and display confidence intervals and p-values for quadratic regression
cat("\nQuadratic Regression Coefficients (95% CI and p-values):\n")
print(summary(quadratic_model)$coefficients)
print(confint(quadratic_model, level = 0.95))

# Plot the data and the quadratic regression curve
ggplot() + 
  geom_point(aes(x, y)) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "red") +
  ggtitle("Quadratic Regression")

# Part 4(b) - Exponential Model Transformation and Regression
T <- c(1, 20, 60, 80, 100)
mu <- c(2.5e-3, 3.5e-4, 4.98e-5, 2.12e-5, 1.05e-5)

# Transforming the data for linear regression
transformed_T <- 1 / T
transformed_mu <- log(mu)

# Perform linear regression on transformed data
exp_model <- lm(transformed_mu ~ transformed_T)

# Compute and display confidence intervals and p-values for exponential model
cat("\nExponential Model Regression Coefficients (95% CI and p-values):\n")
print(summary(exp_model)$coefficients)
print(confint(exp_model, level = 0.95))

# Plot the original data and transformed data
ggplot() + 
  geom_point(aes(T, mu)) +
  ggtitle("Original Data - Kinematic Viscosity vs Temperature")

ggplot() + 
  geom_point(aes(transformed_T, transformed_mu)) +
  geom_smooth(method = "lm", aes(transformed_T, transformed_mu), se = FALSE, color = "green") +
  ggtitle("Transformed Data - Linearized Model")
