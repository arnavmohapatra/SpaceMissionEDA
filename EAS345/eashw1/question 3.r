# Online class scores
online_scores <- c(67.6, 41.2, 85.3, 55.9, 82.4, 91.2, 73.5, 94.1, 64.7, 64.7,
                   70.6, 38.2, 61.8, 88.2, 70.6, 58.8, 91.2, 73.5, 82.4, 35.5,
                   94.1, 88.2, 64.7, 55.9, 88.2, 97.1, 85.3, 61.8, 79.4, 79.4)

# In-person class scores
inperson_scores <- c(77.9, 95.3, 81.2, 74.1, 98.8, 88.2, 85.9, 92.9, 87.1, 88.2,
                     69.4, 57.6, 69.4, 67.1, 97.6, 85.9, 88.2, 91.8, 78.8, 71.8,
                     98.8, 61.2, 92.9, 90.6, 97.6, 100,  95.3, 83.5, 92.9, 89.4)

# Define the significance level
alpha <- 0.05

# Perform the t-test
t_test_result <- t.test(online_scores, inperson_scores, var.equal = TRUE)

# Check if the degrees of freedom match the expected value
if(t_test_result$parameter != 58) {
  stop("The degrees of freedom do not match the expected value of 58.")
}

# Print the t-test result
print(t_test_result)

# Answers to the 8 questions:
cat("\nAnswers to the questions:\n")
cat("(a) This is a test of two means (online vs in-person class scores).\n")
cat("(b) The population standard deviations are unknown.\n")
cat("(c) The random variable is the difference in the means of the two samples. The test statistic used is the t-statistic.\n")
cat("(d) The Student's t-distribution is used for the test.\n")
cat("(e) The null hypothesis (in symbols) is H0: mu_online = mu_in-person. The alternative hypothesis is Ha: mu_online != mu_in-person.\n")
cat("(f) This is a two-tailed test.\n")
cat("(g) ", if(t_test_result$p.value < alpha) "Reject the null hypothesis." else "Do not reject the null hypothesis.", "\n")
cat("At the 0.05 level of significance, from the sample data, there is", if(t_test_result$p.value < alpha) "sufficient" else "not sufficient", "evidence to conclude that the mean scores are different.\n")
cat("(h) The P-value of the test is", t_test_result$p.value, ".\n")

# Additional details based on the rubric
cat("The test statistic t0 is", t_test_result$statistic, ".\n")
cat("The degrees of freedom v (nu) are", t_test_result$parameter, ".\n")
cat("The critical t-value for alpha = 0.05 is", qt(1 - alpha/2, df = t_test_result$parameter), "for a two-tailed test.\n")
