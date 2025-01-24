set.seed(123)

# Simulated data
n <- 500
data <- data.frame(
  treatment = rbinom(n, 1, 0.4), # Binary treatment (40% treated)
  age = rnorm(n, 50, 10),        # Age covariate
  bmi = rnorm(n, 25, 4),         # BMI covariate
  smoker = rbinom(n, 1, 0.3),    # Smoking status (30% smokers)
  noise = rnorm(n, 0, 1)         # Random noise
)

# Outcome (Y) depends on treatment and covariates
data$outcome <- with(data, 
                     5 + 3 * treatment - 0.1 * age + 0.5 * bmi + 2 * smoker + noise
)

# Logistic regression for propensity scores
propensity_model <- glm(treatment ~ age + bmi + smoker, 
                        family = binomial(), data = data)

propensity_model <- glm(shift_dic ~ age + chrono + sleephr + edu,
                        family = binomial(), data = df_fin)

#imputation
# Analyze each imputed dataset
ps_models <- with(imputed_subtot, glm(shift_dic ~ 
                                        age + chrono + sleephr,
                                      family = binomial))

# Pool results
pooled_results <- pool(ps_models)
summary(pooled_results)

# Generate predictions for each imputed dataset
imputed_predictions <- with(imputed_subtot, predict(glm(shift_dic ~ 
              age + chrono + sleephr, family = binomial), type = "response"))

# Access predictions for each dataset
imputed_predictions$analyses[[1]]  # Predictions for the first imputed dataset
imputed_predictions$analyses[[2]]  # Predictions for the second imputed dataset
imputed_predictions$analyses[[3]]  # Predictions for the third imputed dataset
imputed_predictions$analyses[[4]]  
imputed_predictions$analyses[[5]]
# Combine predictions from all imputations into a matrix
prediction_matrix <- do.call(cbind, imputed_predictions$analyses)

# Calculate the average prediction for each observation
average_predictions <- rowMeans(prediction_matrix)

# View the averaged predictions
subtot$average_predictions = average_predictions


# Predicted propensity scores
data$propensity_score <- predict(propensity_model, type = "response")

subtot$propensity_score <- predict(pooled_results, type = "response")

data$weights <- ifelse(
  data$treatment == 1,
  1 / data$propensity_score,            # Treated weights
  1 / (1 - data$propensity_score)      # Control weights
)

subtot$weights <- ifelse(
  subtot$shift_dic == 1,
  1 / subtot$average_predictions,            # Treated weights
  1 / (1 - subtot$average_predictions)      # Control weights
)


# Weighted regression
outcome_model <- lm(outcome ~ treatment, data = data, weights = weights)

outcome_model <- lm(mortScore ~ shift_dic, data = subtot, weights = weights)

# Weighted regression with covariates
outcome_model <- lm(outcome ~ treatment + age + bmi + smoker, 
                    data = data, weights = weights)

# Summary of the model
summary(outcome_model)

# ATE (treatment effect)
ate <- coef(outcome_model)["treatment"]
cat("The Average Treatment Effect (ATE) is:", round(ate, 2), "\n")

ate <- coef(outcome_model)["shift_dicnight"]
cat("The Average Treatment Effect (ATE) is:", round(ate, 2), "\n")

# Predicted outcomes under treatment
data$treat_pred <- predict(outcome_model, newdata = transform(data, treatment = 1))

subtot$treat_pred <- predict(outcome_model, newdata = transform(subtot, shift_dic = 'night'))

# Predicted outcomes under control
data$control_pred <- predict(outcome_model, newdata = transform(data, treatment = 0))

subtot$control_pred <- predict(outcome_model, newdata = transform(subtot, treatment = 'control'))

# Marginal means
marginal_treated <- mean(subtot$treat_pred)
marginal_control <- mean(subtot$control_pred)

# Marginal contrast
marginal_contrast <- marginal_treated - marginal_control
cat("Marginal Treated Mean:", round(marginal_treated, 2), "\n")
cat("Marginal Control Mean:", round(marginal_control, 2), "\n")
cat("Marginal Contrast (ATE):", round(marginal_contrast, 2), "\n")


# causal forest model
library('grf')
causal_forest_model <- causal_forest(X = data, Y = data$outcome, 
                               W = data$treatment)
hte <- predict(causal_forest_model)$predictions

# Average Treatment Effect
ate <- average_treatment_effect(causal_forest_model)
print(ate)

# Variable importance for treatment effect heterogeneity
variable_imp <- variable_importance(causal_forest_model)
print(variable_imp)

# Example: Plot treatment effects vs. a covariate
plot(data$age, treatment_effects, main = "HTE by Age", xlab = "Age", ylab = "Treatment Effect")
