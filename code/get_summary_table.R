
format_model_table <- function(model) {
  
  # --- 1. Input Validation ---
  if (!inherits(model, "glm")) {
    stop("Error: The 'model' object must be of class 'glm'.")
  }
  
  # --- 2. Extract Coefficients and Summary Statistics ---
  # The summary() function provides most of what we need.
  model_summary <- summary(model)
  
  # Extract the coefficients table (which is a matrix) and convert it to a data frame.
  # This table contains the Estimate, Std. Error, z value, and p-value.
  coef_table <- as.data.frame(model_summary$coefficients)
  
  # The term names (e.g., "Intercept") are stored as row names. Let's move them
  # into a proper column called 'term'.
  coef_table$term <- rownames(coef_table)
  rownames(coef_table) <- NULL # Clean up by removing the row names
  
  # --- 3. Calculate Odds Ratios and Confidence Intervals ---
  # The coefficients ('Estimate') are on the log-odds scale. To get the odds
  # ratio, we need to exponentiate the estimate.
  
  # The confidence interval for the odds ratio is calculated by first finding the
  # CI on the log-odds scale and then exponentiating the bounds.
  # CI_log_odds = Estimate +/- 1.96 * Std. Error
  
  # We use `!!` (bang-bang) from dplyr to correctly handle the column names
  # that have spaces, like "Std. Error".
  
  z_value <- qnorm(0.975) # Use qnorm for precision (approx 1.96)
  
  table_with_or <- coef_table %>%
    mutate(
      OR = exp(Estimate),
      OR_lo95 = exp(Estimate - z_value * `Std. Error`),
      OR_hi95 = exp(Estimate + z_value * `Std. Error`)
    )
  
  # --- 4. Final Formatting ---
  # Reorder and rename columns to exactly match the desired output format.
  final_table <- table_with_or %>%
    select(
      term,
      Estimate,
      `Std. Error`,
      `z value`,
      `Pr(>|z|)`,
      OR,
      OR_lo95,
      OR_hi95
    ) %>%
    # Rename columns to match the user's example image
    rename(
      Std.Error = `Std. Error`,
      z.value = `z value`
    )
  
  return(final_table)
}

get_lrt_table <- function(m1, data, m0 = NULL) {
  
  # Input validation
  if (!inherits(m1, "glm")) stop("m1 must be a glm model object.")
  
  # Helper to get the right-hand side of the formula as a string
  get_formula_str <- function(model) {
    full_formula <- deparse(formula(model))
    # Split by the tilde and take the second part (the predictors)
    rhs <- trimws(strsplit(full_formula, "~")[[1]][2])
    return(paste0("(", rhs, ")"))
  }
  
  # --- Scenario 1: Only one model is provided (m1) ---
  # This scenario is now simplified to just return the model p-value vs intercept-only
  if (is.null(m0)) {
    is_intercept_only <- length(attr(terms(formula(m1)), "term.labels")) == 0
    
    if (is_intercept_only) {
      model_summary <- summary(m1)
      p_value <- model_summary$coefficients["(Intercept)", "Pr(>|z|)"]
      return(data.frame(Comparison = "Intercept vs 0", `p-value` = p_value, check.names = FALSE))
    } else {
      response_var <- all.vars(formula(m1))[1]
      null_formula <- as.formula(paste(response_var, "~ 1"))
      m_null <- glm(null_formula, family = family(m1), data = data)
      lrt <- anova(m_null, m1, test = "LRT")
      return(data.frame(Comparison = paste(get_formula_str(m1), "vs (Intercept-Only)"), `Chi-sq` = lrt$Deviance[2], Df = lrt$Df[2], `p-value` = lrt$`Pr(>Chi)`[2], check.names = FALSE))
    }
  }
  
  # --- Scenario 2: One or more reduced models are provided ---
  else {
    # Ensure m0 is a list
    if (!is.list(m0)) {
      m0 <- list(m0)
    }
    
    # --- Create Likelihood Ratio Tests Table ---
    lrt_results <- lapply(m0, function(reduced_model) {
      # Verify models are nested
      terms_m0 <- attr(terms(formula(reduced_model)), "term.labels")
      terms_m1 <- attr(terms(formula(m1)), "term.labels")
      if (!all(terms_m0 %in% terms_m1)) {
        warning(paste("Model", deparse(formula(reduced_model)), "is not nested in the full model. Skipping LRT."))
        return(NULL)
      }
      
      lrt <- anova(reduced_model, m1, test = "LRT")
      data.frame(
        Comparison = paste(get_formula_str(m1), "vs", get_formula_str(reduced_model)),
        `Chi-sq` = lrt$Deviance[2],
        Df = lrt$Df[2],
        `p-value` = lrt$`Pr(>Chi)`[2],
        check.names = FALSE
      )
    })
    
    # Combine LRT results into a single table
    lrt_table <- do.call(rbind, lrt_results)
    
    return(lrt_table)
  }
}


calculate_correctness_prob <- function(model, data) {
  
  # --- 1. Input Validation ---
  # Check if the 'correct' column exists in the provided data
  if (!"correct" %in% names(data)) {
    stop("Error: The input 'data' frame must contain a column named 'correct'.")
  }
  # Check if the model is a glm object
  if (!inherits(model, "glm")) {
    stop("Error: The 'model' object must be of class 'glm'.")
  }
  
  # --- 2. Identify Predictor Variables ---
  # Extract the predictor variable names from the original model's formula
  predictor_vars <- all.vars(formula(model))[-1]
  # if (length(predictor_vars) == 0) {
  #   stop("Error: The model does not contain any predictor variables.")
  # }
  
  # --- 3. Create All Unique Combinations of Predictors ---
  # Create a list where each element is the vector of unique levels for a predictor
  levels_list <- lapply(predictor_vars, function(var) {
    if (is.factor(data[[var]])) {
      levels(data[[var]])
    } else {
      unique(data[[var]])
    }
  })
  names(levels_list) <- predictor_vars
  
  # Generate a data frame with all unique combinations of these levels
  combinations <- expand.grid(levels_list, stringsAsFactors = TRUE)
  
  # --- 4. Model Correctness ---
  # Create a new formula with 'correct' as the response variable
  new_formula <- as.formula(paste("correct ~", paste(predictor_vars, collapse = " + ")))
  
  # Fit a new logistic regression model to predict the probability of being correct
  correctness_model <- glm(new_formula, family = binomial(), data = data)
  
  # --- 5. Predict Probabilities and Standard Errors ---
  # Use the new model to predict outcomes for our combinations data frame.
  # 'type = "link"' gives us the log-odds, which is what we need for CI calculation.
  # 'se.fit = TRUE' gives us the standard errors of these predictions.
  predictions <- predict(correctness_model, newdata = combinations, type = "link", se.fit = TRUE)
  
  # --- 6. Calculate Confidence Intervals ---
  # We first calculate the CI on the log-odds (link) scale and then convert back.
  z_value <- qnorm(0.975) # Z-score for a 95% confidence interval (approx 1.96)
  
  log_odds <- predictions$fit
  lower_ci_log_odds <- log_odds - (z_value * predictions$se.fit)
  upper_ci_log_odds <- log_odds + (z_value * predictions$se.fit)
  
  # Define the inverse logit function to convert log-odds back to probabilities
  inv_logit <- function(x) {
    return(1 / (1 + exp(-x)))
  }
  
  # --- 7. Assemble Final Results ---
  # Create the final data frame with the combinations and their calculated values
  result_df <- combinations %>%
    mutate(
      probability = inv_logit(log_odds),
      lower_ci = inv_logit(lower_ci_log_odds),
      upper_ci = inv_logit(upper_ci_log_odds)
    )
  
  # --- 8. Calculate and Append Overall Row ---
  # Fit an intercept-only model to get the overall probability and SE
  overall_model <- glm(correct ~ 1, family = binomial(), data = data)
  overall_pred <- predict(overall_model, newdata = data.frame(x = 1), type = "link", se.fit = TRUE)
  
  # Calculate the CI on the log-odds scale for the overall probability
  overall_log_odds <- overall_pred$fit
  overall_lower_ci_log_odds <- overall_log_odds - (z_value * overall_pred$se.fit)
  overall_upper_ci_log_odds <- overall_log_odds + (z_value * overall_pred$se.fit)
  
  # Create the "overall" row
  overall_row <- data.frame(
    probability = inv_logit(overall_log_odds),
    lower_ci = inv_logit(overall_lower_ci_log_odds),
    upper_ci = inv_logit(overall_upper_ci_log_odds)
  )
  
  # Create a list of "Overall" values named by the predictor variables
  overall_labels <- setNames(as.list(rep("Overall", length(predictor_vars))), predictor_vars)
  
  # Combine the labels and the calculated values for the overall row
  overall_row <- bind_cols(as.data.frame(overall_labels), overall_row)
  
  # Convert factor columns in the main result to character to allow binding
  result_df <- result_df %>%
    mutate(across(all_of(predictor_vars), as.character))
  
  # Bind the overall row to the main results table
  final_df <- bind_rows(result_df, overall_row)
  
  return(final_df)
}

calculate_base_correctness_prob <- function(data) {
  # --- 1. Input Validation ---
  if (!"correct" %in% names(data)) {
    stop("Error: The input 'data' frame must contain a column named 'correct'.")
  }
  
  # --- 2. Model Correctness ---
  # Fit an intercept-only model to get the overall probability and standard error
  overall_model <- glm(correct ~ 1, family = binomial(), data = data)
  
  # Predict for a single new data point to get a single result.
  # The content of newdata doesn't matter for an intercept-only model.
  overall_pred <- predict(overall_model, newdata = data.frame(x = 1), type = "link", se.fit = TRUE)
  
  # --- 3. Calculate Confidence Intervals ---
  z_value <- qnorm(0.975) # Z-score for 95% CI
  inv_logit <- function(x) { 1 / (1 + exp(-x)) } # Inverse logit function
  
  # Calculate CI on the log-odds scale first
  log_odds <- overall_pred$fit
  lower_ci_log_odds <- log_odds - (z_value * overall_pred$se.fit)
  upper_ci_log_odds <- log_odds + (z_value * overall_pred$se.fit)
  
  # --- 4. Assemble Final Results ---
  # Create the final single-row data frame
  result_df <- data.frame(
    term = "Overall",
    probability = inv_logit(log_odds),
    lower_ci = inv_logit(lower_ci_log_odds),
    upper_ci = inv_logit(upper_ci_log_odds)
  )
  
  return(result_df)
}


calculate_model_correctness_prob <- function(model, data) {
  # --- 1. Input Validation ---
  if (!"correct" %in% names(data)) {
    stop("Error: The input 'data' frame must contain a column named 'correct'.")
  }
  if (!inherits(model, "glm")) {
    stop("Error: The 'model' object must be of class 'glm'.")
  }
  
  # --- 2. Identify Predictor Variables ---
  # Extract predictor variable names from the model's formula
  predictor_vars <- all.vars(formula(model))[-1]
  if (length(predictor_vars) == 0) {
    stop("Error: The model does not contain any predictor variables.")
  }
  
  # --- 3. Create All Unique Combinations of Predictors ---
  levels_list <- lapply(predictor_vars, function(var) {
    if (is.factor(data[[var]])) {
      levels(data[[var]])
    } else {
      unique(data[[var]])
    }
  })
  names(levels_list) <- predictor_vars
  combinations <- expand.grid(levels_list, stringsAsFactors = TRUE)
  
  # --- 4. Predict Probabilities and Standard Errors ---
  # Use the provided model to predict outcomes for the combinations
  predictions <- predict(model, newdata = combinations, type = "link", se.fit = TRUE)
  
  # --- 5. Calculate Confidence Intervals ---
  z_value <- qnorm(0.975) # Z-score for 95% CI
  inv_logit <- function(x) { 1 / (1 + exp(-x)) }
  
  log_odds <- predictions$fit
  lower_ci_log_odds <- log_odds - (z_value * predictions$se.fit)
  upper_ci_log_odds <- log_odds + (z_value * predictions$se.fit)
  
  # --- 6. Assemble Combination Results ---
  result_df <- combinations %>%
    mutate(
      probability = inv_logit(log_odds),
      lower_ci = inv_logit(lower_ci_log_odds),
      upper_ci = inv_logit(upper_ci_log_odds)
    )
  
  # --- 7. Calculate and Append Overall Row ---
  overall_model <- glm(correct ~ 1, family = binomial(), data = data)
  overall_pred <- predict(overall_model, newdata = data.frame(x=1), type = "link", se.fit = TRUE)
  
  overall_log_odds <- overall_pred$fit
  overall_lower_ci_log_odds <- overall_log_odds - (z_value * overall_pred$se.fit)
  overall_upper_ci_log_odds <- overall_log_odds + (z_value * overall_pred$se.fit)
  
  overall_row_data <- data.frame(
    probability = inv_logit(overall_log_odds),
    lower_ci = inv_logit(overall_lower_ci_log_odds),
    upper_ci = inv_logit(overall_upper_ci_log_odds)
  )
  
  overall_labels <- setNames(as.list(rep("Overall", length(predictor_vars))), predictor_vars)
  overall_row <- bind_cols(as.data.frame(overall_labels), overall_row_data)
  
  result_df <- result_df %>%
    mutate(across(all_of(predictor_vars), as.character))
  
  final_df <- bind_rows(result_df, overall_row)
  
  return(final_df)
}

  