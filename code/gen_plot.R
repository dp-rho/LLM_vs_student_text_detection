create_prob_plot <- function(prob_table, n_covariates) {
  # --- 1. Input Validation ---
  if (n_covariates < 1 || n_covariates >= ncol(prob_table)) {
    stop("n_covariates must be a positive integer less than the number of columns in prob_table.")
  }
  
  # --- 2. Identify Covariate Columns ---
  covariate_names <- names(prob_table)[1:n_covariates]
  fill_variable <- tail(covariate_names, 1)
  
  # --- 3. Prepare Data for Plotting ---
  plot_data <- prob_table %>%
    # Create a temporary united column for label creation logic
    unite("temp_label", all_of(covariate_names), sep = "\n", remove = FALSE) %>%
    mutate(
      # Create a clean x-axis label, handling the "Overall" case specifically
      x_label = if_else(
        .data[[covariate_names[1]]] == "Overall", 
        "Overall", 
        temp_label
      ),
      # Ensure order is preserved from the table by creating a factor
      x_label = factor(x_label, levels = unique(x_label))
    )
  
  # --- 4. Create the Plot ---
  p <- ggplot(plot_data, aes(x = x_label, y = probability, fill = .data[[fill_variable]])) +
    geom_bar(stat = "identity", position = position_dodge()) +
    geom_errorbar(
      aes(ymin = lower_ci, ymax = upper_ci),
      width = 0.2,
      position = position_dodge(0.9)
    ) +
    scale_y_continuous(limits = c(0, 1), name = "Probability of Correct Assessment") +
    labs(
      title = "Empirical Probability of Correctness",
      x = "Condition",
      fill = fill_variable # Use the variable name for the legend title
    ) +
    theme_minimal()
  
  return(p)
}