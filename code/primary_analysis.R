
# --- Primary analysis script considering only assessment

# --- Logistic regression "summary" table (coefficients + ORs) ---
coef_df <- format_model_table(base.assessment.model)

# --- Overall accuracy using observed mix of assessments ---
acc_table <- calculate_base_correctness_prob(long_data)

# Model fit extras + LRT
model_info <- get_lrt_table(base.assessment.model, long_data)

# --- Write to Excel: "Primary Analysis" sheet ---
export_model_summary_to_excel(wb, 'Primary Analysis', coef_df, acc_table, model_info, NULL)


# --- Logistic regression "summary" table (coefficients + ORs) ---
coef_df <- format_model_table(assessment.type.model)

# --- Overall accuracy using observed mix of assessments ---
acc_table <- calculate_correctness_prob(assessment.type.model, long_data)

# Model fit extras + LRT
model_info <- get_lrt_table(assessment.type.model, long_data, m0 = list(base.assessment.model))

# --- Write to Excel: "Primary Analysis" sheet ---

# Define colors for plot
bar_colors <- c(
  "AI" = "#F08080", 
  "student" = "#0072B2", 
  "Overall" = "#009E73"
)

# Save model and plot to excel
export_model_summary_to_excel(
  wb, 'Assessment Type Analysis', coef_df, acc_table, model_info, 
  create_prob_plot(acc_table, 1, bar_colors, show_legend = FALSE)
)
