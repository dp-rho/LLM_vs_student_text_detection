# --- Logistic regression "summary" table (coefficients + ORs) ---
coef_df <- format_model_table(institutional.model)

# --- Overall accuracy using observed mix of assessments ---
acc_table <- calculate_correctness_prob(institutional.model, long_data)

# Model fit extras + LRT
model_info <- get_lrt_table(institutional.model, long_data, m0 = list(base.assessment.model))

# --- Write to Excel: "Primary Analysis" sheet ---

# Define colors for plot
bar_colors <- c(
  "other" = '#56B4E9',
  "R2" = '#0072B2',
  "R1" = '#003B5C',
  "Overall" = "#009E73"
)

# Save model and plot to excel
export_model_summary_to_excel(
  wb, 'Institution Type Analysis', coef_df, acc_table, model_info, 
  create_prob_plot(
    acc_table, 1, bar_colors, legend_title = 'Institution Type',
    show_legend = FALSE
  )
)
