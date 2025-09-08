# --- Logistic regression "summary" table (coefficients + ORs) ---
coef_df <- format_model_table(YoE.model)

# --- Overall accuracy using observed mix of assessments ---
acc_table <- calculate_correctness_prob(YoE.model, long_data)

# Model fit extras + LRT
model_info <- get_lrt_table(YoE.model, long_data, m0 = list(base.assessment.model))

# --- Write to Excel: "Primary Analysis" sheet ---

# Define colors for plot
bar_colors <- c(
  "1-2 years" = '#B3D1E1',
  "3-4 years" = '#85B9D9',
  "5-6 years" = '#56B4E9',
  "7-8 years" = '#4194C7',
  "9-10 years" = '#005481',
  "10+ years" = '#00324D',
  "Overall" = "#009E73"
)

# Save model and plot to excel
export_model_summary_to_excel(
  wb, 'YoE Analysis', coef_df, acc_table, model_info, 
  create_prob_plot(
    acc_table, 1, bar_colors, legend_title = 'Years of Experience', 
    show_legend = FALSE
  )
)
