# --- Logistic regression "summary" table (coefficients + ORs) ---
coef_df <- format_model_table(familiarity.model)

# --- Overall accuracy using observed mix of assessments ---
acc_table <- calculate_correctness_prob(familiarity.model, long_data)

# Model fit extras + LRT
model_info <- get_lrt_table(familiarity.model, long_data, m0 = list(base.assessment.model))

# --- Write to Excel: "Primary Analysis" sheet ---


# Define colors for plot
bar_colors <- c(
  "Somewhat familiar" = '#56B4E9',
  "Very familiar" = '#005481',
  "Overall" = "#009E73"
)

export_model_summary_to_excel(
  wb, 'AI Familiarity Analysis', coef_df, acc_table, model_info, 
  create_prob_plot(
    acc_table, 1, bar_colors, legend_title = 'AI Familiarity',
    show_legend = FALSE
  )
)
