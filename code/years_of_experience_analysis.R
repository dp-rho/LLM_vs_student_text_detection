# --- Logistic regression "summary" table (coefficients + ORs) ---
coef_df <- format_model_table(YoE.model)

# --- Overall accuracy using observed mix of assessments ---
acc_table <- calculate_correctness_prob(YoE.model, long_data)

# Model fit extras + LRT
model_info <- get_lrt_table(YoE.model, long_data, m0 = list(base.assessment.model))

# --- Write to Excel: "Primary Analysis" sheet ---
export_model_summary_to_excel(wb, 'YoE Analysis', coef_df, acc_table, model_info, create_prob_plot(acc_table, 1))