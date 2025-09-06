
# Confidence analysis

# --- Logistic regression "summary" table (coefficients + ORs) ---
coef_df2 <- format_model_table(confidence.model)

# --- Overall accuracy using observed mix of assessments ---
acc2_table <- calculate_correctness_prob(confidence.model, long_data)

# Model fit extras + LRT
model_info2 <- get_lrt_table(confidence.model, long_data, m0 = list(base.assessment.model))

# --- Write to Excel: "Confidence Dependent Analysis" sheet ---
export_model_summary_to_excel(wb, 'Confidence Analysis', coef_df2, acc2_table, model_info2, create_prob_plot(acc2_table, 1))


# Confidence + Assessment analysis

# --- Logistic regression "summary" table (coefficients + ORs) ---
coef_df3 <- format_model_table(assessment.plus.confidence.model)

# --- Overall accuracy using observed mix of assessments ---
acc3_table <- calculate_correctness_prob(assessment.plus.confidence.model, long_data)

# Model fit extras + LRT
model_info3 <- get_lrt_table(assessment.plus.confidence.model, long_data, m0 = list(assessment.type.model, confidence.model))

# --- Write to Excel: "Confidence Dependent Analysis" sheet ---
export_model_summary_to_excel(wb, 'Conf and Assessment Analysis', coef_df3, acc3_table, model_info3, create_prob_plot(acc3_table, 2))
