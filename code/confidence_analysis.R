
# Confidence analysis

# --- Logistic regression "summary" table (coefficients + ORs) ---
coef_df2 <- format_model_table(confidence.model)

# --- Overall accuracy using observed mix of assessments ---
acc2_table <- calculate_correctness_prob(confidence.model, long_data)

# Model fit extras + LRT
model_info2 <- get_lrt_table(confidence.model, long_data, m0 = list(base.assessment.model))

# --- Write to Excel: "Confidence Dependent Analysis" sheet ---

# Define colors for plot
bar_colors <- c(
  "Likely" = '#56B4E9', 
  "Very likely" = "#0072B2",
  "Certainly" = "#003B5C",
  "Overall" = "#009E73"
)

# Save model and plot to excel
export_model_summary_to_excel(
  wb, 'Confidence Analysis', coef_df2, acc2_table, model_info2, 
  create_prob_plot(
    acc2_table, 1, bar_colors, show_legend = FALSE
  )
)


# Confidence + Assessment analysis

# --- Logistic regression "summary" table (coefficients + ORs) ---
coef_df3 <- format_model_table(assessment.plus.confidence.model)

# --- Overall accuracy using observed mix of assessments ---
acc3_table <- calculate_correctness_prob(assessment.plus.confidence.model, long_data)

# Model fit extras + LRT
model_info3 <- get_lrt_table(assessment.plus.confidence.model, long_data, m0 = list(assessment.type.model, confidence.model))

# --- Write to Excel: "Confidence Dependent Analysis" sheet ---

# Define colors for plot
bar_colors <- c(
  "Likely\nAI" = "#FAD4D4",
  "Very likely\nAI" = "#F08080",
  "Certainly\nAI" = "#D92B2B",
  "Likely\nstudent" = '#56B4E9',
  "Very likely\nstudent" = "#0072B2",
  "Certainly\nstudent" = "#003B5C",
  "Overall" = "#009E73"
)

# Save model and plot to excel
export_model_summary_to_excel(
  wb, 'Conf and Assessment Analysis', coef_df3, acc3_table, model_info3, 
  create_prob_plot(
    acc3_table, 2, bar_colors, legend_title = 'Assessment & Confidence',
    show_legend = FALSE, x_label_wrap_width = 12
  )
)
