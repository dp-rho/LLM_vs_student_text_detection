export_model_summary_to_excel <- function(wb, sheet_name, model_summary_tbl, correctness_prob_tbl, model_fit_tbl, prob_plot) {
  
  addWorksheet(wb, sheet_name)
  header_style <- createStyle(textDecoration = "bold")
  current_row <- 1
  
  # --- Section 1: Probabilities Table and Plot ---
  writeData(wb, sheet = sheet_name, x = "Correctness Probabilities", startCol = 1, startRow = current_row)
  addStyle(wb, sheet = sheet_name, style = header_style, rows = current_row, cols = 1)
  current_row <- current_row + 1
  
  writeData(wb, sheet = sheet_name, x = correctness_prob_tbl, startRow = current_row)
  
  if (!is.null(prob_plot)) {
    print(prob_plot)
    insertPlot(wb, sheet = sheet_name, startRow = current_row, startCol = ncol(correctness_prob_tbl) + 6, width = 8, height = 6)
  }
    
  current_row <- current_row + nrow(correctness_prob_tbl) + 2
  
  # --- Section 2: Model Summary Table ---
  writeData(wb, sheet = sheet_name, x = "Model Summary", startCol = 1, startRow = current_row)
  addStyle(wb, sheet = sheet_name, style = header_style, rows = current_row, cols = 1)
  current_row <- current_row + 1
  writeData(wb, sheet = sheet_name, x = model_summary_tbl, startRow = current_row)
  current_row <- current_row + nrow(model_summary_tbl) + 2
  
  # --- Section 3: Likelihood Ratio Tests ---
  table_name <- "Likelihood Ratio Tests"
  if (sheet_name == 'Primary Analysis')
    table_name <- 'Model Test for Statistical Significance'
  writeData(wb, sheet = sheet_name, x = table_name, startCol = 1, startRow = current_row)
  addStyle(wb, sheet = sheet_name, style = header_style, rows = current_row, cols = 1)
  current_row <- current_row + 1
  
  # The model_fit_tbl is now always a single data frame of LRT results
  writeData(wb, sheet = sheet_name, x = model_fit_tbl, startRow = current_row)
  
  # --- Final Formatting ---
  setColWidths(wb, sheet = sheet_name, cols = 1:(ncol(model_summary_tbl)+1), widths = "auto")
}
