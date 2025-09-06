get_long_format <- function(raw_data,
                            demographic_cols = c("E","F","J","R"),
                            likert_levels = c("Very likely","Likely","Neutral","Unlikely","Very unlikely")) {
  # Use names() if present; otherwise, build from first row after dropping row 2
  assess_pattern <- "^[A-S][0-9]{3}\\s+Q1$"
  names_have_assess <- any(grepl(assess_pattern, names(raw_data), perl = TRUE))
  
  # Helper: Excel letters -> numeric indices
  col_letters_to_index <- function(x) {
    sapply(x, function(s) {
      s <- toupper(gsub("\\s+", "", s))
      ch <- strsplit(s, "")[[1]]
      sum((match(ch, LETTERS)) * 26^(rev(seq_along(ch)) - 1))
    })
  }
  
  if (names_have_assess) {
    # Column names are already set correctly; just drop row 2
    df <- raw_data[-2, , drop = FALSE]
  } else {
    # Promote row 1 to column names, then drop rows 1 and 2
    header_vals <- as.character(unlist(raw_data[1, ]))
    header_vals[is.na(header_vals) | header_vals == ""] <- paste0("V", seq_along(header_vals))[is.na(header_vals) | header_vals == ""]
    unique_names <- make.unique(header_vals, sep = "__dup")
    df <- raw_data[-c(1, 2), , drop = FALSE]
    names(df) <- unique_names
  }
  
  # Subject ID = row number among participant rows in this working data
  df$Subject_ID <- seq_len(nrow(df))
  
  # Identify assessment columns by *current* column names
  assess_cols <- names(df)[grepl(assess_pattern, names(df), perl = TRUE)]
  if (length(assess_cols) == 0) {
    message("No assessment columns found (pattern: '^[A-S][0-9]{3}\\s+Q1$').")
    return(tibble::tibble(
      Subject_ID = integer(),
      Question_ID = character(),
      Response = character()
    ))
  }
  
  # Demographic columns by Excel letters (E, F, J, R) -> positions -> names
  demo_idx <- col_letters_to_index(demographic_cols)
  demo_idx <- demo_idx[demo_idx <= ncol(df)]
  demo_cols <- names(df)[demo_idx]
  
  # Pivot longer and clean responses
  long <- tidyr::pivot_longer(
    df,
    cols = dplyr::all_of(assess_cols),
    names_to = "Assessment_Label",
    values_to = "Response"
  )
  
  long$Response <- ifelse(is.na(long$Response), NA_character_, trimws(as.character(long$Response)))
  long$Response <- dplyr::na_if(long$Response, "")  # treat blanks as missing
  
  # Keep only non-missing responses
  long <- dplyr::filter(long, !is.na(Response))
  
  # Question_ID from the column header (drop the trailing " Q1")
  long$Question_ID <- sub("\\s+Q1$", "", long$Assessment_Label, perl = TRUE)
  
  # Select output columns (Subject_ID, Question_ID, Response, demographics)
  out <- dplyr::select(long, Subject_ID, Question_ID, Response, dplyr::all_of(demo_cols))
  
  # Optional: coerce to ordered Likert factor when appropriate
  uniq_resp <- unique(stats::na.omit(out$Response))
  if (length(uniq_resp) > 0 && all(uniq_resp %in% likert_levels)) {
    out$Response <- factor(out$Response, levels = likert_levels, ordered = TRUE)
  }
  
  out
}