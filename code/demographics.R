# Helper: convert Excel column letters -> numeric indices
col_letters_to_index <- function(x) {
  sapply(x, function(s) {
    s <- toupper(gsub("\\s+", "", s))
    chars <- strsplit(s, "")[[1]]
    sum((match(chars, LETTERS)) * 26^(rev(seq_along(chars)) - 1))
  })
}

# Derive indices for E, F, J, R
demographic_cols <- col_letters_to_index(demographic_cols_letters)

# Data rows start AFTER the header_row
data_start_row <- header_row + 1
dat <- raw_data[data_start_row:nrow(raw_data), ]
n_total <- nrow(dat)

# Build the Demographics table block-by-block
blocks <- list()
question_row_idxs <- integer(0)  # for styling (will offset later)
row_counter <- 0

for (col in demographic_cols) {
  # Question text from the 2nd row of the column
  question_text <- as.character(raw_data[[col]][header_row])
  question_text <- ifelse(is.na(question_text) | question_text == "",
                          paste0("Column ", col), question_text)
  
  # Values for participants
  v <- dat[[col]]
  
  # Coerce to character, trim, make blanks into NA (so blanks count as missing)
  if (!is.character(v)) v <- as.character(v)
  v <- trimws(v)
  v[v == ""] <- NA
  
  # Counts for observed levels (exclude NAs); track missing (NA) separately
  counts <- sort(table(v, useNA = "no"), decreasing = TRUE)
  missing_count <- sum(is.na(v))
  
  # First line is "<text> | n (%)"
  q_row_left  <- question_text
  q_row_right <- ""
  
  level_left  <- if (length(counts) > 0) paste0("   ", names(counts)) else character(0)
  level_right <- if (length(counts) > 0) {
    sprintf("%d (%.2f%%)", as.integer(counts),
            100 * as.integer(counts) / n_total)
  } else character(0)
  
  # Add a 'missing' row if any NA/blank values
  if (missing_count > 0) {
    level_left  <- c(level_left, "   missing")
    level_right <- c(level_right,
                     sprintf("%d (%.2f%%)", missing_count,
                             100 * missing_count / n_total))
  }
  
  block <- data.frame(
    `Question:` = c(q_row_left, level_left),
    `n (%)`     = c(q_row_right, level_right),
    check.names = FALSE
  )
  
  blocks[[length(blocks) + 1]] <- block
  
  # Track which rows are the "Question:" lines (for bold styling)
  question_row_idxs <- c(question_row_idxs, row_counter + 1)
  row_counter <- row_counter + nrow(block)
}

demo_table <- do.call(rbind, blocks)

# --- NEW: Insert a top header row "Demographics" | "N=X" ---
header_row_df <- data.frame(
  `Question:` = "Demographics",
  `n (%)`     = sprintf("N=%d", n_total),
  check.names = FALSE
)

demo_table <- rbind(header_row_df, demo_table)

# Write to Excel
addWorksheet(wb, "Demographics")

# Write without a separate header row (layout matches your spec)
writeData(wb, "Demographics", demo_table, colNames = FALSE)

# Styling: bold the top header row AND the "Question:" lines
bold <- createStyle(textDecoration = "bold")
# Offset question rows by +1 due to inserted top header
rows_to_bold <- c(1, question_row_idxs + 1)
addStyle(wb, "Demographics", style = bold,
         rows = rows_to_bold, cols = 1:2, gridExpand = TRUE)

# Column widths for readability
setColWidths(wb, "Demographics", cols = 1, widths = 60)
setColWidths(wb, "Demographics", cols = 2, widths = 18)
