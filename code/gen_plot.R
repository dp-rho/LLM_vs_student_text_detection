create_prob_plot <- function(
    prob_table, 
    n_covariates, 
    my_colors, 
    legend_title = NULL, 
    show_legend = TRUE,
    x_label_wrap_width = NULL
) {
  # --- 1. Input Validation ---
  if (n_covariates < 1 || n_covariates >= ncol(prob_table)) {
    stop("n_covariates must be a positive integer less than the number of columns in prob_table.")
  }
  
  # --- 2. Identify Covariate Columns ---
  names(prob_table) <- c(
    stringr::str_to_title(names(prob_table)[1:n_covariates]),
    names(prob_table)[(n_covariates + 1):length(names(prob_table))]
  )
  covariate_names <- names(prob_table)[1:n_covariates]
  
  # --- 3. Prepare Data for Plotting ---
  plot_data <- prob_table %>%
    unite("interaction_label", all_of(covariate_names), sep = "\n", remove = FALSE) %>%
    mutate(
      interaction_label = if_else(.data[[covariate_names[1]]] == "Overall", "Overall", interaction_label),
      x_label = if_else(
        .data[[covariate_names[1]]] == "Overall",
        "Overall",
        capitalize_selectively(interaction_label)
      ),
      x_label = factor(x_label, levels = unique(x_label))
    )
  
  font.size = 20
  
  # --- 4. Conditional Logic for Fill and Legend ---
  # --- NEW: Use the show_legend argument to set the guide ---
  if (show_legend) {
    legend_guide <- "legend"
  } else {
    legend_guide <- "none"
  }
  
  if (n_covariates > 1) {
    fill_variable <- "interaction_label"
    if (is.null(legend_title)) { legend_title <- "" }
    custom_labels <- capitalize_selectively
  } else {
    fill_variable <- covariate_names[1]
    if (is.null(legend_title)) {
      legend_title <- stringr::str_to_title(fill_variable)
    }
    custom_labels <- capitalize_selectively
  }
  
  # --- 5. Create the Plot ---
  p <- ggplot(plot_data, aes(x = x_label, y = probability, fill = .data[[fill_variable]])) +
    geom_bar(stat = "identity", position = position_dodge()) +
    geom_errorbar(
      aes(ymin = lower_ci, ymax = upper_ci),
      width = 0.2,
      position = position_dodge(0.9)
    ) +
    scale_y_continuous(limits = c(0, 1), name = "Probability of Correct Assessment") +
    scale_fill_manual(
      name = legend_title,
      labels = custom_labels,
      values = my_colors,
      guide = legend_guide # <-- This now uses the new setting
    ) +
    labs(x = "Condition") +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(family = "Times New Roman", size = font.size),
      axis.title.x = element_text(family = "Times New Roman", size = font.size + 2),
      axis.title.y = element_text(family = "Times New Roman", size = font.size + 2),
      axis.text.x = element_text(family = "Times New Roman", size = font.size),
      axis.text.y = element_text(family = "Times New Roman", size = font.size),
      legend.title = element_text(family = "Times New Roman", size = font.size + 2),
      legend.text = element_text(family = "Times New Roman", size = font.size)
    )
  
  # --- 6. NEW: Conditionally Apply Text Wrapping ---
  if (!is.null(x_label_wrap_width)) {
    p <- p + scale_x_discrete(labels = scales::label_wrap(x_label_wrap_width))
  }
  
  return(p)
}


# This function capitalizes the first letter of only all-lowercase words
capitalize_selectively <- function(text) {
  # Use sapply to apply this logic to each element of the input vector
  sapply(text, function(s) {
    # Split the string into words
    words <- stringr::str_split(s, "\\s+")[[1]]
    
    # Process each word
    processed_words <- lapply(words, function(word) {
      if (word == tolower(word)) {
        # If the word is all lowercase, capitalize its first letter
        return(stringr::str_to_sentence(word))
      } else {
        # Otherwise, leave the word as is
        return(word)
      }
    })
    
    # Join the words back into a single string
    paste(processed_words, collapse = " ")
  }, USE.NAMES = FALSE)
}