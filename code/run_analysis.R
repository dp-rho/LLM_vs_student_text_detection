# Packages
library(readxl)
library(openxlsx)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# Set working directory to project dir
setwd("D:/independent_research/AI_lit_identification/R_proj")

# Read in excel datafile
data_file <- 'AI_v_Student_lit_results.xlsx'
raw_data <- read_excel(file.path('data', data_file))

# --- Settings you can tweak ---
proj_dir <- "D:/independent_research/AI_lit_identification/R_proj"
data_file <- "AI_v_Student_lit_results.xlsx"
output_file <- "AI_v_Student_primary_analysis.xlsx"
demographic_cols_letters <- c("E", "F", "J", "R")
header_row <- 2  # question text lives here
# --------------------------------

# Set working directory to project dir
setwd(proj_dir)

# Source utility files
source(file.path('code', 'get_long_format.R'))
source(file.path('code', 'get_summary_table.R'))
source(file.path('code', 'export_model_to_excel.R'))
source(file.path('code', 'gen_plot.R'))

# Read WITHOUT using row 1 as column names (questions on row 2)
raw_data <- read_excel(file.path("data", data_file), col_names = FALSE)

# Initialize workbook
wb <- createWorkbook()

## Call demographics script ##
source(file.path('code', 'demographics.R'))

# Convert raw data to long format for quantitative analyses
long_data <- get_long_format(raw_data)

# code answers as correct or incorrect, AI/Student
long_data %<>%
  dplyr::mutate(
    AI_generated = dplyr::if_else(
      startsWith(as.character(Question_ID), "S"),
      1L, 0L
    ),
    assessment = factor(dplyr::if_else(
      Response %in% c(
        "Certainly student authored",
        "Very likely student authored",
        "Likely student authored"
      ),
      "student", "AI"
    ),
    levels = c('student', 'AI')),
    correct = dplyr::if_else(
      (
        (AI_generated & (assessment == "AI")) | 
        (!AI_generated & (assessment == "student"))
      ),
      1L, 0L
    ),
    confidence = factor(dplyr::case_when(
      startsWith(as.character(Response), "Likely") ~ 'Likely',
      startsWith(as.character(Response), "Very likely") ~ 'Very likely',
      startsWith(as.character(Response), "Certainly") ~ 'Certainly'
    ), levels = c('Likely', 'Very likely', 'Certainly')),
    YoE = factor(Q181, levels = c('1-2 years', '3-4 years', '5-6 years', '7-8 years', '9-10 years', '10+ years')),
    AI_familiarity = factor(Q182, levels = c('Somewhat familiar', 'Very familiar')),
    uni_level = factor(
      # Use case_when for multiple conditions
      dplyr::case_when(
        # Condition 1: If the column contains the specific R1 string
        str_detect(Q8, "R1 research university or state flagship") ~ "R1",
        
        # Condition 2: If the column contains "R2"
        str_detect(Q8, "R2") ~ "R2",
        
        # Default case: For everything else
        TRUE ~ "other"
      ),
      # Define the factor levels to set the baseline and order
      levels = c("other", "R2", "R1")
    )
  )


# The model which estimates the overall probability of a correct assessment
base.assessment.model <- glm(correct ~ 1, family = binomial(), data = long_data)

# Model with survey assessment as predictor
assessment.type.model <- glm(correct ~ 1 + assessment, family = binomial(), data = long_data)

# Model with only confidence as a predictor
confidence.model <- glm(correct ~ 1 + confidence, family = binomial(), data = long_data)

# Institutional type model
institutional.model <- glm(correct ~ 1 + uni_level, family = binomial(), data = long_data)

# Familiarity type model
familiarity.model <- glm(correct ~ 1 + AI_familiarity, family = binomial(), data = long_data)

# YOE model
YoE.model <- glm(correct ~ 1 + YoE, family = binomial(), data = long_data)

# Model with confidence also included
assessment.plus.confidence.model <- glm(correct ~ 1 + confidence + assessment, family = binomial(), data = long_data)


## Call primary analysis script ##
source(file.path('code', 'primary_analysis.R'))

## Call confidence analysis script ##
source(file.path('code', 'educational_institution_analysis.R'))

## Call YoE analysis script ##
source(file.path('code', 'years_of_experience_analysis.R'))

## Call AI familiarity analysis script ##
source(file.path('code', 'familiarity_analysis.R'))


## Call confidence analysis script ##
source(file.path('code', 'confidence_analysis.R'))


# Save workbook
saveWorkbook(wb, file = file.path('outputs', 'AI_v_Student_results.xlsx'), overwrite = TRUE)


