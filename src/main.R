# ---
# file will pull in all parts of the modelling pipeline
source("src/data_processing_2.R")
source("src/quality_assurance/output_summary.R")
source("utils.R")

# ----
# Load in vars from config needed in main
config <- load_config()

# NOTE: currently where data_processing2 output is stored in the config. But I 
# think this should change and outputs should have a dedicated section in the
# config.
data_processing_qa_filepath <- config$qa$baseline_summarized_csv

# ----
# section 1
# previous data processing steps go here, raser mosaicking ect.

# call in data procssing
output_df <- data_processing_wrapper()

# call in qa function that reads output of data processing
# qa_checks and outputs
produce_csv_summary(output_df, data_processing_qa_filepath)

# user inputs prompt questions
## does checks look good?
##
# ----

    ## Further pipeline