# ---
# file will pull in all parts of the modelling pipeline
source("src/data_processing_2.R")
source("src/quality_assurance/output_summary.R")

# ---
# section 1
    # previous data processing steps go here, raser mosaicking ect.

    # call in data procssing
    output_df <- data_processing_wrapper()

    # call in qa function that reads output of data processing
    # qa_checks and outputs
    
    # user inputs prompt questions
    ## does checks look good?
    ## 
# ---

    ## Further pipeline