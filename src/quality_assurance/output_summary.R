# this will house our quality checks

## look into library 'modelsummary' as a quick fix
## https://modelsummary.com/vignettes/datasummary.html#datasummary_skim

library("modelsummary")
library("tidyverse")

# read in output dataset

#' Uses package 'modelsummary' to produces an overview of the given dataset,
#' This can be used as a temporary measure when comparing datasets
#' 
#' @param output_df (DataFrame-like) DataFrame/Tibble containing information to
#'      summarise.
#' @param output_file (character) Path to output HTML file. Must contain 
#'      filetype ".html".
#' 
produce_csv_summary <- function(output_df, output_fileL){
    summary <- datasummary_skim(data = output_df,output = output_file)
    
    return(summary)
}


# example usecase of function to be deleted later

# Load in dataset to summaries
output_df <- read_csv("D:/GitHub/worldpop-malawi/data/Output_Data/summarized_survey_data.csv")

# call produce_csv_summary with output_df and an output file path 
# In the future this can be expanded to produce bespoke outputs 
produce_csv_summary(output_df,"D:/GitHub/worldpop-malawi/data/Output_Data/comparison_html.html")