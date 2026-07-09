# This script contains a wrapper function for the data  processing pipeline
# it is called upon in main.R
#
# TODO: This script needs a better name!
library(nngeo)
library(haven)
library(tidyverse)

# functions from within this package
source("utils.R")

# load all variables into config
# TODO: little bit worried about how much less visible this is!
config <- load_config()

if (!is.null(config)) {
    print("config loaded!")
}



#Specify Drive Path
data_dirs <- config$paths

drive_path <- data_dirs$drive_path
input_path <- file.path(drive_path, data_dirs$mnso_data_dir)
output_path <- file.path(drive_path, data_dirs$output_dirs)
shapefile_path <- file.path(drive_path, data_dirs$shapefile_dir)

data_sources <- config$sources

#Load datasets
mphc_2018 <- read_dta(file.path(input_path, data_sources$mphc))
ICT_data <- read_dta(file.path(input_path, data_sources$ict))
IHS6_data <- read_dta(file.path(input_path, data_sources$ihs6))
Naca_data <- read_dta(file.path(input_path, data_sources$naca))
dhs_data <- read_dta(file.path(input_path, data_sources$dhs_survey))
dhs_listing <- read_dta(file.path(input_path, data_sources$dhs_list))

ea <- st_read(file.path(shapefile_path, "2018_MPHC_EAs_Final_for_Use.shp")) # replaces "2018_MPHC_EAs_Final_for_Use_Corrected.shp"

#' This function will process all steps in the data_processing_2 pipeline
#' 
#' 
data_processing_wrapper <- function() {

    # setup

    ## this section includes loading config, variables and any datasets
    


    # data processing

    ## this section calls in functions from the  helper script to clean
    ## and process the data

    # output section

    ## write out outputs here
}