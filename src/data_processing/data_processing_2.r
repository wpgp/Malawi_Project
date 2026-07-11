# This script contains a wrapper function for the data  processing pipeline
# it is called upon in main.R
#
# TODO: This script needs a better name!
library(nngeo)
library(haven)
library(tidyverse)
library(logger)

# functions from within this package
source("utils.R")
source("src/data_processing/data_processing_helpers.R")

# load all variables into config
# TODO: little bit worried about how much less visible this is!
config <- load_config()

if (!is.null(config)) {
    print("config loaded!")
}


log_info("Begining data Processing,")

log_info("dp1 - Loading config, reading data..")
#Specify Drive Path
data_dirs <- config$paths

drive_path <- data_dirs$drive_path
input_path <- file.path(drive_path, data_dirs$mnso_data_dir)
output_path <- file.path(drive_path, data_dirs$output_dirs)
shapefile_path <- file.path(drive_path, data_dirs$shapefile_dir)

data_sources <- config$sources

#Load datasets
mphc_2018 <- read_dta(file.path(input_path, data_sources$mphc$data_file))
ICT_data <- read_dta(file.path(input_path, data_sources$ict$data_file))
IHS6_data <- read_dta(file.path(input_path, data_sources$ihs6$data_file))
Naca_data <- read_dta(file.path(input_path, data_sources$naca$data_file))
dhs_data <- read_dta(file.path(input_path, data_sources$dhs_survey$data_file))
dhs_listing <- read_dta(file.path(input_path, data_sources$dhs_list$data_file))

ea <- st_read(file.path(shapefile_path, "2018_MPHC_EAs_Final_for_Use.shp")) # replaces "2018_MPHC_EAs_Final_for_Use_Corrected.shp"
log_info("dp1 - .. config and datasets loaded successfully")


log_info("dp2 - cleaning census data")
mphc_pop_no_gps <- clean_census_data(mphc_2018)

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