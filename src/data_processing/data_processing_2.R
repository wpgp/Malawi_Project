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

#' This function will process all steps in the data_processing_2 pipeline
#'
#'
data_processing_wrapper <- function() {

  log_info("Begining data Processing,")

  # setup
  ## this section includes loading config, variables and any datasets
  config <- get("load_config", mode = "function")()
  if (is.null(config)) {
    stop("Config could not be loaded.")
  }
  print("Config loaded")
  log_info("dp1 - Loading config, reading data..")

  data_dirs <- config$paths
  drive_path <- data_dirs$drive_path
  input_path <- file.path(drive_path, data_dirs$mnso_data_dir)
  output_path <- file.path(drive_path, data_dirs$output_dir)
  shapefile_path <- file.path(drive_path, data_dirs$shapefile_dir)

  data_sources <- config$sources

  mphc_2018 <- read_dta(file.path(input_path, data_sources$mphc$data_file))
  ea <- sf::st_read(file.path(shapefile_path, "2018_MPHC_EAs_Final_for_Use.shp"))
  log_info("dp1 - .. config and datasets loaded successfully")

  # data processing
  ## this section calls in functions from the  helper script to clean
  ## and process the data

  log_info("dp2 - processing census data")
  mphc_rbind <- get("process_census_data", mode = "function")(mphc_2018, ea, output_path)

  # output section

  ## write out outputs here

  return(mphc_rbind)
}