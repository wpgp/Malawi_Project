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
  data_thresholds <- config$thresholds

  mphc_2018 <- read_dta(file.path(input_path, data_sources$mphc$data_file))
  ICT_data <- read_dta(file.path(input_path, data_sources$ict$data_file))
  IHS6_data <- read_dta(file.path(input_path, data_sources$ihs6$data_file))
  ea <- sf::st_read(file.path(shapefile_path, "2018_MPHC_EAs_Final_for_Use.shp"))
  log_info("dp1 - .. config and datasets loaded successfully")

  # data processing
  ## this section calls in functions from the  helper script to clean
  ## and process the data

  log_info("dp2 - processing census data")
  mphc_rbind <- get("process_census_data", mode = "function")(mphc_2018, ea, output_path)

  log_info("dp3 - processing ICT data")
  ICT_rbind <- get("process_gps_household_data", mode = "function")(
    survey_data = ICT_data,
    ea_shapefile = ea,
    source_config = data_sources$ict,
    output_count_col = "ict_hh_count",
    gps_accuracy_threshold_m = data_thresholds$gps_accuracy_threshold_m
  )

  output_df <- mphc_rbind %>%
    left_join(ICT_rbind, by = c("EA_CODE" = "EA_Number"))

  log_info("dp4 - processing IHS6 data")
  IHS_rbind <- get("process_gps_household_data", mode = "function")(
    survey_data = IHS6_data,
    ea_shapefile = ea,
    source_config = data_sources$ihs6,
    output_count_col = "ihs_hh_count",
    gps_accuracy_threshold_m = data_thresholds$gps_accuracy_threshold_m
  )

  output_df <- output_df %>%
    left_join(IHS_rbind, by = "EA_CODE")

  # output section

  ## write out outputs here

  return(output_df)
}