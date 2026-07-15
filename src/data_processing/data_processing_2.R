# This script contains a wrapper function for the data  processing pipeline
# it is called upon in main.R
#
# TODO: This script needs a better name!
library(sf)
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
data_processing_2_function <- function() {

log_info("Begining data Processing,")

  # setup
  ## this section includes loading config, variables and any datasets
  config <- get("load_config", mode = "function")()
  if (is.null(config)) {
    stop("Config could not be loaded.")
  }
  print("Config loaded")
log_info("dp1 - Loading config, reading data..")
#Specify Drive Path
data_dirs <- config$paths

drive_path <- data_dirs$drive_path
input_path <- file.path(drive_path, data_dirs$mnso_data_dir)
output_path <- file.path(drive_path, data_dirs$output_dir)
shapefile_path <- file.path(drive_path, data_dirs$shapefile_dir)

data_sources <- config$sources
data_thresholds <- config$thresholds

#Load datasets
mphc_2018 <- read_dta(file.path(input_path, data_sources$mphc$data_file))
ICT_data <- read_dta(file.path(input_path, data_sources$ict$data_file))
IHS6_data <- read_dta(file.path(input_path, data_sources$ihs6$data_file))
Naca_data <- read_dta(file.path(input_path, data_sources$naca$data_file))
dhs_listing <- read_dta(file.path(input_path, data_sources$dhs_listing$data_file))
dhs_data <- read_dta(file.path(input_path, data_sources$dhs_survey$data_file))
segmented_csv_path <- file.path(input_path, data_sources$dhs_listing$segmented_csv)
zomba_csv_dir <- file.path(input_path, data_sources$zomba$csv_dir)
zomba_output_file <- file.path(output_path, data_sources$zomba$output_file)
malemia_data <- read.csv(file.path(input_path, data_sources$malemia$data_file))
ea <- sf::st_read(file.path(shapefile_path, "2018_MPHC_EAs_Final_for_Use.shp"))
log_info("dp1 - .. config and datasets loaded successfully")

  # data processing 2
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

  log_info("dp5 - processing NACA data")
  Naca_rbind <- get("process_gps_household_data", mode = "function")(
    survey_data = Naca_data,
    ea_shapefile = ea,
    source_config = data_sources$naca,
    output_count_col = "naca_hh_count",
    gps_accuracy_threshold_m = data_thresholds$gps_accuracy_threshold_m
  )

  output_df <- output_df %>%
    left_join(Naca_rbind, by = c("EA_CODE" = "EA_Number"))

  log_info("dp6 - processing DHS listing data")
  dhs_hh_count <- get("process_dhs_listing_data", mode = "function")(
    dhs_listing_data = dhs_listing,
    segmented_csv_path = segmented_csv_path,
    ea_shapefile = ea,
    source_config = data_sources$dhs_listing,
    dhs_max_distance_m = data_thresholds$dhs_max_distance_m
  )

  output_df <- output_df %>%
    left_join(dhs_hh_count, by = "EA_CODE")

  log_info("dp7 - processing DHS survey data")
  dhs_hh_size <- get("process_dhs_survey_data", mode = "function")(
    dhs_survey_data = dhs_data,
    ea_shapefile = ea,
    source_config = data_sources$dhs_survey,
    dhs_max_distance_m = data_thresholds$dhs_max_distance_m
  )

  output_df <- output_df %>%
    left_join(dhs_hh_size, by = "EA_CODE")

  log_info("dp8 - processing Zomba data")
  zomba_tibble <- get("process_zomba_data", mode = "function")(
    zomba_csv_dir = zomba_csv_dir,
    zomba_output_file = zomba_output_file,
    ea_shapefile = ea,
    source_config = data_sources$zomba
  )

  output_df <- output_df %>%
    left_join(zomba_tibble, by = "EA_CODE")

  log_info("dp9 - processing Malemia data")
  malemia_tibble <- get("process_malemia_data", mode = "function")(
    malemia_data = malemia_data,
    ea_shapefile = ea,
    source_config = data_sources$malemia
  )

  output_df <- output_df %>%
    left_join(malemia_tibble, by = "EA_CODE")

  log_info("dp10 - deriving observed household counts")
  output_df <- output_df %>%
    mutate(
      observed_hh_count = case_when(
        !is.na(malemia_hh_count) ~ malemia_hh_count,
        !is.na(dhs_hh_count) ~ dhs_hh_count,
        !is.na(ihs_hh_count) ~ ihs_hh_count,
        !is.na(naca_hh_count) ~ naca_hh_count,
        !is.na(ict_hh_count) ~ ict_hh_count,
        !is.na(zomba_hh_count) ~ zomba_hh_count,
        TRUE ~ NA_real_
      )
    ) %>%
    select(
      EA_CODE,
      mphc_total_pop,
      mphc_median_hh_size,
      mphc_mean_hh_size,
      dhs_median_hh_size,
      dhs_mean_hh_size,
      observed_hh_count,
      dhs_hh_count,
      mphc_hh_count,
      ict_hh_count,
      ihs_hh_count,
      naca_hh_count,
      zomba_hh_count,
      malemia_hh_count,
      female_count,
      male_count,
      starts_with("age_")
    )

  log_info("dp11 - writing summarized survey csv")
  write.csv(
    output_df,
    config$outputs$data_processing2_summarized_csv,
    row.names = FALSE
  )

  log_info("dp12 - creating household size geopackage")
  hh_size <- output_df %>%
    mutate(EA_CODE = as.character(EA_CODE)) %>%
    dplyr::select(EA_CODE, mphc_total_pop, mphc_median_hh_size, mphc_mean_hh_size)

  hh_ea <- full_join(ea, hh_size, by = "EA_CODE") %>%
    dplyr::select(EA_CODE, mphc_total_pop, mphc_median_hh_size, mphc_mean_hh_size)

  st_write(
    hh_ea,
    config$outputs$data_processing2_hh_size_gpkg,
    file.path(output_path, config$outputs$hh_size_gpkg),
    append = T
  )

  invisible(NULL)
}