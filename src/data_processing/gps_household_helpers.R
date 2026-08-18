# file contains helper functions used during the processing of gps household data
library(sf)
library(tidyverse)
library(logger)

#' Generic processor for GPS-based household listing data at EA level.
#'
#' @param survey_data (tibble) source listing data.
#' @param ea_shapefile (sf) EA boundaries used for nearest-neighbour assignment.
#' @param source_config (list) source-specific config containing EA, accuracy,
#'      longitude and latitude column names.
#' @param output_count_col (character) name of the output count column.
#' @param gps_accuracy_threshold_m (numeric) threshold in meters used to split
#'      original EA assignment versus spatial reassignment.
#'
#' @return survey_rbind (tibble) EA-level household counts.
process_gps_household_data <- function(
    survey_data,
    ea_shapefile,
    source_config,
    output_count_col,
    gps_accuracy_threshold_m
) {

    source_ea_col <- source_config$source_ea_col
    accuracy_col <- source_config$accuracy_col
    longitude_col <- source_config$longitude_col
    latitude_col <- source_config$latitude_col
    reassigned_ea_col <- paste0(source_ea_col, "_spatial")

    survey_data <- survey_data %>%
        mutate(hh_count = 1)

    no_gps <- survey_data %>%
        filter(is.na(.data[[longitude_col]]) | is.na(.data[[latitude_col]])) %>%
        group_by(.data[[source_ea_col]]) %>%
        summarise(hh_count = sum(hh_count, na.rm = T), .groups = "drop") %>%
        rename(!!output_count_col := hh_count)

    survey_sf <- survey_data %>%
        drop_na(all_of(c(longitude_col, latitude_col))) %>%
        st_as_sf(coords = c(longitude_col, latitude_col))

    st_crs(survey_sf) <- 4326
    survey_sf <- st_transform(survey_sf, crs = st_crs(ea_shapefile))

    nearest_indices <- st_nearest_feature(survey_sf, ea_shapefile)
    survey_sf[[reassigned_ea_col]] <- ea_shapefile$EA_CODE[nearest_indices]

    survey_sf <- survey_sf %>% as_tibble()

    greater <- survey_sf %>%
        filter(.data[[accuracy_col]] > gps_accuracy_threshold_m) %>%
        group_by(.data[[source_ea_col]]) %>%
        summarise(hh_count = sum(hh_count, na.rm = T), .groups = "drop") %>%
        rename(!!output_count_col := hh_count)

    less <- survey_sf %>%
        filter(.data[[accuracy_col]] < gps_accuracy_threshold_m) %>%
        group_by(.data[[reassigned_ea_col]]) %>%
        summarise(hh_count = sum(hh_count, na.rm = T), .groups = "drop") %>%
        rename(!!source_ea_col := all_of(reassigned_ea_col)) %>%
        rename(!!output_count_col := hh_count)

    survey_rbind <- bind_rows(no_gps, greater, less) %>%
        group_by(.data[[source_ea_col]]) %>%
        summarise(across(everything(), \(x) sum(x, na.rm = TRUE)), .groups = "drop")

    # ── EA change stats for accurate GPS rows ───────────────────────────────────────
    accurate_rows <- survey_sf %>%
        filter(.data[[accuracy_col]] < gps_accuracy_threshold_m)
    n_accurate   <- nrow(accurate_rows)
    n_ea_changed <- sum(
        as.character(accurate_rows[[source_ea_col]]) !=
        as.character(accurate_rows[[reassigned_ea_col]]),
        na.rm = TRUE
    )

    return(list(
        data  = survey_rbind,
        stats = tibble::tibble(
            n_total          = nrow(survey_data),
            n_no_gps         = nrow(survey_data) - nrow(survey_sf),
            n_accurate_gps   = n_accurate,
            n_inaccurate_gps = nrow(survey_sf) - n_accurate,
            n_ea_changed     = n_ea_changed
        )
    ))
}