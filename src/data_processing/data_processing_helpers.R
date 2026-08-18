# file contains helper functions used during the data processing steps
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


#' Process DHS listing data to EA-level household counts.
#'
#' @param dhs_listing_data (tibble) DHS listing data.
#' @param segmented_csv_path (character) path to segmented DHS CSV.
#' @param ea_shapefile (sf) EA boundaries used for nearest-neighbour assignment.
#' @param source_config (list) config block for DHS listing source.
#' @param dhs_max_distance_m (numeric) maximum centroid distance allowed.
#'
#' @return dhs_hh_count (tibble) EA-level household counts from DHS listing.
process_dhs_listing_data <- function(
    dhs_listing_data,
    segmented_csv_path,
    ea_shapefile,
    source_config,
    dhs_max_distance_m
) {
    cluster_col <- source_config$cluster_id_col
    longitude_col <- source_config$longitude_col
    latitude_col <- source_config$latitude_col

    dhs_file <- read.csv(segmented_csv_path)
    non_seg_cluster <- dhs_file %>%
        filter(grepl("^no\\b", Cluster.Segmented, ignore.case = TRUE))

    n_total_clusters <- n_distinct(dhs_listing_data[[cluster_col]])

    dhs_listing_data <- dhs_listing_data %>%
        mutate(hh_count = 1) %>%
        filter(.data[[cluster_col]] %in% unique(non_seg_cluster$DHScluster))

    n_nonseg_clusters <- n_distinct(dhs_listing_data[[cluster_col]])

    dhs_hh_summary <- dhs_listing_data %>%
        group_by(.data[[cluster_col]]) %>%
        summarise(dhs_hh_count = sum(hh_count, na.rm = T), .groups = "drop")

    dhs_centroids <- dhs_listing_data %>%
        group_by(.data[[cluster_col]]) %>%
        summarise(
            lon = mean(.data[[longitude_col]], na.rm = TRUE),
            lat = mean(.data[[latitude_col]], na.rm = TRUE),
            .groups = "drop"
        ) %>%
        left_join(dhs_hh_summary, by = cluster_col)

    dhs_centroids_sf <- dhs_centroids %>%
        drop_na(lon, lat) %>%
        st_as_sf(coords = c("lon", "lat"))

    st_crs(dhs_centroids_sf) <- 4326
    dhs_centroids_sf <- st_transform(dhs_centroids_sf, crs = st_crs(ea_shapefile))

    nearest <- nngeo::st_nn(dhs_centroids_sf, ea_shapefile, k = 1, returnDist = TRUE)
    distances <- sapply(nearest$dist, function(x) x[1])

    dhs_centroids_sf <- dhs_centroids_sf %>%
        mutate(nearest_dist_m = distances) %>%
        filter(nearest_dist_m < dhs_max_distance_m)

    n_within_dist <- nrow(dhs_centroids_sf)

    nearest_indices <- st_nearest_feature(dhs_centroids_sf, ea_shapefile)
    dhs_centroids_sf$EA_CODE <- ea_shapefile$EA_CODE[nearest_indices]

    dhs_hh_count <- dhs_centroids_sf %>%
        as_tibble() %>%
        group_by(EA_CODE) %>%
        summarise(dhs_hh_count = sum(dhs_hh_count, na.rm = T), .groups = "drop")

    return(list(
        data  = dhs_hh_count,
        stats = tibble::tibble(
            n_total_clusters  = n_total_clusters,
            n_nonseg_clusters = n_nonseg_clusters,
            n_seg_excluded    = n_total_clusters - n_nonseg_clusters,
            n_within_dist     = n_within_dist,
            n_dist_excluded   = n_nonseg_clusters - n_within_dist
        )
    ))
}


#' Process DHS survey data to EA-level household size summaries.
#'
#' @param dhs_survey_data (tibble) DHS survey person-level data.
#' @param ea_shapefile (sf) EA boundaries used for nearest-neighbour assignment.
#' @param source_config (list) config block for DHS survey source.
#' @param dhs_max_distance_m (numeric) maximum centroid distance allowed.
#'
#' @return dhs_hh_size (tibble) EA-level median/mean household size from DHS survey.
process_dhs_survey_data <- function(
    dhs_survey_data,
    ea_shapefile,
    source_config,
    dhs_max_distance_m
) {
    cluster_col <- source_config$cluster_id_col
    longitude_col <- source_config$longitude_col
    latitude_col <- source_config$latitude_col
    household_col <- source_config$household_id_col

    dhs_size <- dhs_survey_data %>%
        mutate(
            unique_id = str_c(
                .data[[cluster_col]],
                REG_NAME,
                DIST_NAME,
                TA_NAME,
                EA_NUMBER,
                .data[[household_col]]
            ),
            no_persons = 1
        ) %>%
        group_by(unique_id) %>%
        mutate(hh_size = sum(no_persons, na.rm = T)) %>%
        ungroup() %>%
        group_by(.data[[cluster_col]]) %>%
        summarise(
            median_hh_size = median(hh_size, na.rm = T),
            mean_hh_size = mean(hh_size, na.rm = T),
            .groups = "drop"
        )

    dhs_centroids <- dhs_survey_data %>%
        group_by(.data[[cluster_col]]) %>%
        summarise(
            lon = mean(.data[[longitude_col]], na.rm = TRUE),
            lat = mean(.data[[latitude_col]], na.rm = TRUE),
            .groups = "drop"
        ) %>%
        left_join(dhs_size, by = cluster_col)

    dhs_centroids_sf <- dhs_centroids %>%
        drop_na(lon, lat) %>%
        st_as_sf(coords = c("lon", "lat"))

    st_crs(dhs_centroids_sf) <- 4326
    dhs_centroids_sf <- st_transform(dhs_centroids_sf, crs = st_crs(ea_shapefile))

    nearest <- nngeo::st_nn(dhs_centroids_sf, ea_shapefile, k = 1, returnDist = TRUE)
    distances <- sapply(nearest$dist, function(x) x[1])

    dhs_centroids_sf <- dhs_centroids_sf %>%
        mutate(nearest_dist_m = distances) %>%
        filter(nearest_dist_m < dhs_max_distance_m)

    nearest_indices <- st_nearest_feature(dhs_centroids_sf, ea_shapefile)
    dhs_centroids_sf$EA_CODE <- ea_shapefile$EA_CODE[nearest_indices]

    dhs_hh_size <- dhs_centroids_sf %>%
        as_tibble() %>%
        group_by(EA_CODE) %>%
        summarise(
            dhs_median_hh_size = median(median_hh_size, na.rm = T),
            dhs_mean_hh_size = mean(mean_hh_size, na.rm = T),
            .groups = "drop"
        )

    return(dhs_hh_size)
}


#' Process Zomba district CSV listings to EA-level counts.
#'
#' @param zomba_csv_dir (character) folder containing Zomba CSV files.
#' @param zomba_output_file (character) path for bound intermediate CSV.
#' @param ea_shapefile (sf) EA boundaries used for nearest-neighbour assignment.
#' @param source_config (list) config block for Zomba source.
#'
#' @return zomba_tibble (tibble) EA-level Zomba household counts.
process_zomba_data <- function(
    zomba_csv_dir,
    zomba_output_file,
    ea_shapefile,
    source_config
) {
    longitude_col <- source_config$longitude_col
    latitude_col <- source_config$latitude_col
    household_size_col <- source_config$household_size_col

    rbind_zomba_csvs(
        csv_dir = zomba_csv_dir,
        output_file = zomba_output_file
    )

    zomba_data <- read.csv(zomba_output_file)

    zomba_data <- zomba_data %>%
        mutate(hh_count = 1)

    zomba_sf <- zomba_data %>%
        drop_na(all_of(c(longitude_col, latitude_col))) %>%
        st_as_sf(coords = c(longitude_col, latitude_col))

    st_crs(zomba_sf) <- 4326
    zomba_sf <- st_transform(zomba_sf, crs = st_crs(ea_shapefile))

    nearest_indices <- st_nearest_feature(zomba_sf, ea_shapefile)
    zomba_sf$EA_CODE <- ea_shapefile$EA_CODE[nearest_indices]

    zomba_tibble <- zomba_sf %>%
        as_tibble() %>%
        group_by(EA_CODE) %>%
        summarise(
            zomba_hh_count = sum(hh_count, na.rm = T),
            zomba_pop = sum(.data[[household_size_col]], na.rm = T),
            .groups = "drop"
        )

    return(zomba_tibble)
}


#' Process Malemia listing data to EA-level counts.
#'
#' @param malemia_data (tibble) Malemia source data.
#' @param ea_shapefile (sf) EA boundaries used for nearest-neighbour assignment.
#' @param source_config (list) config block for Malemia source.
#'
#' @return malemia_tibble (tibble) EA-level Malemia household counts.
process_malemia_data <- function(
    malemia_data,
    ea_shapefile,
    source_config
) {
    longitude_col <- source_config$longitude_col
    latitude_col <- source_config$latitude_col

    malemia_data <- malemia_data %>%
        mutate(hh_count = 1)

    malemia_sf <- malemia_data %>%
        drop_na(all_of(c(longitude_col, latitude_col))) %>%
        st_as_sf(coords = c(longitude_col, latitude_col))

    st_crs(malemia_sf) <- 4326
    malemia_sf <- st_transform(malemia_sf, crs = st_crs(ea_shapefile))

    nearest_indices <- st_nearest_feature(malemia_sf, ea_shapefile)
    malemia_sf$EA_CODE <- ea_shapefile$EA_CODE[nearest_indices]

    malemia_tibble <- malemia_sf %>%
        as_tibble() %>%
        group_by(EA_CODE) %>%
        summarise(malemia_hh_count = sum(hh_count, na.rm = T), .groups = "drop")

    return(malemia_tibble)
}