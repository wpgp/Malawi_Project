# file contains helper functions used during the data processing steps
library(sf)
library(logger)


#' Applies cleaning steps to the census data
#' 
#' @param mphc_data (tibble) census data to clean, this usually will be mphc_2018
#' 
#' @return mphc_pop_no_gps (tibble) cleaned census data, with age summary.
#' 
clean_census_data <- function(mphc_data){    
    #Mutate and add a  variable called no_persons = 1 (individual record)
    mphc_data <- mphc_data %>% 
    mutate(no_persons = 1)  # Individual observation

    # Filter records without GPS coordinates
    log_info("filtering records with no GPS")
    mphc_data_no_gps <- mphc_data %>% 
    filter(is.na(hh_longitude) | is.na(hh_latitude))

    #Add additional digits to EA and TA code
    log_info("Pad digits of identifier codes")
    mphc_data_no_gps <- mphc_data_no_gps %>%  
    mutate(new_ta = str_pad(ta, width = 2, pad = 0),
            new_ea = str_pad(ea, width = 3, pad = 0))

    # NOTE: currently errors with this 
    # log_info(paste0("Unique Values for 'new_ta' column:",unique(mphc_data_no_gps$new_ta)))
    # log_info(paste0("Unique Values for 'new_ea' column:",unique(mphc_data_no_gps$new_ea)))

    #Create EA_CODE by concatenating district, new_ta and new_ea code
    mphc_data_no_gps <- mphc_data_no_gps %>%  
    mutate(EA_CODE = str_c(district, new_ta, new_ea),
            new_ta_ea = str_c(new_ta, new_ea),
            unique_hh_id = str_c(EA_CODE, hhnumber))

    ## Summarise no gps data at EA 

    #total population
    mphc_pop_no_gps <- mphc_data_no_gps %>%  
    group_by(EA_CODE) %>%  
    summarise(mphc_total_pop = sum(no_persons, na.rm = T),
                mphc_hh_count = n_distinct(hhnumber),   #Distinct count of household
                male_count   = sum(p03 == 1, na.rm = TRUE),
                female_count = sum(p03 == 2, na.rm = TRUE))
    
    # summarise EA counts by age
    # this works on it's own but not here?
    age_summary <- summarise_age(mphc_pop_no_gps)

    #Join age summary to population data
    mphc_pop_no_gps <- mphc_pop_no_gps %>%  
    left_join(age_summary, by = "EA_CODE")

    return(mphc_pop_no_gps)
}


#' Summarise EA counts by age column for the given dataset.
#'
#' @param df (tibble) DataFrame containing data to be summarised by age.
#' @param age_col (character) name of the column containing age. Defaults to
#'      "p05".
#' @param ea_col (character, Optional) name contianing EA identifying code. 
#'      Defaults to "EA_CODE".
#' 
#' @return age_summary_df (tibble, Optional) Dataframe containing a count of 
#'      EA's by age. 
summarise_age <- function(df, age_col = "p05", ea_col = "EA_CODE") {
    
    #Create a bin for each age category
    age_summary_df <- df %>%  
    mutate(age_group = case_when(
        df[[age_col]] < 1  ~ "age_group_01_less",      #less than 1
        df[[age_col]] >= 1 & df[[age_col]] <= 4   ~ "age_group_01_04",
        df[[age_col]] >= 5 & df[[age_col]] <= 9   ~ "age_group_05_09",
        df[[age_col]] >= 10 & df[[age_col]] <= 14 ~ "age_group_10_14",
        df[[age_col]] >= 15 & df[[age_col]] <= 19 ~ "age_group_15_19",
        df[[age_col]] >= 20 & df[[age_col]] <= 24 ~ "age_group_20_24",
        df[[age_col]] >= 25 & df[[age_col]] <= 29 ~ "age_group_25_29",
        df[[age_col]] >= 30 & df[[age_col]] <= 34 ~ "age_group_30_34",
        df[[age_col]] >= 35 & df[[age_col]] <= 39 ~ "age_group_35_39",
        df[[age_col]] >= 40 & df[[age_col]] <= 44 ~ "age_group_40_44",
        df[[age_col]] >= 45 & df[[age_col]] <= 49 ~ "age_group_45_49",
        df[[age_col]] >= 50 & df[[age_col]] <= 54 ~ "age_group_50_54",
        df[[age_col]] >= 55 & df[[age_col]] <= 59 ~ "age_group_55_59",
        df[[age_col]] >= 60 & df[[age_col]] <= 64 ~ "age_group_60_64",
        df[[age_col]] >= 65 & df[[age_col]] <= 69 ~ "age_group_65_69",
        df[[age_col]] >= 70 & df[[age_col]] <= 74 ~ "age_group_70_74",
        df[[age_col]] >= 75 & df[[age_col]] <= 79 ~ "age_group_75_79",
        df[[age_col]] >= 80             ~ "age_group_80plus",
        TRUE ~ NA_character_
    )) %>%  
    # summarise counts per EA_CODE × age_group
    group_by(EA_CODE, age_group) %>%  
    summarise(count = n(), .groups = "drop") %>%  
    arrange(EA_CODE, age_group)%>%  
    #Pivot to wide columns
    pivot_wider(
        names_from = age_group,
        values_from = count,
        values_fill = 0
    ) %>%  
    arrange(EA_CODE)


    return(age_summary_df)
}

#' Checks that the mphc geopakage exists and creates one if not.
#' 
#' Both the 'output_path' 'gpkg_file_path' will be replaced by the config's geopak
#' 
#' @param mphc_df (tibble) dataframe containing census data. Only used when no
#'      gpkg existing
#' @param input path (filepath-like, Optional) File path where the mphc data can
#'      should b. Defaults to "mphc_2018_sf_ea.gpkg"
check_mphc_gpkg_exists <- function(mphc_df, input_path, sources ){

    
    mphc_2018_sf_filepath <- file.path(config$paths$output_path, gpkg_file_path)
    if (!file.exists(mphc_2018_sf_filepath)) {
    print("The geopackage version of mphc_2018 is not available, creating dataframe and saving to disk.")
    
    # Convert remaining mphc_2018 data to shapefiles
    
    #Convert to sf object
    mphc_2018_sf <- mphc_df %>%
    drop_na(hh_longitude, hh_latitude) %>%
    st_as_sf(coords = c("hh_longitude", "hh_latitude"))
    
    #set the spatial reference
    st_crs(mphc_2018_sf) <- 4326

    #Fix corrupt geometries
    st_make_valid(ea)

    #Turn off invalid geometries
    sf::sf_use_s2(FALSE)

    #transform
    mphc_2018_sf <- st_transform(mphc_2018_sf, crs = st_crs(ea))

    # EA Nearest Neighbor Assignment
    nearest_indices <- st_nearest_feature(mphc_2018_sf, ea)

    # Extract the EA_CODE  of the nearest polygons
    nearest_ids <- ea$EA_CODE[nearest_indices]

    # Add the EA_CODE to data
    mphc_2018_sf$EA_CODE <- nearest_ids

    #Write to file
    st_write(mphc_2018_sf ,
    dsn = file.path(output_path, "mphc_2018_sf_ea.gpkg"),
    driver = "GPKG",
    delete_layer = TRUE
    )
    }
}