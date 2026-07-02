library(sf)

generate_buffered_country_boundary <- function(
    shape_path,
    file_name,
    buffer
){
  #' Produce buffered country boundary shapefile by dissolving EA geometries and
  #' adding 10km buffer
  #' 
  ea_geoms <- st_read(file.path(shape_path, "2018_MPHC_EAs_Final_for_Use.shp"))
  country_buffer <- ea_geoms %>% 
    st_union() %>% 
    st_buffer(buffer)
  st_write(country_buffer, file.path(shape_path, file_name))
  return(country_buffer)
}

rbind_zomba_csvs <- function(csv_dir, output_file){
  #' Produce rbind csv file for zomba from 14 files provided by NSO in "zomba_csv"
  #' folder. 
  #' Headers are normalised, NA rows are removed, and then rbound together to
  #' make "zomba_rbind_data.csv" file.
  if (!dir.exists(csv_dir)) {
    stop("Directory does not exist: ", csv_dir)
  }

  output_path <- file.path(output_file)
  csv_files <- list.files(
    csv_dir,
    pattern = "\\.csv$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  csv_files <- sort(csv_files)

  if (length(csv_files) == 0) {
    stop("No CSV files found in: ", csv_dir)
  }

  zomba_data_list <- lapply(
    csv_files,
    function(csv_file) {
      zomba_data <- read.csv(csv_file, stringsAsFactors = FALSE, check.names = FALSE)
      # change headers to lowercase and replace spaces to _ to normalise
      names(zomba_data) <- gsub("\\s+", "_", tolower(trimws(names(zomba_data))))
      # "Nkapita.csv" had "household_name" instead of "household_number" like
      # the rest
      names(zomba_data)[names(zomba_data) == "household_name"] <- "household_number"
      # remove rows where every field is blank or missing
      empty_rows <- apply(
        zomba_data,
        1,
        function(row) all(is.na(row) | trimws(as.character(row)) == "")
      )
      zomba_data <- zomba_data[!empty_rows, , drop = FALSE]
      zomba_data
    }
  )
  
  # Look at the first file and determine what columns are expected
  expected_columns <- names(zomba_data_list[[1]])
  
  matching_columns <- vapply(
    zomba_data_list,
    function(zomba_data) identical(names(zomba_data), expected_columns),
    logical(1)
  )

  # Check that every CSV has the same normalized columns as the first file and
  # stop with the filenames of any CSVs that still do not match.
  if (!all(matching_columns)) {
    mismatched_files <- basename(csv_files[!matching_columns])
    stop(
      "CSV files do not share identical columns and cannot be row-bound with rbind: ",
      paste(mismatched_files, collapse = ", ")
    )
  }

  zomba_rbind_data <- do.call(rbind, zomba_data_list)
  write.csv(zomba_rbind_data, output_path, row.names = FALSE)

  return()
}
