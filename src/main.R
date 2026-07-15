# ---
# file will pull in all parts of the modelling pipeline
source("src/data_processing/data_processing_2.R")
source("src/quality_assurance/output_summary.R")
source("utils.R")

# ----
# Load in vars from config needed in main
config <- load_config()

# ----
# section 1
# previous data processing steps go here, raster mosaicking etc.

# call in data processing
data_processing_2_function()

qa_output_dir <- file.path(config$paths$drive_path, "quality_assurance")

# Run QA for every preprocessing QA block in config and create one combined summary.
qa_block_names <- names(config)[grepl("_qa$", names(config))]
all_preprocessing_qa_summary <- data.frame()

for (qa_block_name in qa_block_names) {
    qa_cfg <- config[[qa_block_name]]

    required_fields <- c(
        "report_prefix",
        "baseline_summarized_csv",
        "baseline_hh_size_gpkg"
    )
    missing_fields <- setdiff(required_fields, names(qa_cfg))

    if (length(missing_fields) > 0) {
        stop(
            paste0(
                "Missing required fields in config block '",
                qa_block_name,
                "': ",
                paste(missing_fields, collapse = ", ")
            )
        )
    }

    current_summarized_csv_filepath <- if (!is.null(qa_cfg$current_summarized_csv_output_key)) {
        output_key <- qa_cfg$current_summarized_csv_output_key
        if (is.null(config$outputs[[output_key]])) {
            stop(paste0("Output key not found in config$outputs: ", output_key))
        }
        config$outputs[[output_key]]
    } else {
        qa_cfg$current_summarized_csv
    }

    current_hh_size_gpkg_filepath <- if (!is.null(qa_cfg$current_hh_size_gpkg_output_key)) {
        output_key <- qa_cfg$current_hh_size_gpkg_output_key
        if (is.null(config$outputs[[output_key]])) {
            stop(paste0("Output key not found in config$outputs: ", output_key))
        }
        config$outputs[[output_key]]
    } else {
        qa_cfg$current_hh_size_gpkg
    }

    if (is.null(current_summarized_csv_filepath) || is.null(current_hh_size_gpkg_filepath)) {
        stop(
            paste0(
                "QA block '",
                qa_block_name,
                "' must define either current path fields or *_output_key fields."
            )
        )
    }

    qa_result <- run_parity_qa(
        output_csv_path = current_summarized_csv_filepath,
        output_gpkg_path = current_hh_size_gpkg_filepath,
        baseline_csv_path = qa_cfg$baseline_summarized_csv,
        baseline_gpkg_path = qa_cfg$baseline_hh_size_gpkg,
        qa_output_dir = qa_output_dir,
        report_prefix = qa_cfg$report_prefix
    )

    qa_summary_row <- as.data.frame(qa_result$summary)
    qa_summary_row$qa_block <- qa_block_name
    qa_summary_row$report_prefix <- qa_cfg$report_prefix

    all_preprocessing_qa_summary <- rbind(all_preprocessing_qa_summary, qa_summary_row)
}

write.csv(
    all_preprocessing_qa_summary,
    file.path(qa_output_dir, "all_preprocessing_parity_summary.csv"),
    row.names = FALSE
)

print(all_preprocessing_qa_summary)

# user inputs prompt questions
## does checks look good?
##
# ----

    ## Further pipeline