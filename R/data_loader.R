# ============================================================================
#
#   _phytoclass_Shiny V1.0 - DATA IMPORTER
#
#   Description:
#   Responsible for reading your Excel files. It cleans up column names and
#   standardizes date formats to ensure your data is ready for analysis.
#
# ============================================================================



load_all_files <- function(file_input_df, config, session_log_f) {
  datasets <- list()
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = "Loading datasets...", value = 0)
  
  for (i in 1:nrow(file_input_df)) {
    file_info <- file_input_df[i, ]
    dataset_id <- tools::file_path_sans_ext(file_info$name)
    progress$inc(1/nrow(file_input_df), detail = paste("Processing", dataset_id))
    tryCatch({
      datasets[[dataset_id]] <- .load_standardize_single_file(file_info$datapath, dataset_id, config)
    }, error = function(e) {
      error_msg <- sprintf("Failed to load '%s': %s", dataset_id, e$message)
      session_log_f(paste("ERROR:", error_msg))
      showNotification(error_msg, type = "error", duration = 15)
    })
  }
  return(datasets)
}

.load_standardize_single_file <- function(file_path, dataset_name, config) {
  # Read as text to prevent crashes
  raw_data <- readxl::read_excel(file_path, sheet = 1, .name_repair = "unique", col_types = "text")
  if (is.null(raw_data) || nrow(raw_data) == 0) stop("File is empty.")
  
  original_colnames <- colnames(raw_data)
  cleaned_colnames <- make.names(original_colnames, unique = TRUE)
  colnames(raw_data) <- cleaned_colnames
  
  processed_data <- raw_data %>%
    mutate(
      source_dataset = dataset_name,
      original_row_num = row_number(), 
      UniqueID = create_unique_id(dataset_name, .data$original_row_num), 
      .before = 1
    )
  
  # Map columns
  map_result <- .map_columns(processed_data, original_colnames, config$column_aliases)
  
  # Standardize Dates
  processed_data <- .standardize_datetime_columns(processed_data, map_result$rename_map)
  
  # CRITICAL FIX: Update map if standardized date columns were created.
  # This overwrites the map to point to the new numeric columns (year/month/day),
  # preventing name collisions in Step 4.
  if ("year" %in% colnames(processed_data)) map_result$rename_map$year <- "year"
  if ("month" %in% colnames(processed_data)) map_result$rename_map$month <- "month"
  if ("day" %in% colnames(processed_data)) map_result$rename_map$day <- "day"
  
  return(list(
    name = dataset_name,
    data = as_tibble(processed_data),
    data_original = as_tibble(raw_data),
    original_colnames = original_colnames,
    cleaned_colnames = cleaned_colnames,
    rename_map = map_result$rename_map,
    log = list(initial_rows = nrow(raw_data), initial_cols = ncol(raw_data))
  ))
}

.map_columns <- function(data, original_colnames, aliases_config) {
  rename_map <- list()
  for (std_key in names(aliases_config)) {
    aliases <- unlist(aliases_config[[std_key]])
    matches <- original_colnames[tolower(original_colnames) %in% tolower(aliases)]
    if (length(matches) > 0) {
      rename_map[[std_key]] <- make.names(matches[1], unique=TRUE)
    }
  }
  return(list(rename_map = rename_map))
}

.standardize_datetime_columns <- function(data, rename_map) {
  
  .clean_ymd <- function(d) {
    d %>% mutate(year=safe_as_numeric(year), month=safe_as_numeric(month), day=safe_as_numeric(day))
  }
  
  # 1. Check for explicit Year/Month/Day
  has_ymd <- all(c("year", "month", "day") %in% names(rename_map))
  if (has_ymd) {
    data <- data %>%
      mutate(
        year = !!sym(rename_map$year),
        month = !!sym(rename_map$month),
        day = !!sym(rename_map$day)
      )
    return(.clean_ymd(data))
  }
  
  # 2. Check for single 'Date' column
  has_date <- "date" %in% names(rename_map)
  if (has_date) {
    date_col <- rename_map$date
    raw_dates <- data[[date_col]]
    
    date_formats <- c("Ymd", "ymd", "mdY", "mdy", "dmY", "dmy", "Ymd HMS", "ymd HMS")
    parsed <- lubridate::parse_date_time(raw_dates, orders = date_formats, quiet = TRUE)
    
    # Excel Serial Fallback
    na_indices <- which(is.na(parsed))
    if (length(na_indices) > 0) {
      raw_subset <- raw_dates[na_indices]
      numeric_vals <- suppressWarnings(as.numeric(raw_subset))
      valid_mask <- !is.na(numeric_vals) & numeric_vals > 1000
      
      if (any(valid_mask)) {
        restored_dates <- as.POSIXct(as.Date(numeric_vals[valid_mask], origin = "1899-12-30"))
        parsed[na_indices[valid_mask]] <- restored_dates
      }
    }
    
    data$year <- lubridate::year(parsed)
    data$month <- lubridate::month(parsed)
    data$day <- lubridate::day(parsed)
    
    return(.clean_ymd(data))
  }
  
  # 3. Fallback
  data$year <- NA_real_; data$month <- NA_real_; data$day <- NA_real_
  return(data)
}