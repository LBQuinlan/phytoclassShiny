# ============================================================================
#
#   _phytoclass_Shiny V1.0 - QUALITY CONTROL ENGINE
#
#   Description:
#   Performs the heavy lifting for Step 4. It scans your data to flag duplicates,
#   fix missing values, and apply the environmental filters (Depth, Location,
#   Date) you defined in the Setup.
#
# ============================================================================




#' Execute the QC and Filtering Pipeline
#'
#' @param datasets_processed A list of dataset objects (from the Loader).
#' @param config The global configuration object.
#' @return A list containing `datasets_annotated` (with status columns) and `master_qc_data` (clean combined data).
run_qc_pipeline <- function(datasets_processed, config) {
  
  datasets_annotated <- list()
  master_list <- list()
  
  # Extract relevant config sections for easier access
  cleaning_cfg <- config$data_cleaning
  filter_cfg <- config$filtering
  meta_keys <- config$general$metadata_keys %||% character(0)
  
  for (ds_name in names(datasets_processed)) {
    ds_obj <- datasets_processed[[ds_name]]
    data <- ds_obj$data
    rename_map <- ds_obj$rename_map
    
    # --- 0a. COLUMN STANDARDIZATION (THE FIX) ---
    # Rename the mapped columns in the data to their Standard Keys (e.g. "Fuco")
    # This ensures that when we bind rows later, "Fuco" aligns with "Fuco".
    
    pigment_keys <- setdiff(names(config$column_aliases), meta_keys)
    
    # We only rename pigments and essential meta keys to keep it clean
    keys_to_standardize <- c(pigment_keys, "latitude", "longitude", "depth", "date", "time", "year", "month", "day")
    
    for (key in keys_to_standardize) {
      if (key %in% names(rename_map)) {
        actual_col <- rename_map[[key]]
        # Check if this column exists and isn't already named 'key'
        if (!is.null(actual_col) && actual_col %in% colnames(data) && actual_col != key) {
          # Rename it
          data <- dplyr::rename(data, !!key := !!sym(actual_col))
          # Update the map so downstream logic knows 'key' is now at 'key'
          rename_map[[key]] <- key 
        }
      }
    }
    
    # Now that columns are standardized, valid_pigment_cols are just the keys present in data
    valid_pigment_cols <- intersect(pigment_keys, colnames(data))
    
    # --- 0b. MANDATORY TYPE COERCION (BRUTE FORCE) ---
    if (length(valid_pigment_cols) > 0) {
      for (col in valid_pigment_cols) {
        vals <- safe_as_numeric(data[[col]])
        if (is.list(vals)) vals <- unlist(vals)
        data[[col]] <- vals
      }
    }
    
    # --- 1. Data Cleaning ---
    
    # Initialize status columns
    data$cleaning_status <- "Keep"
    data$duplicate_status <- "Keep_Unique"
    
    if (length(valid_pigment_cols) > 0) {
      
      # A. Handle NAs
      if (isTRUE(cleaning_cfg$handle_pigment_nas$enabled)) {
        for (col in valid_pigment_cols) {
          vals <- data[[col]]
          if (any(is.na(vals))) {
            vals[is.na(vals)] <- 0
            data[[col]] <- vals
          }
        }
      } else {
        rows_with_na <- rowSums(is.na(data[, valid_pigment_cols, drop=FALSE])) > 0
        data$cleaning_status[rows_with_na] <- "Flagged_NA"
      }
      
      # B. Handle Negatives
      if (isTRUE(cleaning_cfg$enforce_non_negative_pigments$enabled)) {
        for (col in valid_pigment_cols) {
          vals <- data[[col]]
          neg_indices <- which(!is.na(vals) & vals < 0)
          if (length(neg_indices) > 0) {
            vals[neg_indices] <- 0
            data[[col]] <- vals
          }
        }
      }
      
      # C. Handle Zero-Sum Rows
      if (isTRUE(cleaning_cfg$handle_zero_pigment_sum$enabled)) {
        tryCatch({
          row_sums <- rowSums(data[, valid_pigment_cols, drop = FALSE], na.rm = TRUE)
          data$cleaning_status[row_sums <= 1e-9] <- "Flagged_ZeroSum"
        }, error = function(e) {
          # Log but don't crash, assume keeps
          warning("rowSums check failed: ", e$message)
        })
      }
    }
    
    # D. Handle Duplicates (using standardized keys)
    if (isTRUE(cleaning_cfg$handle_duplicates$enabled)) {
      # Since we renamed them, we can look for "latitude" directly if it exists
      lat_col <- if("latitude" %in% colnames(data)) "latitude" else NULL
      lon_col <- if("longitude" %in% colnames(data)) "longitude" else NULL
      depth_col <- if("depth" %in% colnames(data)) "depth" else NULL
      
      cols_for_sig <- c(valid_pigment_cols, lat_col, lon_col, depth_col, "year", "month", "day")
      cols_for_sig <- intersect(cols_for_sig, colnames(data))
      
      if (length(cols_for_sig) > 0) {
        is_dup <- duplicated(data[, cols_for_sig])
        data$duplicate_status[is_dup] <- "Flagged_Duplicate"
      }
    }
    
    # --- 2. Environmental Filtering ---
    data$filter_status_geo <- "Keep"
    data$filter_status_temporal <- "Keep"
    data$filter_status_depth <- "Keep"
    
    # A. Geospatial
    if (isTRUE(filter_cfg$geospatial$enabled)) {
      if ("latitude" %in% colnames(data) && "longitude" %in% colnames(data)) {
        lat <- safe_as_numeric(data$latitude)
        lon <- safe_as_numeric(data$longitude)
        
        bad_geo <- is.na(lat) | is.na(lon) |
          lat < filter_cfg$geospatial$min_latitude | lat > filter_cfg$geospatial$max_latitude |
          lon < filter_cfg$geospatial$min_longitude | lon > filter_cfg$geospatial$max_longitude
        
        data$filter_status_geo[bad_geo] <- "Flagged_GeoRange"
      }
    }
    
    # B. Temporal
    if (isTRUE(filter_cfg$temporal$enabled)) {
      if (all(c("year", "month", "day") %in% colnames(data))) {
        dates <- tryCatch(
          as.Date(paste(data$year, data$month, data$day, sep="-")),
          error = function(e) rep(NA, nrow(data))
        )
        start_d <- as.Date(filter_cfg$temporal$start_date)
        end_d <- as.Date(filter_cfg$temporal$end_date)
        bad_time <- is.na(dates) | dates < start_d | dates > end_d
        data$filter_status_temporal[bad_time] <- "Flagged_DateRange"
      }
    }
    
    # C. Depth
    if (isTRUE(filter_cfg$depth$enabled)) {
      if ("depth" %in% colnames(data)) {
        d <- safe_as_numeric(data$depth)
        bad_depth <- is.na(d) | d < filter_cfg$depth$min_depth | d > filter_cfg$depth$max_depth
        data$filter_status_depth[bad_depth] <- "Flagged_DepthRange"
      }
    }
    
    # --- 3. Final Aggregation ---
    
    data$qc_pass <- (
      data$cleaning_status == "Keep" &
        data$duplicate_status == "Keep_Unique" &
        data$filter_status_geo == "Keep" &
        data$filter_status_temporal == "Keep" &
        data$filter_status_depth == "Keep"
    )
    
    ds_obj$data_annotated <- data
    ds_obj$data <- data %>% filter(qc_pass)
    # Update rename_map in the object to reflect the standard keys are now the actual columns
    
    datasets_annotated[[ds_name]] <- ds_obj
    
    if (nrow(ds_obj$data) > 0) {
      master_list[[ds_name]] <- ds_obj$data
    }
  }
  
  # Binding rows is now safe because columns have been standardized!
  master_qc_data <- if (length(master_list) > 0) dplyr::bind_rows(master_list) else NULL
  
  return(list(
    datasets_annotated = datasets_annotated,
    master_qc_data = master_qc_data
  ))
}