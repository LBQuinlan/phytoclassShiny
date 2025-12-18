# ============================================================================
#
#   _phytoclass_Shiny V1.0 - HELPER TOOLS
#
#   Description:
#   A collection of background tools used by the app to perform calculations,
#   load the Fm matrices, and generate text summaries.
#
# ============================================================================


# --- General Purpose Operators & Helpers ---

`%||%` <- function(a, b) if (!is.null(a)) a else b

safe_as_numeric <- function(x) {
  if (is.numeric(x)) return(x)
  suppressWarnings(as.numeric(as.character(x)))
}


# --- Data Loading & Mapping Helpers ---

create_unique_id <- function(dataset_name, row_number) {
  clean_dataset_name <- gsub("[^A-Za-z0-9_\\-]", "_", as.character(dataset_name))
  formatted_row_number <- sprintf("%05d", as.integer(row_number))
  paste(clean_dataset_name, "Row", formatted_row_number, sep = "_")
}

.get_mapping_status <- function(ds_obj, essential_keys) {
  if (is.null(ds_obj)) return(list(Dataset = "Unknown", `Mapping Health` = "ERROR", Missing = list(character(0))))
  map <- ds_obj$rename_map %||% list(); resolved_keys <- names(map)
  missing <- setdiff(essential_keys, resolved_keys)
  date_keys_present <- "date" %in% resolved_keys || all(c("year", "month", "day") %in% resolved_keys)
  date_keys_required <- any(c("year", "month", "day") %in% essential_keys)
  if (date_keys_required && !date_keys_present) missing <- union(missing, "date/time info")
  else if (date_keys_present) missing <- setdiff(missing, c("date", "year", "month", "day"))
  status <- if (length(missing) == 0) "OK" else "MISSING ESSENTIALS"
  return(list(Dataset = ds_obj$name, `Mapping Health` = status, Missing = list(missing)))
}

update_config_with_new_aliases <- function(config, datasets_processed) {
  if (length(datasets_processed) == 0) return(config)
  cat("--- [Config] Scanning for new aliases to save...\n")
  new_aliases_found <- FALSE
  for(ds in datasets_processed) {
    if (is.null(ds$rename_map) || length(ds$rename_map) == 0) next
    for(key in names(ds$rename_map)) {
      mapped_clean_col <- ds$rename_map[[key]]
      if (is.null(mapped_clean_col) || mapped_clean_col == "") next
      original_col_idx <- which(ds$cleaned_colnames == mapped_clean_col)
      if (length(original_col_idx) > 0) {
        original_col_name <- ds$original_colnames[original_col_idx[1]]
        if (length(original_col_name) == 0 || is.na(original_col_name) || original_col_name == "") next
        existing_aliases <- config$column_aliases[[key]]
        if (is.null(existing_aliases)) existing_aliases <- list() else if (!is.list(existing_aliases)) existing_aliases <- as.list(unlist(existing_aliases))
        flat_aliases <- unlist(existing_aliases)
        if (is.null(flat_aliases)) flat_aliases <- character(0)
        if (!original_col_name %in% flat_aliases) {
          config$column_aliases[[key]] <- c(flat_aliases, original_col_name); new_aliases_found <- TRUE
          cat(sprintf("     - Found new alias for '%s': '%s'\n", key, original_col_name))
        }
      }
    }
  }
  if (!new_aliases_found) cat("     ...No new aliases found to save.\n")
  return(config)
}


# --- Data Cleaning & Analysis Helpers ---

check_resolution_capabilities <- function(ds_obj, config, fm_matrices) {
  if (is.null(fm_matrices) || is.null(fm_matrices$Fm_Pro)) return(NULL)
  warnings_list <- character(0); data <- ds_obj$data; rename_map <- ds_obj$rename_map
  dvchla_key <- "Dvchla"; has_dvchla <- dvchla_key %in% names(rename_map)
  use_pro <- FALSE
  if (has_dvchla) { col_name <- rename_map[[dvchla_key]]; if (sum(safe_as_numeric(data[[col_name]]), na.rm = TRUE) > 0) use_pro <- TRUE }
  fm_matrix <- if(use_pro) fm_matrices$Fm_Pro else fm_matrices$Fm_NoPro
  required_pigments <- colnames(fm_matrix); available_pigments <- character(0)
  for (pig in required_pigments) {
    user_col <- NA
    if (pig %in% names(rename_map)) user_col <- rename_map[[pig]] else { for (key in names(config$column_aliases)) { if (pig %in% config$column_aliases[[key]]) { if (key %in% names(rename_map)) user_col <- rename_map[[key]]; break } } }
    if (!is.na(user_col)) { if (sum(safe_as_numeric(data[[user_col]]), na.rm = TRUE) > 1e-9) available_pigments <- c(available_pigments, pig) }
  }
  phyto_classes <- rownames(fm_matrix)
  for (cls in phyto_classes) {
    class_ratios <- fm_matrix[cls, ]; needed <- names(class_ratios)[class_ratios > 0]
    missing_for_class <- setdiff(needed, available_pigments)
    if (length(missing_for_class) > 0) { msg <- paste0("Class '", cls, "' cannot be determined. Missing: ", paste(missing_for_class, collapse=", ")); warnings_list <- c(warnings_list, msg) }
  }
  return(warnings_list)
}

create_signature_hash <- function(row_vector, precision = 6, threshold = 1e-9) {
  if (!requireNamespace("digest", quietly = TRUE)) stop("Package 'digest' is required.")
  valid_indices <- which(is.finite(row_vector) & abs(row_vector) > threshold)
  if (length(valid_indices) == 0) return(NA_character_)
  col_names <- names(row_vector)[valid_indices]; values_str <- formatC(row_vector[valid_indices], format = "f", digits = precision)
  sorted_indices <- order(col_names); sorted_pairs <- paste(col_names[sorted_indices], values_str[sorted_indices], sep = ":", collapse = "|")
  return(digest::digest(sorted_pairs, algo = "md5"))
}

load_fm_matrices <- function(config) {
  workspace_cfg <- config$workspace %||% list(); fm_pro_path <- workspace_cfg$fm_pro_matrix_path %||% ""; fm_nopro_path <- workspace_cfg$fm_nopro_matrix_path %||% ""
  .load_single_fm <- function(file_path, matrix_name) {
    if (!nzchar(file_path)) return(list(error = paste(matrix_name, "path is empty.")))
    if (!file.exists(file_path)) return(list(error = paste(matrix_name, "file not found at:", file_path)))
    tryCatch({
      fm_df <- readxl::read_excel(file_path, sheet = 1, .name_repair = "unique")
      if (colnames(fm_df)[1] == "...1") { warning(paste("Warning:", basename(file_path), "is missing a header in cell A1. Assuming first column is 'Phytoplankton_Class'."), call. = FALSE); colnames(fm_df)[1] <- "Phytoplankton_Class" }
      phyto_classes <- make.unique(as.character(fm_df[[1]])); fm_matrix <- as.matrix(fm_df[, -1, drop = FALSE]); rownames(fm_matrix) <- phyto_classes; fm_matrix[!is.finite(fm_matrix)] <- 0; fm_matrix[fm_matrix < 0] <- 0
      return(fm_matrix)
    }, error = function(e) { return(list(error = paste("Failed to read", matrix_name, "file:", e$message))) })
  }
  Fm_Pro <- .load_single_fm(fm_pro_path, "Fm_Pro"); Fm_NoPro <- .load_single_fm(fm_nopro_path, "Fm_NoPro")
  if (is.list(Fm_Pro) || is.list(Fm_NoPro)) { error_msg <- c(if(is.list(Fm_Pro)) Fm_Pro$error, if(is.list(Fm_NoPro)) Fm_NoPro$error); return(list(error = paste(error_msg, collapse = " "))) }
  return(list(Fm_Pro = Fm_Pro, Fm_NoPro = Fm_NoPro, error = NULL))
}

sanitize_pigment_data <- function(data, rename_map, config) {
  meta_keys <- config$general$metadata_keys; pigment_keys <- setdiff(names(config$column_aliases), meta_keys)
  pigment_cols_in_data <- unlist(rename_map[pigment_keys]); pigment_cols_in_data <- pigment_cols_in_data[!is.na(pigment_cols_in_data)]
  if (length(pigment_cols_in_data) == 0) return(data)
  data %>% mutate(across(any_of(pigment_cols_in_data), ~ { vals <- safe_as_numeric(.); vals[!is.finite(vals)] <- 0; vals[vals < 0] <- 0; return(vals) }))
}

calculate_sMAPE_R2 <- function(S_actual, C_estimated, F_estimated) {
  if(is.null(S_actual) || is.null(C_estimated) || is.null(F_estimated)) return(list(mean_sMAPE = NA, R_squared = NA))
  S_estimated <- C_estimated %*% F_estimated
  if (!all(dim(S_actual) == dim(S_estimated))) return(list(mean_sMAPE = NA, R_squared = NA))
  numerator <- abs(S_actual - S_estimated); denominator <- (abs(S_actual) + abs(S_estimated)) / 2
  sMAPE_matrix <- ifelse(denominator < 1e-9, 0, numerator / denominator); mean_sMAPE <- mean(sMAPE_matrix, na.rm = TRUE) * 100
  actual_flat <- as.vector(S_actual); estimated_flat <- as.vector(S_estimated)
  correlation <- cor(actual_flat, estimated_flat, use = "complete.obs"); r_squared <- ifelse(is.na(correlation), 0, correlation^2)
  return(list(mean_sMAPE = mean_sMAPE, R_squared = r_squared))
}

generate_run_summary_text <- function(config, master_qc_data, analysis_datasets, cluster_results_log) {
  
  qc_rules <- c(
    if (isTRUE(config$data_cleaning$handle_duplicates$enabled)) "Duplicates",
    if (isTRUE(config$data_cleaning$handle_pigment_nas$enabled)) "NAs",
    if (isTRUE(config$data_cleaning$enforce_non_negative_pigments$enabled)) "Negatives",
    if (isTRUE(config$data_cleaning$handle_zero_pigment_sum$enabled)) "Zero-Sum"
  )
  
  filters <- c(
    if (isTRUE(config$filtering$geospatial$enabled)) "Geospatial",
    if (isTRUE(config$filtering$temporal$enabled)) "Temporal",
    if (isTRUE(config$filtering$depth$enabled)) "Depth"
  )
  
  # *** INTELLIGENT STRATEGY DISPLAY ***
  if (!is.null(cluster_results_log)) {
    # If this log exists, Clustering was selected in Step 5
    method <- config$clustering$cluster_method %||% "Unknown"
    norm <- config$clustering$normalization_method %||% "Unknown"
    trans <- config$clustering$transformation_method %||% "Unknown"
    
    # Prettify labels
    if(method == "hclust") method <- "Hierarchical (Ward.D2)"
    if(method == "kmeans") method <- "K-Means"
    if(norm == "tchla") norm <- "Ratio to Tchla"
    if(norm == "none") norm <- "Raw Concentration"
    if(trans == "log1p") trans <- "Log(x+1)"
    
    strategy_block <- paste(
      "\n--- Analysis Strategy (CLUSTERING) ---",
      paste("Method:", method),
      paste("Normalization:", norm),
      paste("Transformation:", trans),
      paste("Total Clusters:", length(analysis_datasets)),
      sep = "\n"
    )
  } else {
    # Default Source File strategy
    strategy_block <- paste(
      "\n--- Analysis Strategy ---",
      "Grouping Method: By Source File",
      paste("Analysis Groups:", length(analysis_datasets)),
      sep = "\n"
    )
  }
  
  paste(
    "--- QC & Filtering ---",
    paste("Samples Passing:", nrow(master_qc_data)),
    paste("QC Rules On:", paste(qc_rules, collapse=", ")),
    paste("Filters On:", if(length(filters) > 0) paste(filters, collapse=", ") else "None"),
    strategy_block,
    "\n--- Phytoclass Parameters ---",
    paste("Niter:", config$phytoclass$niter),
    paste("Step Size:", config$phytoclass$step_size),
    sep = "\n"
  )
}