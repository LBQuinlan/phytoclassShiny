`%||%` <- function(a, b) if (!base::is.null(a)) a else b

safe_as_numeric <- function(x) { 
  if (base::is.numeric(x)) return(x)
  base::suppressWarnings(base::as.numeric(base::as.character(x))) 
}

create_unique_id <- function(dataset_name, row_number) { 
  clean_dataset_name <- base::gsub("[^A-Za-z0-9_\\-]", "_", base::as.character(dataset_name))
  formatted_row_number <- base::sprintf("%05d", base::as.integer(row_number))
  base::paste(clean_dataset_name, "Row", formatted_row_number, sep = "_") 
}

load_all_files <- function(file_input_df, config, session_log_f) {
  datasets <- base::list()
  for (i in 1:base::nrow(file_input_df)) {
    f_path <- file_input_df$datapath[i]
    original_name <- file_input_df$name[i]
    dataset_id <- tools::file_path_sans_ext(original_name)
    safe_name <- dataset_id
    counter <- 1
    
    while (safe_name %in% base::names(datasets)) { 
      safe_name <- base::paste0(dataset_id, "_", counter)
      counter <- counter + 1 
    }
    
    tryCatch({ 
      datasets[[safe_name]] <- .load_standardize_single_file(f_path, safe_name, config) 
    }, error = function(e) { 
      error_msg <- base::sprintf("Failed to load '%s': %s", original_name, e$message)
      session_log_f(base::paste("ERROR:", error_msg))
      shiny::showNotification(error_msg, type = "error", duration = 15) 
    })
  }
  return(datasets)
}

.load_standardize_single_file <- function(file_path, dataset_name, config) {
  raw_data <- readxl::read_excel(file_path, sheet = 1, .name_repair = "unique", col_types = "text")
  if (base::is.null(raw_data) || base::nrow(raw_data) == 0) base::stop("File is empty.")
  
  original_colnames <- base::colnames(raw_data)
  
  # --- SMART EMPTY HEADER RECOVERY FIX ---
  # If the first column is blank, readxl names it "...1" or similar. 
  # We recover it as Station_AutoRecovered to protect text IDs from being coerced to 0.
  if (base::grepl("^\\.\\.\\.\\d+$", original_colnames[1])) {
    original_colnames[1] <- "Station_AutoRecovered"
    base::colnames(raw_data)[1]  <- "Station_AutoRecovered"
    if (!"Station_AutoRecovered" %in% config$column_aliases$station) {
      config$column_aliases$station <- base::c(config$column_aliases$station, "Station_AutoRecovered")
    }
  }
  
  cleaned_colnames <- base::make.names(original_colnames, unique = TRUE)
  base::colnames(raw_data) <- cleaned_colnames
  
  processed_data <- raw_data |> 
    dplyr::mutate(SourceFile = dataset_name, original_row_num = dplyr::row_number(), UniqueID = create_unique_id(dataset_name, original_row_num), .before = 1)
  
  map_result <- .map_columns(processed_data, original_colnames, config$column_aliases)
  processed_data <- .standardize_datetime_columns(processed_data, map_result$rename_map)
  
  if ("year" %in% base::colnames(processed_data)) map_result$rename_map$year <- "year"
  if ("month" %in% base::colnames(processed_data)) map_result$rename_map$month <- "month"
  if ("day" %in% base::colnames(processed_data)) map_result$rename_map$day <- "day"
  
  return(base::list(
    name = dataset_name, 
    data = tibble::as_tibble(processed_data), 
    data_original = tibble::as_tibble(raw_data), 
    original_colnames = original_colnames, 
    cleaned_colnames = cleaned_colnames, 
    rename_map = map_result$rename_map, 
    log = base::list(initial_rows = base::nrow(raw_data), initial_cols = base::ncol(raw_data))
  ))
}

.map_columns <- function(data, original_colnames, aliases_config) {
  rename_map <- base::list()
  for (std_key in base::names(aliases_config)) { 
    aliases <- base::unlist(aliases_config[[std_key]])
    matches <- original_colnames[base::tolower(original_colnames) %in% base::tolower(aliases)]
    if (base::length(matches) > 0) { 
      rename_map[[std_key]] <- base::make.names(matches[1], unique=TRUE) 
    } 
  }
  return(base::list(rename_map = rename_map))
}

.standardize_datetime_columns <- function(data, rename_map) {
  .clean_ymd <- function(d) { 
    if (base::is.character(d$month) || base::is.factor(d$month)) { 
      d$month <- base::match(base::substr(base::tolower(base::as.character(d$month)), 1, 3), base::tolower(month.abb)) %||% d$month 
    }
    d |> dplyr::mutate(year = safe_as_numeric(year), month = safe_as_numeric(month), day = safe_as_numeric(day)) 
  }
  
  if (base::all(base::c("year", "month", "day") %in% base::names(rename_map))) { 
    data <- data |> dplyr::mutate(year = !!rlang::sym(rename_map$year), month = !!rlang::sym(rename_map$month), day = !!rlang::sym(rename_map$day))
    return(.clean_ymd(data)) 
  }
  
  if ("date" %in% base::names(rename_map)) {
    date_col <- rename_map$date
    raw_dates <- data[[date_col]]
    date_formats <- base::c("Ymd", "ymd", "mdY", "mdy", "dmY", "dmy", "Ymd HMS", "ymd HMS")
    parsed <- lubridate::parse_date_time(raw_dates, orders = date_formats, quiet = TRUE)
    na_indices <- base::which(base::is.na(parsed))
    
    if (base::length(na_indices) > 0) { 
      raw_subset <- raw_dates[na_indices]
      numeric_vals <- base::suppressWarnings(base::as.numeric(raw_subset))
      valid_mask <- !base::is.na(numeric_vals) & numeric_vals > 1000
      if (base::any(valid_mask)) parsed[na_indices[valid_mask]] <- base::as.POSIXct(base::as.Date(numeric_vals[valid_mask], origin = "1899-12-30")) 
    }
    
    data$year <- lubridate::year(parsed)
    data$month <- lubridate::month(parsed)
    data$day <- lubridate::day(parsed)
    return(.clean_ymd(data))
  }
  
  data$year <- NA_real_; data$month <- NA_real_; data$day <- NA_real_
  return(data)
}

run_phytoclass_analysis <- function(data_for_phyto, config, fm_matrices, rename_map = NULL) {
  dataset_name <- "UnknownDataset"
  if ("UniqueID" %in% base::colnames(data_for_phyto) && base::nrow(data_for_phyto) > 0) { 
    dataset_name <- data_for_phyto$SourceFile[1] %||% data_for_phyto$ClusterID[1] %||% base::gsub("_Row_.*$", "", data_for_phyto$UniqueID[1]) 
  }
  
  log <- base::list(status = "Not Started", rows_input_to_phyto = base::nrow(data_for_phyto), fm_matrix_used = NA_character_, seed_used = NA_character_, niter_value = NA_integer_, mean_rmse = NA, mean_condnum = NA, mean_smape = NA, r_squared = NA, error_details = NULL)
  
  if (!base::requireNamespace("phytoclass", quietly = TRUE)) base::stop("FATAL: Package 'phytoclass' is required.")
  if (log$rows_input_to_phyto == 0) { log$status <- "Skipped (No rows)"; return(base::list(results = NULL, log = log, pigment_matrix_used = NULL, f_matrix_final = NULL, phytoclass_raw = NULL)) }
  if (base::is.null(fm_matrices) || base::is.null(fm_matrices$Fm_Pro)) { log$status <- "Skipped (No Fm matrix)"; return(base::list(results = NULL, log = log, pigment_matrix_used = NULL, f_matrix_final = NULL, phytoclass_raw = NULL)) }
  
  sm_matrix <- NULL
  tryCatch({
    log$status <- "Processing"
    .get_col_name <- function(key) { 
      if (!base::is.null(rename_map) && key %in% base::names(rename_map)) { 
        col <- rename_map[[key]]
        if (!base::is.null(col) && col %in% base::colnames(data_for_phyto)) return(col) 
      }
      aliases <- config$column_aliases[[key]] %||% key
      match <- base::intersect(aliases, base::colnames(data_for_phyto))[1]
      if (!base::is.na(match)) return(match)
      match_clean <- base::intersect(base::make.names(aliases), base::colnames(data_for_phyto))[1]
      if (!base::is.na(match_clean)) return(match_clean)
      return(NA) 
    }
    
    dvchla_col <- .get_col_name("Dvchla")
    use_pro <- FALSE
    if (!base::is.na(dvchla_col)) { 
      dvchla_sum <- base::sum(safe_as_numeric(data_for_phyto[[dvchla_col]]), na.rm = TRUE)
      if (base::is.finite(dvchla_sum) && dvchla_sum > 1e-9) use_pro <- TRUE 
    }
    
    Fm_to_use <- if (use_pro) fm_matrices$Fm_Pro else fm_matrices$Fm_NoPro
    log$fm_matrix_used <- if (use_pro) "Fm_Pro" else "Fm_NoPro"
    sm_matrix <- .build_sm_matrix_advanced(data_for_phyto, base::colnames(Fm_to_use), .get_col_name)
    
    if (base::sum(sm_matrix, na.rm = TRUE) == 0) {
      log$status <- "Skipped (Zero Sum Pigments)"
      return(base::list(results = NULL, log = log, pigment_matrix_used = sm_matrix, f_matrix_final = NULL, phytoclass_raw = NULL))
    }
    
    niter_val <- base::as.integer(config$phytoclass$niter %||% 500)
    step_val <- base::as.numeric(config$phytoclass$step_size %||% 0.009)
    use_seed <- base::isTRUE(base::as.logical(config$phytoclass$use_fixed_seed))
    seed_val <- base::as.integer(config$phytoclass$fixed_seed %||% 131234)
    
    log$niter_value <- niter_val
    log$seed_used <- if(use_seed) base::as.character(seed_val) else "Unconstrained"
    
    phyto_func <- if (use_pro) phytoclass::simulated_annealing_Prochloro else phytoclass::simulated_annealing
    
    if (use_seed) { 
      if (!base::exists(".Random.seed", envir = base::.GlobalEnv)) base::set.seed(NULL)
      old_seed <- base::get(".Random.seed", envir = base::.GlobalEnv)
      base::set.seed(seed_val) 
    }
    
    phyto_raw_out <- base::suppressWarnings({ 
      phyto_func(
        Fmat = Fm_to_use, 
        S = sm_matrix, 
        user_defined_min_max = fm_matrices$min_max, 
        niter = niter_val, 
        step = step_val, 
        verbose = FALSE
      ) 
    })
    
    if (use_seed) { base::assign(".Random.seed", old_seed, envir = base::.GlobalEnv) }
    
    if (base::is.null(phyto_raw_out) || base::is.null(phyto_raw_out[["Class abundances"]])) { base::stop("Phytoclass returned a NULL result.") }
    
    results_df <- base::as.data.frame(phyto_raw_out[["Class abundances"]])
    base::colnames(results_df) <- base::paste0("Phyto_", base::make.names(base::colnames(results_df)), "_Abund")
    results_df$UniqueID <- base::rownames(phyto_raw_out[["Class abundances"]])
    results_df$Phyto_RMSE <- phyto_raw_out$RMSE
    results_df$Phyto_CondNum <- phyto_raw_out[["condition number"]]
    final_results_df <- dplyr::select(results_df, UniqueID, Phyto_RMSE, Phyto_CondNum, tidyselect::starts_with("Phyto_"))
    
    metrics <- calculate_sMAPE_R2(S_actual = sm_matrix, C_estimated = phyto_raw_out[["Class abundances"]], F_estimated = phyto_raw_out[[1]])
    
    log$status <- "Success"
    log$mean_rmse <- base::mean(final_results_df$Phyto_RMSE, na.rm = TRUE)
    log$mean_condnum <- base::mean(final_results_df$Phyto_CondNum, na.rm = TRUE)
    log$mean_smape <- metrics$mean_sMAPE
    log$r_squared <- metrics$R_squared
    
    return(base::list(results = final_results_df, log = log, pigment_matrix_used = sm_matrix, f_matrix_final = phyto_raw_out[[1]], phytoclass_raw = phyto_raw_out))
    
  }, error = function(e) { 
    log_err <- log
    log_err$status <- "Failed"
    log_err$error_details <- base::list(message = e$message)
    return(base::list(results = NULL, log = log_err, pigment_matrix_used = sm_matrix, f_matrix_final = NULL, phytoclass_raw = NULL)) 
  })
}

.build_sm_matrix_advanced <- function(data, standard_pigment_names, col_finder_func) {
  sm_df <- base::data.frame(base::matrix(0, nrow = base::nrow(data), ncol = base::length(standard_pigment_names)))
  base::colnames(sm_df) <- standard_pigment_names
  base::rownames(sm_df) <- if("UniqueID" %in% base::colnames(data)) base::as.character(data$UniqueID) else base::as.character(base::seq_len(base::nrow(data)))
  
  for (std_name in standard_pigment_names) { 
    actual_col_name <- col_finder_func(std_name)
    if (!base::is.na(actual_col_name) && actual_col_name %in% base::colnames(data)) { 
      vals <- safe_as_numeric(data[[actual_col_name]])
      vals[!base::is.finite(vals)] <- 0
      vals[vals < 0] <- 0
      sm_df[[std_name]] <- vals 
    } 
  }
  return(base::as.matrix(sm_df))
}

.get_mapping_status <- function(ds_obj, essential_keys) {
  if (base::is.null(ds_obj)) return(base::list(Dataset = "Unknown", `Mapping Health` = "ERROR", Missing = base::list(base::character(0))))
  map <- ds_obj$rename_map %||% base::list()
  resolved_keys <- base::names(map)
  missing <- base::setdiff(essential_keys, resolved_keys)
  date_keys_present <- "date" %in% resolved_keys || base::all(base::c("year", "month", "day") %in% resolved_keys)
  date_keys_required <- base::any(base::c("year", "month", "day") %in% essential_keys)
  
  if (date_keys_required && !date_keys_present) {
    missing <- base::union(missing, "date_time_info") 
  } else if (date_keys_present) {
    missing <- base::setdiff(missing, base::c("date", "year", "month", "day"))
  }
  
  status <- if (base::length(missing) == 0) "OK" else "NEEDS MAPPING"
  return(base::list(Dataset = ds_obj$name, `Mapping Health` = status, Missing = base::list(missing)))
}

update_config_with_new_aliases <- function(config, datasets_processed) {
  if (base::length(datasets_processed) == 0) return(config)
  for(ds in datasets_processed) { 
    if (base::is.null(ds$rename_map) || base::length(ds$rename_map) == 0) next
    for(key in base::names(ds$rename_map)) { 
      mapped_col <- ds$rename_map[[key]]
      if (base::is.null(mapped_col) || mapped_col == "") next
      original_idx <- base::which(ds$cleaned_colnames == mapped_col)
      if (base::length(original_idx) > 0) { 
        orig_name <- ds$original_colnames[original_idx[1]]
        existing_aliases <- base::unlist(config$column_aliases[[key]])
        if (!orig_name %in% existing_aliases) config$column_aliases[[key]] <- base::c(existing_aliases, orig_name) 
      } 
    } 
  }
  return(config)
}

check_resolution_capabilities <- function(ds_obj, config, fm_matrices) {
  if (base::is.null(fm_matrices) || base::is.null(fm_matrices$Fm_Pro)) return(NULL)
  warnings_list <- base::character(0)
  data <- ds_obj$data
  rename_map <- ds_obj$rename_map
  use_pro <- FALSE
  
  if ("Dvchla" %in% base::names(rename_map)) { 
    col_name <- rename_map[["Dvchla"]]
    if (base::sum(safe_as_numeric(data[[col_name]]), na.rm = TRUE) > 0) use_pro <- TRUE 
  }
  
  fm_matrix <- if(use_pro) fm_matrices$Fm_Pro else fm_matrices$Fm_NoPro
  available_pigments <- base::character(0)
  
  for (pig in base::colnames(fm_matrix)) { 
    user_col <- NA
    if (pig %in% base::names(rename_map)) {
      user_col <- rename_map[[pig]] 
    } else { 
      for (key in base::names(config$column_aliases)) { 
        if (pig %in% config$column_aliases[[key]]) { 
          if (key %in% base::names(rename_map)) user_col <- rename_map[[key]]
          break 
        } 
      } 
    }
    if (!base::is.na(user_col)) { 
      if (base::sum(safe_as_numeric(data[[user_col]]), na.rm = TRUE) > 1e-9) available_pigments <- base::c(available_pigments, pig) 
    } 
  }
  
  for (cls in base::rownames(fm_matrix)) { 
    class_ratios <- fm_matrix[cls, ]
    needed <- base::names(class_ratios)[class_ratios > 0]
    missing_for_class <- base::setdiff(needed, available_pigments)
    if (base::length(missing_for_class) > 0) { 
      warnings_list <- base::c(warnings_list, base::paste0("Class '", cls, "' cannot be calculated (Missing: ", base::paste(missing_for_class, collapse=", "), ")")) 
    } 
  }
  return(warnings_list)
}

# --- MINMAX LOADER FIX: Capital "Class" Enforcement ---
load_fm_matrices <- function(config) {
  fm_pro_path <- config$workspace$fm_pro_matrix_path %||% ""
  fm_nopro_path <- config$workspace$fm_nopro_matrix_path %||% ""
  use_minmax <- base::isTRUE(config$phytoclass$use_custom_minmax)
  selected_minmax <- config$phytoclass$selected_minmax_file %||% ""
  
  .load_single_fm <- function(file_path) { 
    if (!base::nzchar(file_path)) return(base::list(error = "Path empty."))
    if (!base::file.exists(file_path)) return(base::list(error = base::paste("File not found:", file_path)))
    tryCatch({ 
      fm_df <- readxl::read_excel(file_path, sheet = 1, .name_repair = "unique")
      if (base::colnames(fm_df)[1] == "...1") { base::colnames(fm_df)[1] <- "Phytoplankton_Class" }
      fm_matrix <- base::as.matrix(fm_df[, -1, drop = FALSE])
      base::rownames(fm_matrix) <- base::make.unique(base::as.character(fm_df[[1]]))
      fm_matrix[!base::is.finite(fm_matrix)] <- 0
      fm_matrix[fm_matrix < 0] <- 0
      return(fm_matrix) 
    }, error = function(e) { return(base::list(error = e$message)) }) 
  }
  
  .load_min_max <- function(file_name) {
    if (!base::nzchar(file_name) || file_name == "No MinMax files found in directory.") return(NULL)
    
    full_path <- base::file.path("R/Reference_Tables", file_name)
    if (!base::file.exists(full_path)) return(base::list(error = base::paste("Min/Max file not found:", full_path)))
    
    tryCatch({
      mm_df <- readxl::read_excel(full_path, sheet = 1)
      
      # FIX: Strictly enforce the exact capitalization the phytoclass package requires
      colnames_lower <- base::tolower(base::colnames(mm_df))
      if ("class" %in% colnames_lower) { base::colnames(mm_df)[colnames_lower == "class"] <- "Class" }
      if ("pig_abbrev" %in% colnames_lower) { base::colnames(mm_df)[colnames_lower == "pig_abbrev"] <- "Pig_Abbrev" }
      
      required_cols <- base::c("Class", "Pig_Abbrev", "min", "max")
      if (!base::all(required_cols %in% base::colnames(mm_df))) { 
        return(base::list(error = base::paste("File", file_name, "is missing required columns: 'Class', 'Pig_Abbrev', 'min', 'max'."))) 
      }
      mm_df$min <- base::as.numeric(mm_df$min)
      mm_df$max <- base::as.numeric(mm_df$max)
      return(base::as.data.frame(mm_df))
    }, error = function(e) { return(base::list(error = base::paste("Min/Max read error:", e$message))) })
  }
  
  .validate_minmax_coverage <- function(f_mat, mm_df, mat_name) {
    if (base::is.null(mm_df) || base::is.list(f_mat) && !base::is.null(f_mat$error)) return(NULL)
    
    missing_rules <- base::character(0)
    classes <- base::rownames(f_mat)
    pigments <- base::colnames(f_mat)
    
    for (cls in classes) {
      for (pig in pigments) {
        if (f_mat[cls, pig] > 0) {
          match_found <- base::any(mm_df$Class == cls & mm_df$Pig_Abbrev == pig)
          if (!match_found) { missing_rules <- base::c(missing_rules, base::paste0(cls, " (", pig, ")")) }
        }
      }
      anchor_found <- base::any(mm_df$Class == cls & mm_df$Pig_Abbrev %in% base::c("Tchla", "Dvchla"))
      if (!anchor_found) { missing_rules <- base::c(missing_rules, base::paste0(cls, " (Tchla/Dvchla Anchor)")) }
    }
    
    if (base::length(missing_rules) > 0) {
      return(base::paste("Incompatible Min/Max File! Missing rules for", mat_name, ":", base::paste(missing_rules, collapse = ", ")))
    }
    return(NULL)
  }
  
  Fm_Pro <- .load_single_fm(fm_pro_path)
  Fm_NoPro <- .load_single_fm(fm_nopro_path)
  Min_Max_df <- NULL
  
  if (use_minmax) { Min_Max_df <- .load_min_max(selected_minmax) }
  
  if (base::is.list(Fm_Pro) && !base::is.null(Fm_Pro$error)) return(base::list(error = Fm_Pro$error))
  if (base::is.list(Fm_NoPro) && !base::is.null(Fm_NoPro$error)) return(base::list(error = Fm_NoPro$error))
  if (base::is.list(Min_Max_df) && !base::is.null(Min_Max_df$error)) return(base::list(error = Min_Max_df$error))
  
  if (!base::is.null(Min_Max_df)) {
    pro_err <- .validate_minmax_coverage(Fm_Pro, Min_Max_df, "Fm_Pro")
    nopro_err <- .validate_minmax_coverage(Fm_NoPro, Min_Max_df, "Fm_NoPro")
    
    if (!base::is.null(pro_err) || !base::is.null(nopro_err)) {
      combined_err <- base::paste(base::c(pro_err, nopro_err), collapse = "\n\n")
      return(base::list(error = combined_err))
    }
  }
  
  return(base::list(Fm_Pro = Fm_Pro, Fm_NoPro = Fm_NoPro, min_max = Min_Max_df, error = NULL))
}

calculate_sMAPE_R2 <- function(S_actual, C_estimated, F_estimated) {
  if(base::is.null(S_actual) || base::is.null(C_estimated) || base::is.null(F_estimated)) return(base::list(mean_sMAPE = NA, R_squared = NA))
  C_mat <- base::as.matrix(C_estimated)
  F_mat <- base::as.matrix(F_estimated)
  S_mat <- base::as.matrix(S_actual)
  S_estimated <- C_mat %*% F_mat
  if (!base::all(base::dim(S_mat) == base::dim(S_estimated))) return(base::list(mean_sMAPE = NA, R_squared = NA))
  
  numerator <- base::abs(S_mat - S_estimated)
  denominator <- (base::abs(S_mat) + base::abs(S_estimated)) / 2
  sMAPE_matrix <- base::ifelse(denominator < 1e-9, 0, numerator / denominator)
  mean_sMAPE <- base::mean(sMAPE_matrix, na.rm = TRUE) * 100
  correlation <- stats::cor(base::as.vector(S_mat), base::as.vector(S_estimated), use = "complete.obs")
  
  return(base::list(mean_sMAPE = mean_sMAPE, R_squared = base::ifelse(base::is.na(correlation), 0, correlation^2)))
}

generate_run_summary_text <- function(config, master_qc_data, analysis_datasets, cluster_diagnostics = NULL) {
  qc_rules <- base::c(if (base::isTRUE(config$data_cleaning$handle_duplicates$enabled)) "Duplicates", if (base::isTRUE(config$data_cleaning$handle_pigment_nas$enabled)) "NAs", if (base::isTRUE(config$data_cleaning$enforce_non_negative_pigments$enabled)) "Negatives")
  filters <- base::c(if (base::isTRUE(config$filtering$geospatial$enabled)) "Location", if (base::isTRUE(config$filtering$temporal$enabled)) "Date", if (base::isTRUE(config$filtering$depth$enabled)) "Depth")
  qc_block <- base::paste("--- QC & Filtering ---", base::paste("Total Eligible Samples:", base::nrow(master_qc_data)), base::paste("QC Rules Active:", base::paste(qc_rules, collapse=", ")), base::paste("Filters Active:", if(base::length(filters) > 0) base::paste(filters, collapse=", ") else "None"), sep = "\n")
  method_raw <- config$strategy$method %||% "By Source File"
  
  if (method_raw == "By Pigment Cluster") { 
    size_str <- base::paste(base::paste0("C", base::seq_along(analysis_datasets), ": ", base::sapply(analysis_datasets, function(x) base::nrow(x$data))), collapse = " | ")
    strategy_block <- base::paste("\n--- Analysis Strategy ---", base::paste("Method: Clustering"), base::paste("Total Clusters:", base::length(analysis_datasets)), base::paste("Samples Breakdown:", size_str), sep = "\n")
  } else { 
    strategy_block <- base::paste("\n--- Analysis Strategy ---", base::paste("Method: By Source File"), base::paste("Total Analysis Groups:", base::length(analysis_datasets)), sep = "\n") 
  }
  
  seed_text <- if(base::isTRUE(base::as.logical(config$phytoclass$use_fixed_seed))) base::as.character(config$phytoclass$fixed_seed) else "Unconstrained"
  
  # CRITICAL UPDATE: Pointing to the new block
  mm_text <- if(base::isTRUE(config$phytoclass$use_custom_minmax)) base::paste("Active File:", config$phytoclass$selected_minmax_file) else "Phytoclass Internal Default"
  
  param_block <- base::paste("\n--- Phytoclass Parameters ---", base::paste("Iterations (Niter):", config$phytoclass$niter), base::paste("Cooling Step Size:", config$phytoclass$step_size), base::paste("Random Seed:", seed_text), base::paste("Min/Max Profile:", mm_text), sep = "\n") 
  return(base::paste(qc_block, strategy_block, param_block, sep = "\n"))
}