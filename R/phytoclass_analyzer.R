# ============================================================================
#
#   _phytoclass_Shiny V1.0 - SCIENTIFIC ANALYZER
#
#   Description:
#   The connection to the core 'phytoclass' algorithm. It automatically
#   selects the correct math matrix (Pro vs NoPro) based on your data and
#   runs the Simulated Annealing optimization to calculate abundance.
#
# ============================================================================




#' Run Phytoclass Analysis on a Prepared Dataset
#'
#' @param data_for_phyto A data frame containing samples for a specific run.
#' @param config The global configuration object.
#' @param fm_matrices A list containing the loaded `Fm_Pro` and `Fm_NoPro` matrices.
#' @param rename_map Optional list mapping standard keys to actual column names.
#' @return A list containing `results`, a detailed `log`, and the `pigment_matrix_used`.
run_phytoclass_analysis <- function(data_for_phyto, config, fm_matrices, rename_map = NULL) {
  
  dataset_name <- "UnknownDataset"
  if ("UniqueID" %in% colnames(data_for_phyto) && nrow(data_for_phyto) > 0) {
    dataset_name <- data_for_phyto$source_dataset[1] %||% data_for_phyto$ClusterID[1] %||% gsub("_Row_.*$", "", data_for_phyto$UniqueID[1])
  }
  cat(sprintf("\n--- [Analyzer] Starting Phytoclass Analysis for: %s ---\n", dataset_name))
  
  log <- list(status = "Not Started", rows_input_to_phyto = nrow(data_for_phyto), fm_matrix_used = NA_character_, niter_value = NA_integer_, mean_rmse = NA, mean_condnum = NA, mean_smape = NA, r_squared = NA, error_details = NULL)
  
  if (!requireNamespace("phytoclass", quietly = TRUE)) stop("FATAL [Analyzer]: Package 'phytoclass' is required.")
  if (log$rows_input_to_phyto == 0) { log$status <- "Skipped (No eligible rows)"; return(list(results = NULL, log = log, pigment_matrix_used = NULL)) }
  if (is.null(fm_matrices) || is.null(fm_matrices$Fm_Pro)) { log$status <- "Skipped (Fm matrices not loaded)"; return(list(results = NULL, log = log, pigment_matrix_used = NULL)) }
  if (!"UniqueID" %in% colnames(data_for_phyto)) stop("FATAL [Analyzer]: 'UniqueID' column is missing.")
  
  sm_matrix <- NULL
  
  tryCatch({
    log$status <- "Processing"
    
    # --- Step 1: Determine Pro vs. NoPro analysis type ---
    cat("     ...Step 1: Determining analysis type...\n")
    
    # Helper to find a column: Priority 1 = rename_map, Priority 2 = aliases
    .get_col_name <- function(key) {
      # 1. Check explicit map first
      if (!is.null(rename_map) && key %in% names(rename_map)) {
        col <- rename_map[[key]]
        if (!is.null(col) && col %in% colnames(data_for_phyto)) return(col)
      }
      # 2. Check aliases (Robust Match)
      aliases <- config$column_aliases[[key]] %||% key
      match <- intersect(aliases, colnames(data_for_phyto))[1]
      if (!is.na(match)) return(match)
      match_clean <- intersect(make.names(aliases), colnames(data_for_phyto))[1]
      if (!is.na(match_clean)) return(match_clean)
      
      return(NA)
    }
    
    dvchla_col <- .get_col_name("Dvchla")
    
    use_pro <- FALSE
    if (!is.na(dvchla_col)) {
      dvchla_sum <- sum(safe_as_numeric(data_for_phyto[[dvchla_col]]), na.rm = TRUE)
      if (is.finite(dvchla_sum) && dvchla_sum > 1e-9) use_pro <- TRUE
    }
    
    Fm_selected <- if (use_pro) fm_matrices$Fm_Pro else fm_matrices$Fm_NoPro
    log$fm_matrix_used <- if (use_pro) "Fm_Pro" else "Fm_NoPro"
    cat(sprintf("     ...Decision: Using '%s' matrix.\n", log$fm_matrix_used))
    
    # --- Step 2: Prepare and clean pigment matrix (Sm) ---
    cat("     ...Step 2: Preparing pigment matrix (Sm)...\n")
    standard_pigment_names <- colnames(Fm_selected)
    
    # Pass the .get_col_name function logic into build_sm_matrix via closure or logic duplication
    sm_matrix <- .build_sm_matrix_advanced(data_for_phyto, standard_pigment_names, .get_col_name)
    
    Fm_to_use <- Fm_selected
    Sm_to_use <- sm_matrix
    
    col_sums <- colSums(Sm_to_use, na.rm = TRUE)
    zero_sum_cols <- which(col_sums < 1e-9)
    
    if (length(zero_sum_cols) > 0) {
      zero_col_names <- names(zero_sum_cols)
      warning_msg <- sprintf("For dataset '%s', the following pigments were zero in all samples and were excluded from this analysis: %s",
                             dataset_name, paste(zero_col_names, collapse = ", "))
      warning(warning_msg, call. = FALSE)
      
      Fm_to_use <- Fm_selected[, -zero_sum_cols, drop = FALSE]
      Sm_to_use <- sm_matrix[, -zero_sum_cols, drop = FALSE]
    }
    
    # SAFETY CHECK: Dimensions
    if (ncol(Fm_to_use) < 2) {
      msg <- paste("Skipped: Not enough pigments remaining (>1 required) after removing zero-sum columns. Remaining:", paste(colnames(Fm_to_use), collapse=", "))
      log$status <- "Skipped (Insufficient Pigments)"
      warning(msg, call. = FALSE)
      return(list(results = NULL, log = log, pigment_matrix_used = sm_matrix))
    }
    
    # --- Step 3: Execute Phytoclass algorithm ---
    cat("     ...Step 3: Executing Phytoclass...\n")
    niter_val <- as.integer(config$phytoclass$niter %||% 500)
    step_val <- as.numeric(config$phytoclass$step_size %||% 0.009)
    log$niter_value <- niter_val
    
    phyto_func <- if (use_pro) phytoclass::simulated_annealing_Prochloro else phytoclass::simulated_annealing
    
    phyto_raw_out <- suppressWarnings({
      phyto_func(Fmat = Fm_to_use, S = Sm_to_use, niter = niter_val, step = step_val, verbose = FALSE)
    })
    
    if (is.null(phyto_raw_out) || is.null(phyto_raw_out[["Class abundances"]])) { stop("Phytoclass returned a NULL or invalid result.") }
    
    # --- Step 4: Format results and calculate metrics ---
    cat("     ...Step 4: Formatting results...\n")
    results_df <- as.data.frame(phyto_raw_out[["Class abundances"]])
    colnames(results_df) <- paste0("Phyto_", make.names(colnames(results_df)), "_Abund")
    results_df$UniqueID <- rownames(phyto_raw_out[["Class abundances"]])
    results_df$Phyto_RMSE <- phyto_raw_out$RMSE
    results_df$Phyto_CondNum <- phyto_raw_out[["condition number"]]
    
    final_results_df <- dplyr::select(results_df, UniqueID, Phyto_RMSE, Phyto_CondNum, starts_with("Phyto_"))
    
    metrics <- calculate_sMAPE_R2(
      S_actual = Sm_to_use, 
      C_estimated = phyto_raw_out[["Class abundances"]], 
      F_estimated = phyto_raw_out[["Final pigment ratios"]]
    )
    
    log$status <- "Success"
    log$mean_rmse <- mean(final_results_df$Phyto_RMSE, na.rm = TRUE)
    log$mean_condnum <- mean(final_results_df$Phyto_CondNum, na.rm = TRUE)
    log$mean_smape <- metrics$mean_sMAPE
    log$r_squared <- metrics$R_squared
    
    cat("--- [Analyzer] Phytoclass Analysis Successfully Completed ---\n")
    return(list(results = final_results_df, log = log, pigment_matrix_used = Sm_to_use))
    
  }, error = function(e) {
    log$status <<- "Failed"
    log$error_details <<- list(message = e$message)
    warning(sprintf("Analyzer ERROR for %s: %s", dataset_name, e$message), call. = FALSE)
    return(list(results = NULL, log = log, pigment_matrix_used = sm_matrix))
  })
}


#' Advanced Matrix Builder with Priority Lookup
.build_sm_matrix_advanced <- function(data, standard_pigment_names, col_finder_func) {
  
  sm_df <- data.frame(matrix(0, nrow = nrow(data), ncol = length(standard_pigment_names)))
  colnames(sm_df) <- standard_pigment_names
  rownames(sm_df) <- data$UniqueID
  
  for (std_name in standard_pigment_names) {
    # Use the passed function to determine the column name
    actual_col_name <- col_finder_func(std_name)
    
    if (!is.na(actual_col_name)) {
      # This pipeline guarantees a clean numeric vector.
      vals <- data[[actual_col_name]] %>%
        safe_as_numeric() %>%
        tidyr::replace_na(0)
      
      vals[vals < 0] <- 0
      sm_df[[std_name]] <- vals
    }
  }
  
  sm_matrix <- as.matrix(sm_df)
  
  if (any(!is.finite(sm_matrix))) {
    warning("Unexpected non-finite values were found in the final Sm matrix and coerced to 0.", call. = FALSE)
    sm_matrix[!is.finite(sm_matrix)] <- 0
  }
  
  return(sm_matrix)
}