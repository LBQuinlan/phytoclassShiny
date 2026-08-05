# ============================================================================
# CORE BACKEND HELPERS
#
# Shared, module-agnostic functions used across the application: file
# import and standardisation, date/time resolution, pigment matrix
# construction, the phytoclass analysis wrapper, and result plotting.
#
# Design notes:
#   - resolve_datetime_columns() is the single source of truth for turning
#     whatever date/time information a dataset has into the app's fixed
#     year/month/day/hour/minute columns. It is called both at import and
#     again after Step 3 commits any manual column mapping, and is written
#     to produce the same correct result either time (see its own header
#     comment for the column-lookup logic that makes this safe).
#   - pigment_display_name()/pigment_short_code()/apply_pigment_display_names()
#     form a one-way translation layer: every matrix calculation continues
#     to use the short pigment codes phytoclass itself requires; these
#     functions only affect what a user sees on screen or in an export.
#   - run_phytoclass_analysis() sets log$flagged_for_review when RMSE
#     exceeds config$phytoclass$rmse_review_threshold, following Hayward
#     et al.'s (2023) published guidance.
# ============================================================================

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

# ----------------------------------------------------------------------------
# PIGMENT DISPLAY NAME TRANSLATION LAYER
# These are the ONLY functions that should ever be used to show a pigment
# name to a user, or to save one into an exported file's column headers.
# Everywhere else in the codebase (Fm matrix loading, sm_matrix construction,
# phytoclass function calls) MUST continue to use the short code directly,
# unchanged, since phytoclass's F/S matrix alignment depends on it.
# ----------------------------------------------------------------------------

pigment_display_name <- function(short_code, config) {
  registry <- config$pigment_registry
  out <- base::vapply(short_code, function(code) {
    nm <- registry[[code]]
    if (base::is.null(nm) || !base::nzchar(nm)) code else nm
  }, character(1))
  base::unname(out)
}

pigment_short_code <- function(display_name, config) {
  registry <- config$pigment_registry
  # Build the reverse lookup once per call; registries are small (~15 entries)
  # so this is cheap and avoids needing a persisted reverse index.
  reverse <- stats::setNames(base::names(registry), base::unlist(registry, use.names = FALSE))
  out <- base::vapply(display_name, function(nm) {
    code <- reverse[[nm]]
    if (base::is.null(code)) nm else code
  }, character(1))
  base::unname(out)
}

# Renames a data.frame's columns from short codes to display names, for
# export only. Non-pigment columns (metadata, UniqueID, etc.) pass through
# unchanged. Safe to call on any data.frame; it only touches columns that
# exist as keys in the pigment registry.
apply_pigment_display_names <- function(df, config) {
  registry <- config$pigment_registry
  current_names <- base::colnames(df)
  new_names <- base::vapply(current_names, function(nm) {
    if (nm %in% base::names(registry)) registry[[nm]] else nm
  }, character(1))
  base::colnames(df) <- base::unname(new_names)
  df
}

# ----------------------------------------------------------------------------
# DATE/TIME RESOLUTION
#
# This is the single source of truth for turning whatever date/time
# information a dataset has, in whatever shape it arrived in, into the
# app's fixed year/month/day/hour/minute columns. It is designed to be
# called MORE THAN ONCE on the same dataset (at import, and again after any
# manual re-mapping), and produces the same correct result either time,
# because it is a pure function of (data, rename_map): it does not depend on
# any hidden prior state.
#
# Precedence, checked in this order:
#   1. Separate year/month/day columns already resolved (most reliable, no
#      parsing ambiguity possible)
#   2. A single combined date(-time) column, parsed with lubridate
#   3. Neither available: year/month/day set to NA
#
# Time-of-day, independently of which date path was used above:
#   a. If the combined date column in path 2 included a time component,
#      that is used first
#   b. Else, a separately-mapped 'time' column is parsed on its own
#   c. Else, separately-mapped 'hour'/'minute' columns are used directly
#   d. Else, hour/minute are set to NA (this is not an error: many pigment
#      datasets genuinely have no time-of-day resolution, only a sampling
#      date, so this must fail gracefully rather than block the workflow)
# ----------------------------------------------------------------------------

resolve_datetime_columns <- function(data, rename_map) {

  # This function runs at two different points in a dataset's lifecycle:
  # once at import, when `data` still has its original raw column names
  # and `rename_map` points target-key -> raw-name; and again after Step
  # 3's mapping is committed, by which point a separate rename step has
  # already renamed those columns to their target names directly (e.g. a
  # column literally named "time" now exists in place of its raw source
  # name). `rename_map` is not updated by that rename step, so a lookup
  # that only ever trusts `rename_map` would break the second time this
  # runs: it would look for a raw column name that no longer exists.
  #
  # .resolve_col() checks both possible states: is the key already a
  # column name directly (the post-rename case), and only if not, fall
  # back to looking it up via rename_map (the pre-rename, import-time
  # case). This makes the function correct regardless of which point in
  # the lifecycle it's called from.
  # called from, rather than assuming one and breaking on the other.
  .resolve_col <- function(key) {
    if (key %in% base::names(data)) return(data[[key]])
    raw_col <- rename_map[[key]]
    if (!base::is.null(raw_col) && base::nzchar(raw_col) && raw_col %in% base::names(data)) return(data[[raw_col]])
    return(NULL)
  }
  .key_available <- function(key) !base::is.null(.resolve_col(key))

  .clean_ymd <- function(d) {
    if (base::is.character(d$year) && "month" %in% base::names(d) &&
        (base::is.character(d$month) || base::is.factor(d$month))) {
      d$month <- base::match(base::substr(base::tolower(base::as.character(d$month)), 1, 3), base::tolower(month.abb)) %||% d$month
    } else if (base::is.character(d$month) || base::is.factor(d$month)) {
      d$month <- base::match(base::substr(base::tolower(base::as.character(d$month)), 1, 3), base::tolower(month.abb)) %||% d$month
    }
    d |> dplyr::mutate(
      year  = safe_as_numeric(year),
      month = safe_as_numeric(month),
      day   = safe_as_numeric(day)
    )
  }

  # Always start from a clean slate for the fields we own, so re-invoking
  # this function after a re-mapping doesn't leave stale values behind if
  # the user's remapping changed which path applies.
  data$year   <- if ("year"   %in% base::names(data)) data$year   else NA_real_
  data$month  <- if ("month"  %in% base::names(data)) data$month  else NA_real_
  data$day    <- if ("day"    %in% base::names(data)) data$day    else NA_real_
  data$hour   <- NA_real_
  data$minute <- NA_real_

  combined_datetime_parsed <- NULL

  # --- DATE RESOLUTION ---
  if (.key_available("year") && .key_available("month") && .key_available("day")) {
    data$year  <- .resolve_col("year")
    data$month <- .resolve_col("month")
    data$day   <- .resolve_col("day")
    data <- .clean_ymd(data)

  } else if (.key_available("date")) {
    raw_dates <- .resolve_col("date")

    # NOTE: these are passed to parse_date_time() as a single vector of
    # candidate orders in ONE call, not tried sequentially one-at-a-time.
    # lubridate scores all candidates together and picks the best match per
    # element; trying formats one-at-a-time with base R's as.POSIXct is NOT
    # equivalent and can silently produce a wrong date from a mismatched
    # format rather than failing safely. Do not simplify this into a manual
    # loop over formats.
    date_formats <- base::c(
      "Ymd HMS", "ymd HMS", "Ymd HM", "ymd HM",
      "mdY HMS", "mdy HMS", "dmY HMS", "dmy HMS",
      "Ymd", "ymd", "mdY", "mdy", "dmY", "dmy"
    )
    parsed <- lubridate::parse_date_time(raw_dates, orders = date_formats, quiet = TRUE)

    na_indices <- base::which(base::is.na(parsed))
    if (base::length(na_indices) > 0) {
      raw_subset <- raw_dates[na_indices]
      numeric_vals <- base::suppressWarnings(base::as.numeric(raw_subset))
      # Excel serial dates: valid range is roughly serial > 1000 (~1902) up
      # to serial < 60000 (~2064), guarding against an unrelated small or
      # implausibly large number being mistaken for a date.
      valid_mask <- !base::is.na(numeric_vals) & numeric_vals > 1000 & numeric_vals < 60000
      if (base::any(valid_mask)) {
        parsed[na_indices[valid_mask]] <- base::as.POSIXct(base::as.Date(numeric_vals[valid_mask], origin = "1899-12-30"))
      }
    }

    data$year  <- lubridate::year(parsed)
    data$month <- lubridate::month(parsed)
    data$day   <- lubridate::day(parsed)
    data <- .clean_ymd(data)
    combined_datetime_parsed <- parsed
  }

  # --- TIME RESOLUTION (independent of which date path fired above) ---
  if (!base::is.null(combined_datetime_parsed) &&
      base::any(!base::is.na(lubridate::hour(combined_datetime_parsed)) &
                (lubridate::hour(combined_datetime_parsed) != 0 | lubridate::minute(combined_datetime_parsed) != 0))) {
    # The combined date column already carried a real time component
    # (i.e. it wasn't parsed as midnight-by-default on a date-only string).
    data$hour   <- lubridate::hour(combined_datetime_parsed)
    data$minute <- lubridate::minute(combined_datetime_parsed)

  } else if (.key_available("time")) {
    raw_times <- .resolve_col("time")
    time_formats <- base::c("HMS", "HM", "IMp", "IMSp")
    parsed_time <- lubridate::parse_date_time(base::as.character(raw_times), orders = time_formats, quiet = TRUE)
    data$hour   <- lubridate::hour(parsed_time)
    data$minute <- lubridate::minute(parsed_time)

  } else if (.key_available("hour") && .key_available("minute")) {
    data$hour   <- safe_as_numeric(.resolve_col("hour"))
    data$minute <- safe_as_numeric(.resolve_col("minute"))

  } else if (.key_available("hour")) {
    # Hour without minute: still useful, record it, leave minute as NA
    # rather than assuming 0, since assuming would misrepresent precision.
    data$hour <- safe_as_numeric(.resolve_col("hour"))
  }

  data
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
  if (base::grepl("^\\.\\.\\.\\d+$", original_colnames[1])) {
    original_colnames[1] <- "Station_AutoRecovered"
    base::colnames(raw_data)[1]  <- "Station_AutoRecovered"
    # Registers under internal_source_id, not a separate "station" key --
    # every other lookup of a station/site identifier (.map_columns(), the
    # Step 3 wizard, run_phytoclass_analysis()'s .get_col_name()) reads
    # config$column_aliases$internal_source_id specifically. The previous
    # version wrote to config$column_aliases$station, a key nothing else in
    # the codebase ever reads, so the recovered column always showed up as
    # "(not mapped)" in Step 3 despite this block's intent.
    if (!"Station_AutoRecovered" %in% config$column_aliases$internal_source_id) {
      config$column_aliases$internal_source_id <- base::c(config$column_aliases$internal_source_id, "Station_AutoRecovered")
    }
  }

  cleaned_colnames <- base::make.names(original_colnames, unique = TRUE)
  base::colnames(raw_data) <- cleaned_colnames

  processed_data <- raw_data |>
    dplyr::mutate(SourceFile = dataset_name, original_row_num = dplyr::row_number(), UniqueID = create_unique_id(dataset_name, original_row_num), .before = 1)

  map_result <- .map_columns(processed_data, original_colnames, config$column_aliases)

  # This is the first of two call sites for resolve_datetime_columns().
  # The second is in module_step3_validation.R's .finalize_commit(), which
  # fires after any manual mapping is saved. Both call sites must stay in
  # sync; if this function's signature changes, update both.
  processed_data <- resolve_datetime_columns(processed_data, map_result$rename_map)

  if ("year"  %in% base::colnames(processed_data)) map_result$rename_map$year  <- "year"
  if ("month" %in% base::colnames(processed_data)) map_result$rename_map$month <- "month"
  if ("day"   %in% base::colnames(processed_data)) map_result$rename_map$day   <- "day"
  if ("hour"  %in% base::colnames(processed_data)) map_result$rename_map$hour  <- "hour"
  if ("minute" %in% base::colnames(processed_data)) map_result$rename_map$minute <- "minute"

  # Every key resolved at this point was resolved automatically, via alias
  # matching, not by a person. rename_source tracks this per key so the
  # exported mapping log can later distinguish an automatic match from a
  # person confirming or choosing it in Step 3, which matters when
  # auditing a mapping across many datasets: manual overrides are exactly
  # the ones worth double-checking, auto-matches are the routine case.
  rename_source <- stats::setNames(base::as.list(base::rep("auto", base::length(map_result$rename_map))), base::names(map_result$rename_map))

  return(base::list(
    name = dataset_name,
    data = tibble::as_tibble(processed_data),
    data_original = tibble::as_tibble(raw_data),
    original_colnames = original_colnames,
    cleaned_colnames = cleaned_colnames,
    rename_map = map_result$rename_map,
    rename_source = rename_source,
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

run_phytoclass_analysis <- function(data_for_phyto, config, fm_matrices, rename_map = NULL) {
  dataset_name <- "UnknownDataset"
  if ("UniqueID" %in% base::colnames(data_for_phyto) && base::nrow(data_for_phyto) > 0) {
    dataset_name <- data_for_phyto$SourceFile[1] %||% data_for_phyto$ClusterID[1] %||% base::gsub("_Row_.*$", "", data_for_phyto$UniqueID[1])
  }

  log <- base::list(status = "Not Started", rows_input_to_phyto = base::nrow(data_for_phyto), fm_matrix_used = NA_character_, seed_used = NA_character_, niter_value = NA_integer_, mean_rmse = NA, mean_condnum = NA, mean_smape = NA, smape_diagnostic = NA_character_, r_squared = NA, flagged_for_review = FALSE, excluded_pigments = base::character(0), excluded_classes = base::character(0), error_details = NULL)

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
      dvchla_vals <- safe_as_numeric(data_for_phyto[[dvchla_col]])
      dvchla_sum <- base::sum(dvchla_vals, na.rm = TRUE)
      # Requiring a nonzero SUM alone (the previous condition) commits the
      # whole cluster to the Prochlorococcus-specific model even when only
      # a couple of rows out of thousands carry any Dvchla at all -- too
      # sparse for phytoclass to fit a distinct "Pro" class from, and a
      # likely source of a degenerate/near-singular fit deep in
      # simulated_annealing_Prochloro. Also require a minimum fraction of
      # rows actually carrying the signal (1% is a heuristic floor, not a
      # value derived from phytoclass's own fitting requirements).
      dvchla_nonzero_frac <- base::mean(dvchla_vals > 1e-9, na.rm = TRUE)
      if (base::is.finite(dvchla_sum) && dvchla_sum > 1e-9 && base::is.finite(dvchla_nonzero_frac) && dvchla_nonzero_frac >= 0.01) use_pro <- TRUE
    }

    Fm_to_use <- if (use_pro) fm_matrices$Fm_Pro else fm_matrices$Fm_NoPro
    log$fm_matrix_used <- if (use_pro) "Fm_Pro" else "Fm_NoPro"
    sm_matrix <- .build_sm_matrix_advanced(data_for_phyto, base::colnames(Fm_to_use), .get_col_name)

    # Pre-flight guard: rows with a zero-sum pigment profile (all pigments,
    # including Tchla, at or below the detection floor) are not just weak
    # signal, they are mathematically invalid input for a pigment-ratio
    # model. handle_zero_pigment_sum in Step 4 QC is meant to catch these
    # before they ever get this far, but if that toggle is off (or the
    # dataset was re-imported directly, bypassing Step 4), nothing else
    # currently stops them from reaching phyto_func() and corrupting the
    # fit for the whole cluster with NaN/Inf. Dropping them here, and
    # logging exactly how many, makes this step robust on its own rather
    # than solely depending on an upstream toggle staying enabled.
    row_sums <- base::rowSums(sm_matrix, na.rm = TRUE)
    n_zero_rows <- base::sum(row_sums <= 1e-9)
    if (n_zero_rows > 0) {
      log$rows_dropped_zero_signal <- n_zero_rows
      keep <- row_sums > 1e-9
      sm_matrix <- sm_matrix[keep, , drop = FALSE]
      data_for_phyto <- data_for_phyto[keep, , drop = FALSE]
    }

    # Second, more specific pre-flight guard: a row can pass the check
    # above (nonzero OVERALL pigment sum) while Tchla itself is exactly
    # zero - real accessory-pigment signal (Fuco, Chl_b, etc.) but no
    # reported Total Chlorophyll a. That combination still reached
    # phyto_func() and crashed with "NA/NaN/Inf in foreign function call":
    # CHEMTAX-style pigment:Tchla ratios are undefined when the
    # denominator is zero, no matter how much signal sits in the
    # numerator pigments. These rows are not weak signal, they are
    # unusable by this method, and need to be dropped and counted
    # separately from the zero-signal rows above so the log doesn't
    # conflate the two different reasons a row was excluded.
    if ("Tchla" %in% base::colnames(sm_matrix) && base::nrow(sm_matrix) > 0) {
      tchla_zero <- sm_matrix[, "Tchla"] <= 1e-9
      n_tchla_zero <- base::sum(tchla_zero, na.rm = TRUE)
      if (n_tchla_zero > 0) {
        log$rows_dropped_zero_tchla <- n_tchla_zero
        keep <- !tchla_zero
        sm_matrix <- sm_matrix[keep, , drop = FALSE]
        data_for_phyto <- data_for_phyto[keep, , drop = FALSE]
      }
    }

    if (base::nrow(sm_matrix) == 0 || base::sum(sm_matrix, na.rm = TRUE) == 0) {
      log$status <- if (base::nrow(sm_matrix) == 0 && base::isTRUE(log$rows_dropped_zero_tchla > 0)) {
        "Skipped (No Rows With Valid Tchla)"
      } else {
        "Skipped (Zero Sum Pigments)"
      }
      return(base::list(results = NULL, log = log, pigment_matrix_used = sm_matrix, f_matrix_final = NULL, phytoclass_raw = NULL))
    }

    pruned <- prune_unresolvable_classes(sm_matrix, Fm_to_use, fm_matrices$min_max)
    log$excluded_pigments <- pruned$excluded_pigments
    log$excluded_classes <- pruned$excluded_classes

    if (base::isTRUE(pruned$fully_unresolvable)) {
      log$status <- "Skipped (No Resolvable Classes)"
      log$error_details <- base::list(message = base::sprintf(
        "Every pigment required by %s was absent from this group (excluded: %s). No class could be resolved.",
        log$fm_matrix_used, base::paste(pruned$excluded_pigments, collapse = ", ")))
      return(base::list(results = NULL, log = log, pigment_matrix_used = sm_matrix, f_matrix_final = NULL, phytoclass_raw = NULL))
    }

    Fm_to_use <- pruned$Fm
    sm_matrix <- pruned$Sm
    min_max_to_use <- pruned$min_max

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
        user_defined_min_max = min_max_to_use,
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
    log$smape_diagnostic <- metrics$diagnostic
    log$r_squared <- metrics$R_squared

    # RMSE is compared against the review threshold here so a run that
    # converges numerically but produces a poor fit is still flagged.
    # Hayward et al.'s (2023) published guidance is that RMSE > 0.1
    # warrants increasing iterations/step size or reclustering.
    review_threshold <- base::as.numeric(config$phytoclass$rmse_review_threshold %||% 0.1)
    log$flagged_for_review <- base::isTRUE(log$mean_rmse > review_threshold)

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

# ----------------------------------------------------------------------------
# PER-GROUP FM PRUNING
#
# The Fm_Pro/Fm_NoPro selection above is a curated decision: the two
# reference matrices differ not only in whether Prochlorococcus is
# present, but in which other classes are jointly resolvable under that
# condition (Fm_NoPro also omits Chlorophytes, since its pigment signature
# is a strict subset of Prasinophytes' and the two become indistinguishable
# without Prochlorococcus present to anchor the rest of the matrix). That
# curation cannot be reproduced by a simple presence/absence check, so it
# stays as two explicit reference files rather than being derived
# automatically.
#
# This function handles a different, complementary problem: a specific
# analysis group (typically a pigment cluster) may have a pigment that is
# genuinely absent across every one of its samples, for reasons no
# reference-matrix author could have anticipated, since which samples end
# up in which group is a property of this particular dataset's clustering,
# not something fixed in advance. Simulated annealing cannot estimate a
# ratio for a pigment with no variance to inform it, so this pigment (and
# any class left with no remaining marker as a result) is removed from a
# group-specific copy of the matrix before that group's analysis runs. The
# shared Fm_Pro/Fm_NoPro/min-max reference files are never modified; only
# the copy passed to this one group's simulated annealing call is.
#
# Uses the same absolute-sum threshold as the existing Dvchla presence
# check, for consistency, since both are testing the same underlying
# condition (is this pigment present at all in this data) at different
# scopes (whole dataset vs. one group).
# ----------------------------------------------------------------------------

prune_unresolvable_classes <- function(Sm, Fm, min_max = NULL, abs_threshold = 1e-9) {
  pigment_sums <- base::colSums(Sm, na.rm = TRUE)
  dead_pigments <- base::names(pigment_sums)[pigment_sums < abs_threshold]

  Fm_pruned <- Fm
  Sm_pruned <- Sm
  if (base::length(dead_pigments) > 0) {
    keep_cols <- base::setdiff(base::colnames(Fm_pruned), dead_pigments)
    Fm_pruned <- Fm_pruned[, keep_cols, drop = FALSE]
    Sm_pruned <- Sm_pruned[, keep_cols, drop = FALSE]
  }

  # A class with no pigment columns left at all is unresolvable regardless
  # of row sums; row sums are not a meaningful test on a zero-column
  # matrix, so this case is handled directly rather than falling through
  # to the row-sum check below.
  if (base::ncol(Fm_pruned) == 0) {
    dead_classes <- base::rownames(Fm)
    Fm_pruned <- Fm_pruned[0, , drop = FALSE]
  } else {
    row_sums <- base::rowSums(Fm_pruned)
    dead_classes <- base::rownames(Fm_pruned)[row_sums == 0]
    if (base::length(dead_classes) > 0) {
      Fm_pruned <- Fm_pruned[base::setdiff(base::rownames(Fm_pruned), dead_classes), , drop = FALSE]
    }
  }

  min_max_pruned <- min_max
  if (!base::is.null(min_max) && base::nrow(Fm_pruned) > 0) {
    min_max_pruned <- min_max[min_max$Class %in% base::rownames(Fm_pruned) & min_max$Pig_Abbrev %in% base::colnames(Fm_pruned), , drop = FALSE]
  }

  base::list(
    Fm = Fm_pruned,
    Sm = Sm_pruned,
    min_max = min_max_pruned,
    excluded_pigments = dead_pigments,
    excluded_classes = dead_classes,
    fully_unresolvable = base::nrow(Fm_pruned) == 0
  )
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

# ----------------------------------------------------------------------------
# COLUMN MAPPING LOG
#
# Builds a wide, one-row-per-dataset table: every app-required variable as
# its own column, cell value = the raw source column it was mapped to (or
# "(not mapped)"). This is the format that stays traceable at scale: with a
# handful of datasets a long list would read fine, but with a hundred, a
# reviewer needs to be able to scan straight down a single variable's column
# and immediately spot the one dataset whose source naming convention
# differs from the other ninety-nine, which a per-dataset comma-separated
# string cannot offer at a glance.
#
# Returns a list(table = data.frame, manual_cells = logical matrix) rather
# than just the data.frame, so the caller (report_builder.R) can apply
# distinct styling to manually-mapped cells without re-deriving which ones
# they were; manual overrides are exactly the entries worth a reviewer's
# closer attention, since they reflect a human judgement call rather than a
# routine alias match.
# ----------------------------------------------------------------------------

build_column_mapping_log <- function(staging_datasets, config) {
  if (base::length(staging_datasets) == 0) return(base::list(table = NULL, manual_cells = NULL))

  # Union of every key that appears in ANY dataset's mapping, plus the full
  # set config considers meaningful, so a key that's unmapped in EVERY
  # dataset still gets its own column (visibly "(not mapped)" throughout)
  # rather than silently disappearing from the log.
  all_possible_keys <- base::unique(base::c(
    config$general$essential_pigments, "Tchla",
    "latitude", "longitude", "depth", "date", "year", "month", "day", "hour", "minute",
    base::unlist(base::lapply(staging_datasets, function(ds) base::names(ds$rename_map %||% base::list())))
  ))

  dataset_names <- base::names(staging_datasets)
  table_out <- base::data.frame(Dataset = dataset_names, stringsAsFactors = FALSE)
  manual_matrix <- base::matrix(FALSE, nrow = base::length(dataset_names), ncol = base::length(all_possible_keys),
                                 dimnames = base::list(dataset_names, all_possible_keys))

  for (key in all_possible_keys) {
    col_label <- pigment_display_name(key, config)
    col_label <- if (!base::identical(col_label, key)) base::paste0(col_label, " (", key, ")") else key

    cell_values <- base::vapply(dataset_names, function(ds_name) {
      ds <- staging_datasets[[ds_name]]
      raw_col <- ds$rename_map[[key]]
      if (base::is.null(raw_col) || !base::nzchar(raw_col)) return("(not mapped)")
      source_type <- ds$rename_source[[key]] %||% "auto"
      if (base::identical(source_type, "manual")) base::paste0(raw_col, " *") else raw_col
    }, character(1))

    manual_matrix[, key] <- base::vapply(dataset_names, function(ds_name) {
      ds <- staging_datasets[[ds_name]]
      base::identical(ds$rename_source[[key]] %||% "auto", "manual") && base::nzchar(ds$rename_map[[key]] %||% "")
    }, logical(1))

    table_out[[col_label]] <- cell_values
  }

  base::list(table = table_out, manual_cells = manual_matrix)
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

    full_path <- base::file.path("R/reference tables", file_name)
    if (!base::file.exists(full_path)) return(base::list(error = base::paste("Min/Max file not found:", full_path)))

    tryCatch({
      mm_df <- readxl::read_excel(full_path, sheet = 1)

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

# ----------------------------------------------------------------------------
# SHARED RESULT PLOTTING
#
# Used both by Step 7's interactive "View Graphs" panel and by
# report_builder.R when generating the PNGs bundled into a downloaded
# report. Kept at the top level, rather than inside a module's own scope,
# so both consumers call the exact same plotting code and can never
# visually drift apart from each other.
# ----------------------------------------------------------------------------

clean_result_colnames <- function(x) {
  x <- base::gsub("Phyto_RMSE", "RMSE", x)
  x <- base::gsub("Phyto_CondNum", "Condition_Number", x)
  x <- base::gsub("^Phyto_", "", x)
  x <- base::gsub("_Abund$", "", x)
  return(x)
}

get_group_palette <- function(data_classes, config) {
  config_palette <- config$reporting$plotting$custom_palette
  if (base::is.null(config_palette)) return(NULL)
  user_colors <- base::unlist(config_palette)
  final_palette <- base::c()
  for (cls in data_classes) {
    if (cls %in% base::names(user_colors)) {
      final_palette[cls] <- user_colors[[cls]]
    } else {
      hyphenated <- base::gsub("\\.", "-", cls)
      if (hyphenated %in% base::names(user_colors)) {
        final_palette[cls] <- user_colors[[hyphenated]]
      } else { final_palette[cls] <- NA }
    }
  }
  missing_entries <- base::names(final_palette)[base::is.na(final_palette)]
  if (base::length(missing_entries) > 0) {
    default_colors <- scales::hue_pal()(base::length(missing_entries))
    for(i in base::seq_along(missing_entries)) { final_palette[missing_entries[i]] <- default_colors[i] }
  }
  return(final_palette)
}

plot_community_area <- function(data, config) {
  long_df <- data |>
    dplyr::select(UniqueID, tidyselect::starts_with("Phyto_")) |>
    dplyr::select(-tidyselect::ends_with("RMSE"), -tidyselect::ends_with("CondNum")) |>
    tidyr::pivot_longer(cols = -UniqueID, names_to = "Class", values_to = "Abundance") |>
    dplyr::mutate(Class = clean_result_colnames(Class))

  unique_classes <- base::unique(long_df$Class)
  custom_pal <- get_group_palette(unique_classes, config)

  p <- ggplot2::ggplot(long_df, ggplot2::aes(x = UniqueID, y = Abundance, fill = Class, group = Class)) +
    ggplot2::geom_area(alpha = 0.85, position = "fill") +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank()) +
    ggplot2::labs(title = "Relative Phytoplankton Community Composition", x = "Sample", y = "Share of Total Chlorophyll a (%)")

  if (!base::is.null(custom_pal)) p <- p + ggplot2::scale_fill_manual(values = custom_pal)
  return(p)
}

plot_community_bar <- function(data, config) {
  long_df <- data |>
    dplyr::select(UniqueID, tidyselect::starts_with("Phyto_")) |>
    dplyr::select(-tidyselect::ends_with("RMSE"), -tidyselect::ends_with("CondNum")) |>
    tidyr::pivot_longer(cols = -UniqueID, names_to = "Class", values_to = "Abundance") |>
    dplyr::mutate(Class = clean_result_colnames(Class))

  unique_classes <- base::unique(long_df$Class)
  custom_pal <- get_group_palette(unique_classes, config)

  p <- ggplot2::ggplot(long_df, ggplot2::aes(x = UniqueID, y = Abundance, fill = Class)) +
    ggplot2::geom_bar(stat = "identity", position = "stack", width=1) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank()) +
    ggplot2::labs(title = "Absolute Phytoplankton Community Estimates", x = "Sample", y = "Biomass Concentration (\u00b5g/L)")

  if (!base::is.null(custom_pal)) p <- p + ggplot2::scale_fill_manual(values = custom_pal)
  return(p)
}

# Formats an optimised F-matrix (classes x pigments, numeric ratios) for
# display: classes become a proper column rather than row names (so it
# exports/renders as a normal table), and pigment columns are renamed to
# their full display names. Shared by the Step 7 "Optimised Pigment
# Ratios" table and the exported F-matrix sheet, so both show the exact
# same thing rather than two independently-formatted versions of it.
format_fmatrix_for_display <- function(f_matrix, config) {
  if (base::is.null(f_matrix)) return(NULL)
  f_df <- base::as.data.frame(f_matrix)
  f_df <- base::cbind(Phytoplankton_Class = base::rownames(f_df), f_df)
  base::rownames(f_df) <- NULL
  base::colnames(f_df) <- base::vapply(base::colnames(f_df), function(nm) {
    if (nm %in% base::names(config$pigment_registry)) config$pigment_registry[[nm]] else nm
  }, character(1))
  f_df
}

calculate_sMAPE_R2 <- function(S_actual, C_estimated, F_estimated) {
  if(base::is.null(S_actual) || base::is.null(C_estimated) || base::is.null(F_estimated)) {
    return(base::list(mean_sMAPE = NA, R_squared = NA, diagnostic = "One of the required matrices (S, C, or F) was missing."))
  }
  C_mat <- base::as.matrix(C_estimated)
  F_mat <- base::as.matrix(F_estimated)
  S_mat <- base::as.matrix(S_actual)

  # phytoclass's returned F matrix can legitimately have fewer pigment
  # columns than the S matrix passed in: if a pigment was zero across
  # every sample in a particular group (a small cluster from a region
  # where that pigment genuinely wasn't detected, for instance), there's
  # nothing to optimise for it, and phytoclass may drop it from the
  # matrices it returns. Realigning by pigment name lets the comparison
  # still run over whichever pigments phytoclass actually resolved for
  # this group; the `diagnostic` field records what happened either way,
  # so a genuinely unexpected mismatch stays distinguishable from this
  # ordinary, explainable one rather than both collapsing to an
  # unexplained NA.
  shared_pigments <- base::intersect(base::colnames(S_mat), base::colnames(F_mat))

  if (base::length(shared_pigments) == 0) {
    return(base::list(mean_sMAPE = NA, R_squared = NA,
      diagnostic = base::sprintf("No pigment columns in common between the input data (%s) and the matrix phytoclass returned (%s).",
                                  base::paste(base::colnames(S_mat), collapse = ", "), base::paste(base::colnames(F_mat), collapse = ", "))))
  }

  dropped <- base::setdiff(base::colnames(S_mat), shared_pigments)
  diag_note <- if (base::length(dropped) > 0) {
    base::sprintf("Computed over %d of %d input pigments; '%s' had zero variance for this group and were not returned by phytoclass.",
                   base::length(shared_pigments), base::ncol(S_mat), base::paste(dropped, collapse = "', '"))
  } else NA_character_

  S_mat_aligned <- S_mat[, shared_pigments, drop = FALSE]
  F_mat_aligned <- F_mat[, shared_pigments, drop = FALSE]
  S_estimated <- C_mat %*% F_mat_aligned

  if (!base::all(base::dim(S_mat_aligned) == base::dim(S_estimated))) {
    return(base::list(mean_sMAPE = NA, R_squared = NA,
      diagnostic = base::sprintf("Dimension mismatch even after aligning by pigment name: input data is %s, phytoclass's estimate is %s. This is not the ordinary zero-variance case; worth checking this group's data directly.",
                                  base::paste(base::dim(S_mat_aligned), collapse = " x "), base::paste(base::dim(S_estimated), collapse = " x "))))
  }

  numerator <- base::abs(S_mat_aligned - S_estimated)
  denominator <- (base::abs(S_mat_aligned) + base::abs(S_estimated)) / 2
  sMAPE_matrix <- base::ifelse(denominator < 1e-9, 0, numerator / denominator)
  mean_sMAPE <- base::mean(sMAPE_matrix, na.rm = TRUE) * 100
  correlation <- stats::cor(base::as.vector(S_mat_aligned), base::as.vector(S_estimated), use = "complete.obs")

  return(base::list(mean_sMAPE = mean_sMAPE, R_squared = base::ifelse(base::is.na(correlation), 0, correlation^2), diagnostic = diag_note))
}

generate_run_summary_text <- function(config, master_qc_data, analysis_datasets, cluster_diagnostics = NULL) {
  # handle_zero_pigment_sum was never added here when the 4th cleaning
  # toggle was introduced, so this summary could never show it regardless
  # of its actual state - a purely cosmetic gap, but a confusing one, since
  # it looks like evidence the toggle isn't taking effect when it says
  # nothing about that either way.
  qc_rules <- base::c(if (base::isTRUE(config$data_cleaning$handle_duplicates$enabled)) "Duplicates", if (base::isTRUE(config$data_cleaning$handle_pigment_nas$enabled)) "NAs", if (base::isTRUE(config$data_cleaning$enforce_non_negative_pigments$enabled)) "Negatives", if (base::isTRUE(config$data_cleaning$handle_zero_pigment_sum$enabled)) "Empty Samples")
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
  mm_text <- if(base::isTRUE(config$phytoclass$use_custom_minmax)) base::paste("Active File:", config$phytoclass$selected_minmax_file) else "Phytoclass Internal Default"

  param_block <- base::paste("\n--- Phytoclass Parameters ---", base::paste("Iterations (Niter):", config$phytoclass$niter), base::paste("Cooling Step Size:", config$phytoclass$step_size), base::paste("Random Seed:", seed_text), base::paste("Min/Max Profile:", mm_text), sep = "\n")
  return(base::paste(qc_block, strategy_block, param_block, sep = "\n"))
}
