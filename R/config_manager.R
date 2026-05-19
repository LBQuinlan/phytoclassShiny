# ============================================================================
#
#   _phytoclass_Shiny V1.0 - MASTER CONFIGURATION MANAGER
#
#   Description:
#   Handles state serialization, YAML read/writes, and UI synchronization.
#   Hardened against corrupted files, UI ghosting, and OS-level locks.
#
# ============================================================================

# Global Path Constants (Safely defaults to current working directory)
CONFIG_SESSION_PATH <- base::file.path(base::getwd(), "config_session.yaml")
CONFIG_TEMPLATE_PATH <- base::file.path(base::getwd(), "config_template.yaml")

#' Safely initialize the application state
#' Attempts to load the last session. If missing or corrupted, falls back to template.
initialize_config <- function() {
  config <- load_config(CONFIG_SESSION_PATH)
  
  if (base::is.null(config)) {
    base::message("--- [SYSTEM] Session config not found or corrupted. Booting from Template. ---")
    config <- load_config(CONFIG_TEMPLATE_PATH)
  } else {
    base::message("--- [SYSTEM] Restored previous session state successfully. ---")
  }
  
  # Absolute fallback if even the template is missing
  if (base::is.null(config)) {
    base::stop("FATAL BOOT ERROR: Both session and template YAML files are missing or unreadable.")
  }
  
  return(config)
}

#' Safely read a YAML file into an R list
load_config <- function(path) {
  if (!base::file.exists(path)) return(NULL)
  
  base::tryCatch({
    cfg <- yaml::read_yaml(path)
    if (!base::is.list(cfg) || base::length(cfg) == 0) return(NULL)
    return(cfg)
  }, error = function(e) {
    base::warning(base::sprintf("Failed to parse YAML at %s: %s", path, e$message), call. = FALSE)
    return(NULL)
  })
}

#' Safely serialize an R list to a YAML file with OS lock protection
save_config <- function(config, path) {
  if (base::is.null(config) || !base::is.list(config)) return(FALSE)
  
  base::tryCatch({
    yaml::write_yaml(config, file = path)
    return(TRUE)
  }, error = function(e) {
    base::warning(base::sprintf("Permission denied writing to %s. File may be open in another program.", path), call. = FALSE)
    return(FALSE)
  })
}

#' Synchronize UI Inputs into the Config Object (Memory Save)
#' Hardened to ignore NULL values caused by suspended UI elements
sync_config_with_ui <- function(config, input, ns_prefix = "") {
  
  # Helper to safely grab and check input
  .safe_grab <- function(id, default = NULL) {
    val <- input[[base::paste0(ns_prefix, id)]]
    if (base::is.null(val)) return(default)
    return(val)
  }
  
  # Only process base step1 inputs if no prefix is supplied
  if (ns_prefix == "") {
    
    # 1. Run Settings (Math Boundaries)
    niter_val <- base::as.numeric(.safe_grab("niter_input"))
    step_val <- base::as.numeric(.safe_grab("step_size_input"))
    if (!base::is.null(niter_val) && !base::is.na(niter_val) && niter_val > 0) config$phytoclass$niter <- niter_val
    if (!base::is.null(step_val) && !base::is.na(step_val) && step_val > 0) config$phytoclass$step_size <- step_val
    
    # 2. File Paths
    fm_pro <- .safe_grab("fm_pro_path_ui")
    fm_nopro <- .safe_grab("fm_nopro_path_ui")
    if (!base::is.null(fm_pro)) config$workspace$fm_pro_matrix_path <- fm_pro
    if (!base::is.null(fm_nopro)) config$workspace$fm_nopro_matrix_path <- fm_nopro
    
    # 3. QC Toggles
    config$data_cleaning$handle_duplicates$enabled <- base::isTRUE(.safe_grab("toggle_handle_duplicates", config$data_cleaning$handle_duplicates$enabled))
    config$data_cleaning$handle_pigment_nas$enabled <- base::isTRUE(.safe_grab("toggle_handle_nas", config$data_cleaning$handle_pigment_nas$enabled))
    config$data_cleaning$enforce_non_negative_pigments$enabled <- base::isTRUE(.safe_grab("toggle_handle_negatives", config$data_cleaning$enforce_non_negative_pigments$enabled))
    config$data_cleaning$handle_zero_pigment_sum$enabled <- base::isTRUE(.safe_grab("toggle_handle_zerosum", config$data_cleaning$handle_zero_pigment_sum$enabled))
    
    # 4. Filters
    config$filtering$geospatial$enabled <- base::isTRUE(.safe_grab("toggle_geo_filter", config$filtering$geospatial$enabled))
    lat1 <- .safe_grab("min_lat_ui"); lat2 <- .safe_grab("max_lat_ui")
    if (!base::is.null(lat1) && !base::is.null(lat2) && !base::is.na(lat1) && !base::is.na(lat2)) {
      config$filtering$geospatial$min_latitude <- base::min(lat1, lat2)
      config$filtering$geospatial$max_latitude <- base::max(lat1, lat2)
    }
    lon1 <- .safe_grab("min_lon_ui"); lon2 <- .safe_grab("max_lon_ui")
    if (!base::is.null(lon1) && !base::is.null(lon2) && !base::is.na(lon1) && !base::is.na(lon2)) {
      config$filtering$geospatial$min_longitude <- base::min(lon1, lon2)
      config$filtering$geospatial$max_longitude <- base::max(lon1, lon2)
    }
    
    config$filtering$temporal$enabled <- base::isTRUE(.safe_grab("toggle_temporal_filter", config$filtering$temporal$enabled))
    d1 <- .safe_grab("start_date_ui"); d2 <- .safe_grab("end_date_ui")
    if (!base::is.null(d1) && !base::is.null(d2)) {
      config$filtering$temporal$start_date <- base::as.character(base::min(base::as.Date(d1), base::as.Date(d2)))
      config$filtering$temporal$end_date <- base::as.character(base::max(base::as.Date(d1), base::as.Date(d2)))
    }
    
    config$filtering$depth$enabled <- base::isTRUE(.safe_grab("toggle_depth_filter", config$filtering$depth$enabled))
    dep1 <- .safe_grab("min_depth_ui"); dep2 <- .safe_grab("max_depth_ui")
    if (!base::is.null(dep1) && !base::is.null(dep2) && !base::is.na(dep1) && !base::is.na(dep2)) {
      config$filtering$depth$min_depth <- base::min(dep1, dep2)
      config$filtering$depth$max_depth <- base::max(dep1, dep2)
    }
  }
  
  # Process Step 5 Module specific inputs (Clustering Strategy)
  if (ns_prefix == "step5_strategy-") {
    grp_method <- .safe_grab("grouping_method_input")
    if (!base::is.null(grp_method)) config$strategy$method <- grp_method
    
    norm_method <- .safe_grab("normalization_method_input")
    if (!base::is.null(norm_method)) config$clustering$normalization_method <- if(norm_method == "Ratio to Tchla") "tchla" else "none"
    
    trans_method <- .safe_grab("transformation_method_input")
    if (!base::is.null(trans_method)) config$clustering$transformation_method <- if(trans_method == "Log(x+1)") "log" else "none"
    
    clust_method <- .safe_grab("cluster_method_input")
    if (!base::is.null(clust_method)) config$clustering$cluster_method <- if(clust_method == "Hierarchical (Ward)") "hclust" else "kmeans"
    
    k_mode <- .safe_grab("k_determination_mode")
    if (!base::is.null(k_mode)) config$clustering$k_determination <- base::tolower(k_mode)
    
    k_max <- .safe_grab("k_max_input")
    if (!base::is.null(k_max) && !base::is.na(k_max)) config$clustering$k_max <- k_max
  }
  
  return(config)
}

#' Synchronize Config Object out to the UI (Memory Load)
update_all_ui_from_config <- function(config, session) {
  if (base::is.null(config)) return()
  
  # Helper to safely extract and prevent NA injection into UI
  `%||%` <- function(a, b) if (!base::is.null(a) && !base::is.na(a)) a else b
  
  # 1. Run Settings
  shiny::updateNumericInput(session, "niter_input", value = config$phytoclass$niter %||% 500)
  shiny::updateNumericInput(session, "step_size_input", value = config$phytoclass$step_size %||% 0.009)
  
  # 2. File Paths
  shiny::updateTextInput(session, "output_dir_ui", value = config$workspace$output_directory %||% "")
  shiny::updateTextInput(session, "fm_pro_path_ui", value = config$workspace$fm_pro_matrix_path %||% "")
  shiny::updateTextInput(session, "fm_nopro_path_ui", value = config$workspace$fm_nopro_matrix_path %||% "")
  
  # 3. Data Cleaning
  shiny::updateCheckboxInput(session, "toggle_handle_duplicates", value = base::isTRUE(config$data_cleaning$handle_duplicates$enabled))
  shiny::updateCheckboxInput(session, "toggle_handle_nas", value = base::isTRUE(config$data_cleaning$handle_pigment_nas$enabled))
  shiny::updateCheckboxInput(session, "toggle_handle_negatives", value = base::isTRUE(config$data_cleaning$enforce_non_negative_pigments$enabled))
  shiny::updateCheckboxInput(session, "toggle_handle_zerosum", value = base::isTRUE(config$data_cleaning$handle_zero_pigment_sum$enabled))
  
  # 4. Filters
  shiny::updateCheckboxInput(session, "toggle_geo_filter", value = base::isTRUE(config$filtering$geospatial$enabled))
  shiny::updateNumericInput(session, "min_lat_ui", value = config$filtering$geospatial$min_latitude %||% -90)
  shiny::updateNumericInput(session, "max_lat_ui", value = config$filtering$geospatial$max_latitude %||% 90)
  shiny::updateNumericInput(session, "min_lon_ui", value = config$filtering$geospatial$min_longitude %||% -180)
  shiny::updateNumericInput(session, "max_lon_ui", value = config$filtering$geospatial$max_longitude %||% 180)
  
  shiny::updateCheckboxInput(session, "toggle_temporal_filter", value = base::isTRUE(config$filtering$temporal$enabled))
  shiny::updateDateInput(session, "start_date_ui", value = config$filtering$temporal$start_date %||% "1900-01-01")
  shiny::updateDateInput(session, "end_date_ui", value = config$filtering$temporal$end_date %||% base::Sys.Date())
  
  shiny::updateCheckboxInput(session, "toggle_depth_filter", value = base::isTRUE(config$filtering$depth$enabled))
  shiny::updateNumericInput(session, "min_depth_ui", value = config$filtering$depth$min_depth %||% 0)
  shiny::updateNumericInput(session, "max_depth_ui", value = config$filtering$depth$max_depth %||% 1000)
  
  # 5. Step 5 Module (Clustering Strategy)
  ns <- function(id) base::paste0("step5_strategy-", id)
  
  shinyWidgets::updateRadioGroupButtons(session, ns("grouping_method_input"), selected = config$strategy$method %||% "By Source File")
  
  norm_str <- if((config$clustering$normalization_method %||% "tchla") == "tchla") "Ratio to Tchla" else "Raw Data"
  shiny::updateSelectInput(session, ns("normalization_method_input"), selected = norm_str)
  
  trans_str <- if((config$clustering$transformation_method %||% "log") == "log") "Log(x+1)" else "None"
  shiny::updateSelectInput(session, ns("transformation_method_input"), selected = trans_str)
  
  clust_str <- if((config$clustering$cluster_method %||% "hclust") == "hclust") "Hierarchical (Ward)" else "K-Means"
  shiny::updateSelectInput(session, ns("cluster_method_input"), selected = clust_str)
  
  k_str <- if((config$clustering$k_determination %||% "auto") == "auto") "Auto" else "Manual"
  shiny::updateRadioButtons(session, ns("k_determination_mode"), selected = k_str)
  
  shiny::updateNumericInput(session, ns("k_max_input"), value = config$clustering$k_max %||% 4)
}