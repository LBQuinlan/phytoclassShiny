# ============================================================================
#
#   _phytoclass_Shiny V1.0 - SETTINGS MANAGER
#
#   Description:
#   Handles the saving and loading of your session settings. It ensures that
#   if you load an old session, it is automatically updated to include any
#   new features from the latest template.
#
# ============================================================================



CONFIG_TEMPLATE_PATH <- "config_template.yaml"
CONFIG_SESSION_PATH <- "config_session.yaml"

ui_config_map <- list(
  list(id="output_dir_ui",    path=c("workspace", "output_directory"),            type="text"),
  list(id="fm_pro_path_ui",   path=c("workspace", "fm_pro_matrix_path"),          type="text"),
  list(id="fm_nopro_path_ui", path=c("workspace", "fm_nopro_matrix_path"),        type="text"),
  
  list(id="toggle_handle_duplicates", path=c("data_cleaning", "handle_duplicates", "enabled"),       type="checkbox"),
  list(id="toggle_handle_nas",        path=c("data_cleaning", "handle_pigment_nas", "enabled"),      type="checkbox"),
  list(id="toggle_handle_negatives",  path=c("data_cleaning", "enforce_non_negative_pigments", "enabled"), type="checkbox"),
  list(id="toggle_handle_zerosum",    path=c("data_cleaning", "handle_zero_pigment_sum", "enabled"), type="checkbox"),
  
  list(id="niter_input",      path=c("phytoclass", "niter"),                     type="numeric"),
  list(id="step_size_input",  path=c("phytoclass", "step_size"),                 type="numeric"),
  
  list(id="toggle_geo_filter",      path=c("filtering", "geospatial", "enabled"),      type="checkbox"),
  list(id="min_lat_ui",             path=c("filtering", "geospatial", "min_latitude"), type="numeric"),
  list(id="max_lat_ui",             path=c("filtering", "geospatial", "max_latitude"), type="numeric"),
  list(id="min_lon_ui",             path=c("filtering", "geospatial", "min_longitude"),type="numeric"),
  list(id="max_lon_ui",             path=c("filtering", "geospatial", "max_longitude"),type="numeric"),
  
  list(id="toggle_temporal_filter", path=c("filtering", "temporal", "enabled"),        type="checkbox"),
  list(id="start_date_ui",          path=c("filtering", "temporal", "start_date"),     type="date"),
  list(id="end_date_ui",            path=c("filtering", "temporal", "end_date"),       type="date"),
  
  list(id="toggle_depth_filter",    path=c("filtering", "depth", "enabled"),           type="checkbox"),
  list(id="min_depth_ui",           path=c("filtering", "depth", "min_depth"),        type="numeric"),
  list(id="max_depth_ui",           path=c("filtering", "depth", "max_depth"),        type="numeric")
)

ui_config_map_modules <- list(
  list(id="imputation_method_input",   path=c("clustering", "imputation_method"),    type="select", ns_prefix="step5_strategy-"),
  list(id="normalization_method_input",path=c("clustering", "normalization_method"), type="select", ns_prefix="step5_strategy-"),
  list(id="transformation_method_input", path=c("clustering", "transformation_method"),type="select", ns_prefix="step5_strategy-"),
  list(id="cluster_method_input",      path=c("clustering", "cluster_method"),       type="select", ns_prefix="step5_strategy-"),
  list(id="distance_method_input",     path=c("clustering", "distance_method"),      type="select", ns_prefix="step5_strategy-"),
  list(id="k_max_input",               path=c("clustering", "k_max"),                type="numeric", ns_prefix="step5_strategy-")
)


#' Initialize Configuration
#'
#' Loads the saved session config if it exists, BUT overlays it on top of the
#' template. This ensures that if the template has new fields (like colors),
#' they are preserved.
initialize_config <- function() {
  if (file.exists(CONFIG_SESSION_PATH)) {
    cat(sprintf("--- Found saved session. Merging '%s' with Template ---\n", CONFIG_SESSION_PATH))
    return(.load_and_merge(CONFIG_SESSION_PATH))
  } else {
    cat(sprintf("--- No session found. Loading default template '%s' ---\n", CONFIG_TEMPLATE_PATH))
    return(load_config(CONFIG_TEMPLATE_PATH))
  }
}

#' Internal helper to load session and merge with template defaults
.load_and_merge <- function(session_path) {
  # 1. Load the pristine template (contains all structure + colors)
  template <- load_config(CONFIG_TEMPLATE_PATH)
  
  # 2. Load the user's saved session
  session <- load_config(session_path)
  
  # 3. Merge: Update template with session values. 
  # This keeps template keys (like plotting colors) that might be missing in session.
  merged_config <- utils::modifyList(template, session)
  
  return(merged_config)
}

#' Load and Parse a YAML Configuration File
load_config <- function(config_path) {
  if (!file.exists(config_path)) stop("FATAL ERROR: Config file not found: ", config_path)
  tryCatch({
    config <- yaml::read_yaml(config_path)
    # Basic validation
    if (!is.list(config)) stop("Config did not load as a list.")
    return(config)
  }, error = function(e) {
    stop(sprintf("FATAL ERROR: Failed to parse %s. Check formatting. Error: %s", config_path, e$message))
  })
}

#' Save the Configuration List to a YAML File
save_config <- function(config, config_path) {
  tryCatch({
    yaml::write_yaml(config, config_path)
  }, error = function(e) {
    stop(sprintf("FATAL ERROR: Failed to write config to %s. Error: %s", config_path, e$message))
  })
}


#' Update All UI Inputs from a Loaded Config Object
update_all_ui_from_config <- function(config, session) {
  cat("--- Syncing UI with loaded configuration ---\n")
  
  full_map <- c(ui_config_map, ui_config_map_modules)
  
  for (item in full_map) {
    value <- purrr::pluck(config, !!!item$path)
    
    if (!is.null(value)) {
      update_function <- switch(item$type,
                                "text"     = updateTextInput,
                                "numeric"  = updateNumericInput,
                                "checkbox" = updateCheckboxInput,
                                "date"     = updateDateInput,
                                "select"   = updateSelectInput,
                                "picker"   = updatePickerInput
      )
      
      if (item$type == "checkbox") {
        update_function(session, item$id, value = as.logical(value))
      } else if (item$type %in% c("select", "picker")) {
        update_function(session, item$id, selected = value)
      } else {
        if(item$type == "date") value <- as.Date(value)
        update_function(session, item$id, value = value)
      }
    }
  }
}

#' Sync the Config Object with Current UI Settings
sync_config_with_ui <- function(config, input, ns_prefix = "") {
  cat("--- Syncing configuration with current UI settings ---\n")
  
  map_to_use <- if (ns_prefix != "") ui_config_map_modules else ui_config_map
  
  for (item in map_to_use) {
    ui_id <- if (ns_prefix != "") paste0(item$ns_prefix, item$id) else item$id
    ui_value <- input[[ui_id]]
    
    if (!is.null(ui_value)) {
      if (item$type == "date") ui_value <- as.character(ui_value)
      config <- purrr::assign_in(config, item$path, ui_value)
    }
  }
  return(config)
}