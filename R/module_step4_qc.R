# ============================================================================
#
#   _phytoclass_Shiny V1.0 - STEP 4: QC SUMMARY
#
#   Description:
#   The visual interface for Quality Control. It runs the checks and displays
#   a summary table showing exactly how many samples passed or failed your
#   cleaning rules.
#
# ============================================================================

# --- UI Function ---
qcUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Step 4: QC & Filtering"),
    p("Apply cleaning rules and environmental filters. This process merges all your files into one standardized dataset."),
    hr(),
    
    fluidRow(
      column(10, offset = 1,
             wellPanel(
               h4("1. Execute Pipeline"),
               div(style = "text-align: center;",
                   actionButton(ns("run_qc_and_aggregate_btn"), "Run QC Pipeline", 
                                class = "btn-primary btn-lg", icon = icon("cogs"))
               )
             ),
             
             wellPanel(
               h4("2. QC Audit Summary"),
               p("Summary of samples passing/failing based on the rules defined in Step 1."),
               DTOutput(ns("qc_summary_table"))
             )
      )
    )
  )
}

# --- Server Function ---
qcServer <- function(id, rv, .log_event, .update_workflow_state, reset_downstream_data) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    observeEvent(input$run_qc_and_aggregate_btn, {
      req(rv$config, length(rv$datasets_processed) > 0)
      
      show_modal_spinner(text = "Processing and standardizing datasets...")
      tryCatch({
        # Run the core logic
        qc_results <- run_qc_pipeline(rv$datasets_processed, rv$config)
        
        # Update global state
        rv$datasets_processed <- qc_results$datasets_annotated
        rv$master_qc_data <- qc_results$master_qc_data
        
        if(!is.null(rv$master_qc_data) && nrow(rv$master_qc_data) > 0) {
          .log_event(paste("Success: QC complete.", nrow(rv$master_qc_data), "samples standardized and passed."))
          reset_downstream_data("strategy")
          .update_workflow_state("step5")
          updateNavbarPage(session, "main_navbar", selected = "step5")
        } else {
          .log_event("Warning: No samples passed QC filters.")
          showNotification("No samples passed. Check your Latitude/Longitude or Depth bounds in Step 1.", 
                           type = "warning", duration = 10)
        }
        
        rv$qc_summary_df <- .generate_qc_summary_df(rv$datasets_processed)
        
      }, error = function(e){ 
        .log_event(paste("FATAL QC ERROR:", e$message))
        showModal(modalDialog(title = "QC Pipeline Failure", e$message))
      }, finally = { 
        remove_modal_spinner() 
      })
    })
    
    output$qc_summary_table <- renderDT({
      req(rv$qc_summary_df)
      datatable(rv$qc_summary_df, rownames = FALSE, options = list(dom = 't', ordering = FALSE)) %>%
        formatStyle('Failed', color = styleInterval(0, c('black', '#dc3545')), 
                    fontWeight = styleInterval(0, c('normal', 'bold'))) %>%
        formatStyle('Passed', color = '#28a745')
    })
  })
}

#' Execute the QC and Filtering Pipeline
run_qc_pipeline <- function(datasets_processed, config) {
  
  datasets_annotated <- list()
  master_list <- list()
  
  cleaning_cfg <- config$data_cleaning
  filter_cfg <- config$filtering
  
  for (ds_name in names(datasets_processed)) {
    ds_obj <- datasets_processed[[ds_name]]
    data <- ds_obj$data
    rename_map <- ds_obj$rename_map
    
    # -----------------------------------------------------------------------
    # CRITICAL FIX: FORCE NUMERIC CONVERSION BEFORE ANY CHECKS
    # -----------------------------------------------------------------------
    # Identify every column the user has mapped (Pigments + Meta)
    cols_to_convert <- unique(unlist(rename_map))
    cols_to_convert <- cols_to_convert[cols_to_convert %in% colnames(data)]
    
    # Also include standard generated columns if they exist
    cols_to_convert <- c(cols_to_convert, intersect(c("year", "month", "day"), colnames(data)))
    
    # Force conversion now. This prevents 'x must be numeric' in rowSums
    data <- data %>%
      mutate(across(all_of(cols_to_convert), safe_as_numeric))
    # -----------------------------------------------------------------------
    
    # Identify pigment columns for cleaning logic
    meta_keys <- config$general$metadata_keys
    pigment_keys <- setdiff(names(config$column_aliases), meta_keys)
    mapped_pigment_cols <- unlist(rename_map[pigment_keys])
    mapped_pigment_cols <- mapped_pigment_cols[!is.na(mapped_pigment_cols)]
    
    # --- 1. Data Cleaning ---
    data$cleaning_status <- "Keep"
    data$duplicate_status <- "Keep_Unique"
    
    if (length(mapped_pigment_cols) > 0) {
      
      # A. Handle NAs (Already numeric now)
      if (isTRUE(cleaning_cfg$handle_pigment_nas$enabled)) {
        data <- data %>% mutate(across(all_of(mapped_pigment_cols), ~ {
          vals <- . ; vals[is.na(vals)] <- 0; vals
        }))
      }
      
      # B. Handle Negatives
      if (isTRUE(cleaning_cfg$enforce_non_negative_pigments$enabled)) {
        data <- data %>% mutate(across(all_of(mapped_pigment_cols), ~ {
          vals <- . ; vals[vals < 0] <- 0; vals
        }))
      }
      
      # C. Handle Zero-Sum (This is where 'x must be numeric' usually triggers)
      if (isTRUE(cleaning_cfg$handle_zero_pigment_sum$enabled)) {
        # Force into a matrix to ensure rowSums is happy
        pigment_matrix <- as.matrix(data[, mapped_pigment_cols, drop = FALSE])
        row_sums <- rowSums(pigment_matrix, na.rm = TRUE)
        data$cleaning_status[row_sums <= 1e-9] <- "Flagged_ZeroSum"
      }
    }
    
    # --- 2. Environmental Filtering (With Range Safety) ---
    data$filter_status_geo <- "Keep"
    data$filter_status_temporal <- "Keep"
    data$filter_status_depth <- "Keep"
    
    if (isTRUE(filter_cfg$geospatial$enabled)) {
      lat_col <- rename_map$latitude
      lon_col <- rename_map$longitude
      if (!is.null(lat_col) && !is.null(lon_col)) {
        lat <- safe_as_numeric(data[[lat_col]])
        lon <- safe_as_numeric(data[[lon_col]])
        # Fix: ensure min is actually the smaller number
        min_lat <- min(filter_cfg$geospatial$min_latitude, filter_cfg$geospatial$max_latitude)
        max_lat <- max(filter_cfg$geospatial$min_latitude, filter_cfg$geospatial$max_latitude)
        min_lon <- min(filter_cfg$geospatial$min_longitude, filter_cfg$geospatial$max_longitude)
        max_lon <- max(filter_cfg$geospatial$min_longitude, filter_cfg$geospatial$max_longitude)
        
        bad_geo <- is.na(lat) | is.na(lon) | lat < min_lat | lat > max_lat | lon < min_lon | lon > max_lon
        data$filter_status_geo[bad_geo] <- "Flagged_GeoRange"
      }
    }
    
    if (isTRUE(filter_cfg$temporal$enabled)) {
      if (all(c("year", "month", "day") %in% colnames(data))) {
        dates <- as.Date(ISOdate(data$year, data$month, data$day))
        d1 <- as.Date(filter_cfg$temporal$start_date)
        d2 <- as.Date(filter_cfg$temporal$end_date)
        bad_time <- is.na(dates) | dates < min(d1, d2) | dates > max(d1, d2)
        data$filter_status_temporal[bad_time] <- "Flagged_DateRange"
      }
    }
    
    if (isTRUE(filter_cfg$depth$enabled) && !is.null(rename_map$depth)) {
      depth_val <- safe_as_numeric(data[[rename_map$depth]])
      d_min <- min(filter_cfg$depth$min_depth, filter_cfg$depth$max_depth)
      d_max <- max(filter_cfg$depth$min_depth, filter_cfg$depth$max_depth)
      data$filter_status_depth[is.na(depth_val) | depth_val < d_min | depth_val > d_max] <- "Flagged_DepthRange"
    }
    
    # --- 3. Final Pass & Standardization ---
    data$qc_pass <- (data$cleaning_status == "Keep" & data$duplicate_status == "Keep_Unique" &
                       data$filter_status_geo == "Keep" & data$filter_status_temporal == "Keep" &
                       data$filter_status_depth == "Keep")
    
    ds_obj$data_annotated <- data
    datasets_annotated[[ds_name]] <- ds_obj
    
    passed_rows <- data %>% filter(qc_pass)
    
    if (nrow(passed_rows) > 0) {
      # Build Rename Map: c(STANDARD_KEY = "USER_COL_NAME")
      rename_vec <- c(UniqueID = "UniqueID", source_dataset = "source_dataset")
      if("year" %in% names(passed_rows)) rename_vec["year"] <- "year"
      if("month" %in% names(passed_rows)) rename_vec["month"] <- "month"
      if("day" %in% names(passed_rows)) rename_vec["day"] <- "day"
      
      for(std_key in names(rename_map)) {
        user_col <- rename_map[[std_key]]
        if(!is.null(user_col) && user_col %in% names(passed_rows)) rename_vec[std_key] <- user_col
      }
      
      # Select and rename simultaneously
      std_data <- passed_rows %>% dplyr::select(all_of(rename_vec))
      
      # Force numeric types to prevent bind_rows crashes
      numeric_cols <- setdiff(names(std_data), c("UniqueID", "source_dataset"))
      std_data <- std_data %>% mutate(across(all_of(numeric_cols), safe_as_numeric))
      
      master_list[[ds_name]] <- std_data
    }
  }
  
  master_qc_data <- if (length(master_list) > 0) dplyr::bind_rows(master_list) else NULL
  return(list(datasets_annotated = datasets_annotated, master_qc_data = master_qc_data))
}

.generate_qc_summary_df <- function(datasets_processed) {
  if(length(datasets_processed) == 0) return(tibble())
  all_ann <- dplyr::bind_rows(lapply(datasets_processed, `[[`, "data_annotated"))
  total <- sum(purrr::map_int(datasets_processed, ~nrow(.x$data_original)))
  
  summary_df <- tibble(
    Category = c("Start", "Pigment QC", "Pigment QC", "Pigment QC", "Filtering", "Filtering", "Filtering", "Finish"),
    Rule = c("Total Loaded", "NAs/Negatives", "Zero Sums", "Duplicates", "Geospatial", "Temporal", "Depth", "Total Passed"),
    Failed = c(0, sum(all_ann$cleaning_status == "Flagged_NA"), sum(all_ann$cleaning_status == "Flagged_ZeroSum"),
               sum(all_ann$duplicate_status == "Flagged_Duplicate"), sum(all_ann$filter_status_geo != "Keep"),
               sum(all_ann$filter_status_temporal != "Keep"), sum(all_ann$filter_status_depth != "Keep"), 0)
  )
  
  # Final Count
  pass_count <- sum(all_ann$qc_pass)
  summary_df$Failed[8] <- total - pass_count
  summary_df$Passed <- total - summary_df$Failed
  return(summary_df)
}