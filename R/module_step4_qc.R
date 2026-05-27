# ============================================================================
# MODULE: Step 4 - Filter & Clean
# Description: Handles automated quality control and data filtering.
# ============================================================================

qcUI <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::h3("Step 4: Filter & Clean"),
    shiny::p("Review the automated quality control and filtering results before grouping."),
    shiny::hr(),
    
    shiny::fluidRow(
      shiny::column(4,
                    shiny::wellPanel(
                      shiny::h4(shiny::icon("shield-alt"), " Execute Quality Control"),
                      shiny::p("Apply the cleaning rules and data filters configured in Step 1."),
                      shiny::actionButton(ns("run_qc_btn"), "Clean My Data", class = "btn-primary w-100 fw-bold", icon = shiny::icon("magic"))
                    )
      ),
      shiny::column(8,
                    shiny::wellPanel(
                      shiny::h4("Quality Control & Filtering Breakdown"),
                      DT::DTOutput(ns("qc_summary_table"))
                    )
      )
    )
  )
}

qcServer <- function(id, rv, .log_event, .update_workflow_state, reset_downstream_data) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    shiny::observeEvent(input$run_qc_btn, {
      if (base::length(rv$staging_datasets) == 0) {
        shiny::showNotification("No data available to clean. Please complete Step 3.", type = "warning")
        return()
      }
      
      .log_event("QC", "Initiating Quality Control pipeline...")
      shinybusy::show_modal_spinner(text = "Applying cleaning rules and filters...")
      
      temp_qc_list <- base::list()
      summary_rows <- base::list()
      
      cfg_clean <- rv$config$data_cleaning
      cfg_filt <- rv$config$filtering
      
      for (ds_name in base::names(rv$staging_datasets)) {
        ds <- rv$staging_datasets[[ds_name]]
        df <- ds$data
        rename_map <- ds$rename_map
        
        initial_n <- base::nrow(df)
        

        # Shield coordinates, dates, and ID columns from the QC sweep
        protected_cols <- base::c("UniqueID", "SourceFile", "original_row_num", "year", "month", "day", "date", "time", "Station", "Cruise", "Zone")
        
        # Add mapped coordinates if they exist
        if (!base::is.null(rename_map$latitude)) protected_cols <- base::c(protected_cols, rename_map$latitude)
        if (!base::is.null(rename_map$longitude)) protected_cols <- base::c(protected_cols, rename_map$longitude)
        if (!base::is.null(rename_map$depth)) protected_cols <- base::c(protected_cols, rename_map$depth)
        
        # Catch any column name containing coordinate terms using a fuzzy match
        geo_matches <- base::grep("(?i)lat|lon|depth", base::colnames(df), value = TRUE)
        protected_cols <- base::unique(base::c(protected_cols, geo_matches))
        
        # Define the target columns (Only the Pigment data)
        target_cols <- base::setdiff(base::colnames(df), protected_cols)
        # =========================================================================
        
        # 1. Duplicates
        if (base::isTRUE(cfg_clean$handle_duplicates$enabled)) {
          df <- df |> dplyr::distinct(dplyr::across(dplyr::any_of(target_cols)), .keep_all = TRUE)
        }
        n_after_dup <- base::nrow(df)
        dropped_dup <- initial_n - n_after_dup
        
        # 2. Handle NAs (Targeted ONLY at pigments)
        if (base::isTRUE(cfg_clean$handle_pigment_nas$enabled)) {
          df <- df |> 
            dplyr::mutate(dplyr::across(dplyr::any_of(target_cols), ~ base::suppressWarnings(base::as.numeric(.x)))) |>
            dplyr::mutate(dplyr::across(dplyr::any_of(target_cols), ~ base::ifelse(base::is.na(.x), 0, .x)))
        }
        
        # 3. Handle Negatives (Targeted ONLY at pigments - SAVES LATITUDE DATA)
        if (base::isTRUE(cfg_clean$enforce_non_negative_pigments$enabled)) {
          df <- df |> 
            dplyr::mutate(dplyr::across(dplyr::any_of(target_cols), ~ base::ifelse(base::is.numeric(.x) & .x < 0, 0, .x)))
        }
        
        # 4. Filter empty samples (zerosum rows where no pigments exist)
        if (base::isTRUE(cfg_clean$handle_zerosum$enabled) && base::length(target_cols) > 0) {
          # Use safe_as_numeric to prevent char crashes
          num_df <- df[, target_cols, drop = FALSE] |> dplyr::mutate(dplyr::across(dplyr::everything(), ~ base::suppressWarnings(base::as.numeric(.x))))
          row_sums <- base::rowSums(num_df, na.rm = TRUE)
          df <- df[row_sums > 0, ]
        }
        n_after_zero <- base::nrow(df)
        dropped_zero <- n_after_dup - n_after_zero
        
        # 5. Geo Filter
        n_before_geo <- base::nrow(df)
        if (base::isTRUE(cfg_filt$geospatial$enabled) && !base::is.null(rename_map$latitude) && !base::is.null(rename_map$longitude)) {
          lat_col <- rename_map$latitude
          lon_col <- rename_map$longitude
          min_lat <- base::as.numeric(rv$config$filtering$geospatial$min_lat %||% -90)
          max_lat <- base::as.numeric(rv$config$filtering$geospatial$max_lat %||% 90)
          min_lon <- base::as.numeric(rv$config$filtering$geospatial$min_lon %||% -180)
          max_lon <- base::as.numeric(rv$config$filtering$geospatial$max_lon %||% 180)
          
          df <- df |> dplyr::filter(
            base::as.numeric(.data[[lat_col]]) >= min_lat & base::as.numeric(.data[[lat_col]]) <= max_lat &
              base::as.numeric(.data[[lon_col]]) >= min_lon & base::as.numeric(.data[[lon_col]]) <= max_lon
          )
        }
        dropped_geo <- n_before_geo - base::nrow(df)
        
        # 6. Temporal Filter
        n_before_temp <- base::nrow(df)
        if (base::isTRUE(cfg_filt$temporal$enabled) && base::all(base::c("year", "month", "day") %in% base::colnames(df))) {
          start_date <- base::as.Date(rv$config$filtering$temporal$start_date %||% "1900-01-01")
          end_date <- base::as.Date(rv$config$filtering$temporal$end_date %||% base::Sys.Date())
          
          df <- df |> 
            dplyr::mutate(parsed_date_tmp = base::suppressWarnings(base::as.Date(base::paste(year, month, day, sep="-")))) |>
            dplyr::filter(!base::is.na(parsed_date_tmp) & parsed_date_tmp >= start_date & parsed_date_tmp <= end_date) |>
            dplyr::select(-parsed_date_tmp)
        }
        dropped_temp <- n_before_temp - base::nrow(df)
        
        # 7. Depth Filter
        n_before_depth <- base::nrow(df)
        if (base::isTRUE(cfg_filt$depth$enabled) && !base::is.null(rename_map$depth)) {
          depth_col <- rename_map$depth
          min_depth <- base::as.numeric(rv$config$filtering$depth$min_depth %||% 0)
          max_depth <- base::as.numeric(rv$config$filtering$depth$max_depth %||% 10000)
          
          df <- df |> dplyr::filter(base::as.numeric(.data[[depth_col]]) >= min_depth & base::as.numeric(.data[[depth_col]]) <= max_depth)
        }
        dropped_depth <- n_before_depth - base::nrow(df)
        
        final_n <- base::nrow(df)
        
        ds$data <- df
        temp_qc_list[[ds_name]] <- ds
        
        summary_rows[[ds_name]] <- tibble::tibble(
          Dataset = ds_name,
          `Original Samples` = initial_n,
          `Dropped (Duplicates)` = dropped_dup,
          `Dropped (Empty)` = dropped_zero,
          `Dropped (Geo Filter)` = dropped_geo,
          `Dropped (Date Filter)` = dropped_temp,
          `Dropped (Depth Filter)` = dropped_depth,
          `Final Samples` = final_n
        )
      }
      
      rv$master_qc_data <- dplyr::bind_rows(purrr::map(temp_qc_list, ~.x$data))
      rv$analysis_datasets <- temp_qc_list
      rv$qc_summary_df <- dplyr::bind_rows(summary_rows)
      
      .update_workflow_state("step5")
      
      shinybusy::remove_modal_spinner()
      shiny::showNotification("Quality Control & Filtering complete.", type = "message")
    })
    
    output$qc_summary_table <- DT::renderDT({
      shiny::req(rv$qc_summary_df)
      DT::datatable(rv$qc_summary_df, rownames = FALSE, options = base::list(pageLength = 10, scrollX = TRUE)) |>
        DT::formatStyle("Final Samples", fontWeight = "bold", color = "#198754")
    })
    
  })
}