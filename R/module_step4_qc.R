qcUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Step 4: Clean Data"),
    shiny::p("This step automatically removes bad data based on the rules you set in Step 1.", class="text-muted"),
    shiny::hr(),
    bslib::card(shiny::h4(shiny::icon("cogs"), " 1. Run Cleaner"), shiny::actionButton(ns("run_qc_btn"), "Clean My Data", icon = shiny::icon("shield-alt"), class = "btn-primary w-100 fw-bold")),
    shinyjs::hidden(
      shiny::div(id = ns("qc_results_container"),
                 bslib::card(shiny::h4(shiny::icon("chart-bar"), " Data Kept vs. Data Dropped"), shiny::fluidRow(shiny::column(3, shiny::wellPanel(class="text-center", shiny::h5("Total Input"), shiny::h3(shiny::textOutput(ns("tot_in"))))), shiny::column(1, shiny::div(style="font-size: 2em; color: #ced4da; text-align: center; margin-top: 15px;", shiny::icon("chevron-right"))), shiny::column(3, shiny::wellPanel(class="text-center", style="background-color: #fff3cd;", shiny::h5("Removed"), shiny::h3(shiny::textOutput(ns("tot_rem")), style="color: #856404;"))), shiny::column(1, shiny::div(style="font-size: 2em; color: #ced4da; text-align: center; margin-top: 15px;", shiny::icon("chevron-right"))), shiny::column(4, shiny::wellPanel(class="text-center", style="background-color: #d1e7dd;", shiny::h5("Total Passed"), shiny::h3(shiny::textOutput(ns("tot_pass")), style="color: #0f5132;"))))),
                 bslib::card(shiny::h4(shiny::icon("table"), " 2. File Breakdown"), shiny::p("See how many samples were removed from each file.", class="text-muted small"), DT::DTOutput(ns("dataset_breakdown_table")))
      )
    )
  )
}

qcServer <- function(id, rv, .log_event, .update_workflow_state, reset_downstream_data) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- UI RESET WATCHER ---
    # Instantly hides the results container if upstream settings wipe the memory buffer
    shiny::observeEvent(rv$qc_summary_df, {
      if (base::is.null(rv$qc_summary_df)) {
        shinyjs::hide("qc_results_container")
      }
    }, ignoreNULL = FALSE)
    
    shiny::observeEvent(input$run_qc_btn, {
      shiny::req(base::length(rv$staging_datasets) > 0)
      .log_event("QC", "Commencing strict Quality Control triage pipeline.")
      reset_downstream_data("strategy")
      master_rows <- base::list(); summary_rows <- base::list()
      meta_anchors <- base::c("UniqueID", "SourceFile", "Lat", "Lon", "Depth", "Date", "Time", "Station", "Cruise", "year", "month", "day")
      
      for (ds in rv$staging_datasets) {
        df_clean <- ds$data; initial_n <- base::nrow(df_clean)
        target_pigments <- base::setdiff(base::names(df_clean), meta_anchors)
        if (base::length(target_pigments) > 0) df_clean[target_pigments] <- base::lapply(df_clean[target_pigments], function(x) base::suppressWarnings(base::as.numeric(base::as.character(x))))
        
        numeric_cols <- base::sapply(df_clean, base::is.numeric)
        if (base::isTRUE(rv$config$data_cleaning$handle_pigment_nas$enabled)) df_clean[numeric_cols][base::is.na(df_clean[numeric_cols])] <- 0
        if (base::isTRUE(rv$config$data_cleaning$enforce_non_negative_pigments$enabled)) df_clean[numeric_cols][df_clean[numeric_cols] < 0] <- 0
        
        fail_pigment <- 0
        if (base::isTRUE(rv$config$data_cleaning$handle_zero_pigment_sum$enabled)) {
          pigment_sums <- base::rowSums(df_clean[target_pigments], na.rm = TRUE); valid_rows <- pigment_sums > 0
          fail_pigment <- base::sum(!valid_rows, na.rm = TRUE); df_clean <- df_clean[valid_rows, , drop = FALSE]
        }
        
        fail_dup <- 0
        if (base::isTRUE(rv$config$data_cleaning$handle_duplicates$enabled) && base::nrow(df_clean) > 0) {
          is_dup <- base::duplicated(df_clean); fail_dup <- base::sum(is_dup); df_clean <- df_clean[!is_dup, , drop = FALSE]
        }
        
        fail_env <- 0
        if (base::nrow(df_clean) > 0) {
          env_pass_mask <- base::rep(TRUE, base::nrow(df_clean))
          if (base::isTRUE(rv$config$filtering$geospatial$enabled) && base::all(base::c("Lat", "Lon") %in% base::names(df_clean))) { 
            lat_mask <- df_clean$Lat >= rv$config$filtering$geospatial$min_latitude & df_clean$Lat <= rv$config$filtering$geospatial$max_latitude; 
            lon_mask <- df_clean$Lon >= rv$config$filtering$geospatial$min_longitude & df_clean$Lon <= rv$config$filtering$geospatial$max_longitude; 
            lat_mask[base::is.na(lat_mask)] <- FALSE; lon_mask[base::is.na(lon_mask)] <- FALSE; env_pass_mask <- env_pass_mask & lat_mask & lon_mask 
          }
          if (base::isTRUE(rv$config$filtering$temporal$enabled) && base::all(base::c("year", "month", "day") %in% base::names(df_clean))) { 
            parsed_dates <- base::as.Date(base::sprintf("%04d-%02d-%02d", df_clean$year, df_clean$month, df_clean$day))
            date_mask <- parsed_dates >= base::as.Date(rv$config$filtering$temporal$start_date) & parsed_dates <= base::as.Date(rv$config$filtering$temporal$end_date)
            date_mask[base::is.na(date_mask)] <- FALSE
            env_pass_mask <- env_pass_mask & date_mask 
          }
          if (base::isTRUE(rv$config$filtering$depth$enabled) && "Depth" %in% base::names(df_clean)) { 
            depth_mask <- df_clean$Depth >= rv$config$filtering$depth$min_depth & df_clean$Depth <= rv$config$filtering$depth$max_depth; 
            depth_mask[base::is.na(depth_mask)] <- FALSE; env_pass_mask <- env_pass_mask & depth_mask 
          }
          fail_env <- base::sum(!env_pass_mask); df_clean <- df_clean[env_pass_mask, , drop = FALSE]
        }
        
        passed_n <- base::nrow(df_clean)
        yield_pct <- if(initial_n > 0) base::round((passed_n / initial_n) * 100, 1) else 0
        summary_rows[[base::length(summary_rows) + 1]] <- tibble::tibble(`DATASET` = ds$name, `INPUT (N)` = initial_n, `FAIL: PIGMENT` = fail_pigment, `FAIL: DUPLICATE` = fail_dup, `FAIL: ENV. FILTER` = fail_env, `PASSED (N)` = passed_n, `YIELD (%)` = yield_pct)
        if (passed_n > 0) master_rows[[base::length(master_rows) + 1]] <- df_clean
      }
      
      rv$qc_summary_df <- dplyr::bind_rows(summary_rows)
      if (base::length(master_rows) > 0) { rv$master_qc_data <- dplyr::bind_rows(master_rows); .log_event("QC", base::sprintf("Success. %d samples passed.", base::nrow(rv$master_qc_data))) } else { rv$master_qc_data <- base::data.frame(); .log_event("WARNING", "Zero samples passed the QC pipeline criteria.") }
      shinyjs::show("qc_results_container")
      .update_workflow_state("step5")
    })
    
    output$tot_in <- shiny::renderText({ shiny::req(rv$qc_summary_df); base::sum(rv$qc_summary_df$`INPUT (N)`) })
    output$tot_rem <- shiny::renderText({ shiny::req(rv$qc_summary_df); base::sum(rv$qc_summary_df$`INPUT (N)`) - base::sum(rv$qc_summary_df$`PASSED (N)`) })
    output$tot_pass <- shiny::renderText({ shiny::req(rv$qc_summary_df); base::sum(rv$qc_summary_df$`PASSED (N)`) })
    
    output$dataset_breakdown_table <- DT::renderDT({
      shiny::req(rv$qc_summary_df); summary_df <- rv$qc_summary_df
      if (!base::isTRUE(rv$config$data_cleaning$handle_duplicates$enabled)) summary_df$`FAIL: DUPLICATE` <- NULL
      if (!base::isTRUE(rv$config$data_cleaning$handle_pigment_nas$enabled) && !base::isTRUE(rv$config$data_cleaning$enforce_non_negative_pigments$enabled) && !base::isTRUE(rv$config$data_cleaning$handle_zero_pigment_sum$enabled)) summary_df$`FAIL: PIGMENT` <- NULL
      if (!base::isTRUE(rv$config$filtering$geospatial$enabled) && !base::isTRUE(rv$config$filtering$temporal$enabled) && !base::isTRUE(rv$config$filtering$depth$enabled)) { if("FAIL: ENV. FILTER" %in% base::names(summary_df)) summary_df$`FAIL: ENV. FILTER` <- NULL }
      DT::datatable(summary_df, rownames = FALSE, options = base::list(scrollX = TRUE, pageLength = 10, dom = 't')) |> DT::formatStyle('YIELD (%)', backgroundColor = DT::styleInterval(c(50, 80), c('#f8d7da', '#fff3cd', '#d1e7dd')))
    })
  })
}