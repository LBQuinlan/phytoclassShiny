# ============================================================================
#
#   _phytoclass_Shiny V1.0 - MAIN APPLICATION LAUNCHER
#
#   Description:
#   Main Driver.
#  
# ============================================================================

# --- 0. PREAMBLE & INTELLIGENT SETUP ---
cat("--- Initializing _phytoclass_Shiny V1.0 ---\n")
options(shiny.maxRequestSize = 500 * 1024^2)

required_packages <- c(
  "shiny", "shinythemes", "shinyjs", "shinyWidgets", "shinybusy", 
  "DT", "yaml", "dplyr", "tidyr", "readxl", "openxlsx", 
  "lubridate", "digest", "rlang", "tibble", "fs", "purrr", 
  "scales", "vegan", "cluster", "factoextra", "ggplot2", 
  "MASS", "dynamicTreeCut", "glue", "stringdist", "phytoclass"
)

cat("--> Checking system dependencies...\n")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat(sprintf("    ...Installing missing package: %s\n", pkg))
    tryCatch({ install.packages(pkg, dependencies = TRUE) }, error = function(e) { stop(paste("Failed to install", pkg, ":", e$message)) })
  }
}

invisible(lapply(required_packages, library, character.only = TRUE))
cat("--> All libraries loaded.\n")

cat("--> Sourcing modules from 'R/'...\n")
tryCatch({
  scripts_to_source <- list.files("R", pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
  for (script in scripts_to_source) { source(script) }
}, error = function(e) { stop("FATAL ERROR: Failed to source R modules. Error: ", e$message) })
cat("--> All modules sourced successfully.\n\n")


# ============================================================================
# --- 1. USER INTERFACE (UI) ---
# ============================================================================

# --- JS: Simple Button Click Interceptor (No Timers) ---
jscode <- "
$(document).on('click', '.btn', function() {
  var id = $(this).attr('id');
  if (id) {
    Shiny.setInputValue('last_btn_clicked', id, {priority: 'event'});
  }
});
"

ui <- fluidPage(
  theme = shinytheme("spacelab"),
  shinyjs::useShinyjs(),
  tags$head(tags$script(HTML(jscode))), 
  add_busy_spinner(spin = "orbit", color = "#007bff", margins = c(70, 70)),
  
  tags$head(
    tags$style(HTML("
      .btn-primary { background-color: #007bff; border-color: #007bff; }
      .btn-success { background-color: #28a745; border-color: #28a745; }
      .well { background-color: #f8f9fa; border: 1px solid #dee2e6; }
      h4 { color: #0056b3; border-bottom: 2px solid #dee2e6; padding-bottom: 5px; margin-top: 0; }
      .navbar-right { display: flex; align-items: center; padding-right: 15px; }
      .navbar-right .btn { margin-left: 5px; }
      #session_audit_log { font-family: monospace; font-size: 0.85em; white-space: pre-wrap; word-wrap: break-word; background-color: #212529; color: #f8f9fa; padding: 10px; border-radius: 5px; height: 200px; overflow-y: auto; }
      .text-success { color: #28a745; font-weight: bold; }
      .text-primary { color: #007bff; font-weight: bold; }
    "))
  ),
  
  navbarPage(
    title = HTML("<i>phytoclass</i>Shiny"),
    collapsible = TRUE, 
    id = "main_navbar",
    
    header = tags$div(class = "navbar-right",
                      actionButton("save_config_btn_global", "Save Session", icon = icon("save"), class = "btn btn-success btn-sm"),
                      actionButton("help_btn_global", "Help", icon = icon("question-circle"), class = "btn btn-info btn-sm")
    ),
    
    tabPanel("1. Configuration", value="step1", icon = icon("cogs"),
             h3("Step 1: Configure Session"),
             p("Configure your session parameters below."),
             hr(),
             fluidRow(
               column(8, offset = 2,
                      wellPanel(
                        h4("1. Load Configuration"),
                        div(class = "alert alert-info", icon("info-circle"), "Start by loading a saved session. To start from scratch, use the reset option below."),
                        actionButton("load_session_config_btn", "Load Saved Session", icon = icon("folder-open"), width = "100%", class = "btn-primary btn-lg"),
                        hr(),
                        actionLink("toggle_reset_link", "Show option to reset to default template..."),
                        shinyjs::hidden(
                          div(id = "reset_div", class = "mt-3",
                              tags$p("This will discard all current settings and start a fresh session from the default template.", class = "text-muted"),
                              actionButton("reset_to_default_btn", "Reset to Default", icon = icon("redo"), class = "btn-outline-danger", width = "100%")
                          )
                        )
                      ),
                      wellPanel(
                        h4("2. Validate Matrix Paths"),
                        shinyjs::disabled(textInput("output_dir_ui", "Output Directory (from config):", width = "100%")),
                        textInput("fm_pro_path_ui", "Fm_Pro.xlsx Path:", width="100%"),
                        textInput("fm_nopro_path_ui", "Fm_NoPro.xlsx Path:", width="100%"),
                        actionButton("load_fm_btn", "Validate Paths", icon = icon("check-double"), width="100%")
                      ),
                      wellPanel(
                        h4("3. Tune Optimization Parameters"),
                        h5(strong("Data Cleaning Rules")),
                        checkboxInput("toggle_handle_duplicates", "Flag Duplicates", value = TRUE),
                        checkboxInput("toggle_handle_nas", "Handle NAs as zero", value = TRUE),
                        checkboxInput("toggle_handle_negatives", "Handle Negatives as zero", value = TRUE),
                        checkboxInput("toggle_handle_zerosum", "Flag Zero-Sum Rows", value = TRUE),
                        hr(),
                        h5(strong("Phytoclass Algorithm")),
                        numericInput("niter_input", "Annealing Iterations (Niter):", value = 500, min = 10, step = 10),
                        numericInput("step_size_input", "Temperature Step (Cooling Rate):", value = 0.009, min = 0.0001, step = 0.001)
                      ),
                      wellPanel(
                        h4("4. Define Filtering Rules"),
                        fluidRow(
                          column(4, checkboxInput("toggle_geo_filter", strong("Geospatial"), value = FALSE)),
                          column(4, checkboxInput("toggle_temporal_filter", strong("Temporal"), value = FALSE)),
                          column(4, checkboxInput("toggle_depth_filter", strong("Depth"), value = FALSE))
                        ),
                        hr(),
                        shinyjs::hidden(div(id = "geo_filters_div", h5("Geospatial Bounds"), numericInput("min_lat_ui", "Min Lat:", -90), numericInput("max_lat_ui", "Max Lat:", 90), numericInput("min_lon_ui", "Min Lon:", -180), numericInput("max_lon_ui", "Max Lon:", 180))),
                        shinyjs::hidden(div(id = "temporal_filters_div", h5("Temporal Bounds"), dateInput("start_date_ui", "Start Date:", "1900-01-01"), dateInput("end_date_ui", "End Date:", Sys.Date()))),
                        shinyjs::hidden(div(id = "depth_filters_div", h5("Depth Bounds"), numericInput("min_depth_ui", "Min Depth (m):", 0), numericInput("max_depth_ui", "Max Depth (m):", 1000)))
                      )
               )
             )
    ),
    
    tabPanel("2. Upload Data", value="step2", icon = icon("upload"), h3("Step 2: Load HPLC Data"), p("Select one or more `.xlsx` files containing pigment data."), hr(), fluidRow(column(8, offset = 2, wellPanel(fileInput("hplc_data_files_input", "1. Select Files:", multiple = TRUE, accept = c(".xlsx"), width="100%"), actionButton("load_data_btn", "2. Load Selected File(s)", icon = icon("play"), class = "btn-primary", width="300px")), wellPanel(h4("Loading Summary"), DTOutput("batch_file_load_status_table"))))),
    tabPanel("3. Check Columns", value="step3", icon = icon("check-square"), validationUI("step3_validation")),
    tabPanel("4. QC & Filtering", value="step4", icon = icon("cogs"), qcUI("step4_qc")),
    tabPanel("5. Grouping Strategy", value="step5", icon = icon("sitemap"), strategyUI("step5_strategy")),
    tabPanel("6. Run Optimization", value="step6", icon = icon("microscope"), h3("Step 6: Execute Phytoclass Analysis"), p("Select the datasets to include, review the run parameters, and execute the algorithm."), hr(), sidebarLayout(sidebarPanel(width = 4, h4("1. Select Datasets for this Run"), pickerInput(inputId = "datasets_for_phytoclass_run", label = NULL, choices = NULL, multiple = TRUE, options = pickerOptions(actionsBox = TRUE, selectAllText="Select All", deselectAllText="Deselect All")), hr(), wellPanel(style="background-color: #e9ecef;", h5(icon("tasks"), "Run Parameter Checklist"), verbatimTextOutput("analysis_params_review")), hr(), h4("2. Execute"), actionButton("run_phytoclass_btn", "Start Optimization", class = "btn-primary btn-lg", width="100%", icon = icon("rocket"))), mainPanel(width = 8, wellPanel(h4("Analysis Run Summary"), DTOutput("phytoclass_batch_summary_table"))))),
    tabPanel("7. Results & Export", value="step7", icon = icon("file-download"), reportingUI("step7_reporting"))
  ),
  
  hr(),
  fluidRow(column(10, offset = 1, h4(icon("clipboard-list"), "Session Audit Log"), verbatimTextOutput("session_audit_log")))
)

# ============================================================================
# --- 3. SERVER LOGIC ---
# ============================================================================

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    session_id = paste0("Run_", format(Sys.time(), "%Y%m%d_%H%M%S")),
    config = NULL, datasets_processed = list(), master_qc_data = NULL,       
    analysis_datasets = list(), analyzed_datasets = list(), 
    cluster_diagnostics = NULL, qc_summary_df = NULL, 
    performance_metrics = NULL, 
    session_log = character(0), staging_datasets = list(),
    mapping_trigger = 0, current_mapping_dataset = NULL, mapping_history = list(),
    resolution_warnings = list()
  )
  
  # --- LOGGING V2: CATEGORIZED & TIMESTAMPED ---
  .log_event <- function(category = "SYSTEM", message) {
    if (missing(message)) { message <- category; category <- "INFO" }
    timestamp <- format(Sys.time(), "%H:%M:%S")
    log_entry <- sprintf("[%s] [%s] %s", timestamp, category, message)
    rv$session_log <- c(rv$session_log, log_entry)
  }
  
  # --- SURVEILLANCE: NAVIGATION TRACKER ---
  observeEvent(input$main_navbar, {
    .log_event("NAV", paste("Switched tab to:", input$main_navbar))
  }, ignoreInit = TRUE)
  
  # --- SURVEILLANCE: BUTTON CLICK INTERCEPTOR ---
  observeEvent(input$last_btn_clicked, {
    btn_id <- input$last_btn_clicked
    .log_event("USER", paste("Clicked Button:", btn_id))
  })
  
  # --- SURVEILLANCE: "GOD MODE" INPUT TRACKER ---
  inputs_to_watch <- c(
    "output_dir_ui", "fm_pro_path_ui", "fm_nopro_path_ui",
    "toggle_handle_duplicates", "toggle_handle_nas", "toggle_handle_negatives", "toggle_handle_zerosum",
    "niter_input", "step_size_input",
    "toggle_geo_filter", "toggle_temporal_filter", "toggle_depth_filter",
    "min_lat_ui", "max_lat_ui", "min_lon_ui", "max_lon_ui",
    "min_depth_ui", "max_depth_ui", "start_date_ui", "end_date_ui",
    "step5_strategy-normalization_method_input", "step5_strategy-transformation_method_input",
    "step5_strategy-cluster_method_input", "step5_strategy-k_max_input", "step5_strategy-k_determination_mode",
    "datasets_for_phytoclass_run"
  )
  
  lapply(inputs_to_watch, function(id) {
    observeEvent(input[[id]], {
      val <- input[[id]]
      if(length(val) > 1) val <- paste0("(", length(val), " items selected)")
      .log_event("INPUT", paste0(id, " changed to: ", val))
    }, ignoreInit = TRUE)
  })
  
  
  # --- WORKFLOW STATE MANAGER ---
  .update_workflow_state <- function(enable_up_to) {
    tabs_to_manage <- c("step2", "step3", "step4", "step5", "step6", "step7")
    if (enable_up_to == "step1") {
      for (tab in tabs_to_manage) { shinyjs::disable(selector = glue("#main_navbar li a[data-value='{tab}']")) }
    } else {
      target_index <- which(tabs_to_manage == enable_up_to)
      if (length(target_index) > 0) {
        for (i in seq_along(tabs_to_manage)) {
          if (i <= target_index) shinyjs::enable(selector = glue("#main_navbar li a[data-value='{tabs_to_manage[i]}']"))
          else shinyjs::disable(selector = glue("#main_navbar li a[data-value='{tabs_to_manage[i]}']"))
        }
      }
    }
  }
  
  output$session_audit_log <- renderText({paste(rev(rv$session_log), collapse = "\n")})
  
  reset_downstream_data <- function(level = "all") {
    .log_event("SYSTEM", sprintf("Resetting downstream data (level: %s)", level))
    if (level %in% c("all", "config", "qc")) {
      rv$master_qc_data <- NULL; rv$analysis_datasets <- list(); rv$analyzed_datasets <- list()
      rv$qc_summary_df <- NULL; rv$cluster_diagnostics <- NULL; rv$staging_datasets <- list()
      rv$mapping_history <- list(); rv$mapping_trigger <- rv$mapping_trigger + 1
      rv$resolution_warnings <- list()
    }
    if (level == "strategy") {
      rv$analysis_datasets <- list(); rv$analyzed_datasets <- list(); rv$cluster_diagnostics <- NULL
    }
    if (level == "analysis") {
      rv$analyzed_datasets <- list()
    }
  }
  
  observeEvent(once = TRUE, eventExpr = session, {
    .update_workflow_state("step1")
    .log_event("SYSTEM", "Application session started.")
    tryCatch({
      rv$config <- initialize_config()
      update_all_ui_from_config(rv$config, session)
      .log_event("SYSTEM", "Initial configuration loaded.")
      
      fm_result <- load_fm_matrices(rv$config)
      if (!is.null(fm_result$error)) {
        .log_event("WARNING", paste("Startup: Fm matrices issue.", fm_result$error))
      } else {
        rv$fm_matrices <- fm_result
        .log_event("SYSTEM", "Fm matrices loaded automatically.")
      }
      
      if (!is.null(rv$config)) .update_workflow_state("step2")
    }, error = function(e) { .log_event("FATAL", paste("Startup Error:", e$message)) })
  })
  
  observeEvent(input$load_session_config_btn, {
    tryCatch({
      rv$config <- load_config(CONFIG_SESSION_PATH)
      update_all_ui_from_config(rv$config, session)
      reset_downstream_data("config"); .update_workflow_state("step2")
      .log_event("CONFIG", "Saved session config reloaded.")
      fm_result <- load_fm_matrices(rv$config)
      if (is.null(fm_result$error)) { rv$fm_matrices <- fm_result; .log_event("CONFIG", "Fm matrices reloaded.") }
      showNotification("Saved session config reloaded.", type="message")
    }, error = function(e) { .log_event("ERROR", paste("Load Session:", e$message)); showModal(modalDialog(title="Error Loading Session", e$message)) })
  })
  
  observeEvent(input$toggle_reset_link, { shinyjs::toggle("reset_div", anim = TRUE) })
  
  observeEvent(input$reset_to_default_btn, {
    showModal(modalDialog(
      title = "Reset to Default?",
      "Are you sure? This will discard all unsaved changes.",
      easyClose = TRUE,
      footer = tagList(modalButton("Cancel"), actionButton("confirm_reset_btn", "Yes, Reset", class = "btn-danger"))
    ))
  })
  
  observeEvent(input$confirm_reset_btn, {
    removeModal()
    tryCatch({
      rv$config <- load_config(CONFIG_TEMPLATE_PATH)
      update_all_ui_from_config(rv$config, session)
      reset_downstream_data("config"); .update_workflow_state("step2")
      .log_event("CONFIG", "Default config template loaded.")
      fm_result <- load_fm_matrices(rv$config)
      if (is.null(fm_result$error)) { rv$fm_matrices <- fm_result; .log_event("CONFIG", "Fm matrices reloaded.") }
      showNotification("Default config template loaded.", type="message")
    }, error = function(e) { .log_event("ERROR", paste("Load Default:", e$message)) })
  })
  
  observeEvent(input$save_config_btn_global, {
    req(rv$config)
    rv$config <- sync_config_with_ui(rv$config, input)
    rv$config <- sync_config_with_ui(rv$config, input, ns_prefix = "step5_strategy-")
    rv$config <- update_config_with_new_aliases(rv$config, rv$datasets_processed)
    tryCatch({
      save_config(rv$config, CONFIG_SESSION_PATH)
      .log_event("CONFIG", "Session settings saved to disk.")
      showNotification("Session settings saved successfully.", type = "message")
    }, error = function(e) { .log_event("ERROR", paste("Save Session:", e$message)) })
  })
  
  observe({ shinyjs::toggle(id = "geo_filters_div", condition = isTRUE(input$toggle_geo_filter)) })
  observe({ shinyjs::toggle(id = "temporal_filters_div", condition = isTRUE(input$toggle_temporal_filter)) })
  observe({ shinyjs::toggle(id = "depth_filters_div", condition = isTRUE(input$toggle_depth_filter)) })
  
  observeEvent(input$load_fm_btn, {
    req(rv$config)
    rv$config <- sync_config_with_ui(rv$config, input)
    show_modal_spinner(text="Validating matrix paths...")
    fm_result <- load_fm_matrices(rv$config)
    if (!is.null(fm_result$error)) {
      rv$fm_matrices <- NULL; .log_event("ERROR", paste("Fm Load:", fm_result$error)); showModal(modalDialog(title="Fm Matrix Error", fm_result$error))
    } else {
      rv$fm_matrices <- fm_result; .log_event("SYSTEM", "Fm matrices loaded and validated."); showNotification("Matrix paths validated successfully.", type="message")
    }
    remove_modal_spinner()
  })
  
  observeEvent(input$load_data_btn, {
    req(input$hplc_data_files_input)
    
    start_time <- Sys.time()
    
    file_count <- nrow(input$hplc_data_files_input)
    .log_event("ACTION", paste("Loading", file_count, "file(s)."))
    
    reset_downstream_data("all"); .update_workflow_state("step2")
    rv$datasets_processed <- load_all_files(input$hplc_data_files_input, rv$config, .log_event)
    
    end_time <- Sys.time()
    duration <- round(difftime(end_time, start_time, units="secs"), 2)
    .log_event("PERFORMANCE", paste("Data Load completed in", duration, "seconds."))
    
    if(length(rv$datasets_processed) > 0) {
      .log_event("SYSTEM", paste("Success: Loaded", length(rv$datasets_processed), "datasets."))
      cat("--- [Step 3] Initializing staging buffer ---\n")
      rv$staging_datasets <- purrr::map(rv$datasets_processed, rlang::duplicate)
      rv$mapping_history <- list()
      rv$mapping_trigger <- rv$mapping_trigger + 1
      .log_event("SYSTEM", "Staging buffer initialized for column mapping.")
      .update_workflow_state("step3")
    }
  })
  
  output$batch_file_load_status_table <- renderDT({
    req(length(rv$datasets_processed) > 0)
    summary_df <- purrr::map_df(rv$datasets_processed, ~tibble(Name = .x$name, Rows = .x$log$initial_rows, Cols = .x$log$initial_cols))
    datatable(summary_df, options = list(pageLength = 10, searching=FALSE), rownames=FALSE)
  })
  
  validationServer("step3_validation", rv, .log_event, .update_workflow_state)
  qcServer("step4_qc", rv, .log_event, .update_workflow_state, reset_downstream_data)
  strategyServer("step5_strategy", rv, .log_event, .update_workflow_state, session)
  reportingServer("step7_reporting", rv, .log_event)
  
  output$analysis_params_review <- renderText({
    req(rv$config, rv$master_qc_data, rv$analysis_datasets)
    generate_run_summary_text(
      config = rv$config, 
      master_qc_data = rv$master_qc_data, 
      analysis_datasets = rv$analysis_datasets,
      cluster_diagnostics = rv$cluster_diagnostics
    )
  })
  
  # --- STABLE TRACKER: Global Total ETA (Batch-Based) ---
  # --- STABLE TRACKER: Cumulative Global ETA ---
  observeEvent(input$run_phytoclass_btn, {
    if (is.null(rv$fm_matrices)) {
      showNotification("Fm matrices are not loaded. Please validate paths in Step 1.", type = "error")
      return()
    }
    if (length(rv$analysis_datasets) == 0 || length(input$datasets_for_phytoclass_run) == 0) {
      showNotification("Please prepare and select datasets to run.", type="error")
      return()
    }
    
    datasets_to_run <- rv$analysis_datasets[input$datasets_for_phytoclass_run]
    total_samples_global <- sum(sapply(datasets_to_run, function(x) nrow(x$data)))
    
    # Safe fallback for niter
    current_niter <- if(!is.null(rv$config$phytoclass$niter)) rv$config$phytoclass$niter else 500
    
    # Initial Guess: Use historical speed from config (default 0.5s/sample)
    # This prevents the bar from saying "Infinity" or "0" at the very start
    historical_rate <- if(!is.null(rv$config$performance$avg_speed)) rv$config$performance$avg_speed else 0.5
    
    .log_event("ACTION", paste("Running Phytoclass on", length(datasets_to_run), "groups containing", total_samples_global, "samples (Niter:", current_niter, ")"))
    reset_downstream_data("analysis")
    
    samples_finished_so_far <- 0
    start_time_global <- Sys.time()
    
    # Simple Progress Bar
    progress <- shiny::Progress$new(session, min=0, max=length(datasets_to_run))
    on.exit(progress$close())
    shinyjs::hide(selector = ".shinybusy-spinner") 
    
    temp_analyzed_list <- list()
    
    for (i in seq_along(datasets_to_run)) {
      ds_obj <- datasets_to_run[[i]]
      current_batch_size <- nrow(ds_obj$data)
      
      # --- CALCULATE REFINED GLOBAL ETA ---
      elapsed_sec <- as.numeric(difftime(Sys.time(), start_time_global, units="secs"))
      
      # 1. Determine Rate
      # If we have finished at least one batch (or partway), use REAL measured speed.
      # Otherwise, use the Historical Guess.
      current_speed_per_sample <- if (samples_finished_so_far > 0) {
        elapsed_sec / samples_finished_so_far
      } else {
        historical_rate
      }
      
      # 2. Calculate Remaining Time
      # Total samples in run - Samples done = Samples left to do
      samples_remaining <- total_samples_global - samples_finished_so_far
      eta_seconds <- current_speed_per_sample * samples_remaining
      
      # 3. Format Message
      m <- floor(eta_seconds / 60)
      s <- round(eta_seconds %% 60)
      
      # "Processing C1 (n=324)... Global Progress: 0/2094... ETA for all: ~10m 30s"
      msg_header <- paste0("Processing Group ", i, "/", length(datasets_to_run), ": ", ds_obj$name)
      msg_detail <- sprintf("Global Progress: %d/%d samples | Est. Time Remaining: ~%02dm %02ds", 
                            samples_finished_so_far, total_samples_global, m, s)
      
      progress$set(value = i-1, message = msg_header, detail = msg_detail)
      
      # Ensure function exists
      if (!exists("run_phytoclass_analysis")) stop("Module phytoclass_analyzer.R not loaded!")
      
      # --- BLOCKING CALL (Analysis) ---
      analyzer_out <- run_phytoclass_analysis(ds_obj$data, rv$config, rv$fm_matrices)
      
      ds_obj$log_analyzer <- analyzer_out$log
      ds_obj$pigment_matrix_final <- analyzer_out$pigment_matrix_used
      if (!is.null(analyzer_out$results)) {
        ds_obj$data_final <- left_join(ds_obj$data, analyzer_out$results, by = "UniqueID")
      }
      temp_analyzed_list[[ds_obj$name]] <- ds_obj
      
      # UPDATE COUNTER for next loop
      samples_finished_so_far <- samples_finished_so_far + current_batch_size
    }
    
    rv$analyzed_datasets <- temp_analyzed_list
    shinyjs::show(selector = ".shinybusy-spinner") 
    
    end_time <- Sys.time()
    total_duration <- as.numeric(difftime(end_time, start_time_global, units="secs"))
    final_avg_speed <- round(total_duration / max(1, total_samples_global), 4)
    
    # Safe Config Update
    local_config <- rv$config
    local_config$performance$avg_speed <- final_avg_speed
    local_config$performance$last_niter <- current_niter
    rv$config <- local_config
    
    rv$performance_metrics <- list(
      total_time_sec = round(total_duration, 2),
      time_per_sample = final_avg_speed,
      total_samples = total_samples_global,
      groups_processed = length(datasets_to_run)
    )
    
    # Safe Auto-Save
    if (exists("save_config") && is.function(save_config)) {
      tryCatch({
        save_config(rv$config, CONFIG_SESSION_PATH)
        .log_event("CONFIG", paste0("Auto-saved performance: ", final_avg_speed, "s/sample"))
      }, error = function(e) {
        .log_event("WARNING", paste("Failed to auto-save speed:", e$message))
      })
    }
    
    .log_event("PERFORMANCE", paste("Optimization complete in", round(total_duration, 2), "secs."))
    .log_event("PERFORMANCE", paste("Speed:", final_avg_speed, "seconds per sample."))
    
    .log_event("SYSTEM", "Phytoclass Analysis Complete.")
    .update_workflow_state("step7")
  })
  
  output$phytoclass_batch_summary_table <- renderDT({ 
    req(rv$analyzed_datasets)
    ds_with_log <- purrr::keep(rv$analyzed_datasets, ~!is.null(.x$log_analyzer))
    if (length(ds_with_log) == 0) return(datatable(tibble(Status="No analysis results available.")))
    
    summary_df <- purrr::map_df(ds_with_log, ~tibble(
      Dataset = .x$name, Status = .x$log_analyzer$status %||% "N/A", `Fm Used` = .x$log_analyzer$fm_matrix_used %||% "N/A", 
      `Mean RMSE` = round(.x$log_analyzer$mean_rmse %||% NA, 4), `Mean Cond Num` = round(.x$log_analyzer$mean_condnum %||% NA, 2)
    ))
    datatable(summary_df, rownames=FALSE) %>%
      formatStyle("Status", backgroundColor = styleEqual(c("Success", "Failed", "Skipped"), c("#C6EFCE", "#FFEB9C", "#D3D3D3")))
  })
  
  # --- CONTEXT-AWARE HELP MENU (UPDATED: FULL VISUAL COVERAGE) ---
  observeEvent(input$help_btn_global, {
    current_tab <- input$main_navbar
    
    # --- 1. Visual Helpers (CSS Mimicry) ---
    btn_prim <- function(label, ico) {
      span(class = "help-mimic-btn btn-primary", icon(ico), paste0(" ", label))
    }
    btn_succ <- function(label, ico) {
      span(class = "help-mimic-btn btn-success", icon(ico), paste0(" ", label))
    }
    btn_def <- function(label, ico) {
      span(class = "help-mimic-btn btn-default", icon(ico), paste0(" ", label))
    }
    btn_dang <- function(label, ico) {
      span(class = "help-mimic-btn btn-outline-danger", icon(ico), paste0(" ", label))
    }
    ui_tog <- function(label, checked=TRUE) {
      icn <- if(checked) icon("check-square") else icon("square")
      span(class = "help-mimic-input", icn, paste0(" ", label))
    }
    ui_in <- function(label) {
      span(class = "help-mimic-input", icon("keyboard"), paste0(" ", label))
    }
    
    # --- 2. CSS Styles for the Modal ---
    help_styles <- tags$style(HTML("
      .help-mimic-btn {
        padding: 4px 10px; border-radius: 4px; display: inline-block; font-size: 0.9em; margin: 2px 4px; border: 1px solid transparent;
      }
      .btn-primary { background-color: #007bff; color: white; }
      .btn-success { background-color: #28a745; color: white; }
      .btn-info { background-color: #17a2b8; color: white; }
      .btn-default { background-color: #f8f9fa; color: #333; border: 1px solid #ccc; }
      .btn-outline-danger { background-color: transparent; color: #dc3545; border: 1px solid #dc3545; }
      .help-mimic-input {
        background-color: #fff; border: 1px solid #ced4da; border-radius: 4px; padding: 2px 6px; color: #495057; font-family: monospace; display: inline-block; margin: 0 2px;
      }
      .help-section-title {
        border-bottom: 2px solid #dee2e6; color: #495057; margin-top: 15px; margin-bottom: 10px; padding-bottom: 5px; font-weight: bold;
      }
      ul.help-list li { margin-bottom: 8px; }
    "))
    
    # --- 3. Content Definitions ---
    
    content_step1 <- tagList(
      h4("Step 1: Configuration"),
      p("Configure your session parameters below. Click", 
        btn_succ("Save Session", "save"), 
        "to update the 'config_session.yaml' file, which is automatically detected on startup to restore your settings."),
      
      div(class="help-section-title", "1. Session Management"),
      tags$ul(class="help-list",
              tags$li(btn_prim("Load Saved Session", "folder-open"), ": Loads settings from a previously saved 'config_session.yaml'."),
              tags$li(span(style="color:#007bff; text-decoration:underline;", "Show option to reset..."), ": Reveals the reset button."),
              tags$li(btn_dang("Reset to Default", "redo"), ": Discards all changes and restores the original template settings.")
      ),
      
      div(class="help-section-title", "2. Matrix & Paths"),
      tags$ul(class="help-list",
              tags$li(ui_in("Fm_Pro.xlsx Path"), ": Location of the pigment ratio matrix for samples containing Divinyl Chlorophyll a (Prochlorococcus)."),
              tags$li(ui_in("Fm_NoPro.xlsx Path"), ": Location of the matrix for samples without Divinyl Chlorophyll a."),
              tags$li(btn_def("Validate Paths", "check-double"), ": Checks if the files exist at the specified locations.")
      ),
      
      div(class="help-section-title", "3. Optimization Parameters"),
      tags$ul(class="help-list",
              tags$li(strong("Data Cleaning Rules:"), " Check to enable."),
              tags$li(ui_tog("Flag Duplicates"), ": Marks samples with identical Lat/Lon/Depth/Time/Pigments."),
              tags$li(ui_tog("Handle NAs as zero"), ": Converts empty cells to 0."),
              tags$li(ui_tog("Handle Negatives as zero"), ": Fixes machine read errors (<0 becomes 0)."),
              tags$li(ui_tog("Flag Zero-Sum Rows"), ": Excludes empty samples."),
              tags$li(ui_in("Annealing Iterations (Niter)"), ": How many steps the algorithm runs (Default: 500). Higher = more precision, slower."),
              tags$li(ui_in("Temperature Step"), ": The cooling rate (Default: 0.009). Lower = slower convergence, potentially better global minima.")
      ),
      
      div(class="help-section-title", "4. Filtering Rules"),
      tags$ul(class="help-list",
              tags$li(ui_tog("Geospatial"), ": Enable to filter by Latitude/Longitude."),
              tags$li(ui_in("Min/Max Lat/Lon"), ": Samples outside this box are flagged as 'Flagged_GeoRange'."),
              tags$li(ui_tog("Temporal"), ": Enable to filter by Date."),
              tags$li(ui_in("Start/End Date"), ": Samples outside this range are flagged 'Flagged_DateRange'."),
              tags$li(ui_tog("Depth"), ": Enable to filter by Depth."),
              tags$li(ui_in("Min/Max Depth"), ": Samples outside range are flagged 'Flagged_DepthRange'.")
      ),
      
      div(class="help-section-title", "Global Header Actions"),
      tags$ul(class="help-list",
              tags$li(btn_succ("Save Session", "save"), ": Writes all current settings to 'config_session.yaml'.")
      )
    )
    
    content_step2 <- tagList(
      h4("Step 2: Upload Data"),
      
      div(class="help-section-title", "File Selection"),
      tags$ul(class="help-list",
              tags$li(span(class="help-mimic-input", "Browse..."), ": Opens your system file picker. You can select multiple .xlsx files at once."),
              tags$li(ui_in("1. Select Files"), ": Shows the list of files currently staged for upload.")
      ),
      
      div(class="help-section-title", "Action"),
      tags$ul(class="help-list",
              tags$li(btn_prim("2. Load Selected File(s)", "play"), ": Reads the Excel files, creates UniqueIDs, and attempts to detect date formats.")
      ),
      
      div(class="help-section-title", "Review"),
      tags$ul(class="help-list",
              tags$li(strong("Loading Summary Table"), ": Confirm that 'Rows' and 'Cols' match your expectations.")
      )
    )
    
    content_step3 <- tagList(
      h4("Step 3: Check Columns"),
      p("Map your raw column headers to the names required by Phytoclass."),
      
      div(class="help-section-title", "1. Validation Table"),
      tags$ul(class="help-list",
              tags$li("Rows marked ", span(style="background-color:#C6EFCE; padding:2px;", "OK"), ": Fully mapped. No action needed."),
              tags$li("Rows marked ", span(style="background-color:#FFEB9C; padding:2px;", "MISSING ESSENTIALS"), ": Click these rows to open the Mapping Wizard.")
      ),
      
      div(class="help-section-title", "2. The Mapping Wizard (Pop-up)"),
      tags$ul(class="help-list",
              tags$li(ui_in("Map [Key] to:"), ": Select the column from your file that corresponds to the required key."),
              tags$li("Select ", em("Leave Unmapped"), ": If you do not have this pigment (it will be treated as 0)."),
              tags$li(ui_tog("Apply to other datasets"), ": If checked, applies these mappings to all other files with the exact same missing columns."),
              tags$li(btn_prim("Apply Selected", "check"), ": Saves these choices to temporary memory.")
      ),
      
      div(class="help-section-title", "3. Final Actions"),
      tags$ul(class="help-list",
              tags$li(span(class = "help-mimic-btn", style="color:#6c757d; border:1px solid #6c757d;", icon("undo"), " Undo Last Change"), 
                      ": Reverts the most recent Wizard action. This button is only visible when there are pending changes to undo."),
              tags$li(btn_succ("Confirm Mappings", "check-double"), 
                      ": Commits mappings to the pipeline and saves them as aliases in your configuration.")
      ),
      
      div(class="help-section-title", "Troubleshooting: Incorrect Mappings"),
      div(class="alert alert-warning", style="font-size:0.9em; margin-top:5px; padding: 10px;",
          div(icon("exclamation-triangle"), strong(" Be careful:"), " Once confirmed, incorrect aliases are saved to your Session Config."),
          p(style="margin-top:8px; margin-bottom:5px;", "If you realize you committed a bad alias (e.g., mapped 'Lat' to 'Depth'):"),
          tags$ol(style="padding-left: 20px;",
                  tags$li(strong("Safe Method:"), " Go to Step 1 and reload a previously saved session or use 'Reset to Default' to clear the bad memory."),
                  tags$li(strong("Expert Method (Discouraged):"), " Open the ", code("config_session.yaml"), " file in a text editor, find the bad entry under ", code("column_aliases:"), ", and delete it. This is effective but risks breaking the file if formatted incorrectly.")
          )
      )
    )
    
    content_step4 <- tagList(
      h4("Step 4: QC & Filtering"),
      
      div(class="help-section-title", "Pipeline Execution"),
      tags$ul(class="help-list",
              tags$li(btn_prim("Run QC Pipeline", "cogs"), ": Runs the cleaning and filtering rules defined in Step 1. Standardizes all column names.")
      ),
      
      div(class="help-section-title", "Review"),
      tags$ul(class="help-list",
              tags$li(strong("QC Audit Summary Table"), ": Shows how many samples were removed by each rule."),
              tags$li(
                "If any 'Failed' counts in the table are ", span(style="color:#dc3545; font-weight:bold;", "red"), 
                ", revisit ", strong("Step 1: Configuration"), " to adjust your settings:",
                tags$ul(
                  tags$li("For issues under ", strong("Pigment QC"), ", adjust the ",
                          ui_tog("Flag Duplicates"), ", ", ui_tog("Handle NAs as zero"), ", ",
                          ui_tog("Handle Negatives as zero"), ", or ", ui_tog("Flag Zero-Sum Rows"), " toggles."),
                  tags$li("For issues under ", strong("Filtering"), ", modify the ",
                          ui_tog("Geospatial"), ", ", ui_tog("Temporal"), ", or ", ui_tog("Depth"), " toggles and their respective min/max parameters.")
                )
              )
      )
    )
    
    content_step5 <- tagList(
      h4("Step 5: Grouping Strategy"),
      p("Determine how samples are partitioned for the optimization algorithm."),
      
      div(class="help-section-title", "1. Method Selection"),
      tags$ul(class="help-list",
              tags$li(ui_tog("By Source File", checked=TRUE), ": Treats each uploaded file as a distinct analysis group to be processed independently."),
              tags$li(ui_tog("Cluster Analysis", checked=FALSE), ": Groups samples based on the similarity of their pigment:TChla ratios, merging data from all source files.")
      ),
      
      div(class="help-section-title", "2. Clustering Configuration (Visible if 'Cluster Analysis' selected)"),
      tags$ul(class="help-list",
              tags$li(ui_in("Input Data"), ": Select ", strong("'Ratio to Tchla'"), " (Recommended) to cluster by community structure independent of biomass, or ", strong("'Raw Data'"), " to cluster by absolute concentration."),
              tags$li(ui_in("Transformation"), ": Use ", strong("'Log(x+1)'"), " to minimize the skewing effect of high-concentration outliers (blooms), or ", strong("'None'"), " to use the linear scale."),
              tags$li(ui_in("Algorithm"), ": ", strong("'Hierarchical (Ward)'"), " is preferred for pigment analysis; ", strong("'K-Means'"), " offers faster processing for very large datasets."),
              tags$li(ui_tog("Auto (Elbow)", checked=TRUE), ": Automatically calculates the statistically optimal number of clusters (k)."),
              tags$li(ui_tog("Manual", checked=FALSE), ": Allows you to enforce a fixed number of clusters using the numeric input.")
      ),
      
      div(class="help-section-title", "3. Execution"),
      tags$ul(class="help-list",
              tags$li(btn_prim("Preview Groups", "play"), ": Runs the selected grouping logic and generates a summary table (and PCA map if Clustering is active)."),
              tags$li(btn_succ("Confirm Strategy", "check"), ": Locks in the groups and prepares the data for Step 6.")
      ),
      
      div(class="help-section-title", "4. Review"),
      tags$ul(class="help-list",
              tags$li(
                span(style="color:red; font-weight:bold;", "Warning (N<12)"), 
                ": Indicates an analysis group contains fewer than 12 samples. The optimization algorithm requires sufficient data density to converge reliably. If you see this, consider reducing the number of clusters (k) or switching to 'Cluster Analysis' to merge small source files."
              )
      )
    )
    
    content_step6 <- tagList(
      h4("Step 6: Run Optimization"),
      
      div(class="help-section-title", "1. Selection"),
      tags$ul(class="help-list",
              tags$li(ui_in("Select Datasets"), ": A dropdown picker."),
              tags$li(btn_def("Select All", "check-square"), " / ", btn_def("Deselect All", "square"), ": Use these inside the picker to manage large lists.")
      ),
      
      div(class="help-section-title", "2. Execution"),
      tags$ul(class="help-list",
              tags$li(btn_prim("Start Optimization", "rocket"), ": Launches Simulated Annealing. This may take time."),
              tags$li(strong("Smart ETA"), ": The progress bar will estimate remaining time based on your computer's speed.")
      )
    )
    
    content_step7 <- tagList(
      h4("Step 7: Results & Export"),
      
      div(class="help-section-title", "1. Exploration"),
      tags$ul(class="help-list",
              tags$li(ui_in("Select Analysis Group"), ": Switch between different files/clusters to update the plots."),
              tags$li(strong("Performance Diagnostics"), ": Shows RMSE (Error) and Condition Number (Stability).")
      ),
      
      div(class="help-section-title", "2. Export"),
      tags$ul(class="help-list",
              tags$li(btn_prim("Download Report Package", "download"), ": Generates a timestamped folder containing:"),
              tags$li(icon("file-excel"), " Excel Results (Cleaned)"),
              tags$li(icon("file-excel"), " Master Audit Report (QC logs)"),
              tags$li(icon("chart-area"), " Plot Images (.png)"),
              tags$li(icon("cogs"), " config_session.yaml (For reproducibility)")
      )
    )
    
    # --- 4. Select Content Based on Tab ---
    selected_content <- switch(current_tab,
                               "step1" = content_step1,
                               "step2" = content_step2,
                               "step3" = content_step3,
                               "step4" = content_step4,
                               "step5" = content_step5,
                               "step6" = content_step6,
                               "step7" = content_step7
    )
    
    # --- 5. Render Modal ---
    showModal(modalDialog(
      title = div(icon("question-circle"), paste("Help: ", current_tab)),
      size = "l",
      easyClose = TRUE,
      help_styles, # Inject CSS
      selected_content,
      footer = modalButton("Close")
    ))
  })
  
  session$onSessionEnded(function() {
    cat("--- _phytoclass_Shiny Session Ended ---\n")
  })
}

# ============================================================================
# --- 4. APP LAUNCH ---
# ============================================================================

shinyApp(ui = ui, server = server)