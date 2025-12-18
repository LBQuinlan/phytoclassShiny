# ============================================================================
#
#   _phytoclass_Shiny V1.0 - MAIN APPLICATION LAUNCHER
#
#   Description:
#   Main Driver.
#   UPDATE: Help system refined for maximum clarity. "Input" headers removed.
#   All button references now visually match the app interface (Blue/Green colors).
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

ui <- fluidPage(
  theme = shinytheme("spacelab"),
  shinyjs::useShinyjs(),
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
      /* Text util classes for help */
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
             p("Define paths and parameters for the analysis workflow."), hr(),
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
    analysis_datasets = list(), analyzed_datasets = list(), cluster_results_log = NULL,
    qc_summary_df = NULL, session_log = character(0), staging_datasets = list(),
    mapping_trigger = 0, current_mapping_dataset = NULL, mapping_history = list(),
    resolution_warnings = list()
  )
  
  .log_event <- function(message) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    log_entry <- paste0("[", timestamp, "] ", message)
    rv$session_log <- c(rv$session_log, log_entry)
  }
  
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
    cat(sprintf("--- Resetting downstream data (level: %s) ---\n", level))
    if (level %in% c("all", "config", "qc")) {
      rv$master_qc_data <- NULL; rv$analysis_datasets <- list(); rv$analyzed_datasets <- list()
      rv$qc_summary_df <- NULL; rv$cluster_results_log <- NULL; rv$staging_datasets <- list()
      rv$mapping_history <- list(); rv$mapping_trigger <- rv$mapping_trigger + 1
      rv$resolution_warnings <- list()
    }
    if (level == "strategy") {
      rv$analysis_datasets <- list(); rv$analyzed_datasets <- list(); rv$cluster_results_log <- NULL
    }
    if (level == "analysis") {
      rv$analyzed_datasets <- list()
    }
  }
  
  observeEvent(once = TRUE, eventExpr = session, {
    .update_workflow_state("step1")
    .log_event("Application session started.")
    tryCatch({
      rv$config <- initialize_config()
      update_all_ui_from_config(rv$config, session)
      .log_event("Initial configuration loaded.")
      
      fm_result <- load_fm_matrices(rv$config)
      if (!is.null(fm_result$error)) {
        .log_event(paste("Warning on startup: Fm matrices could not be loaded automatically.", fm_result$error))
      } else {
        rv$fm_matrices <- fm_result
        .log_event("Fm matrices loaded automatically from config.")
      }
      
      if (!is.null(rv$config)) .update_workflow_state("step2")
    }, error = function(e) { .log_event(paste("FATAL ERROR on startup:", e$message)) })
  })
  
  observeEvent(input$load_session_config_btn, {
    .log_event("Action: Load Saved Session clicked.")
    tryCatch({
      rv$config <- load_config(CONFIG_SESSION_PATH)
      update_all_ui_from_config(rv$config, session)
      reset_downstream_data("config"); .update_workflow_state("step2")
      .log_event("Saved session config reloaded.")
      fm_result <- load_fm_matrices(rv$config)
      if (is.null(fm_result$error)) { rv$fm_matrices <- fm_result; .log_event("Fm matrices reloaded.") }
      showNotification("Saved session config reloaded.", type="message")
    }, error = function(e) { .log_event(paste("ERROR loading session:", e$message)); showModal(modalDialog(title="Error Loading Session", e$message)) })
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
      .log_event("Default config template loaded.")
      fm_result <- load_fm_matrices(rv$config)
      if (is.null(fm_result$error)) { rv$fm_matrices <- fm_result; .log_event("Fm matrices reloaded.") }
      showNotification("Default config template loaded.", type="message")
    }, error = function(e) { .log_event(paste("ERROR loading default:", e$message)) })
  })
  
  observeEvent(input$save_config_btn_global, {
    .log_event("Action: Save Session clicked.")
    req(rv$config)
    rv$config <- sync_config_with_ui(rv$config, input)
    rv$config <- sync_config_with_ui(rv$config, input, ns_prefix = "step5_strategy-")
    rv$config <- update_config_with_new_aliases(rv$config, rv$datasets_processed)
    tryCatch({
      save_config(rv$config, CONFIG_SESSION_PATH)
      .log_event("Session settings saved.")
      showNotification("Session settings saved successfully.", type = "message")
    }, error = function(e) { .log_event(paste("ERROR saving session:", e$message)) })
  })
  
  observe({ shinyjs::toggle(id = "geo_filters_div", condition = isTRUE(input$toggle_geo_filter)) })
  observe({ shinyjs::toggle(id = "temporal_filters_div", condition = isTRUE(input$toggle_temporal_filter)) })
  observe({ shinyjs::toggle(id = "depth_filters_div", condition = isTRUE(input$toggle_depth_filter)) })
  
  observeEvent(input$load_fm_btn, {
    .log_event("Action: Validate Paths clicked.")
    req(rv$config)
    rv$config <- sync_config_with_ui(rv$config, input)
    show_modal_spinner(text="Validating matrix paths...")
    fm_result <- load_fm_matrices(rv$config)
    if (!is.null(fm_result$error)) {
      rv$fm_matrices <- NULL; .log_event(paste("ERROR loading Fm matrices:", fm_result$error)); showModal(modalDialog(title="Fm Matrix Error", fm_result$error))
    } else {
      rv$fm_matrices <- fm_result; .log_event("Fm matrices loaded and validated."); showNotification("Matrix paths validated successfully.", type="message")
    }
    remove_modal_spinner()
  })
  
  observeEvent(input$load_data_btn, {
    req(input$hplc_data_files_input)
    .log_event(paste("Action: Loading", nrow(input$hplc_data_files_input), "file(s)."))
    reset_downstream_data("all"); .update_workflow_state("step2")
    rv$datasets_processed <- load_all_files(input$hplc_data_files_input, rv$config, .log_event)
    if(length(rv$datasets_processed) > 0) {
      .log_event(paste("Success: Loaded", length(rv$datasets_processed), "datasets."))
      cat("--- [Step 3] Initializing staging buffer ---\n")
      rv$staging_datasets <- purrr::map(rv$datasets_processed, rlang::duplicate)
      rv$mapping_history <- list()
      rv$mapping_trigger <- rv$mapping_trigger + 1
      .log_event("Staging buffer initialized for column mapping.")
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
      cluster_results_log = rv$cluster_results_log
    )
  })
  
  observeEvent(input$run_phytoclass_btn, {
    if (is.null(rv$fm_matrices)) {
      showNotification("Fm matrices are not loaded. Please validate paths in Step 1.", type = "error")
      return()
    }
    if (length(rv$analysis_datasets) == 0 || length(input$datasets_for_phytoclass_run) == 0) {
      showNotification("Please prepare and select datasets to run.", type="error")
      return()
    }
    .log_event(paste("Action: Running Phytoclass on", length(input$datasets_for_phytoclass_run), "dataset(s)."))
    reset_downstream_data("analysis")
    
    datasets_to_run <- rv$analysis_datasets[input$datasets_for_phytoclass_run]
    progress <- shiny::Progress$new(session, min=0, max=length(datasets_to_run))
    on.exit(progress$close())
    progress$set(message = "Running Optimization...", value = 0)
    
    temp_analyzed_list <- list()
    for (i in seq_along(datasets_to_run)) {
      ds_obj <- datasets_to_run[[i]]
      progress$set(value = i, detail = paste("Processing group:", ds_obj$name))
      analyzer_out <- run_phytoclass_analysis(ds_obj$data, rv$config, rv$fm_matrices)
      ds_obj$log_analyzer <- analyzer_out$log
      ds_obj$pigment_matrix_final <- analyzer_out$pigment_matrix_used
      if (!is.null(analyzer_out$results)) {
        ds_obj$data_final <- left_join(ds_obj$data, analyzer_out$results, by = "UniqueID")
      }
      temp_analyzed_list[[ds_obj$name]] <- ds_obj
    }
    rv$analyzed_datasets <- temp_analyzed_list
    .log_event(paste("Success: Phytoclass complete on", length(rv$analyzed_datasets), "sets."))
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
  
  # --- CONTEXT-AWARE HELP MENU LOGIC (UPDATED) ---
  observeEvent(input$help_btn_global, {
    current_tab <- input$main_navbar
    
    help_content <- switch(current_tab,
                           "step1" = tagList(
                             h4("Current Step Instructions: Configuration"),
                             p(strong("Goal:"), " Configure session parameters. These settings can be saved and reloaded for future sessions."),
                             p(strong("Action:"), " Adjust the settings below to match your study requirements:"),
                             tags$ul(
                               tags$li("Validate Paths: Ensure the app sees your Fm_Pro and Fm_NoPro Excel files."),
                               tags$li("Data Cleaning: Check all 4 boxes to enable the quality control pipeline."),
                               tags$li("Filters: Enable required filters and set the numeric Min/Max bounds for Date, Depth, and Latitude/Longitude.")
                             ),
                             p(strong("Outcome:"), " Click ", span(class="text-success", icon("save"), "Save Session"), ". These rules will now be applied to any data you upload.")
                           ),
                           "step2" = tagList(
                             h4("Current Step Instructions: Upload Data"),
                             p(strong("Goal:"), " Import your raw HPLC pigment datasets."),
                             p(strong("Action:"), " Select your files using the file browser."),
                             tags$ul(
                               tags$li("Click 'Browse...' and select your .xlsx files (you can select multiple)."),
                               tags$li("Click ", span(class="text-primary", icon("play"), "Load Selected File(s)"), ".")
                             ),
                             p(strong("Outcome:"), " Files are ingested and standardized. Proceed to Step 3 to fix column names.")
                           ),
                           "step3" = tagList(
                             h4("Current Step Instructions: Check Columns"),
                             p(strong("Goal:"), " Map your file's column headers to the app's required variables."),
                             p(strong("Action:"), " Resolve any missing keys."),
                             tags$ul(
                               tags$li("If a file is flagged yellow ('MISSING ESSENTIALS'), click on the row to launch the mapping wizard."),
                               tags$li("In the Wizard, select the column in your file that matches the required variable (e.g., 'Lat_deg' -> 'latitude')."),
                               tags$li("Leave pigment dropdowns blank if that pigment is missing."),
                               tags$li("Click ", span(class="text-primary", icon("check"), "Apply Selected"), " to save your choices for that file."),
                               tags$li("Click ", span(class="text-success", icon("save"), "Save Session"), " to remember these mappings for next time.")
                             ),
                             p(strong("Outcome:"), " Once all rows are Green or Yellow (but acknowledged), click ", span(class="text-success", icon("check-double"), "Confirm Mappings"), " to proceed.")
                           ),
                           "step4" = tagList(
                             h4("Current Step Instructions: QC & Filtering"),
                             p(strong("Goal:"), " Apply the cleaning rules and filters defined in Step 1."),
                             p(strong("Action:"), " Execute the pipeline."),
                             tags$ul(
                               tags$li("Click ", span(class="text-primary", icon("cogs"), "Run QC Pipeline"), "."),
                               tags$li("Review the 'Quality Control Summary'. If too many samples failed, go back to Step 1 and adjust your filter ranges.")
                             ),
                             p(strong("Outcome:"), " A clean dataset where every sample has passed your quality checks. This clean data is passed to Step 5.")
                           ),
                           "step5" = tagList(
                             h4("Current Step Instructions: Grouping Strategy"),
                             p(strong("Goal:"), " Decide how to group samples for the analysis algorithm."),
                             p(strong("Action:"), " Choose a strategy."),
                             tags$ul(
                               tags$li("Select 'Analyze Files Individually' or 'Cluster Samples' (recommended)."),
                               tags$li("Note: Clustering normalizes pigment data to Total Chl-a for grouping purposes only; raw values are passed to the next step."),
                               tags$li("Click ", span(class="text-primary", icon("eye"), "Preview Groups"), " to see the result."),
                               tags$li("Click ", span(class="text-success", icon("check-double"), "Confirm Strategy"), ".")
                             ),
                             p(strong("Outcome:"), " Defined analysis groups are locked in. Proceed to Step 6.")
                           ),
                           "step6" = tagList(
                             h4("Current Step Instructions: Run Optimization"),
                             p(strong("Goal:"), " Calculate phytoplankton class abundances."),
                             p(strong("Action:"), " Execute the Simulated Annealing algorithm."),
                             tags$ul(
                               tags$li("Select the groups you want to process (all groups are selected by default)."),
                               tags$li("Click ", span(class="text-primary", icon("rocket"), "Start Optimization"), ".")
                             ),
                             p(strong("Outcome:"), " Class abundances are calculated. The app automatically chooses Fm_Pro or Fm_NoPro based on the presence/absence of DVChla.")
                           ),
                           "step7" = tagList(
                             h4("Current Step Instructions: Results & Export"),
                             p(strong("Goal:"), " Visualise and save your data."),
                             p(strong("Action:"), " Download the results."),
                             tags$ul(
                               tags$li("Select a file or cluster group from the dropdown to see its result plots."),
                               tags$li("Click ", span(class="text-primary", icon("download"), "Download Report Package"), ".")
                             ),
                             p(strong("Outcome:"), " A timestamped folder is saved in this app folder containing Excel files, result plots, and a Master Report.")
                           )
    )
    
    showModal(modalDialog(
      title = div(icon("question-circle"), paste("Help: ", current_tab)),
      size = "l",
      easyClose = TRUE,
      tabsetPanel(
        tabPanel("Instructions", br(), help_content),
        tabPanel("README",
                 br(),
                 h4("Overview"),
                 p("phytoclassShiny is a user-friendly graphical interface for the 'phytoclass' R package. It enables researchers to determine phytoplankton community structure from HPLC pigment data using Simulated Annealing."),
                 
                 # --- CLARIFICATION BLOCK ---
                 div(class = "alert alert-info", icon("info-circle"), style = "margin-top: 15px; margin-bottom: 15px;",
                     strong("Methodological Distinction:"),
                     tags$br(),
                     "Unlike CHEMTAX, which utilizes steepest-descent algorithms that can be sensitive to initial starting conditions, phytoclass uses Simulated Annealing to search the global solution space. Consequently, this method does ", 
                     em("not"), 
                     " require the strict curation of region-specific initial pigment ratio tables (F0) often needed to prevent bias in traditional approaches."
                 ),
                 
                 hr(),
                 h5("Validation"),
                 p("This application has been validated against the original R package using benchmark Southern Ocean datasets (Viljoen et al., 2025). It achieves exceptionally high agreement (R² ≥ 0.99) for major phytoplankton groups compared to the core algorithm."),
                 
                 hr(),
                 h5("Citation"),
                 p("If you use this tool for publication, please cite the following key references:"),
                 
                 tags$ul(
                   tags$li(strong("Algorithm:"), " Hayward, A., et al. (2023). phytoclass: A pigment-based chemotaxonomic method to determine the biomass of phytoplankton classes. ", em("Limnology and Oceanography: Methods"), ", 21, 220–241."),
                   tags$li(strong("Software:"), " Quinlan, L.B. (2025). phytoclassShiny: An R-Based Application for Phytoplankton Community Analysis (Version 1.0). Zenodo. [DOI Pending]"),
                   tags$li(strong("Original Method:"), " Mackey, M. D., et al. (1996). CHEMTAX - a program for estimating class abundances from chemical markers. ", em("Mar. Ecol. Prog. Ser."), ", 144, 265–283.")
                 ),
                 
                 hr(),
                 h5("License"),
                 p(strong("GNU GPLv3 (General Public License v3.0)")),
                 p("This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License. It is intended to remain a free, open-access tool for the scientific community and cannot be incorporated into proprietary software for profit.")
        )
      ),
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