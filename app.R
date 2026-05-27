# ============================================================================
#
#   _phytoclass_Shiny V1.0 - MAIN APPLICATION LAUNCHER
#
#   Description:
#   Main Driver. Hardened state management, continuous machine-learning 
#   ETA calibration, and streamlined operational manuals.
#  
# ============================================================================

# --- 0. PREAMBLE & INITIAL SETUP ---
base::cat("--- Initializing _phytoclass_Shiny V1.0 ---\n")
base::options(shiny.maxRequestSize = 500 * 1024^2)

required_packages <- base::c(
  "shiny", "bslib", "shinyjs", "shinyWidgets", "DT", "yaml", "dplyr", 
  "tidyr", "readxl", "openxlsx", "lubridate", "digest", "rlang", 
  "tibble", "fs", "purrr", "scales", "vegan", "cluster", "factoextra", 
  "ggplot2", "MASS", "dynamicTreeCut", "glue", "stringdist", "phytoclass"
)

base::cat("--> Checking system dependencies...\n")
for (pkg in required_packages) {
  if (!base::requireNamespace(pkg, quietly = TRUE)) {
    base::cat(base::sprintf("    ...Installing missing package: %s\n", pkg))
    base::tryCatch({ utils::install.packages(pkg, dependencies = TRUE) }, error = function(e) { base::stop(base::paste("Failed to install", pkg, ":", e$message)) })
  }
}

core_ui_packages <- base::c("shiny", "bslib", "shinyjs", "shinyWidgets", "DT")
base::invisible(base::lapply(core_ui_packages, base::library, character.only = TRUE))

`%||%` <- rlang::`%||%`

base::cat("--> Sourcing modules from 'R/'...\n")
base::tryCatch({
  scripts_to_source <- base::list.files("R", pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
  for (script in scripts_to_source) { base::source(script) }
}, error = function(e) { base::stop("FATAL ERROR: Failed to source R modules. Error: ", e$message) })
base::cat("--> All modules sourced successfully.\n\n")


# ============================================================================
# --- 1. USER INTERFACE (UI) ---
# ============================================================================

jscode <- "
$(document).on('click', '.btn', function() {
  var id = $(this).attr('id');
  if (id) { Shiny.setInputValue('last_btn_clicked', id, {priority: 'event'}); }
});
"

ui <- bslib::page_navbar(
  theme = bslib::bs_theme(version = 5, bootswatch = "zephyr", primary = "#0d6efd", success = "#198754"),
  title = shiny::HTML("<strong style='color: #0d6efd'><i>phytoclass</i>Shiny</strong>"),
  id = "main_navbar",
  
  header = shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::tags$head(shiny::tags$script(shiny::HTML(jscode))), 
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
        body, p, h1, h2, h3, h4, h5, h6, .nav-link, .btn, label, input, select, .alert { font-family: system-ui, -apple-system, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif !important; }
        .well { background-color: #f8f9fa; border: 1px solid #dee2e6; padding: 20px; border-radius: 8px; margin-bottom: 20px;}
        h4 { color: #0056b3; border-bottom: 2px solid #dee2e6; padding-bottom: 5px; margin-top: 0; margin-bottom: 15px;}
        #session_audit_log { font-family: monospace !important; font-size: 0.8em; white-space: pre-wrap; word-wrap: break-word; background-color: #212529; color: #00ff00; padding: 10px; border-radius: 5px; height: 100%; overflow-y: auto; }
        .progress-bar-spec { transition: width 0.4s cubic-bezier(0.4, 0, 0.2, 1); }
        table.dataTable { width: 100% !important; margin: 0 auto; clear: both; border-collapse: collapse; }
        .dataTables_wrapper { width: 100% !important; overflow-x: auto; }
        .checkbox, .checkbox label { width: 100% !important; display: block; }
        
        /* SHARP STRAIGHT UNDERLINE FOR ACTIVE TAB */
        .navbar-nav .nav-link.active {
            border-bottom: 3px solid #0d6efd !important;
            border-bottom-left-radius: 0 !important;
            border-bottom-right-radius: 0 !important;
            color: #0d6efd !important;
            font-weight: bold;
        }
      "))
    )
  ),
  
  sidebar = bslib::sidebar(
    id = "audit_log_sidebar", title = shiny::span(shiny::icon("terminal"), " System Console"),
    position = "right", open = "closed", width = 400,
    shiny::verbatimTextOutput("session_audit_log")
  ),
  
  bslib::nav_panel("1. Setup Parameters", value="step1", icon = shiny::icon("sliders-h"),
                   shiny::h3("Step 1: Setup Parameters"),
                   shiny::p("Set up your folders, run settings, and data filters below."),
                   shiny::hr(),
                   bslib::layout_columns(
                     col_widths = base::c(4, 8),
                     shiny::tagList(
                       shiny::wellPanel(
                         shiny::h4("1. Saved Sessions"),
                         shiny::actionButton("load_session_config_btn", "Load Last Session", icon = shiny::icon("folder-open"), width = "100%", class = "btn-primary mb-2"),
                         shiny::actionLink("toggle_reset_link", "Show reset options..."),
                         shinyjs::hidden(
                           shiny::div(id = "reset_div", class = "mt-2",
                                      shiny::actionButton("reset_to_default_btn", "Reset to Default Settings", icon = shiny::icon("redo"), class = "btn-outline-danger w-100")
                           )
                         )
                       ),
                       shiny::wellPanel(
                         shiny::h4("2. Matrix Files"),
                         shinyjs::disabled(shiny::textInput("output_dir_ui", "Output Directory (from config):", width = "100%")),
                         shiny::textInput("fm_pro_path_ui", "Fm_Pro.xlsx Path:", width="100%"),
                         shiny::textInput("fm_nopro_path_ui", "Fm_NoPro.xlsx Path:", width="100%"),
                         shiny::actionButton("load_fm_btn", "Check Matrix Files", icon = shiny::icon("check-double"), width="100%", class="btn-outline-primary")
                       )
                     ),
                     shiny::tagList(
                       bslib::accordion(
                         id = "config_accordion_stack", open = FALSE,
                         bslib::accordion_panel("Phytoclass Run Settings", icon = shiny::icon("sliders-h"),
                                                shiny::fluidRow(
                                                  shiny::column(6, shiny::numericInput("niter_input", "Iterations (Niter):", value = 500, min = 10, step = 10)),
                                                  shiny::column(6, shiny::numericInput("step_size_input", "Cooling Step Size:", value = 0.009, min = 0.0001, step = 0.001))
                                                ),
                                                shiny::hr(),
                                                shiny::fluidRow(
                                                  shiny::column(6, shiny::checkboxInput("toggle_fixed_seed", "Use Fixed Random Seed", value = FALSE)),
                                                  shiny::column(6, shiny::conditionalPanel(condition = "input.toggle_fixed_seed == true", shiny::numericInput("fixed_seed_input", "Seed Value:", value = 131234, step = 1)))
                                                )
                         ),
                         bslib::accordion_panel("Data Cleaning Options", icon = shiny::icon("broom"),
                                                shiny::div(class="mb-3", shiny::checkboxInput("toggle_handle_duplicates", "Remove duplicate samples", value = TRUE, width="100%")),
                                                shiny::div(class="mb-3", shiny::checkboxInput("toggle_handle_nas", "Change blank cells to 0", value = TRUE, width="100%")),
                                                shiny::div(class="mb-3", shiny::checkboxInput("toggle_handle_negatives", "Change negative numbers to 0", value = TRUE, width="100%")),
                                                shiny::div(class="mb-3", shiny::checkboxInput("toggle_handle_zerosum", "Remove empty samples (rows with all zeros)", value = TRUE, width="100%"))
                         ),
                         bslib::accordion_panel("Data Filters", icon = shiny::icon("filter"),
                                                shiny::fluidRow(
                                                  shiny::column(4, 
                                                                shiny::checkboxInput("toggle_geo_filter", shiny::strong("Location Filter"), value = FALSE),
                                                                shiny::conditionalPanel(condition = "input.toggle_geo_filter == true", shiny::div(class = "mt-3 pt-2 border-top", shiny::numericInput("min_lat_ui", "Min Lat:", -90, width="100%"), shiny::numericInput("max_lat_ui", "Max Lat:", 90, width="100%"), shiny::numericInput("min_lon_ui", "Min Lon:", -180, width="100%"), shiny::numericInput("max_lon_ui", "Max Lon:", 180, width="100%")))
                                                  ),
                                                  shiny::column(4, 
                                                                shiny::checkboxInput("toggle_temporal_filter", shiny::strong("Date Filter"), value = FALSE),
                                                                shiny::conditionalPanel(condition = "input.toggle_temporal_filter == true", shiny::div(class = "mt-3 pt-2 border-top", shiny::dateInput("start_date_ui", "Start Date:", "1900-01-01", width="100%"), shiny::dateInput("end_date_ui", "End Date:", base::Sys.Date(), width="100%")))
                                                  ),
                                                  shiny::column(4, 
                                                                shiny::checkboxInput("toggle_depth_filter", shiny::strong("Depth Filter"), value = FALSE),
                                                                shiny::conditionalPanel(condition = "input.toggle_depth_filter == true", shiny::div(class = "mt-3 pt-2 border-top", shiny::numericInput("min_depth_ui", "Min Depth (m):", 0, width="100%"), shiny::numericInput("max_depth_ui", "Max Depth (m):", 1000, width="100%")))
                                                  )
                                                )
                         )
                       )
                     )
                   )
  ),
  
  bslib::nav_panel("2. Import Data", value="step2", icon = shiny::icon("file-import"), 
                   shiny::h3("Step 2: Import Data"), shiny::p("Select one or more `.xlsx` files containing pigment data."), shiny::hr(), 
                   bslib::layout_columns(
                     col_widths = base::c(4, 8),
                     shiny::wellPanel(
                       shiny::fileInput("hplc_data_files_input", "Select Files:", multiple = TRUE, accept = base::c(".xlsx"), width="100%"), 
                       shiny::actionButton("load_data_btn", "Load Data Files", icon = shiny::icon("play"), class = "btn-primary w-100 fw-bold")
                     ),
                     shiny::wellPanel(shiny::h4("Uploaded Files Summary"), DT::DTOutput("batch_file_load_status_table"))
                   )),
  
  bslib::nav_panel("3. Map Variables", value="step3", icon = shiny::icon("exchange-alt"), validationUI("step3_validation")),
  bslib::nav_panel("4. Filter & Clean", value="step4", icon = shiny::icon("filter"), qcUI("step4_qc")),
  bslib::nav_panel("5. Group Samples", value="step5", icon = shiny::icon("object-group"), strategyUI("step5_strategy")),
  
  bslib::nav_panel("6. Run Analysis", value="step6", icon = shiny::icon("play-circle"), 
                   shiny::h3("Step 6: Run Analysis"), shiny::p("Select your data and start processing."), shiny::hr(), 
                   shiny::sidebarLayout(
                     shiny::sidebarPanel(width = 4, 
                                         shiny::h4("1. Select Data to Run"), 
                                         shinyWidgets::pickerInput(inputId = "datasets_for_phytoclass_run", label = NULL, choices = NULL, multiple = TRUE, options = shinyWidgets::pickerOptions(actionsBox = TRUE, selectAllText="Select All", deselectAllText="Deselect All")), 
                                         shiny::hr(), 
                                         shiny::wellPanel(style="background-color: #e9ecef; padding: 15px;", shiny::h5(shiny::icon("tasks"), "Current Run Summary"), shiny::verbatimTextOutput("analysis_params_review")), 
                                         shiny::hr(), 
                                         shiny::h4("2. Run Analysis"), 
                                         shiny::actionButton("run_phytoclass_btn", "Start Analysis", class = "btn-primary btn-lg w-100 fw-bold", icon = shiny::icon("rocket"))), 
                     shiny::mainPanel(width = 8, 
                                      shinyjs::hidden(
                                        shiny::div(id = "live_tracker_card",
                                                   shiny::wellPanel(style = "background-color: #ffffff; border-left: 5px solid #17a2b8;",
                                                                    shiny::h4(style = "color: #17a2b8; margin-top: 0; display: flex; align-items: center; justify-content: space-between;", shiny::span(shiny::icon("stopwatch"), " Live Progress Dashboard"), shiny::div(id = "tracker_spinner_container", style="animation: spin 1s linear infinite; display: inline-block;", shiny::icon("sync-alt", class="text-info fa-spin"))),
                                                                    shiny::hr(),
                                                                    shiny::fluidRow(
                                                                      shiny::column(7, 
                                                                                    shiny::p(shiny::strong("Current Task: "), shiny::span(id = "trk_task", class="text-primary")), 
                                                                                    shiny::p(shiny::strong("Batch Progress: "), shiny::span(id = "trk_prog"))
                                                                      ),
                                                                      shiny::column(5, 
                                                                                    shiny::p(shiny::strong(shiny::span(id="lbl_elapsed", "Time Elapsed: ")), shiny::span(id = "trk_elapsed", style = "font-family: monospace; font-weight:700; color: #28a745;")), 
                                                                                    shiny::p(shiny::strong(shiny::span(id="lbl_eta", "Est. Remaining: ")), shiny::span(id = "trk_eta", style = "font-family: monospace; font-weight:700; color: #0d6efd;"))
                                                                      )
                                                                    ),
                                                                    shiny::div(style = "margin-top: 15px; background: #e9ecef; border-radius: 4px; height: 16px; width: 100%; position: relative; overflow: hidden;", shiny::div(id = "tracker_progress_bar", class="progress-bar-spec", style = "background: #17a2b8; height: 100%; width: 0%;"))
                                                   )
                                        )
                                      ),
                                      shiny::wellPanel(shiny::h4("Analysis Results Logs"), DT::DTOutput("phytoclass_batch_summary_table"))
                     )
                   )
  ),
  
  bslib::nav_panel("7. View & Export", value="step7", icon = shiny::icon("chart-line"), reportingUI("step7_reporting")),
  
  bslib::nav_spacer(),
  bslib::nav_item(shiny::actionButton("save_config_btn_global", "Save Session", icon = shiny::icon("save"), class = "btn-success btn-sm", style="margin-right: 5px;")),
  bslib::nav_item(shiny::actionButton("help_btn_global", "Help", icon = shiny::icon("question-circle"), class = "btn-info btn-sm", style="margin-right: 15px;"))
)

# ============================================================================
# --- 3. SERVER LOGIC ---
# ============================================================================

server <- function(input, output, session) {
  
  rv <- shiny::reactiveValues(
    session_id = base::paste0("Run_", base::format(base::Sys.time(), "%Y%m%d_%H%M%S")),
    config = NULL, datasets_processed = base::list(), master_qc_data = NULL,       
    analysis_datasets = base::list(), analyzed_datasets = base::list(), 
    cluster_diagnostics = NULL, qc_summary_df = NULL, 
    performance_metrics = NULL, session_log = base::character(0), staging_datasets = base::list(),
    mapping_trigger = 0, current_mapping_dataset = NULL, mapping_history = base::list(),
    resolution_warnings = base::list()
  )
  
  step1_inputs <- base::c("output_dir_ui", "fm_pro_path_ui", "fm_nopro_path_ui", "toggle_handle_duplicates", "toggle_handle_nas", "toggle_handle_negatives", "toggle_handle_zerosum", "niter_input", "step_size_input", "toggle_fixed_seed", "fixed_seed_input", "toggle_geo_filter", "toggle_temporal_filter", "toggle_depth_filter", "min_lat_ui", "max_lat_ui", "min_lon_ui", "max_lon_ui", "min_depth_ui", "max_depth_ui", "start_date_ui", "end_date_ui")
  step5_inputs <- base::c("step5_strategy-normalization_method_input", "step5_strategy-transformation_method_input", "step5_strategy-cluster_method_input", "step5_strategy-k_max_input", "step5_strategy-k_determination_mode")
  
  .log_event <- function(category = "SYSTEM", message) {
    if (base::missing(message)) { message <- category; category <- "INFO" }
    timestamp <- base::format(base::Sys.time(), "%H:%M:%S")
    rv$session_log <- base::c(rv$session_log, base::sprintf("[%s] [%s] %s", timestamp, category, message))
  }
  
  shiny::observeEvent(input$main_navbar, { .log_event("NAV", base::paste("Switched tab to:", input$main_navbar)) }, ignoreInit = TRUE)
  shiny::observeEvent(input$last_btn_clicked, { .log_event("USER", base::paste("Clicked Button:", input$last_btn_clicked)) })
  
  base::lapply(step1_inputs, function(id) {
    shiny::observeEvent(input[[id]], {
      val <- input[[id]]; if(base::length(val) > 1) val <- base::paste0("(", base::length(val), " items)")
      .log_event("INPUT", base::paste0(id, " updated to: ", val))
      if (id %in% base::c("fm_pro_path_ui", "fm_nopro_path_ui")) rv$fm_matrices <- NULL 
      if (!base::is.null(rv$master_qc_data) || base::length(rv$datasets_processed) > 0) {
        .log_event("RESET", base::sprintf("Step 1 Settings modified. Wiping downstream to prevent corruption: %s", id))
        reset_downstream_data("qc"); .update_workflow_state("step2")
        shiny::showNotification("Settings changed. Downstream metrics cleared to maintain mathematical consistency.", type = "warning", duration = 6)
      }
    }, ignoreInit = TRUE)
  })
  
  base::lapply(step5_inputs, function(id) {
    shiny::observeEvent(input[[id]], {
      val <- input[[id]]; if(base::length(val) > 1) val <- base::paste0("(", base::length(val), " items)")
      .log_event("INPUT", base::paste0(id, " updated to: ", val))
      if (base::length(rv$analyzed_datasets) > 0) {
        .log_event("RESET", base::sprintf("Step 5 Grouping modified. Wiping Analysis results: %s", id))
        reset_downstream_data("analysis")
        shiny::showNotification("Grouping strategy changed. Analysis results cleared to maintain consistency.", type = "warning", duration = 6)
      }
    }, ignoreInit = TRUE)
  })
  
  .update_workflow_state <- function(enable_up_to) {
    tabs_to_manage <- base::c("step2", "step3", "step4", "step5", "step6", "step7")
    if (enable_up_to == "step1") { for (tab in tabs_to_manage) shinyjs::disable(selector = glue::glue(".navbar-nav a[data-value='{tab}']"))
    } else {
      target_index <- base::which(tabs_to_manage == enable_up_to)
      if (base::length(target_index) > 0) {
        for (i in base::seq_along(tabs_to_manage)) {
          if (i <= target_index) shinyjs::enable(selector = glue::glue(".navbar-nav a[data-value='{tabs_to_manage[i]}']"))
          else shinyjs::disable(selector = glue::glue(".navbar-nav a[data-value='{tabs_to_manage[i]}']"))
        }
      }
    }
  }
  
  output$session_audit_log <- shiny::renderText({ base::paste(base::rev(rv$session_log), collapse = "\n") })
  
  reset_downstream_data <- function(level = "all") {
    .log_event("SYSTEM", base::sprintf("Resetting data level: %s", level))
    if (level %in% base::c("all", "config")) { 
      rv$staging_datasets <- base::list()
      rv$mapping_history <- base::list()
      rv$mapping_trigger <- rv$mapping_trigger + 1
      rv$resolution_warnings <- base::list() 
    }
    if (level %in% base::c("all", "config", "qc")) { 
      rv$master_qc_data <- NULL
      rv$analysis_datasets <- base::list()
      rv$analyzed_datasets <- base::list()
      rv$qc_summary_df <- NULL
      rv$cluster_diagnostics <- NULL 
    }
    if (level == "strategy") { rv$analysis_datasets <- base::list(); rv$analyzed_datasets <- base::list(); rv$cluster_diagnostics <- NULL }
    if (level == "analysis") { rv$analyzed_datasets <- base::list() }
  }
  
  shiny::observeEvent(once = TRUE, eventExpr = session, {
    .update_workflow_state("step1"); .log_event("SYSTEM", "App session started successfully.")
    base::tryCatch({
      rv$config <- initialize_config()
      update_all_ui_from_config(rv$config, session) 
      if (!base::is.null(rv$config$phytoclass$use_fixed_seed)) shiny::updateCheckboxInput(session, "toggle_fixed_seed", value = as.logical(rv$config$phytoclass$use_fixed_seed))
      if (!base::is.null(rv$config$phytoclass$fixed_seed)) shiny::updateNumericInput(session, "fixed_seed_input", value = as.numeric(rv$config$phytoclass$fixed_seed))
      
      fm_result <- load_fm_matrices(rv$config)
      if (!base::is.null(fm_result$error)) .log_event("WARNING", fm_result$error) else rv$fm_matrices <- fm_result
      if (!base::is.null(rv$config)) .update_workflow_state("step2")
    }, error = function(e) { .log_event("FATAL", e$message) })
  })
  
  shiny::observeEvent(input$load_session_config_btn, {
    old_config <- rv$config 
    base::tryCatch({
      temp_config <- load_config(CONFIG_SESSION_PATH)
      if (base::is.null(temp_config)) base::stop("Configuration file returned NULL.")
      rv$config <- temp_config; update_all_ui_from_config(rv$config, session); reset_downstream_data("config"); .update_workflow_state("step2")
      if (!base::is.null(rv$config$phytoclass$use_fixed_seed)) shiny::updateCheckboxInput(session, "toggle_fixed_seed", value = as.logical(rv$config$phytoclass$use_fixed_seed))
      if (!base::is.null(rv$config$phytoclass$fixed_seed)) shiny::updateNumericInput(session, "fixed_seed_input", value = as.numeric(rv$config$phytoclass$fixed_seed))
      
      fm_result <- load_fm_matrices(rv$config)
      if (base::is.null(fm_result$error)) rv$fm_matrices <- fm_result
      shiny::showNotification("Saved configuration profile reloaded.", type="message")
    }, error = function(e) { 
      rv$config <- old_config 
      shiny::showModal(shiny::modalDialog(title="Error Loading File", base::paste("Failed to load session. Reverting to previous state. Error:", e$message))) 
    })
  })
  
  shiny::observeEvent(input$toggle_reset_link, { shinyjs::toggle("reset_div", anim = TRUE) })
  shiny::observeEvent(input$reset_to_default_btn, { shiny::showModal(shiny::modalDialog(title = "Reset Options", "Are you sure? This will discard your current choices.", easyClose = TRUE, footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton("confirm_reset_btn", "Yes, Reset", class = "btn-danger")))) })
  shiny::observeEvent(input$confirm_reset_btn, {
    shiny::removeModal()
    base::tryCatch({
      rv$config <- load_config(CONFIG_TEMPLATE_PATH); update_all_ui_from_config(rv$config, session); reset_downstream_data("config"); .update_workflow_state("step2")
      shiny::updateCheckboxInput(session, "toggle_fixed_seed", value = FALSE)
      
      fm_result <- load_fm_matrices(rv$config)
      if (base::is.null(fm_result$error)) rv$fm_matrices <- fm_result
      shiny::showNotification("Standard defaults reloaded.", type="message")
    }, error = function(e) { .log_event("ERROR", e$message) })
  })
  
  shiny::observeEvent(input$save_config_btn_global, {
    shiny::req(rv$config)
    rv$config <- sync_config_with_ui(rv$config, input)
    rv$config <- sync_config_with_ui(rv$config, input, ns_prefix = "step5_strategy-")
    rv$config$phytoclass$use_fixed_seed <- input$toggle_fixed_seed
    rv$config$phytoclass$fixed_seed <- input$fixed_seed_input
    rv$config <- update_config_with_new_aliases(rv$config, rv$datasets_processed)
    base::tryCatch({ save_config(rv$config, CONFIG_SESSION_PATH); shiny::showNotification("Settings saved successfully.", type = "message") }, error = function(e) {})
  })
  
  shiny::observeEvent(input$load_fm_btn, {
    shiny::req(rv$config)
    rv$config <- sync_config_with_ui(rv$config, input)
    fm_result <- load_fm_matrices(rv$config)
    if (!base::is.null(fm_result$error)) { rv$fm_matrices <- NULL; shiny::showModal(shiny::modalDialog(title="Error", fm_result$error))
    } else { rv$fm_matrices <- fm_result; shiny::showNotification("Files checked successfully.", type="message") }
  })
  
  shiny::observeEvent(input$load_data_btn, {
    shiny::req(input$hplc_data_files_input)
    reset_downstream_data("all"); .update_workflow_state("step2")
    rv$datasets_processed <- load_all_files(input$hplc_data_files_input, rv$config, .log_event)
    if(base::length(rv$datasets_processed) > 0) {
      rv$staging_datasets <- purrr::map(rv$datasets_processed, rlang::duplicate)
      rv$mapping_history <- base::list(); rv$mapping_trigger <- rv$mapping_trigger + 1
      .update_workflow_state("step3")
    } else { shiny::showModal(shiny::modalDialog(title = "Ingestion Failure", "No usable matrices were extracted from the uploaded batch.")) }
  })
  
  output$batch_file_load_status_table <- DT::renderDT({
    shiny::req(base::length(rv$datasets_processed) > 0)
    summary_df <- purrr::map_df(rv$datasets_processed, ~tibble::tibble(Name = .x$name, Rows = .x$log$initial_rows, Cols = .x$log$initial_cols))
    DT::datatable(summary_df, options = base::list(pageLength = 10, searching=FALSE, lengthChange=FALSE), rownames=FALSE)
  })
  
  validationServer("step3_validation", rv, .log_event, .update_workflow_state, session)
  qcServer("step4_qc", rv, .log_event, .update_workflow_state, reset_downstream_data)
  strategyServer("step5_strategy", rv, .log_event, .update_workflow_state, session)
  reportingServer("step7_reporting", rv, .log_event)
  
  output$analysis_params_review <- shiny::renderText({
    shiny::req(rv$config, rv$master_qc_data, rv$analysis_datasets)
    
    # Map the current inputs to the config temporarily just for the display update
    temp_config <- rv$config
    temp_config$phytoclass$use_fixed_seed <- input$toggle_fixed_seed
    temp_config$phytoclass$fixed_seed <- input$fixed_seed_input
    
    generate_run_summary_text(temp_config, rv$master_qc_data, rv$analysis_datasets, rv$cluster_diagnostics)
  })
  
  shiny::observeEvent(rv$analysis_datasets, {
    if (base::length(rv$analysis_datasets) > 0) {
      shinyWidgets::updatePickerInput(session = session, inputId = "datasets_for_phytoclass_run", choices = base::names(rv$analysis_datasets), selected = base::names(rv$analysis_datasets))
    } else { shinyWidgets::updatePickerInput(session, "datasets_for_phytoclass_run", choices = base::character(0)) }
  })
  
  shiny::observeEvent(input$run_phytoclass_btn, {
    if (base::is.null(rv$fm_matrices)) { shiny::showNotification("Execution Blocked: Reference matrices are missing.", type = "error"); return() }
    if (base::length(rv$analysis_datasets) == 0) { shiny::showNotification("Execution Blocked: No grouped data available.", type = "error"); return() }
    if (base::length(input$datasets_for_phytoclass_run) == 0) { shiny::showNotification("Execution Blocked: Select at least one dataset.", type = "warning"); return() }
    
    shinyjs::disable("run_phytoclass_btn")
    shinyjs::show("tracker_spinner_container")
    shinyjs::html("lbl_elapsed", "Time Elapsed: "); shinyjs::html("lbl_eta", "Est. Remaining: ")
    
    # Ensure explicit syncing of the seed options into the reactive config right before processing begins
    rv$config$phytoclass$use_fixed_seed <- input$toggle_fixed_seed
    rv$config$phytoclass$fixed_seed <- input$fixed_seed_input
    
    datasets_to_run <- rv$analysis_datasets[input$datasets_for_phytoclass_run]
    total_samples_global <- base::sum(base::vapply(datasets_to_run, function(x) base::nrow(x$data), base::numeric(1)))
    
    curr_niter <- base::as.numeric(rv$config$phytoclass$niter %||% 500)
    curr_step  <- base::as.numeric(rv$config$phytoclass$step_size %||% 0.009)
    complexity_scale <- curr_niter / curr_step
    
    historical_coef <- base::as.numeric(rv$config$performance$system_calibration_coefficient %||% 0.000009)
    predicted_speed_per_sample <- historical_coef * complexity_scale
    initial_predicted_eta <- predicted_speed_per_sample * total_samples_global
    
    reset_downstream_data("analysis")
    samples_finished_so_far <- 0
    start_time_global <- base::Sys.time()
    
    shinyjs::show("live_tracker_card")
    shinyjs::runjs("$('#tracker_progress_bar').css('width', '0%').css('background', '#17a2b8');")
    shinyjs::runjs("if(window.optTimer) clearInterval(window.optTimer); var startOpt = Date.now(); window.optTimer = setInterval(function() { var diffOpt = Math.floor((Date.now() - startOpt) / 1000); $('#trk_elapsed').text(Math.floor(diffOpt / 60).toString().padStart(2, '0') + 'm ' + (diffOpt % 60).toString().padStart(2, '0') + 's'); }, 1000);")
    
    temp_analyzed_list <- base::list()
    
    for (i in base::seq_along(datasets_to_run)) {
      ds_obj <- datasets_to_run[[i]]
      current_batch_size <- base::nrow(ds_obj$data)
      elapsed_sec <- base::as.numeric(base::difftime(base::Sys.time(), start_time_global, units="secs"))
      
      live_speed_per_sample <- if (samples_finished_so_far == 0) { predicted_speed_per_sample } else { (0.75 * (elapsed_sec / samples_finished_so_far)) + (0.25 * predicted_speed_per_sample) }
      total_remaining_samples <- total_samples_global - samples_finished_so_far
      total_eta_seconds <- live_speed_per_sample * total_remaining_samples
      pct <- if(total_samples_global > 0) base::round((samples_finished_so_far / total_samples_global) * 100) else 0
      
      shinyjs::html("trk_task", base::sprintf("Analyzing '%s' (File %d of %d)", ds_obj$name, i, base::length(datasets_to_run)))
      shinyjs::html("trk_prog", base::sprintf("%d / %d Samples (%d%%)", samples_finished_so_far, total_samples_global, pct))
      shinyjs::html("trk_eta", base::sprintf("~%02dm %02ds", base::floor(total_eta_seconds / 60), base::round(total_eta_seconds %% 60)))
      shinyjs::runjs(base::sprintf("$('#tracker_progress_bar').css('width', '%d%%');", pct))
      
      base::tryCatch({
        analyzer_out <- run_phytoclass_analysis(ds_obj$data, rv$config, rv$fm_matrices)
        ds_obj$log_analyzer <- analyzer_out$log
        ds_obj$pigment_matrix_final <- analyzer_out$pigment_matrix_used
        ds_obj$f_matrix_final <- analyzer_out$f_matrix_final
        ds_obj$phytoclass_raw <- analyzer_out$phytoclass_raw
        
        if (!base::is.null(analyzer_out$results)) ds_obj$data_final <- dplyr::left_join(ds_obj$data, analyzer_out$results, by = "UniqueID")
      }, error = function(e) {
        .log_event("ENGINE FAULT", base::sprintf("Math crash in dataset %s: %s", ds_obj$name, e$message))
        ds_obj$log_analyzer <- base::list(status = "Failed", mean_rmse = NA, mean_condnum = NA, fm_matrix_used = "N/A", error_details = base::list(message = e$message))
      })
      
      temp_analyzed_list[[ds_obj$name]] <- ds_obj
      samples_finished_so_far <- samples_finished_so_far + current_batch_size
    }
    
    rv$analyzed_datasets <- temp_analyzed_list
    shinyjs::runjs("if(window.optTimer) clearInterval(window.optTimer);")
    shinyjs::html("trk_task", "Analysis Execution Complete.")
    shinyjs::html("trk_prog", base::sprintf("%d / %d Samples (%d%%)", total_samples_global, total_samples_global, 100))
    shinyjs::runjs("$('#tracker_progress_bar').css('width', '100%').css('background', '#198754');")
    shinyjs::hide("tracker_spinner_container")
    
    end_time <- base::Sys.time()
    total_duration_sec <- base::as.numeric(base::difftime(end_time, start_time_global, units="secs"))
    final_avg_speed <- base::round(total_duration_sec / base::max(1, total_samples_global), 4)
    
    shinyjs::html("lbl_elapsed", "Actual Total Runtime: ")
    shinyjs::html("trk_elapsed", base::sprintf("%02dm %02ds", base::floor(total_duration_sec / 60), base::round(total_duration_sec %% 60)))
    shinyjs::html("lbl_eta", "Initial Predicted ETA: ")
    shinyjs::html("trk_eta", base::sprintf("%02dm %02ds", base::floor(initial_predicted_eta / 60), base::round(initial_predicted_eta %% 60)))
    
    if (base::any(base::vapply(temp_analyzed_list, function(x) !base::is.null(x$data_final), base::logical(1)))) {
      local_config <- rv$config
      total_historical_runs <- base::as.numeric(local_config$performance$total_historical_runs %||% 0)
      current_run_coefficient <- total_duration_sec / (total_samples_global * complexity_scale)
      
      if (total_historical_runs == 0) { new_rolling_coefficient <- current_run_coefficient
      } else { new_rolling_coefficient <- ((historical_coef * total_historical_runs) + current_run_coefficient) / (total_historical_runs + 1) }
      
      local_config$performance$total_historical_runs <- total_historical_runs + 1
      local_config$performance$system_calibration_coefficient <- new_rolling_coefficient
      local_config$performance$avg_speed <- final_avg_speed
      local_config$performance$last_niter <- curr_niter
      local_config$performance$last_step_size <- curr_step
      
      rv$config <- local_config
      if (base::exists("save_config") && base::is.function(save_config)) { 
        base::tryCatch({ 
          save_config(rv$config, CONFIG_SESSION_PATH) 
          .log_event("CALIBRATION", base::sprintf("System learned from run #%d. Adjusted baseline factor: %.6g", total_historical_runs + 1, new_rolling_coefficient))
        }, error = function(e) {}) 
      }
    }
    
    base::tryCatch({
      checkpoint_dir <- rv$config$workspace$output_directory %||% "phytoclass_output"
      if (!base::dir.exists(checkpoint_dir)) base::dir.create(checkpoint_dir, recursive = TRUE)
      base::saveRDS(rv$analyzed_datasets, file = base::file.path(checkpoint_dir, "AUTOSAVE_LATEST_RUN.rds"))
      .log_event("SYSTEM", "Data safely checkpointed to disk.")
    }, error = function(e) { .log_event("WARNING", "Autosave checkpoint failed.") })
    
    .update_workflow_state("step7")
    shinyjs::enable("run_phytoclass_btn")
  })
  
  output$phytoclass_batch_summary_table <- DT::renderDT({ 
    shiny::req(rv$analyzed_datasets)
    ds_with_log <- purrr::keep(rv$analyzed_datasets, ~!base::is.null(.x$log_analyzer))
    if (base::length(ds_with_log) == 0) return(DT::datatable(tibble::tibble(Status="No analysis results available.")))
    summary_df <- purrr::map_df(ds_with_log, ~tibble::tibble(Dataset = .x$name, Status = .x$log_analyzer$status %||% "N/A", `Fm Used` = .x$log_analyzer$fm_matrix_used %||% "N/A", `Seed Used` = .x$log_analyzer$seed_used %||% "N/A", `Mean RMSE` = base::round(.x$log_analyzer$mean_rmse %||% NA, 4), `Mean Cond Num` = base::round(.x$log_analyzer$mean_condnum %||% NA, 2), `Details` = .x$log_analyzer$error_details$message %||% ""))
    DT::datatable(summary_df, rownames=FALSE, options = base::list(scrollX = TRUE)) |> DT::formatStyle("Status", backgroundColor = DT::styleEqual(base::c("Success", "Failed"), base::c("#d1e7dd", "#fff3cd"))) |> DT::formatStyle("Details", color = "#dc3545", fontSize = "0.9em")
  })
  
  # =========================================================================
  # --- DIRECT EXPLICIT HELP MANUAL (ROBUST SANITIZED FOR SWITCH PARSING) ---
  # =========================================================================
  shiny::observeEvent(input$help_btn_global, {
    raw_tab <- input$main_navbar
    current_tab <- if (base::is.null(raw_tab) || base::length(raw_tab) == 0) "step1" else base::as.character(raw_tab)
    
    btn_prim <- function(label, ico) { shiny::span(style = "background-color: #0d6efd; color: #fff; padding: 4px 8px; border-radius: 4px; font-size: 0.9em; margin: 0 4px; display: inline-block;", shiny::icon(ico), base::paste0(" ", label)) }
    btn_succ <- function(label, ico) { shiny::span(style = "background-color: #198754; color: #fff; padding: 4px 8px; border-radius: 4px; font-size: 0.9em; margin: 0 4px; display: inline-block;", shiny::icon(ico), base::paste0(" ", label)) }
    btn_dang <- function(label, ico) { shiny::span(style = "border: 1px solid #dc3545; color: #dc3545; background-color: transparent; padding: 4px 8px; border-radius: 4px; font-size: 0.9em; margin: 0 4px; display: inline-block;", shiny::icon(ico), base::paste0(" ", label)) }
    ui_tog <- function(label) { shiny::span(style = "background-color: #f8f9fa; border: 1px solid #ced4da; border-radius: 4px; padding: 2px 6px; font-family: monospace; font-size: 0.9em; margin: 0 4px; display: inline-block;", shiny::icon("check-square"), base::paste0(" ", label)) }
    ui_in  <- function(label) { shiny::span(style = "background-color: #f8f9fa; border: 1px solid #ced4da; border-radius: 4px; padding: 2px 6px; font-family: monospace; font-size: 0.9em; margin: 0 4px; display: inline-block;", shiny::icon("keyboard"), base::paste0(" ", label)) }
    
    help_styles <- shiny::tags$style(shiny::HTML(".help-section-title { border-bottom: 2px solid #e2e8f0; color: #0d6efd; margin-top: 15px; margin-bottom: 10px; padding-bottom: 5px; font-weight: bold; } code { font-family: monospace; color: #d63384; } li { margin-bottom: 10px; line-height: 1.5; }"))
    
    content_step1 <- shiny::tagList(
      shiny::h4("Step 1: Setup Parameters"), 
      shiny::p("Configure file paths, algorithm settings, and data cleaning rules."), 
      shiny::div(class="help-section-title", "1. Saved Sessions & Matrix Files"), 
      shiny::tags$ul(
        shiny::tags$li(btn_prim("Load Last Session", "folder-open"), ": Restores settings and inputs from your previous session."), 
        shiny::tags$li(ui_in("Fm_Pro / Fm_NoPro Path"), ": Specify the file paths for the reference matrices required for calculation."),
        shiny::tags$li(btn_prim("Check Matrix Files", "check-double"), ": Verifies that the application can read your reference files.")
      ), 
      shiny::div(class="help-section-title", "2. Algorithm Settings"), 
      shiny::tags$ul(
        shiny::tags$li(ui_in("Iterations"), " & ", ui_in("Cooling Step Size"), ": Controls the optimization algorithm. Higher iterations and smaller steps increase accuracy but take longer. Leave as default unless required."),
        shiny::tags$li(ui_in("Use Fixed Random Seed"), ": Forces the algorithm to take the exact same randomized mathematical route every run to guarantee 1-to-1 strict decimal reproducibility.")
      ), 
      shiny::div(class="help-section-title", "3. Cleaning Rules & Filters"), 
      shiny::tags$ul(
        shiny::tags$li(ui_tog("Cleaning Options"), ": Automatically fixes formatting issues, such as converting blank cells to 0 and dropping empty rows."), 
        shiny::tags$li(ui_tog("Data Filters"), ": Removes samples that fall outside your specified location, date, or depth parameters.")
      )
    )
    
    content_step2 <- shiny::tagList(
      shiny::h4("Step 2: Import Data"),
      shiny::p("Upload raw HPLC pigment data for analysis."),
      shiny::tags$ul(
        shiny::tags$li(ui_in("Select Files"), ": Choose one or multiple .xlsx files from your computer."),
        shiny::tags$li(btn_prim("Load Data Files", "play"), ": Imports the selected files. Invalid or empty files will be flagged and skipped.")
      )
    )
    
    content_step3 <- shiny::tagList(
      shiny::h4("Step 3: Map Variables"),
      shiny::p("Match your uploaded column names to the standard pigment names required by the algorithm."),
      shiny::tags$ul(
        shiny::tags$li(shiny::span(class="badge bg-warning text-dark", "NEEDS MAPPING"), ": Click rows with this warning to assign column headers."),
        shiny::tags$li(shiny::strong("Mandatory:"), " You must map 'Tchla' (Total Chlorophyll a). Leave missing pigments unmapped; the application will treat them as 0."),
        shiny::tags$li(btn_succ("Save Mappings", "check-double"), ": Saves your column assignments and unlocks Step 4.")
      )
    )
    
    content_step4 <- shiny::tagList(
      shiny::h4("Step 4: Filter & Clean"),
      shiny::p("Automatically process your data using the rules configured in Step 1."),
      shiny::tags$ul(
        shiny::tags$li(btn_prim("Clean My Data", "shield-alt"), ": Removes duplicates, fixes blank values, and applies depth/date/location filters."),
        shiny::tags$li(shiny::strong("File Breakdown:"), " Displays exactly how many samples were removed per file and the reason for removal.")
      )
    )
    
    content_step5 <- shiny::tagList(
      shiny::h4("Step 5: Group Samples"),
      shiny::p("Define how samples are grouped before processing."),
      shiny::tags$ul(
        shiny::tags$li(ui_tog("By Source File"), ": Analyzes each uploaded file separately."),
        shiny::tags$li(ui_tog("By Pigment Cluster"), ": Combines all files and groups samples strictly by pigment similarity."),
        shiny::tags$li(btn_prim("Preview Groups", "play"), ": Displays how the data will be divided, generating cluster graphs if applicable."),
        shiny::tags$li(btn_succ("Lock in Strategy", "check-double"), ": Saves your grouping choice and unlocks Step 6.")
      )
    )
    
    content_step6 <- shiny::tagList(
      shiny::h4("Step 6: Run Analysis"),
      shiny::p("Execute the mathematical optimization to calculate phytoplankton communities."),
      shiny::tags$ul(
        shiny::tags$li(ui_in("Select Data to Run"), ": Choose which specific groups to process."),
        shiny::tags$li(btn_prim("Start Analysis", "rocket"), ": Begins the calculation. A live dashboard tracks progress and estimated completion time."),
      )
    )
    
    content_step7 <- shiny::tagList(
      shiny::h4("Step 7: View & Export"),
      shiny::p("Review performance metrics and export the final calculated data."),
      shiny::tags$ul(
        shiny::tags$li(shiny::strong("RMSE / Condition Number:"), " Diagnostic scores for the mathematical calculation. Lower numbers indicate better mathematical confidence."),
        shiny::tags$li(shiny::strong("View Graphs:"), " Select a group from the dropdown to see its specific community composition charts."),
        shiny::tags$li(btn_succ("Save Results to Computer", "download"), ": Creates a directory on your computer containing the final Excel data, graphs, and run settings.")
      )
    )
    
    selected_content <- base::switch(current_tab, 
                                     "step1" = content_step1, 
                                     "step2" = content_step2, 
                                     "step3" = content_step3, 
                                     "step4" = content_step4, 
                                     "step5" = content_step5, 
                                     "step6" = content_step6, 
                                     "step7" = content_step7,
                                     content_step1)
    
    shiny::showModal(shiny::modalDialog(
      title = shiny::div(shiny::icon("book-open"), base::paste(" Guidelines - Tab Details")), 
      size = "l", 
      easyClose = TRUE, 
      help_styles, 
      selected_content, 
      footer = shiny::modalButton("Close")
    ))
  })
}

shiny::shinyApp(ui = ui, server = server)