# ============================================================================
# 0. LAUNCH VERIFICATION
# ============================================================================
if (base::Sys.getenv("PHYTOCLASSSHINY_SANDBOX_ACTIVE") != "TRUE") {
  base::stop(
    "\n====================================================================\n",
    "[!] CRITICAL LAUNCH ERROR: Sandbox violation detected.\n",
    "====================================================================\n",
    "Please launch using 'LAUNCH_PHYTOCLASSSHINY.bat' (Windows), \n",
    "'LAUNCH_PHYTOCLASSSHINY.command' (Mac), 'LAUNCH_PHYTOCLASSSHINY.sh' \n",
    "(Linux), or open the 'phytoclassShiny.Rproj' project file before \n",
    "running app.R.\n",
    "====================================================================\n"
  )
}

# ============================================================================
#
#   phytoclassShiny - MAIN APPLICATION LAUNCHER
#
#   Wires together the seven-step workflow, the Setup/Staging/Analysis
#   navigation grouping, and the global controls (Save Config, Download
#   Report, Help, System Console).
#
#   The Download Report control is available from any step, as soon as
#   that step's own data exists, not only once the user reaches Step 7.
#   See R/core/report_builder.R for the full design rationale. rv$step_status
#   and rv$report_data hold the session state this requires; they are
#   updated at every point reset_downstream_data()/.update_workflow_state()
#   already fire, using the same trigger points, so the tab-lock mechanism
#   and the report system always describe the same reality.
#
# ============================================================================

# --- PREAMBLE ---
base::cat("--- Initializing phytoclassShiny ---\n")
base::options(shiny.maxRequestSize = 500 * 1024^2)

# --- 1. LIBRARY PATH RESOLUTION ---
app_lib <- base::file.path(base::getwd(), "system", "app_packages")

if (base::dir.exists(app_lib)) {
  base::cat("--> Sandbox located. Restricting environment variables...\n")
  base::.libPaths(app_lib)
} else {
  base::cat("--> Sandbox not found. Defaulting to system library.\n")
}

# --- 2. NAMESPACE CLEARANCE ---
conflict_prone_packages <- base::c("shinybusy", "shinyWidgets", "shinyjs", "DT", "bslib")
for (ns in conflict_prone_packages) {
  if (ns %in% base::loadedNamespaces()) {
    base::tryCatch({ base::detach(base::paste0("package:", ns), unload = TRUE, character.only = TRUE) }, error = function(e) NULL)
    base::tryCatch({ base::unloadNamespace(ns) }, error = function(e) NULL)
  }
}

# --- 3. DEPENDENCY LOADING ---
required_packages <- base::c(
  "shiny", "bslib", "shinyjs", "shinyWidgets", "DT", "htmlwidgets", "jsonlite",
  "yaml", "dplyr", "tidyr", "readxl", "openxlsx", "lubridate", "digest", "rlang",
  "tibble", "fs", "purrr", "scales", "vegan", "cluster", "factoextra",
  "ggplot2", "MASS", "dynamicTreeCut", "glue", "stringdist", "phytoclass",
  "shinybusy", "tidyselect", "zip"
)

for (pkg in required_packages) {
  if (!base::requireNamespace(pkg, quietly = TRUE)) {
    base::cat(base::sprintf("    ...Installing missing package to sandbox: %s\n", pkg))
    base::tryCatch({
      utils::install.packages(pkg, lib = app_lib, dependencies = TRUE)
    }, error = function(e) {
      base::stop(base::paste("Failed to install", pkg, ":", e$message))
    })
  }
}

core_ui_packages <- base::c("shiny", "bslib", "shinyjs", "shinyWidgets", "DT")
base::invisible(base::lapply(core_ui_packages, base::library, character.only = TRUE))

`%||%` <- rlang::`%||%`

base::cat("--> Sourcing modules...\n")
base::tryCatch({
  scripts_to_source <- base::list.files("R", pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
  for (script in scripts_to_source) { base::source(script) }
}, error = function(e) { base::stop("FATAL ERROR: Failed to source R modules. Error: ", e$message) })

# ============================================================================
# --- USER INTERFACE (UI) ---
# ============================================================================

jscode <- "
$(document).on('click', '.btn', function() {
  var id = $(this).attr('id');
  if (id) { Shiny.setInputValue('last_btn_clicked', id, {priority: 'event'}); }
});
"

ui <- bslib::page_navbar(
  theme = bslib::bs_theme(
    version = 5,
    primary = "#0E6E8C",   # marine teal, replaces Bootstrap's stock blue
    success = "#3F7D4F",   # chlorophyll green, replaces Bootstrap's stock green
    warning = "#D98C3D",   # warm amber
    danger  = "#B23A48",   # muted brick red, less alarm-clock than Bootstrap's default
    info    = "#4A8FA6"    # lighter teal, for informational alerts
  ),
  title = shiny::HTML("<strong style='color: #0E6E8C'><i>phytoclass</i>Shiny</strong>"),
  id = "main_navbar",
  # fillable = FALSE here, not on the individual nav_panel() calls: bslib
  # has never supported a per-nav_panel() `fillable` argument (checked
  # against bslib's own source - nav_panel()'s signature is only
  # (title, ..., value, icon), full stop). fillable only exists on
  # page_navbar()/navset_bar() itself, either TRUE/FALSE globally or a
  # character vector of specific panel `value`s to keep fillable. Setting
  # it here, once, is what actually stops content from being squeezed into
  # viewport height with nested internal scrollbars - not per-panel flags
  # that would otherwise just get silently absorbed as meaningless HTML
  # attributes.
  fillable = FALSE,

  header = shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::tags$head(shiny::tags$script(shiny::HTML(jscode))),
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
        /* --- TYPOGRAPHY ---
           Deliberately system-font-only, not a Google/webfont: this app's
           whole design is built around running fully offline after first
           setup, and a font loaded from a CDN would either quietly break
           that guarantee or add a network dependency the rest of the app
           goes out of its way to avoid. Identity here comes from weight,
           spacing, and colour instead of a different typeface. */
        body, p, .btn, label, input, select, .alert { font-family: system-ui, -apple-system, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif !important; }
        h1, h2, h3, h4, h5, h6, .nav-link { font-family: system-ui, -apple-system, 'Segoe UI', Roboto, sans-serif !important; letter-spacing: 0.01em; }
        h4 { color: #0E6E8C; font-weight: 650; border-bottom: 2px solid #dee2e6; padding-bottom: 5px; margin-top: 0; margin-bottom: 15px; }

        /* --- CARD-CONSISTENT CONTAINERS ---
           Every panel now uses bslib::card(); .well is kept only as a
           harmless fallback in case any third-party widget still emits
           one, not as an intentionally-used component. */
        .card { border: 1px solid #dee2e6; border-radius: 10px; margin-bottom: 20px; box-shadow: 0 1px 2px rgba(15, 40, 50, 0.04); }
        .card-header { background-color: #F4F9FA; border-bottom: 1px solid #dee2e6; font-weight: 650; color: #0E6E8C; }
        .well { background-color: #F4F9FA; border: 1px solid #dee2e6; padding: 20px; border-radius: 10px; margin-bottom: 20px; }

        /* --- SYSTEM CONSOLE: re-scoped to the app's own palette rather
           than a green-on-black terminal that belonged to a different
           interface entirely. Monospace kept, since that's about
           scanability of the log, not aesthetics. --- */
        #session_audit_log { font-family: 'SFMono-Regular', Consolas, 'Liberation Mono', Menlo, monospace !important; font-size: 0.8em; white-space: pre-wrap; word-wrap: break-word; background-color: #0E2A33; color: #8FD9C4; padding: 12px; border-radius: 8px; height: 100%; overflow-y: auto; }

        /* --- EPISTEMIC-HUMILITY NOTES: the small, consistently-styled
           callouts placed where the app makes a judgement call on the
           user's behalf (Step 1 matrix selection, Step 3 mapping), so
           the paper's own caution about automation easing the method
           burden without replacing taxonomic judgement has an actual
           presence in the tool it's about. --- */
        .judgement-note { background-color: #FBF3E7; border-left: 3px solid #D98C3D; border-radius: 6px; padding: 8px 12px; font-size: 0.85em; color: #6b4a1f; margin-top: 10px; margin-bottom: 4px; }

        .progress-bar-spec { transition: width 0.4s cubic-bezier(0.4, 0, 0.2, 1); }
        table.dataTable { width: 100% !important; margin: 0 auto; clear: both; border-collapse: collapse; }
        .dataTables_wrapper { width: 100% !important; overflow-x: auto; }
        .checkbox, .checkbox label { width: 100% !important; display: block; }
        .navbar-nav .nav-link.active { border-bottom: 3px solid #0E6E8C !important; border-bottom-left-radius: 0 !important; border-bottom-right-radius: 0 !important; color: #0E6E8C !important; font-weight: bold; }
      "))
    )
  ),

  sidebar = bslib::sidebar(
    id = "audit_log_sidebar", title = shiny::span(title = "A live, timestamped log of everything done this session. Optional, useful for troubleshooting.", shiny::icon("terminal"), " System Console"),
    position = "right", open = "closed", width = 400,
    shiny::verbatimTextOutput("session_audit_log")
  ),

  bslib::nav_panel("Setup", value="step1", icon = shiny::icon("sliders-h"),
                   shiny::h3("Step 1: Setup Parameters"),
                   shiny::p("Set up your folders, run settings, and data filters below."),
                   shiny::hr(),
                   bslib::layout_columns(
                     col_widths = base::c(4, 8),
                     shiny::tagList(
                       bslib::card(
                         bslib::card_header("1. Saved Sessions"),
                         bslib::card_body(
                           shiny::actionButton("load_session_config_btn", "Load Last Session", icon = shiny::icon("folder-open"), width = "100%", class = "btn-primary mb-2"),
                           shiny::hr(class = "my-3"),
                           shiny::actionLink("toggle_reset_link", shiny::tagList(shiny::icon("chevron-down"), " Show reset options...")),
                           shinyjs::hidden(
                             shiny::div(id = "reset_div", class = "mt-3",
                                        shiny::actionButton("reset_to_default_btn", "Reset to Default Settings", icon = shiny::icon("redo"), class = "btn-outline-danger w-100")
                             )
                           )
                         )
                       ),
                       bslib::card(
                         bslib::card_header("2. Reference Tables"),
                         bslib::card_body(
                           shinyjs::disabled(shiny::textInput("output_dir_ui", "Output Directory (from config):", width = "100%")),
                           shiny::textInput("fm_pro_path_ui", "Fm_Pro.xlsx Path:", value = "R/reference tables/Fm_Pro.xlsx", width="100%"),
                           shiny::textInput("fm_nopro_path_ui", "Fm_NoPro.xlsx Path:", value = "R/reference tables/Fm_NoPro.xlsx", width="100%"),
                           shiny::hr(),
                           shiny::actionButton("load_fm_btn", "Check Matrix Files", icon = shiny::icon("check-double"), width="100%", class="btn-outline-primary"),
                           shiny::div(class = "judgement-note", shiny::icon("compass"), " ", shiny::strong("phytoclass"), "Shiny checks that these files are readable; whether the groups and pigments in them are the right ones for your study system is still your call.")
                         )
                       )
                     ),
                     shiny::tagList(
                       bslib::accordion(
                         id = "config_accordion_stack", open = FALSE,
                         bslib::accordion_panel("Engine Settings", icon = shiny::icon("sliders-h"),
                                                shiny::fluidRow(
                                                  shiny::column(6, shiny::numericInput("niter_input", "Iterations (Niter):", value = 500, min = 10, step = 10)),
                                                  shiny::column(6, shiny::numericInput("step_size_input", "Cooling Step Size:", value = 0.009, min = 0.0001, step = 0.001))
                                                ),
                                                shiny::hr(),
                                                shiny::fluidRow(
                                                  shiny::column(6, shiny::checkboxInput("toggle_fixed_seed", "Use Fixed Random Seed", value = FALSE)),
                                                  shiny::column(6, shiny::conditionalPanel(condition = "input.toggle_fixed_seed == true", shiny::numericInput("fixed_seed_input", "Seed Value:", value = 131234, step = 1)))
                                                ),
                                                shiny::hr(),
                                                shiny::fluidRow(
                                                  shiny::column(6, shiny::checkboxInput("toggle_custom_minmax", "Use Custom Min/Max Bounds", value = FALSE)),
                                                  shiny::column(6, shiny::conditionalPanel(condition = "input.toggle_custom_minmax == true", shiny::selectInput("minmax_file_selector", "Select MinMax Profile:", choices = NULL, width = "100%")))
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

  bslib::nav_menu("Staging", icon = shiny::icon("layer-group"),
  bslib::nav_panel("2. Import Data", value="step2", icon = shiny::icon("file-import"),
                   shiny::h3("Step 2: Import Data"), shiny::p("Select one or more `.xlsx` files containing pigment data."), shiny::hr(),
                   bslib::layout_columns(
                     col_widths = base::c(4, 8),
                     bslib::card(bslib::card_body(
                       shiny::fileInput("hplc_data_files_input", "Select Files:", multiple = TRUE, accept = base::c(".xlsx"), width="100%"),
                       shiny::actionButton("load_data_btn", "Load Data Files", icon = shiny::icon("play"), class = "btn-primary w-100 fw-bold")
                     )),
                     bslib::card(bslib::card_header("Uploaded Files Summary"), bslib::card_body(DT::DTOutput("batch_file_load_status_table")))
                   )),

  bslib::nav_panel("3. Map Variables", value="step3", icon = shiny::icon("exchange-alt"), validationUI("step3_validation")),
  bslib::nav_panel("4. Filter & Clean", value="step4", icon = shiny::icon("filter"), qcUI("step4_qc")),
  bslib::nav_panel("5. Group Samples", value="step5", icon = shiny::icon("object-group"), strategyUI("step5_strategy")),
  ),

  bslib::nav_menu("Analysis", icon = shiny::icon("flask"),
  bslib::nav_panel("6. Run Analysis", value="step6", icon = shiny::icon("play-circle"),
                   shiny::h3("Step 6: Run Analysis"), shiny::p("Select your data and start processing."), shiny::hr(),
                   shiny::sidebarLayout(
                     shiny::sidebarPanel(width = 4,
                                         shiny::h4("1. Select Data to Run"),
                                         shinyWidgets::pickerInput(inputId = "datasets_for_phytoclass_run", label = NULL, choices = NULL, multiple = TRUE, options = shinyWidgets::pickerOptions(actionsBox = TRUE, selectAllText="Select All", deselectAllText="Deselect All")),
                                         shiny::hr(),
                                         shiny::h4("2. Run Analysis"),
                                         shiny::actionButton("run_phytoclass_btn", "Start Analysis", class = "btn-primary btn-lg w-100 fw-bold", icon = shiny::icon("rocket")),
                                         shiny::hr(),
                                         bslib::card(bslib::card_header(shiny::icon("tasks"), " Current Run Summary"), bslib::card_body(shiny::uiOutput("analysis_params_review_ui")))),
                     shiny::mainPanel(width = 8,
                                      shinyjs::hidden(
                                        shiny::div(id = "live_tracker_card",
                                                   bslib::card(style = "border-left: 4px solid #4A8FA6;",
                                                                    bslib::card_header(style = "display: flex; align-items: center; justify-content: space-between;", shiny::span(shiny::icon("stopwatch"), " Live Progress Dashboard"), shiny::div(id = "tracker_spinner_container", style="animation: spin 1s linear infinite; display: inline-block;", shiny::icon("sync-alt", class="text-info fa-spin"))),
                                                                    bslib::card_body(
                                                                    shiny::fluidRow(
                                                                      shiny::column(7, shiny::p(shiny::strong("Current Task: "), shiny::span(id = "trk_task", class="text-primary")), shiny::p(shiny::strong("Batch Progress: "), shiny::span(id = "trk_prog"))),
                                                                      shiny::column(5, shiny::p(shiny::strong(shiny::span(id="lbl_elapsed", "Time Elapsed: ")), shiny::span(id = "trk_elapsed", style = "font-family: monospace; font-weight:700; color: #3F7D4F;")), shiny::p(shiny::strong(shiny::span(id="lbl_eta", "Est. Remaining: ")), shiny::span(id = "trk_eta", style = "font-family: monospace; font-weight:700; color: #0E6E8C;")))
                                                                    ),
                                                                    shiny::div(style = "margin-top: 15px; background: #e9ecef; border-radius: 4px; height: 16px; width: 100%; position: relative; overflow: hidden;", shiny::div(id = "tracker_progress_bar", class="progress-bar-spec", style = "background: #4A8FA6; height: 100%; width: 0%;"))
                                                                    )
                                                   )
                                        )
                                      ),
                                      bslib::card(bslib::card_header("Analysis Results Logs"), bslib::card_body(DT::DTOutput("phytoclass_batch_summary_table")))
                     )
                   )
  ),

  bslib::nav_panel("7. View Results", value="step7", icon = shiny::icon("chart-line"), reportingUI("step7_reporting")),
  ),

  bslib::nav_spacer(),
  bslib::nav_item(shiny::div(class = "d-flex align-items-center", style = "margin-right: 10px;",
    shiny::uiOutput("last_saved_status_ui", inline = TRUE)
  )),
  bslib::nav_item(shiny::tags$span(title = "Saves your current settings for next time. For results, use Download Report.",
    shiny::actionButton("save_config_btn_global", "Save Config", icon = shiny::icon("save"), class = "btn-success btn-sm", style="margin-right: 5px;")
  )),
  bslib::nav_item(
    shiny::div(class = "dropdown d-inline-block", style = "margin-right: 5px;",
               shiny::tags$button(class = "btn btn-outline-primary btn-sm dropdown-toggle", type = "button", id = "download_report_dropdown_btn", `data-bs-toggle` = "dropdown", `aria-expanded` = "false", title = "Downloads whatever you've completed so far, from any step, even if later steps haven't run or have failed.", shiny::icon("file-arrow-down"), " Download Report"),
               shiny::tags$ul(class = "dropdown-menu dropdown-menu-end p-3", style = "min-width: 340px;", `aria-labelledby` = "download_report_dropdown_btn",
                              shiny::uiOutput("report_toggle_ui"),
                              shiny::hr(style="margin: 10px 0;"),
                              shiny::downloadButton("download_report_btn", "Download Selected Sections", class = "btn-primary btn-sm w-100")
               )
    )
  ),
  bslib::nav_item(shiny::actionButton("help_btn_global", "Help", icon = shiny::icon("question-circle"), class = "btn-info btn-sm", style="margin-right: 5px;")),
  bslib::nav_item(shiny::actionButton("quit_app_btn", "Exit", icon = shiny::icon("power-off"), class = "btn-danger btn-sm", style="margin-right: 15px;"))
)

# ============================================================================
# --- SERVER LOGIC ---
# ============================================================================

server <- function(input, output, session) {

  app_is_stopping <- FALSE

  .force_console_return <- function() {
    if (!app_is_stopping) {
      app_is_stopping <<- TRUE

      base::cat("\n\n=======================================================\n")
      base::cat("[OK] PHYTOCLASSSHINY SHUTDOWN COMPLETE.\n")
      base::cat("=======================================================\n")

      base::tryCatch({ shiny::stopApp() }, error = function(e) NULL)

      later::later(function() {
        if (base::requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
          base::tryCatch({ rstudioapi::executeCommand("interruptR") }, error = function(e) NULL)
        }
        base::tryCatch({ base::invokeRestart("abort") }, error = function(e) NULL)
      }, delay = 1)
    }
  }

  shiny::observeEvent(input$quit_app_btn, {
    shinyjs::runjs("setTimeout(function(){ window.close(); }, 200);")
    .force_console_return()
  })

  session$onSessionEnded(function() {
    .force_console_return()
  })

  rv <- shiny::reactiveValues(
    session_id = base::paste0("Run_", base::format(base::Sys.time(), "%Y%m%d_%H%M%S")),
    config = NULL, datasets_processed = base::list(), master_qc_data = NULL,
    analysis_datasets = base::list(), analyzed_datasets = base::list(),
    cluster_diagnostics = NULL, qc_summary_df = NULL,
    performance_metrics = NULL, session_log = base::character(0), staging_datasets = base::list(),
    mapping_trigger = 0, current_mapping_dataset = NULL, mapping_history = base::list(),
    resolution_warnings = base::list(),
    # Progressive report state, see report_builder.R
    step_status = initialize_report_state()$status,
    report_data = base::list()
  )

  step1_inputs <- base::c("output_dir_ui", "fm_pro_path_ui", "fm_nopro_path_ui", "toggle_custom_minmax", "minmax_file_selector", "toggle_handle_duplicates", "toggle_handle_nas", "toggle_handle_negatives", "toggle_handle_zerosum", "niter_input", "step_size_input", "toggle_fixed_seed", "fixed_seed_input", "toggle_geo_filter", "toggle_temporal_filter", "toggle_depth_filter", "min_lat_ui", "max_lat_ui", "min_lon_ui", "max_lon_ui", "min_depth_ui", "max_depth_ui", "start_date_ui", "end_date_ui")
  step5_inputs <- base::c("step5_strategy-normalization_method_input", "step5_strategy-transformation_method_input", "step5_strategy-cluster_method_input", "step5_strategy-k_max_input", "step5_strategy-k_determination_mode")

  .log_event <- function(category = "SYSTEM", message) {
    if (base::missing(message)) { message <- category; category <- "INFO" }
    timestamp <- base::format(base::Sys.time(), "%H:%M:%S")
    rv$session_log <- base::c(rv$session_log, base::sprintf("[%s] [%s] %s", timestamp, category, message))
  }

  .refresh_step1_report_data <- function() {
    cfg <- rv$config
    if (base::is.null(cfg)) return(NULL)
    base::data.frame(
      Setting = base::c("Iterations (Niter)", "Cooling Step Size", "Fixed Seed", "RMSE Review Threshold", "Fm_Pro Path", "Fm_NoPro Path"),
      Value = base::c(base::as.character(cfg$phytoclass$niter %||% 500), base::as.character(cfg$phytoclass$step_size %||% 0.009),
                       if (base::isTRUE(cfg$phytoclass$use_fixed_seed)) base::as.character(cfg$phytoclass$fixed_seed) else "Unconstrained",
                       base::as.character(cfg$phytoclass$rmse_review_threshold %||% 0.1),
                       base::as.character(cfg$workspace$fm_pro_matrix_path), base::as.character(cfg$workspace$fm_nopro_matrix_path)),
      stringsAsFactors = FALSE
    )
  }

  shiny::observeEvent(input$main_navbar, { .log_event("NAV", base::paste("Switched tab to:", input$main_navbar)) }, ignoreInit = TRUE)
  shiny::observeEvent(input$last_btn_clicked, { .log_event("USER", base::paste("Clicked Button:", input$last_btn_clicked)) })

  shiny::observe({
    minmax_files <- base::list.files(path = "R/reference tables", pattern = "^MinMax_.*\\.xlsx$", full.names = FALSE)
    if (base::length(minmax_files) > 0) {
      shiny::updateSelectInput(session, "minmax_file_selector", choices = minmax_files)
    } else {
      shiny::updateSelectInput(session, "minmax_file_selector", choices = "No MinMax files found in directory.")
    }
  })

  base::lapply(step1_inputs, function(id) {
    shiny::observeEvent(input[[id]], {
      val <- input[[id]]; if(base::length(val) > 1) val <- base::paste0("(", base::length(val), " items)")
      .log_event("INPUT", base::paste0(id, " updated to: ", val))
      if (id %in% base::c("fm_pro_path_ui", "fm_nopro_path_ui")) {
        rv$fm_matrices <- NULL
      }

      if (!base::is.null(rv$master_qc_data) || base::length(rv$datasets_processed) > 0) {
        .log_event("RESET", base::sprintf("Step 1 Settings modified. Wiping downstream: %s", id))
        reset_downstream_data("qc")
        rv$step_status <- invalidate_from_step(rv$step_status, "step2")
        .update_workflow_state("step2")
        shiny::showNotification("Settings changed. Downstream metrics cleared to maintain consistency.", type = "warning", duration = 6)
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
        rv$step_status <- invalidate_from_step(rv$step_status, "step6")
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
      rv$staging_datasets <- base::list(); rv$mapping_history <- base::list(); rv$mapping_trigger <- rv$mapping_trigger + 1; rv$resolution_warnings <- base::list()
    }
    if (level %in% base::c("all", "config", "qc")) {
      rv$master_qc_data <- NULL; rv$analysis_datasets <- base::list(); rv$analyzed_datasets <- base::list(); rv$qc_summary_df <- NULL; rv$cluster_diagnostics <- NULL
    }
    if (level == "strategy") { rv$analysis_datasets <- base::list(); rv$analyzed_datasets <- base::list(); rv$cluster_diagnostics <- NULL }
    if (level == "analysis") { rv$analyzed_datasets <- base::list() }
  }

  # --- PROGRESSIVE DOWNLOAD REPORT ---
  output$report_toggle_ui <- shiny::renderUI({
    render_report_toggle_ui(base::identity, rv$step_status)
  })

  output$download_report_btn <- shiny::downloadHandler(
    filename = function() base::sprintf("phytoclassShiny_Report_%s.zip", base::format(base::Sys.time(), "%Y%m%d_%H%M%S")),
    content = function(file) {
      status_df <- report_section_status(rv$step_status)
      selected_sections <- base::character(0)
      for (sk in status_df$section_key) {
        if (base::isTRUE(input[[base::paste0("report_include_", sk)]])) selected_sections <- base::c(selected_sections, sk)
      }
      compile_progressive_report(rv$report_data, rv$step_status, selected_sections, rv$config, file)
      .log_event("SYSTEM", base::sprintf("Progress report downloaded (%s).", base::paste(selected_sections, collapse=", ")))
    }
  )

  shiny::observeEvent(once = TRUE, eventExpr = session, {
    .update_workflow_state("step1"); .log_event("SYSTEM", "App session started successfully.")
    base::tryCatch({
      rv$config <- initialize_config()
      update_all_ui_from_config(rv$config, session)
      if (!base::is.null(rv$config$phytoclass$use_fixed_seed)) shiny::updateCheckboxInput(session, "toggle_fixed_seed", value = as.logical(rv$config$phytoclass$use_fixed_seed))
      if (!base::is.null(rv$config$phytoclass$fixed_seed)) shiny::updateNumericInput(session, "fixed_seed_input", value = as.numeric(rv$config$phytoclass$fixed_seed))

      fm_result <- load_fm_matrices(rv$config)
      if (!base::is.null(fm_result$error)) .log_event("WARNING", fm_result$error) else rv$fm_matrices <- fm_result
      if (!base::is.null(rv$config)) {
        .update_workflow_state("step2")
        rv$step_status <- mark_step_available(rv$step_status, "step1")
        rv$report_data$step1 <- .refresh_step1_report_data()
      }
    }, error = function(e) { .log_event("FATAL", e$message) })
  })

  shiny::observeEvent(input$load_session_config_btn, {
    old_config <- rv$config
    base::tryCatch({

      temp_config <- load_config(CONFIG_SESSION_PATH)

      if (base::is.null(temp_config)) {
        temp_config <- load_config(CONFIG_TEMPLATE_PATH)
        if (base::is.null(temp_config)) base::stop("Master config_template.yaml file is missing from system directory.")

        rv$config <- temp_config
        update_all_ui_from_config(rv$config, session)
        reset_downstream_data("config")
        rv$step_status <- invalidate_from_step(rv$step_status, "step2")
        .update_workflow_state("step2")

        if (!base::is.null(rv$config$phytoclass$use_fixed_seed)) shiny::updateCheckboxInput(session, "toggle_fixed_seed", value = as.logical(rv$config$phytoclass$use_fixed_seed))
        if (!base::is.null(rv$config$phytoclass$fixed_seed)) shiny::updateNumericInput(session, "fixed_seed_input", value = as.numeric(rv$config$phytoclass$fixed_seed))

        fm_result <- load_fm_matrices(rv$config)
        if (base::is.null(fm_result$error)) rv$fm_matrices <- fm_result
        rv$step_status <- mark_step_available(rv$step_status, "step1")
        rv$report_data$step1 <- .refresh_step1_report_data()

        shiny::showModal(shiny::modalDialog(
          title = shiny::div(shiny::icon("info-circle", class="text-info"), " Default Template Loaded"),
          shiny::p("No previous custom session history was detected on this computer."),
          shiny::p("The application has successfully loaded the standard default template configuration parameters."),
          shiny::p(shiny::strong("Note:"), " Once you adjust your settings and click the green 'Save Config' button, your personalized parameters will overwrite the workspace and load here automatically in the future."),
          easyClose = TRUE,
          footer = shiny::modalButton("Dismiss")
        ))
        return()
      }

      rv$config <- temp_config
      update_all_ui_from_config(rv$config, session)
      reset_downstream_data("config")
      rv$step_status <- invalidate_from_step(rv$step_status, "step2")
      .update_workflow_state("step2")

      if (!base::is.null(rv$config$phytoclass$use_fixed_seed)) shiny::updateCheckboxInput(session, "toggle_fixed_seed", value = as.logical(rv$config$phytoclass$use_fixed_seed))
      if (!base::is.null(rv$config$phytoclass$fixed_seed)) shiny::updateNumericInput(session, "fixed_seed_input", value = as.numeric(rv$config$phytoclass$fixed_seed))

      fm_result <- load_fm_matrices(rv$config)
      if (base::is.null(fm_result$error)) rv$fm_matrices <- fm_result
      rv$step_status <- mark_step_available(rv$step_status, "step1")
      rv$report_data$step1 <- .refresh_step1_report_data()
      shiny::showNotification("Saved configuration profile reloaded successfully.", type="message")

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
      rv$config <- load_config(CONFIG_TEMPLATE_PATH); update_all_ui_from_config(rv$config, session); reset_downstream_data("config")
      rv$step_status <- invalidate_from_step(rv$step_status, "step2")
      .update_workflow_state("step2")
      shiny::updateCheckboxInput(session, "toggle_fixed_seed", value = FALSE)

      fm_result <- load_fm_matrices(rv$config)
      if (base::is.null(fm_result$error)) rv$fm_matrices <- fm_result
      rv$step_status <- mark_step_available(rv$step_status, "step1")
      rv$report_data$step1 <- .refresh_step1_report_data()
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
    rv$step_status <- mark_step_available(rv$step_status, "step1")
    rv$report_data$step1 <- .refresh_step1_report_data()
    base::tryCatch({
      save_config(rv$config, CONFIG_SESSION_PATH)
      rv$last_config_saved <- base::Sys.time()
      shiny::showNotification("Configuration saved successfully.", type = "message")
    }, error = function(e) {})
  })

  output$last_saved_status_ui <- shiny::renderUI({
    if (base::is.null(rv$last_config_saved)) {
      shiny::span(class = "text-muted small", "Not saved this session")
    } else {
      shiny::span(class = "text-muted small", shiny::icon("check-circle", class = "text-success"), " ",
                  base::sprintf("Saved %s", base::format(rv$last_config_saved, "%H:%M:%S")))
    }
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
    rv$step_status <- invalidate_from_step(rv$step_status, "step2")

    ingested_data <- base::tryCatch({
      base::cat("--> Executing raw file parsing...\n")
      load_all_files(input$hplc_data_files_input, rv$config, .log_event)
    }, error = function(e) {
      .log_event("FATAL_INGEST", base::paste("File loader crashed: ", e$message))

      shiny::showModal(shiny::modalDialog(
        title = shiny::span(shiny::icon("exclamation-triangle", class="text-danger"), " File Ingestion Failed"),
        shiny::p("The underlying Excel parser encountered a fatal format error and stopped."),
        shiny::tags$code(style="color: #B23A48; background-color: #f8f9fa; padding: 5px; display: block; margin-top: 10px;", e$message),
        easyClose = TRUE,
        footer = shiny::modalButton("Close")
      ))
      return(base::list())
    })

    rv$datasets_processed <- ingested_data

    if(base::length(rv$datasets_processed) > 0) {
      rv$staging_datasets <- purrr::map(rv$datasets_processed, rlang::duplicate)
      rv$mapping_history <- base::list(); rv$mapping_trigger <- rv$mapping_trigger + 1
      .update_workflow_state("step3")
      rv$step_status <- mark_step_available(rv$step_status, "step2")
      rv$report_data$step2 <- base::data.frame(
        Dataset = base::names(rv$datasets_processed),
        Rows = base::vapply(rv$datasets_processed, function(x) x$log$initial_rows %||% NA, numeric(1)),
        Columns = base::vapply(rv$datasets_processed, function(x) x$log$initial_cols %||% NA, numeric(1)),
        stringsAsFactors = FALSE
      )
    } else {
      if (!base::any(base::grepl("FATAL_INGEST", rv$session_log))) {
        shiny::showModal(shiny::modalDialog(title = "Ingestion Failure", "No usable matrices were extracted from the uploaded batch."))
      }
    }
  })

  output$batch_file_load_status_table <- DT::renderDT({
    shiny::req(base::length(rv$datasets_processed) > 0)

    clean_rows <- base::list()

    for (i in base::seq_along(rv$datasets_processed)) {
      .x <- rv$datasets_processed[[i]]

      fname <- if (!base::is.null(.x$name)) base::as.character(.x$name) else base::paste0("Dataset_", i)
      rows  <- if (!base::is.null(.x$log$initial_rows)) base::as.character(.x$log$initial_rows) else "Unknown"
      cols  <- if (!base::is.null(.x$log$initial_cols)) base::as.character(.x$log$initial_cols) else "Unknown"

      clean_rows[[i]] <- base::data.frame(
        Name = fname,
        Rows = rows,
        Cols = cols,
        stringsAsFactors = FALSE
      )
    }

    summary_df <- base::do.call(base::rbind, clean_rows)

    DT::datatable(
      summary_df,
      options = base::list(pageLength = 10, searching = FALSE, lengthChange = FALSE),
      rownames = FALSE
    )

  }, server = FALSE)

  validationServer("step3_validation", rv, .log_event, .update_workflow_state, session)
  qcServer("step4_qc", rv, .log_event, .update_workflow_state, reset_downstream_data, session)
  strategyServer("step5_strategy", rv, .log_event, .update_workflow_state, session)
  reportingServer("step7_reporting", rv, .log_event)

  output$analysis_params_review <- shiny::renderText({
    shiny::req(rv$config, rv$master_qc_data, rv$analysis_datasets)
    temp_config <- rv$config
    temp_config$phytoclass$use_fixed_seed <- input$toggle_fixed_seed
    temp_config$phytoclass$fixed_seed <- input$fixed_seed_input
    generate_run_summary_text(temp_config, rv$master_qc_data, rv$analysis_datasets, rv$cluster_diagnostics)
  })

  # Structured, card-styled counterpart to the plain-text summary above (kept
  # for anything else that might still want the raw text form). Same
  # underlying values, presented as labeled rows with icons instead of a
  # monospace block, matching the rest of the app's visual style.
  output$analysis_params_review_ui <- shiny::renderUI({
    shiny::req(rv$config, rv$master_qc_data, rv$analysis_datasets)
    temp_config <- rv$config
    temp_config$phytoclass$use_fixed_seed <- input$toggle_fixed_seed
    temp_config$phytoclass$fixed_seed <- input$fixed_seed_input

    qc_rules <- base::c(
      if (base::isTRUE(temp_config$data_cleaning$handle_duplicates$enabled)) "Duplicates",
      if (base::isTRUE(temp_config$data_cleaning$handle_pigment_nas$enabled)) "NAs",
      if (base::isTRUE(temp_config$data_cleaning$enforce_non_negative_pigments$enabled)) "Negatives",
      if (base::isTRUE(temp_config$data_cleaning$handle_zero_pigment_sum$enabled)) "Empty Samples"
    )
    filters <- base::c(
      if (base::isTRUE(temp_config$filtering$geospatial$enabled)) "Location",
      if (base::isTRUE(temp_config$filtering$temporal$enabled)) "Date",
      if (base::isTRUE(temp_config$filtering$depth$enabled)) "Depth"
    )
    method_raw <- temp_config$strategy$method %||% "By Source File"
    seed_text <- if (base::isTRUE(base::as.logical(temp_config$phytoclass$use_fixed_seed))) base::as.character(temp_config$phytoclass$fixed_seed) else "Unconstrained"
    mm_text <- if (base::isTRUE(temp_config$phytoclass$use_custom_minmax)) (temp_config$phytoclass$selected_minmax_file %||% "N/A") else "Phytoclass Internal Default"

    .row <- function(icon_name, label, value) {
      shiny::div(class = "d-flex justify-content-between py-1", style = "border-bottom: 1px solid #f0f2f4;",
                 shiny::span(shiny::icon(icon_name, class = "text-muted me-2"), label),
                 shiny::span(shiny::strong(base::as.character(value))))
    }

    shiny::tagList(
      shiny::h6(class = "text-muted text-uppercase small mb-2", "QC & Filtering"),
      .row("database", "Total Eligible Samples", base::nrow(rv$master_qc_data)),
      .row("broom", "QC Rules Active", if (base::length(qc_rules) > 0) base::paste(qc_rules, collapse = ", ") else "None"),
      .row("filter", "Filters Active", if (base::length(filters) > 0) base::paste(filters, collapse = ", ") else "None"),
      shiny::h6(class = "text-muted text-uppercase small mt-3 mb-2", "Analysis Strategy"),
      .row("object-group", "Method", if (method_raw == "By Pigment Cluster") "Clustering" else "By Source File"),
      .row("layer-group", "Total Analysis Groups", base::length(rv$analysis_datasets)),
      shiny::h6(class = "text-muted text-uppercase small mt-3 mb-2", "Phytoclass Parameters"),
      .row("stopwatch", "Iterations (Niter)", temp_config$phytoclass$niter),
      .row("temperature-low", "Cooling Step Size", temp_config$phytoclass$step_size),
      .row("dice", "Random Seed", seed_text),
      .row("table", "Min/Max Profile", mm_text)
    )
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
    rv$analysis_running <- TRUE

    # The ad-hoc sync below (use_fixed_seed / fixed_seed) covers only two of
    # the several Step 1 fields this run depends on. niter/step_size (read
    # a few lines down) had the same staleness problem as Step 4's cleaning
    # toggles: rv$config$phytoclass$niter is only ever refreshed by
    # sync_config_with_ui(), which only runs from the "Save Configuration"
    # / "Load Fm Matrix Files" button handlers, never from here - so a
    # niter/step_size value typed into Step 1 but not explicitly saved was
    # silently ignored by this run. Syncing everything here once, rather
    # than patching individual fields as they're noticed, is what actually
    # closes this off.
    rv$config <- sync_config_with_ui(rv$config, input)

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
    rv$step_status <- invalidate_from_step(rv$step_status, "step6")
    samples_finished_so_far <- 0
    start_time_global <- base::Sys.time()

    shinyjs::show("live_tracker_card")
    shinyjs::runjs("$('#tracker_progress_bar').css('width', '0%').css('background', '#4A8FA6');")
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
        # <<- matters here, not <-: error handlers passed to tryCatch() are
        # real closures with their own call frame. Plain `ds_obj$log_analyzer
        # <- ...` would create a LOCAL binding inside this handler's own
        # frame, shadowing (not modifying) the for-loop's ds_obj, and be
        # silently discarded the moment this function returns -- meaning a
        # dataset that failed here would still be stored a few lines below
        # with no failure status recorded at all, even though the ENGINE
        # FAULT line above correctly reached the session log. <<- walks up
        # to the enclosing (for-loop) environment and modifies that ds_obj
        # directly.
        ds_obj$log_analyzer <<- base::list(status = "Failed", mean_rmse = NA, mean_condnum = NA, fm_matrix_used = "N/A", flagged_for_review = FALSE, error_details = base::list(message = e$message))
      })

      temp_analyzed_list[[ds_obj$name]] <- ds_obj
      samples_finished_so_far <- samples_finished_so_far + current_batch_size
    }

    rv$analyzed_datasets <- temp_analyzed_list
    shinyjs::runjs("if(window.optTimer) clearInterval(window.optTimer);")
    shinyjs::html("trk_task", "Analysis Execution Complete.")
    shinyjs::html("trk_prog", base::sprintf("%d / %d Samples (%d%%)", total_samples_global, total_samples_global, 100))
    shinyjs::runjs("$('#tracker_progress_bar').css('width', '100%').css('background', '#3F7D4F');")
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
        base::tryCatch({ save_config(rv$config, CONFIG_SESSION_PATH) }, error = function(e) {})
      }
    }

    base::tryCatch({
      checkpoint_dir <- rv$config$workspace$output_directory %||% "phytoclass_output"
      if (!base::dir.exists(checkpoint_dir)) base::dir.create(checkpoint_dir, recursive = TRUE)
      base::saveRDS(rv$analyzed_datasets, file = base::file.path(checkpoint_dir, "AUTOSAVE_LATEST_RUN.rds"))
    }, error = function(e) {})

    rv$step_status <- mark_step_available(rv$step_status, "step6")
    rv$report_data$step6 <- purrr::map_df(temp_analyzed_list, function(x) {
      tibble::tibble(Dataset = x$name %||% "Unknown", Status = x$log_analyzer$status %||% "N/A",
                     Mean_RMSE = base::as.numeric(x$log_analyzer$mean_rmse %||% NA), Mean_CondNum = base::as.numeric(x$log_analyzer$mean_condnum %||% NA),
                     Flagged_For_Review = base::isTRUE(x$log_analyzer$flagged_for_review),
                     Excluded_Pigments = base::paste(x$log_analyzer$excluded_pigments %||% base::character(0), collapse = ", "),
                     Excluded_Classes = base::paste(x$log_analyzer$excluded_classes %||% base::character(0), collapse = ", "),
                     # Previously dropped on the floor here: run_phytoclass_analysis()
                     # always populates error_details$message on failure, but nothing
                     # carried it into this tibble, so the exported report said
                     # "Status: Failed" with no indication of why. This is the one
                     # piece of information a failed group's report is actually for.
                     Error_Message = x$log_analyzer$error_details$message %||% "")
    })

    # Step 7 has no completion button of its own, viewing is on that tab,
    # but downloading happens through the same global Download Report
    # control every other step uses. Step 7 becomes available the moment
    # analysis produces at least one usable result, not on a separate
    # manual action.
    if (base::any(base::vapply(temp_analyzed_list, function(x) !base::is.null(x$data_final), base::logical(1)))) {
      rv$step_status <- mark_step_available(rv$step_status, "step7")
      rv$report_data$step7 <- base::list(
        analyzed_datasets = temp_analyzed_list,
        qc_summary_df = rv$qc_summary_df,
        session_log = rv$session_log,
        session_id = rv$session_id,
        config_snapshot = rv$config
      )
    }

    .update_workflow_state("step7")
    shinyjs::enable("run_phytoclass_btn")
    rv$analysis_running <- FALSE
  })

  output$phytoclass_batch_summary_table <- DT::renderDT({
    shiny::req(rv$analyzed_datasets)
    ds_with_log <- purrr::keep(rv$analyzed_datasets, ~!base::is.null(.x$log_analyzer))

    if (base::length(ds_with_log) == 0) {
      status_msg <- if (base::isTRUE(rv$analysis_running)) {
        "Running... results will appear here as each group completes."
      } else {
        "No analysis results available."
      }
      return(DT::datatable(
        base::data.frame(Status = status_msg, stringsAsFactors = FALSE),
        rownames = FALSE,
        options = base::list(searching = FALSE, lengthChange = FALSE)
      ))
    }

    summary_df <- purrr::map_df(ds_with_log, ~tibble::tibble(
      Dataset = base::as.character(.x$name %||% "Unknown"),
      Status = base::as.character(.x$log_analyzer$status %||% "N/A"),
      `Fm Used` = base::as.character(.x$log_analyzer$fm_matrix_used %||% "N/A"),
      `Seed Used` = base::as.character(.x$log_analyzer$seed_used %||% "N/A"),
      `Mean RMSE` = base::round(base::as.numeric(.x$log_analyzer$mean_rmse %||% NA), 4),
      `Mean Cond Num` = base::round(base::as.numeric(.x$log_analyzer$mean_condnum %||% NA), 2),
      `Flagged` = base::ifelse(base::isTRUE(.x$log_analyzer$flagged_for_review), "Review RMSE", ""),
      `Classes Excluded` = if (base::length(.x$log_analyzer$excluded_classes %||% base::character(0)) > 0) base::paste(.x$log_analyzer$excluded_classes, collapse = ", ") else "",
      `Details` = base::as.character(.x$log_analyzer$error_details$message %||% "")
    ))

    DT::datatable(summary_df, rownames = FALSE, options = base::list(scrollX = TRUE)) |>
      DT::formatStyle("Status", backgroundColor = DT::styleEqual(base::c("Success", "Failed"), base::c("#d1e7dd", "#fff3cd"))) |>
      DT::formatStyle("Flagged", color = "#fd7e14", fontWeight = "bold") |>
      DT::formatStyle("Classes Excluded", color = "#0E6E8C", fontStyle = "italic") |>
      DT::formatStyle("Details", color = "#B23A48", fontSize = "0.9em")

  }, server = FALSE)

  # =========================================================================
  # --- HELP MANUAL ---
  # =========================================================================
  shiny::observeEvent(input$help_btn_global, {
    raw_tab <- input$main_navbar
    current_tab <- if (base::is.null(raw_tab) || base::length(raw_tab) == 0) "step1" else base::as.character(raw_tab)

    btn_prim <- function(label, ico) { shiny::span(style = "background-color: #0E6E8C; color: #fff; padding: 4px 8px; border-radius: 4px; font-size: 0.9em; margin: 0 4px; display: inline-block;", shiny::icon(ico), base::paste0(" ", label)) }
    btn_succ <- function(label, ico) { shiny::span(style = "background-color: #3F7D4F; color: #fff; padding: 4px 8px; border-radius: 4px; font-size: 0.9em; margin: 0 4px; display: inline-block;", shiny::icon(ico), base::paste0(" ", label)) }
    btn_dang <- function(label, ico) { shiny::span(style = "border: 1px solid #B23A48; color: #B23A48; background-color: transparent; padding: 4px 8px; border-radius: 4px; font-size: 0.9em; margin: 0 4px; display: inline-block;", shiny::icon(ico), base::paste0(" ", label)) }
    ui_tog <- function(label) { shiny::span(style = "background-color: #f8f9fa; border: 1px solid #ced4da; border-radius: 4px; padding: 2px 6px; font-family: monospace; font-size: 0.9em; margin: 0 4px; display: inline-block;", shiny::icon("check-square"), base::paste0(" ", label)) }
    ui_in  <- function(label) { shiny::span(style = "background-color: #f8f9fa; border: 1px solid #ced4da; border-radius: 4px; padding: 2px 6px; font-family: monospace; font-size: 0.9em; margin: 0 4px; display: inline-block;", shiny::icon("keyboard"), base::paste0(" ", label)) }

    help_styles <- shiny::tags$style(shiny::HTML(".help-section-title { border-bottom: 2px solid #e2e8f0; color: #0E6E8C; margin-top: 15px; margin-bottom: 10px; padding-bottom: 5px; font-weight: bold; } code { font-family: monospace; color: #d63384; } li { margin-bottom: 8px; line-height: 1.45; }"))

    content_step1 <- shiny::tagList(
      shiny::h4("Step 1: Setup Parameters"),
      shiny::p("Configure file paths, algorithm settings, and data cleaning rules."),

      shiny::div(class="help-section-title", "1. Saved Sessions & Matrix Files"),
      shiny::tags$ul(
        shiny::tags$li(btn_prim("Load Last Session", "folder-open"), ": restores your previous settings."),
        shiny::tags$li(btn_dang("Reset to Default Settings", "redo"), " (under \"Show reset options...\"): reloads the original template. Asks for confirmation first."),
        shiny::tags$li(ui_in("Fm_Pro / Fm_NoPro Path"), ": your reference matrix files, both editable spreadsheets."),
        shiny::tags$li(shiny::strong("Adding a pigment or class:"), " needs a matching min/max rule below, or it's rejected."),
        shiny::tags$li(btn_prim("Check Matrix Files", "check-double"), ": confirms the app can read your reference files.")
      ),

      shiny::div(class="help-section-title", "2. Algorithm Settings (Simulated Annealing)"),
      shiny::tags$ul(
        shiny::tags$li(ui_in("Iterations"), " & ", ui_in("Cooling Step Size"), ": higher/smaller improves accuracy, costs runtime."),
        shiny::tags$li(ui_in("Use Fixed Random Seed"), ": guarantees identical results on repeat runs. Off by default.")
      ),

      shiny::div(class="help-section-title", "3. Min/Max Profiles (Algorithm Constraints)"),
      shiny::tags$ul(
        shiny::tags$li(shiny::strong("Default:"), " phytoclass's own internal bounds."),
        shiny::tags$li(shiny::strong("Custom file:"), " an Excel file in ", shiny::code("R/reference tables/"), " named starting 'MinMax_', with columns ", shiny::code("Class"), ", ", shiny::code("Pig_Abbrev"), ", ", shiny::code("min"), ", ", shiny::code("max"), "."),
        shiny::tags$li(shiny::strong("Must match your Fm matrix:"), " every non-zero pigment/class pair needs a rule here, checked automatically."),
        shiny::tags$li(ui_tog("Use Region-Specific Min/Max Bounds"), ": switches to your custom profile.")
      ),

      shiny::div(class="help-section-title", "4. Cleaning Rules & Filters"),
      shiny::tags$ul(
        shiny::tags$li(ui_tog("Cleaning Options"), ": fixes blank cells, drops empty rows, automatically."),
        shiny::tags$li(ui_tog("Data Filters"), ": excludes samples outside your location/date/depth bounds; enabling one makes that column mandatory in Step 3.")
      ),

      shiny::div(class="help-section-title", "5. Saving Your Work"),
      shiny::tags$ul(
        shiny::tags$li(btn_succ("Save Config", "save"), " (top navigation bar): saves your ", shiny::em("settings"), " for next time."),
        shiny::tags$li("For your ", shiny::em("results"), " instead, use ", shiny::strong("Download Report"), " next to it (see below).")
      )
    )

    content_step2 <- shiny::tagList(
      shiny::h4("Step 2: Import Data"),
      shiny::p("Upload raw HPLC pigment data for analysis."),
      shiny::tags$ul(
        shiny::tags$li(ui_in("Select Files"), ": one or multiple .xlsx files."),
        shiny::tags$li(btn_prim("Load Data Files", "play"), ": imports them; invalid or empty files are flagged and skipped."),
        shiny::tags$li(shiny::strong("Dates and times:"), " one combined column or separate year/month/day/hour/minute columns both work, resolved automatically.")
      )
    )

    content_step3 <- shiny::tagList(
      shiny::h4("Step 3: Map Variables"),
      shiny::p("Every variable is listed here, auto-assigned and missing alike, so you can verify or correct it."),
      shiny::tags$ul(
        shiny::tags$li(shiny::span(class="badge bg-warning text-dark", "NEEDS MAPPING"), ": click the row to open the mapping wizard."),
        shiny::tags$li(shiny::strong("Always mandatory:"), " Tchla (Total Chlorophyll a)."),
        shiny::tags$li(shiny::strong("Conditionally mandatory:"), " location, date, or depth, if you enabled the matching filter in Step 1."),
        shiny::tags$li(shiny::strong("Data preview:"), " each dropdown shows a live sample of the column you've selected."),
        shiny::tags$li(btn_succ("Save Mappings", "check-double"), ": locks in your choices and unlocks Step 4.")
      )
    )

    content_step4 <- shiny::tagList(
      shiny::h4("Step 4: Filter & Clean"),
      shiny::p("Applies the Quality Control rules configured in Step 1."),
      shiny::tags$ul(
        shiny::tags$li(btn_prim("Clean My Data", "shield-alt"), ": removes duplicates, fixes blanks, applies filters."),
        shiny::tags$li(shiny::strong("File Breakdown:"), " how many samples were removed per file, and why.")
      )
    )

    content_step5 <- shiny::tagList(
      shiny::h4("Step 5: Group Samples"),
      shiny::p("Define how samples are grouped before the optimization engine processes them."),
      shiny::tags$ul(
        shiny::tags$li(ui_tog("By Source File"), ": each uploaded file is its own batch."),
        shiny::tags$li(ui_tog("By Pigment Cluster"), ": recommended; pools similar samples across files."),
        shiny::tags$li(shiny::strong("Large datasets:"), " above a few thousand samples, switches to K-Means automatically and subsamples the diagnostic plots. Every sample is still grouped; a banner explains when this happens."),
        shiny::tags$li(ui_in("Distance Metric"), ": disabled under K-Means, which always measures distance one fixed way. Applies to the two Ward's-based methods only."),
        shiny::tags$li(btn_prim("Preview Groups", "play"), ": shows the split, plus cluster graphs if active."),
        shiny::tags$li(btn_succ("Lock in Strategy", "check-double"), ": saves your choice and builds the arrays for Step 6.")
      )
    )

    content_step6 <- shiny::tagList(
      shiny::h4("Step 6: Run Analysis"),
      shiny::p("Executes the Simulated Annealing optimization."),
      shiny::tags$ul(
        shiny::tags$li(ui_in("Select Data to Run"), ": choose which groups to process."),
        shiny::tags$li(btn_prim("Start Analysis", "rocket"), ": begins the run; a live dashboard tracks progress and ETA."),
        shiny::tags$li(shiny::strong("Flagged runs:"), " RMSE over threshold is marked for review in Step 7, per Hayward et al. (2023)."),
        shiny::tags$li(shiny::strong("Classes Excluded:"), " if a pigment is absent from every sample in a group, it, and any class that depended only on it, is dropped from that group's matrix before analysis, rather than left in with no information to estimate it from. Shown per group in this table.")
      )
    )

    content_step7 <- shiny::tagList(
      shiny::h4("Step 7: View Results"),
      shiny::p("Review performance metrics and preview results. Download via the top navigation bar."),
      shiny::tags$ul(
        shiny::tags$li(shiny::strong("RMSE, Condition Number, sMAPE:"), " lower is better. sMAPE showing 'N/A'? A note beneath explains why."),
        shiny::tags$li(shiny::strong("View Graphs:"), " relative and absolute community composition, per group."),
        shiny::tags$li(shiny::strong("Optimised Pigment Ratios:"), " third tab; the final ratio matrix for the selected group."),
        shiny::tags$li(shiny::strong("Trimmed matrix note:"), " appears here when a group had a pigment absent from every sample; explains exactly which pigments and classes were excluded for that group.")
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
      title = shiny::div(shiny::icon("book-open"), " Guidelines - ", shiny::strong(base::toupper(current_tab))),
      size = "l",
      easyClose = TRUE,
      help_styles,
      selected_content,
      footer = shiny::modalButton("Close")
    ))
  })
}

# =========================================================================
# --- LAUNCH ENGINE ---
# =========================================================================
phytoclassShiny_app <- shiny::shinyApp(ui = ui, server = server)

if (!interactive()) {
  shiny::runApp(phytoclassShiny_app, launch.browser = TRUE)
} else {
  phytoclassShiny_app
}
