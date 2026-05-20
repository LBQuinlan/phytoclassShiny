validationUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Step 3: Check Column Names"),
    shiny::div(style = "margin-bottom: 20px; font-size: 1.1em; color: #555;",
               "Select any row flagged ", 
               shiny::span(class="badge bg-warning text-dark", style="font-size: 0.9em; vertical-align: middle;", "NEEDS MAPPING"), 
               " to teach the app which column is which."
    ),
    bslib::card(DT::DTOutput(ns("mapping_validation_summary_table"))),
    shiny::br(),
    shiny::fluidRow(
      shiny::column(4, shiny::actionButton(ns("rollback_mappings_btn"), "Undo Last Change", icon = shiny::icon("undo"), class = "btn-outline-secondary", width = "100%")),
      shiny::column(4), 
      shiny::column(4, shinyjs::disabled(shiny::actionButton(ns("commit_all_mappings_btn"), "Save Mappings", icon = shiny::icon("check-double"), class = "btn-success btn-lg fw-bold", width = "100%")))
    )
  )
}

validationServer <- function(id, rv, .log_event, .update_workflow_state, session_parent) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- 1. CORE COMMIT FUNCTION SCOPED AT THE TOP ---
    .finalize_commit <- function() {
      mapped_staging <- base::list()
      for (ds_name in base::names(rv$staging_datasets)) {
        ds <- rv$staging_datasets[[ds_name]]; working_df <- ds$data; rename_map <- ds$rename_map
        if (!base::is.null(rename_map) && base::length(rename_map) > 0) { 
          for (target_col in base::names(rename_map)) { 
            raw_col <- rename_map[[target_col]]; 
            if (raw_col != "" && raw_col %in% base::names(working_df)) { 
              base::names(working_df)[base::names(working_df) == raw_col] <- target_col 
            } 
          } 
        }
        ds$data <- working_df; mapped_staging[[ds_name]] <- ds
      }
      rv$staging_datasets <- mapped_staging; rv$datasets_processed <- rv$staging_datasets
      
      # Update active memory configuration
      rv$config <- update_config_with_new_aliases(rv$config, rv$datasets_processed)
      
      # Hard Drive Sync: Lock custom aliases into the YAML immediately
      if (base::exists("save_config") && base::is.function(save_config)) {
        base::tryCatch({
          target_path <- if (base::exists("CONFIG_SESSION_PATH")) CONFIG_SESSION_PATH else "config_session.yaml"
          save_config(rv$config, target_path)
          .log_event("CONFIG", "Alias mapping successfully written to local session config.")
        }, error = function(e) { .log_event("WARNING", base::paste("Failed to save alias config:", e$message)) })
      }
      
      shiny::showNotification("Mappings saved! Proceed to Step 4.", type = "message", duration = 8)
      .update_workflow_state("step4")
      shiny::updateTabsetPanel(session = session_parent, inputId = "main_navbar", selected = "step4")
    }
    
    # --- 2. DYNAMIC WIZARD LOGIC ---
    wizard_keys_dynamic <- shiny::reactive({
      shiny::req(rv$config)
      keys <- base::c(rv$config$general$essential_pigments, "Tchla")
      if (base::isTRUE(rv$config$filtering$geospatial$enabled)) keys <- base::c(keys, "latitude", "longitude")
      if (base::isTRUE(rv$config$filtering$temporal$enabled)) keys <- base::c(keys, "year", "month", "day")
      if (base::isTRUE(rv$config$filtering$depth$enabled)) keys <- base::c(keys, "depth")
      base::unique(keys)
    })
    
    blocker_keys_dynamic <- shiny::reactive({
      shiny::req(rv$config)
      keys <- base::c("Tchla")
      if (base::isTRUE(rv$config$filtering$geospatial$enabled)) keys <- base::c(keys, "latitude", "longitude")
      if (base::isTRUE(rv$config$filtering$temporal$enabled)) keys <- base::c(keys, "year", "month", "day")
      if (base::isTRUE(rv$config$filtering$depth$enabled)) keys <- base::c(keys, "depth")
      base::unique(keys)
    })
    
    mapping_summary <- shiny::eventReactive(base::list(rv$staging_datasets, rv$mapping_trigger, wizard_keys_dynamic()), {
      shiny::req(base::length(rv$staging_datasets) > 0)
      purrr::map_df(rv$staging_datasets, function(ds) { 
        .get_mapping_status(ds, wizard_keys_dynamic()) 
      }) |>
        dplyr::mutate(
          Missing_str = purrr::map_chr(Missing, ~base::paste(.x, collapse = ", ")),
          Missing_Count = purrr::map_int(Missing, base::length)
        )
    })
    
    output$mapping_validation_summary_table <- DT::renderDT({
      shiny::req(mapping_summary())
      df_for_display <- mapping_summary()[, base::c("Dataset", "Mapping Health", "Missing_Count", "Missing_str")]
      base::colnames(df_for_display) <- base::c("Dataset", "Status", "Missing Count", "Unmapped Keys")
      
      DT::datatable(df_for_display, options = base::list(pageLength = 15, searching = FALSE, autoWidth = TRUE, columnDefs = base::list(base::list(className = 'dt-center', targets = 2)), order = base::list(base::list(1, 'asc'), base::list(2, 'desc'))), rownames = FALSE, selection = 'single', class = "cell-border stripe hover compact") |>
        DT::formatStyle("Status", backgroundColor = DT::styleEqual(base::c("OK", "NEEDS MAPPING"), base::c("#d1e7dd", "#fff3cd"))) |>
        DT::formatStyle("Missing Count", fontWeight = "bold")
    })
    
    shiny::observeEvent(input$mapping_validation_summary_table_rows_selected, {
      shiny::req(mapping_summary())
      selected_row <- input$mapping_validation_summary_table_rows_selected
      if (base::length(selected_row) == 0) return()
      dataset_name <- mapping_summary()$Dataset[selected_row]
      ds_status <- mapping_summary()[selected_row, ]
      if (base::length(ds_status$Missing[[1]]) > 0) { rv$current_mapping_dataset <- dataset_name; .show_mapping_modal(dataset_name, ds_status$Missing[[1]])
      } else { shiny::showNotification("All keys are mapped.", type = "message") }
    })
    
    .show_mapping_modal <- function(dataset_name, missing_keys) {
      ds_obj <- rv$staging_datasets[[dataset_name]]
      shiny::req(ds_obj)
      blocker_alerts <- base::list()
      if ("depth" %in% missing_keys && base::isTRUE(rv$config$filtering$depth$enabled)) blocker_alerts <- base::c(blocker_alerts, "Depth is required because the Depth Filter is enabled in Step 1.")
      if (base::any(base::c("latitude", "longitude") %in% missing_keys) && base::isTRUE(rv$config$filtering$geospatial$enabled)) blocker_alerts <- base::c(blocker_alerts, "Lat/Lon are required because the Location Filter is enabled in Step 1.")
      if (base::any(base::c("year", "month", "day") %in% missing_keys) && base::isTRUE(rv$config$filtering$temporal$enabled)) blocker_alerts <- base::c(blocker_alerts, "Date columns are required because the Date Filter is enabled in Step 1.")
      
      ui_header <- if (base::length(blocker_alerts) > 0) {
        shiny::div(class = "alert alert-danger", style = "border: 2px solid #a94442;", shiny::h4("🔥 MANDATORY MAPPING REQUIRED 🔥", style="font-weight:bold; text-align: center; font-size: 1.2em;"), shiny::p("You have active filters in Step 1 that require these columns. You cannot map them to 'Leave Unmapped'."), shiny::tags$ul(base::lapply(blocker_alerts, shiny::tags$li)), shiny::hr(), shiny::p(shiny::strong("Solution:"), "If this file does not have this data, please go back to Step 1 and uncheck the relevant filter."))
      } else { shiny::div(class = "alert alert-warning", shiny::p("Select columns for keys you have. Leave blank if the pigment is missing (it will be treated as 0).")) }
      
      mapper_inputs <- base::lapply(missing_keys, function(key) { shiny::selectInput(inputId = ns(base::paste0("map_", key)), label = base::paste0("Map '", key, "' to:"), choices = base::c("Leave Unmapped" = "", ds_obj$cleaned_colnames), width = "100%") })
      shiny::showModal(shiny::modalDialog(title = shiny::div(shiny::icon("magic"), base::paste("Mapping Wizard:", dataset_name)), size = "l", easyClose = FALSE, footer = shiny::tagList(shiny::checkboxInput(ns("apply_to_all_similar_modal"), "Apply to other datasets", value = TRUE), shiny::modalButton("Cancel", shiny::icon("times")), shiny::actionButton(ns("commit_modal_mappings_btn"), "Apply Selected", class = "btn-primary", icon = shiny::icon("check"))), ui_header, shiny::hr(), mapper_inputs))
    }
    
    shiny::observeEvent(input$commit_modal_mappings_btn, {
      shiny::req(rv$current_mapping_dataset)
      shinybusy::show_modal_spinner(text = "Applying...")
      base::tryCatch({
        success <- .apply_mappings_safe(dataset_name = rv$current_mapping_dataset, apply_to_similar = base::isTRUE(input$apply_to_all_similar_modal))
        if (base::isTRUE(success)) { rv$mapping_trigger <- rv$mapping_trigger + 1; shiny::removeModal(); rv$current_mapping_dataset <- NULL }
      }, error = function(e) { .log_event(base::paste("ERROR:", e$message)); shiny::showNotification(e$message, type = "error", duration = 8)
      }, finally = { shinybusy::remove_modal_spinner() })
    })
    
    .apply_mappings_safe <- function(dataset_name, apply_to_similar) {
      ds_obj <- rv$staging_datasets[[dataset_name]]
      status <- .get_mapping_status(ds_obj, wizard_keys_dynamic())
      missing_keys <- status$Missing[[1]]; new_mappings <- base::list()
      for (key in missing_keys) { user_choice <- input[[base::paste0("map_", key)]]; if (!base::is.null(user_choice) && user_choice != "") new_mappings[[key]] <- user_choice }
      if (base::length(new_mappings) == 0) return(TRUE)
      chosen_raw_cols <- base::unlist(new_mappings)
      if (base::any(base::duplicated(chosen_raw_cols))) base::stop("Mapping Conflict: You assigned multiple standard targets to the exact same raw column. Each pigment must be mapped uniquely.")
      temp_staging <- rv$staging_datasets
      for(key in base::names(new_mappings)) temp_staging[[dataset_name]]$rename_map[[key]] <- new_mappings[[key]] 
      if(apply_to_similar){ similar_datasets <- .find_similar_datasets(dataset_name, missing_keys); for(sim_ds in similar_datasets) for(key in base::names(new_mappings)) temp_staging[[sim_ds]]$rename_map[[key]] <- new_mappings[[key]] }
      rv$mapping_history[[base::length(rv$mapping_history) + 1]] <- purrr::map(rv$staging_datasets, rlang::duplicate)
      rv$staging_datasets <- temp_staging
      return(TRUE)
    }
    
    .find_similar_datasets <- function(ref, missing) { base::names(rv$staging_datasets) |> purrr::keep(function(n) { if(n==ref) return(FALSE); s <- .get_mapping_status(rv$staging_datasets[[n]], wizard_keys_dynamic()); base::identical(base::sort(s$Missing[[1]]), base::sort(missing)) }) }
    
    shiny::observeEvent(input$rollback_mappings_btn, { if (base::length(rv$mapping_history) > 0) { last_idx <- base::length(rv$mapping_history); rv$staging_datasets <- rv$mapping_history[[last_idx]]; rv$mapping_history[[last_idx]] <- NULL; rv$mapping_trigger <- rv$mapping_trigger + 1 } })
    
    shiny::observe({
      shiny::req(mapping_summary())
      has_blocker_missing <- base::any(base::sapply(mapping_summary()$Missing, function(m) base::any(m %in% blocker_keys_dynamic())))
      if (!has_blocker_missing) shinyjs::enable("commit_all_mappings_btn") else shinyjs::disable("commit_all_mappings_btn")
    })
    
    # --- 3. EXPLICIT ERROR INTERCEPT ---
    shiny::observeEvent(input$commit_all_mappings_btn, {
      
      # LOUD CHECK: Did the matrices fail to load?
      if (base::is.null(rv$fm_matrices) || base::is.null(rv$fm_matrices$Fm_Pro)) {
        shiny::showModal(shiny::modalDialog(
          title = shiny::div(shiny::icon("times-circle", class="text-danger"), " Matrix Files Missing"),
          "Your Fm reference matrices are missing from memory! Please go back to Step 1, verify the file paths, and click 'Check Matrix Files'.",
          type = "error"
        ))
        return()
      }
      
      all_warnings <- base::list()
      for (ds_name in base::names(rv$staging_datasets)) { 
        ds <- rv$staging_datasets[[ds_name]]
        if (base::exists("check_resolution_capabilities", mode="function")) { 
          w <- check_resolution_capabilities(ds, rv$config, rv$fm_matrices)
          if (base::length(w) > 0) all_warnings[[ds_name]] <- w 
        } 
      }
      
      if (base::length(all_warnings) > 0) {
        rv$resolution_warnings <- all_warnings
        warning_ui <- base::lapply(base::names(all_warnings), function(n) { shiny::tagList(shiny::h5(shiny::strong(base::paste("Dataset:", n))), shiny::tags$ul(base::lapply(all_warnings[[n]], shiny::tags$li))) })
        shiny::showModal(shiny::modalDialog(title = shiny::div(shiny::icon("exclamation-triangle", class = "text-warning"), "Resolution Capabilities Warning"), shiny::div(class = "alert alert-warning", "Some datasets are missing pigments found in your Fm matrix. The following groups will default to 0:"), shiny::div(style = "max-height: 300px; overflow-y: auto;", warning_ui), footer = shiny::tagList(shiny::modalButton("Go Back"), shiny::actionButton(ns("force_commit_btn"), "Acknowledge & Proceed", class = "btn-warning"))))
      } else { 
        rv$resolution_warnings <- base::list()
        .finalize_commit() 
      }
    })
    
    shiny::observeEvent(input$force_commit_btn, { 
      shiny::removeModal()
      .finalize_commit() 
    })
    
  })
}