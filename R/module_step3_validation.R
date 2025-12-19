# ============================================================================
#
#   _phytoclass_Shiny V1.0 - STEP 3: COLUMN MAPPING
#
#   Description:
#   Manages the Column Mapping Wizard. "Clean Sheet" UI design for maximum
#   data visibility and minimal clutter.
#
# ============================================================================

# --- UI Function ---
validationUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Header & Concise Instructions
    h3("Step 3: Check Column Mappings"),
    div(style = "margin-bottom: 20px; font-size: 1.1em; color: #555;",
        "Select any row flagged ", 
        span(class="label label-warning", style="font-size: 0.9em; vertical-align: middle;", "MISSING ESSENTIALS"), 
        " to launch the Mapping Wizard."
    ),
    
    # The Table (Clean, Full Width)
    DTOutput(ns("mapping_validation_summary_table")),
    
    br(),
    
    # The Action Bar
    fluidRow(
      # Undo (Left, Secondary)
      column(4, 
             actionButton(ns("rollback_mappings_btn"), "Undo Last Change", 
                          icon = icon("undo"), 
                          class = "btn-default", # Grey/Neutral style
                          width = "100%")
      ),
      # Spacer
      column(4), 
      # Confirm (Right, Hero)
      column(4, 
             shinyjs::disabled(
               actionButton(ns("commit_all_mappings_btn"), "Confirm Mappings", 
                            icon = icon("check-double"), 
                            class = "btn-success btn-lg", # Big Green
                            width = "100%")
             )
      )
    )
  )
}

# --- Server Function ---
validationServer <- function(id, rv, .log_event, .update_workflow_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    wizard_keys_dynamic <- reactive({
      req(rv$config)
      keys <- c(rv$config$general$essential_pigments, "Tchla")
      if (isTRUE(rv$config$filtering$geospatial$enabled)) keys <- c(keys, "latitude", "longitude")
      if (isTRUE(rv$config$filtering$temporal$enabled)) keys <- c(keys, "year", "month", "day")
      if (isTRUE(rv$config$filtering$depth$enabled)) keys <- c(keys, "depth")
      unique(keys)
    })
    
    blocker_keys_dynamic <- reactive({
      req(rv$config)
      keys <- c("Tchla")
      if (isTRUE(rv$config$filtering$geospatial$enabled)) keys <- c(keys, "latitude", "longitude")
      if (isTRUE(rv$config$filtering$temporal$enabled)) keys <- c(keys, "year", "month", "day")
      if (isTRUE(rv$config$filtering$depth$enabled)) keys <- c(keys, "depth")
      unique(keys)
    })
    
    mapping_summary <- eventReactive(list(rv$staging_datasets, rv$mapping_trigger, wizard_keys_dynamic()), {
      req(length(rv$staging_datasets) > 0)
      purrr::map_df(rv$staging_datasets, function(ds) { .get_mapping_status(ds, wizard_keys_dynamic()) }) %>%
        mutate(
          Missing_str = purrr::map_chr(Missing, ~paste(.x, collapse = ", ")),
          Missing_Count = purrr::map_int(Missing, length)
        )
    })
    
    output$mapping_validation_summary_table <- renderDT({
      req(mapping_summary())
      
      df_for_display <- mapping_summary()[, c("Dataset", "Mapping Health", "Missing_Count", "Missing_str")]
      colnames(df_for_display) <- c("Dataset", "Status", "Missing Count", "Unmapped Keys")
      
      datatable(df_for_display, 
                options = list(
                  pageLength = 15, # Show more rows by default since we removed panels
                  searching = FALSE,
                  autoWidth = TRUE,
                  columnDefs = list(list(className = 'dt-center', targets = 2)),
                  order = list(list(1, 'asc'), list(2, 'desc'))
                ), 
                rownames = FALSE, 
                selection = 'single',
                class = "cell-border stripe hover compact"
      ) %>%
        formatStyle("Status", backgroundColor = styleEqual(c("OK", "MISSING ESSENTIALS"), c("#C6EFCE", "#FFEB9C"))) %>%
        formatStyle("Missing Count", fontWeight = "bold")
    })
    
    observeEvent(input$mapping_validation_summary_table_rows_selected, {
      req(mapping_summary())
      selected_row <- input$mapping_validation_summary_table_rows_selected
      if (length(selected_row) == 0) return()
      
      dataset_name <- mapping_summary()$Dataset[selected_row]
      ds_status <- mapping_summary()[selected_row, ]
      
      if (length(ds_status$Missing[[1]]) > 0) {
        rv$current_mapping_dataset <- dataset_name
        .show_mapping_modal(dataset_name, ds_status$Missing[[1]])
      } else { 
        showNotification("All keys are mapped.", type = "message") 
      }
    })
    
    .show_mapping_modal <- function(dataset_name, missing_keys) {
      ds_obj <- rv$staging_datasets[[dataset_name]]
      req(ds_obj)
      
      blocker_alerts <- list()
      if ("depth" %in% missing_keys && isTRUE(rv$config$filtering$depth$enabled)) {
        blocker_alerts <- c(blocker_alerts, "Depth is required because the Depth Filter is enabled in Step 1.")
      }
      if (any(c("latitude", "longitude") %in% missing_keys) && isTRUE(rv$config$filtering$geospatial$enabled)) {
        blocker_alerts <- c(blocker_alerts, "Lat/Lon are required because the Geospatial Filter is enabled in Step 1.")
      }
      if (any(c("year", "month", "day") %in% missing_keys) && isTRUE(rv$config$filtering$temporal$enabled)) {
        blocker_alerts <- c(blocker_alerts, "Date columns are required because the Temporal Filter is enabled in Step 1.")
      }
      
      ui_header <- if (length(blocker_alerts) > 0) {
        div(class = "alert alert-danger", style = "border: 2px solid #a94442;",
            h4("🔥🧙‍♂️ YOU SHALL NOT PASS! 🧙‍♂️🔥", style="font-weight:bold; text-align: center; font-size: 1.4em;"), 
            p("You have active filters in Step 1 that require these columns. You cannot map them to 'Leave Unmapped'."),
            tags$ul(lapply(blocker_alerts, tags$li)),
            hr(),
            p(strong("Solution:"), "If this file does not have this data, please go back to Step 1 and uncheck the relevant filter.")
        )
      } else {
        div(class = "alert alert-warning", 
            p("Select columns for keys you have. Leave blank if the pigment is missing (it will be treated as 0).")
        )
      }
      
      mapper_inputs <- lapply(missing_keys, function(key) {
        selectInput(inputId = ns(paste0("map_", key)), label = paste0("Map '", key, "' to:"), choices = c("Leave Unmapped" = "", ds_obj$cleaned_colnames), width = "100%")
      })
      
      showModal(modalDialog(title = div(icon("magic"), paste("Mapping Wizard:", dataset_name)), size = "l", easyClose = FALSE,
                            footer = tagList(
                              checkboxInput(ns("apply_to_all_similar_modal"), "Apply to other datasets", value = TRUE),
                              modalButton("Cancel", icon("times")),
                              actionButton(ns("commit_modal_mappings_btn"), "Apply Selected", class = "btn-primary", icon = icon("check"))
                            ),
                            ui_header,
                            hr(), 
                            mapper_inputs
      ))
    }
    
    observeEvent(input$commit_modal_mappings_btn, {
      req(rv$current_mapping_dataset)
      show_modal_spinner(text = "Applying...")
      tryCatch({
        success <- .apply_mappings_safe(dataset_name = rv$current_mapping_dataset, apply_to_similar = isTRUE(input$apply_to_all_similar_modal))
        rv$mapping_trigger <- rv$mapping_trigger + 1
        removeModal(); rv$current_mapping_dataset <- NULL
      }, error = function(e) { .log_event(paste("ERROR:", e$message)) }, finally = { remove_modal_spinner() })
    })
    
    .apply_mappings_safe <- function(dataset_name, apply_to_similar) {
      ds_obj <- rv$staging_datasets[[dataset_name]]
      status <- .get_mapping_status(ds_obj, wizard_keys_dynamic())
      missing_keys <- status$Missing[[1]]
      new_mappings <- list()
      for (key in missing_keys) {
        user_choice <- input[[paste0("map_", key)]]
        if (!is.null(user_choice) && user_choice != "") new_mappings[[key]] <- user_choice
      }
      if (length(new_mappings) == 0) return(TRUE)
      temp_staging <- rv$staging_datasets
      for(key in names(new_mappings)){ temp_staging[[dataset_name]]$rename_map[[key]] <- new_mappings[[key]] }
      if(apply_to_similar){
        similar_datasets <- .find_similar_datasets(dataset_name, missing_keys)
        for(sim_ds in similar_datasets){ for(key in names(new_mappings)){ temp_staging[[sim_ds]]$rename_map[[key]] <- new_mappings[[key]] } }
      }
      rv$mapping_history[[length(rv$mapping_history) + 1]] <- purrr::map(rv$staging_datasets, rlang::duplicate)
      rv$staging_datasets <- temp_staging
      return(TRUE)
    }
    
    .find_similar_datasets <- function(ref, missing) {
      names(rv$staging_datasets) %>% purrr::keep(function(n) { if(n==ref) return(FALSE); s <- .get_mapping_status(rv$staging_datasets[[n]], wizard_keys_dynamic()); identical(sort(s$Missing[[1]]), sort(missing)) })
    }
    
    observeEvent(input$rollback_mappings_btn, {
      if (length(rv$mapping_history) > 0) {
        rv$staging_datasets <- rv$mapping_history[[length(rv$mapping_history)]]; rv$mapping_history[[length(rv$mapping_history)]] <- NULL; rv$mapping_trigger <- rv$mapping_trigger + 1
      }
    })
    
    observe({
      req(mapping_summary())
      has_blocker_missing <- any(sapply(mapping_summary()$Missing, function(m) any(m %in% blocker_keys_dynamic())))
      if (!has_blocker_missing) shinyjs::enable("commit_all_mappings_btn") else shinyjs::disable("commit_all_mappings_btn")
    })
    
    observeEvent(input$commit_all_mappings_btn, {
      req(rv$fm_matrices)
      all_warnings <- list()
      for (ds_name in names(rv$staging_datasets)) {
        ds <- rv$staging_datasets[[ds_name]]
        if (exists("check_resolution_capabilities")) {
          w <- check_resolution_capabilities(ds, rv$config, rv$fm_matrices)
          if (length(w) > 0) all_warnings[[ds_name]] <- w
        }
      }
      
      if (length(all_warnings) > 0) {
        rv$resolution_warnings <- all_warnings
        warning_ui <- lapply(names(all_warnings), function(n) { tagList(h5(strong(paste("Dataset:", n))), tags$ul(lapply(all_warnings[[n]], tags$li))) })
        showModal(modalDialog(
          title = div(icon("exclamation-triangle", class = "text-warning"), "Resolution Capabilities Warning"),
          div(class = "alert alert-warning", "Some datasets are missing pigments found in your Fm matrix. The following groups will default to 0:"),
          div(style = "max-height: 300px; overflow-y: auto;", warning_ui),
          footer = tagList(modalButton("Go Back"), actionButton(ns("force_commit_btn"), "Acknowledge & Proceed", class = "btn-warning"))
        ))
      } else {
        rv$resolution_warnings <- list() 
        .finalize_commit()
      }
    })
    
    observeEvent(input$force_commit_btn, { removeModal(); .finalize_commit() })
    
    .finalize_commit <- function() {
      rv$datasets_processed <- rv$staging_datasets
      rv$config <- update_config_with_new_aliases(rv$config, rv$datasets_processed)
      showNotification("Mappings committed! Proceed to Step 4.", type = "message", duration = 8)
      .update_workflow_state("step4")
    }
  })
}