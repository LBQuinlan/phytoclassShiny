# ============================================================================
# MODULE: Step 3 - Map Variables
#
# Every app-required variable is listed here, both what was assigned
# automatically and anything still missing, so a user can verify or
# correct it before proceeding. Missing keys are listed first; assigned
# keys follow, shown pre-filled with an option to override.
#
# Date/time handling supports two conventions side by side: "date" (a
# single combined column) is offered as a mapping target alongside
# year/month/day, so either a combined column or separate columns has a
# direct, correct home. The underlying requirement is an either/or: date-
# time information is satisfied by a mapped "date" column OR by all three
# of year/month/day being mapped, never by requiring both at once, which
# mirrors the equivalence backend_helpers.R's .get_mapping_status() uses
# for the dataset summary table.
#
# If a user maps year, month, and day to the exact same raw column, a
# single combined date column mapped three times out of habit rather than
# noticing "date" as an option, the wizard detects that specific pattern
# and treats it as a combined date-column mapping instead of raising a
# conflict, since that's clearly what's meant. Any other kind of
# duplicate mapping (unrelated keys pointed at the same column) still
# correctly raises the conflict error.
# ============================================================================

validationUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Step 3: Check Column Names"),
    shiny::div(style = "margin-bottom: 20px; font-size: 1.1em; color: #555;",
               "Select any row flagged ",
               shiny::span(class="badge bg-warning text-dark", style="font-size: 0.9em; vertical-align: middle;", "NEEDS MAPPING"),
               " to review every variable the app has assigned, and to map anything it couldn't find on its own. A row showing ",
               shiny::span(class="badge bg-success", style="font-size: 0.9em; vertical-align: middle;", "OK"),
               " means every pigment and metadata column this app requires was found for that dataset."
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
    `%||%` <- function(a, b) if (!base::is.null(a)) a else b

    # Date-time info is satisfied by EITHER a mapped "date" column OR all
    # three of year/month/day being mapped; never requires both routes.
    .date_satisfied <- function(mapped_keys) {
      "date" %in% mapped_keys || base::all(base::c("year", "month", "day") %in% mapped_keys)
    }

    # --- 1. CORE COMMIT FUNCTION ---
    .finalize_commit <- function() {
      mapped_staging <- base::list()
      for (ds_name in base::names(rv$staging_datasets)) {
        ds <- rv$staging_datasets[[ds_name]]; working_df <- ds$data; rename_map <- ds$rename_map
        if (!base::is.null(rename_map) && base::length(rename_map) > 0) {
          for (target_col in base::names(rename_map)) {
            raw_col <- rename_map[[target_col]];
            if (raw_col != "" && raw_col %in% base::names(working_df)) {
              # resolve_datetime_columns() unconditionally creates
              # year/month/day/hour/minute placeholder columns at import
              # time (its own "clean slate" logic), so those names already
              # exist on every dataset by the time a manual mapping can be
              # committed here. Renaming raw_col straight into target_col
              # without first checking for that placeholder produces TWO
              # columns with the same name -- which the resolve_datetime_
              # columns() call a few lines below then fails on, since
              # dplyr::mutate() cannot operate on a dataframe with
              # duplicate column names. Drop the placeholder first (but
              # only when raw_col isn't already the same column under the
              # same name, e.g. a dataset whose raw data already used
              # "year" directly).
              if (target_col != raw_col && target_col %in% base::names(working_df)) {
                working_df[[target_col]] <- NULL
              }
              base::names(working_df)[base::names(working_df) == raw_col] <- target_col
            }
          }
        }

        # Re-resolve date/time now that manual mappings (if any) have been
        # applied, so a manually-mapped date column actually gets parsed
        # rather than leaving year/month/day at whatever value the earlier
        # auto-detection pass produced.
        working_df <- resolve_datetime_columns(working_df, rename_map)

        ds$data <- working_df; mapped_staging[[ds_name]] <- ds
      }
      rv$staging_datasets <- mapped_staging; rv$datasets_processed <- rv$staging_datasets

      rv$config <- update_config_with_new_aliases(rv$config, rv$datasets_processed)

      if (base::exists("save_config") && base::is.function(save_config)) {
        base::tryCatch({
          target_path <- if (base::exists("CONFIG_SESSION_PATH")) CONFIG_SESSION_PATH else "config_session.yaml"
          save_config(rv$config, target_path)
          .log_event("CONFIG", "Alias mapping successfully written to local session config.")
        }, error = function(e) { .log_event("WARNING", base::paste("Failed to save alias config:", e$message)) })
      }

      rv$step_status <- mark_step_available(rv$step_status, "step3")
      rv$step_status <- invalidate_from_step(rv$step_status, "step4")
      rv$report_data$step3 <- build_column_mapping_log(rv$staging_datasets, rv$config)

      shiny::showNotification("Mappings saved! Proceed to Step 4.", type = "message", duration = 8)
      .update_workflow_state("step4")
      shiny::updateTabsetPanel(session = session_parent, inputId = "main_navbar", selected = "step4")
    }

    # --- 2. DYNAMIC WIZARD LOGIC ---
    # "date" is offered alongside year/month/day, never instead of them:
    # some datasets have one, some have the other, some (rarely) have both
    # duplicated across columns, and the user should be free to fill in
    # whichever path matches their actual file.
    wizard_keys_dynamic <- shiny::reactive({
      shiny::req(rv$config)
      keys <- base::c(rv$config$general$essential_pigments, "Tchla")
      if (base::isTRUE(rv$config$filtering$geospatial$enabled)) keys <- base::c(keys, "latitude", "longitude")
      if (base::isTRUE(rv$config$filtering$temporal$enabled)) keys <- base::c(keys, "date", "year", "month", "day")
      if (base::isTRUE(rv$config$filtering$depth$enabled)) keys <- base::c(keys, "depth")
      base::unique(keys)
    })

    # Blocker keys drive the Save-button gate. "date_time_info" is a status
    # marker, not a literal mapping target, matching the same marker
    # .get_mapping_status() in backend_helpers.R already produces when
    # neither date route is satisfied. Individual date/year/month/day keys
    # are deliberately NOT listed here: no single one of them is mandatory
    # on its own, only the combined "some valid route exists" requirement
    # is, and that's enforced explicitly in .apply_mappings_safe() below.
    blocker_keys_dynamic <- shiny::reactive({
      shiny::req(rv$config)
      keys <- base::c("Tchla")
      if (base::isTRUE(rv$config$filtering$geospatial$enabled)) keys <- base::c(keys, "latitude", "longitude")
      if (base::isTRUE(rv$config$filtering$temporal$enabled)) keys <- base::c(keys, "date_time_info")
      if (base::isTRUE(rv$config$filtering$depth$enabled)) keys <- base::c(keys, "depth")
      base::unique(keys)
    })

    .display_label <- function(key) {
      pn <- pigment_display_name(key, rv$config)
      if (!base::identical(pn, key)) base::paste0(pn, " (", key, ")") else key
    }

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
    }, server = FALSE)

    shiny::observeEvent(input$mapping_validation_summary_table_rows_selected, {
      shiny::req(mapping_summary())
      selected_row <- input$mapping_validation_summary_table_rows_selected
      if (base::length(selected_row) == 0) return()
      dataset_name <- mapping_summary()$Dataset[selected_row]
      rv$current_mapping_dataset <- dataset_name
      .show_mapping_modal(dataset_name)
    })

    # --- FULL MAPPING MODAL: assigned + missing, missing first ---
    .show_mapping_modal <- function(dataset_name) {
      ds_obj <- rv$staging_datasets[[dataset_name]]
      shiny::req(ds_obj)

      all_keys <- wizard_keys_dynamic()
      rename_map <- ds_obj$rename_map %||% base::list()
      blockers <- blocker_keys_dynamic()
      date_keys <- base::c("date", "year", "month", "day")
      temporal_on <- base::isTRUE(rv$config$filtering$temporal$enabled)
      date_currently_satisfied <- .date_satisfied(base::names(rename_map))

      key_status <- base::vapply(all_keys, function(k) {
        if (k %in% base::names(rename_map) && base::nzchar(rename_map[[k]] %||% "")) "assigned" else "missing"
      }, character(1))

      ordered_keys <- base::c(all_keys[key_status == "missing"], all_keys[key_status == "assigned"])

      blocker_alerts <- base::list()
      if ("depth" %in% ordered_keys[key_status[ordered_keys] == "missing"] && base::isTRUE(rv$config$filtering$depth$enabled)) blocker_alerts <- base::c(blocker_alerts, "Depth is required because the Depth Filter is enabled in Step 1.")
      if (base::any(base::c("latitude", "longitude") %in% ordered_keys[key_status[ordered_keys] == "missing"]) && base::isTRUE(rv$config$filtering$geospatial$enabled)) blocker_alerts <- base::c(blocker_alerts, "Lat/Lon are required because the Location Filter is enabled in Step 1.")
      if (temporal_on && !date_currently_satisfied) blocker_alerts <- base::c(blocker_alerts, "Date information is required because the Date Filter is enabled in Step 1. Map EITHER the single 'date' field below OR all three of Year/Month/Day, whichever matches your file. You don't need both.")

      ui_header <- if (base::length(blocker_alerts) > 0) {
        shiny::div(class = "alert alert-danger", style = "border: 2px solid #a94442;", shiny::h4("Mandatory mapping required", style="font-weight:bold; text-align: center; font-size: 1.2em;"), shiny::p("Active filters from Step 1 require these columns; they cannot be left unmapped."), shiny::tags$ul(base::lapply(blocker_alerts, shiny::tags$li)), shiny::hr(), shiny::p(shiny::strong("If this file doesn't have this data:"), " go back to Step 1 and disable the relevant filter."))
      } else {
        shiny::div(class = "alert alert-info", shiny::p("Every variable the app uses is listed below. Missing items need attention first; assigned items are shown so you can double-check or override them."))
      }

      judgement_note <- shiny::div(class = "judgement-note", shiny::icon("compass"),
        " phytoclassShiny can match column names for you; it can't tell whether these are the right pigments and groups for your study system. That part's still yours.")

      date_note <- if (temporal_on && base::any(date_keys %in% ordered_keys)) {
        shiny::div(class = "alert alert-secondary", style = "font-size:0.85em; padding:8px 10px;",
                   shiny::icon("calendar-days"),
                   " If your file has one combined date column, map it to ", shiny::strong("date"), " only. If it has separate columns, map ", shiny::strong("Year"), ", ", shiny::strong("Month"), ", and ", shiny::strong("Day"), " instead. Either is fine; you don't need to fill in both.")
      }

      row_ui <- base::lapply(ordered_keys, function(key) {
        base::local({
          this_key <- key
          # None of the four date-related keys are individually forced (see
          # blocker_keys_dynamic()'s comment above), so "Leave Unmapped"
          # stays available for each of them; the combined requirement is
          # enforced once, at save time, in .apply_mappings_safe().
          is_blocker <- (this_key %in% blockers) && !(this_key %in% date_keys)
          current_col <- rename_map[[this_key]] %||% ""
          choice_names <- ds_obj$cleaned_colnames
          choices <- if (is_blocker) {
            stats::setNames(choice_names, choice_names)
          } else {
            base::c("Leave Unmapped" = "", stats::setNames(choice_names, choice_names))
          }

          shiny::div(
            style = "border-bottom: 1px solid #eee; padding: 8px 0;",
            shiny::selectInput(
              inputId = ns(base::paste0("map_", this_key)),
              label = base::paste0(.display_label(this_key), if (is_blocker) " *required*" else if (this_key %in% date_keys) " (date/time)" else ""),
              choices = choices,
              selected = if (base::nzchar(current_col)) current_col else "",
              width = "100%"
            ),
            shiny::div(id = ns(base::paste0("preview_wrap_", this_key)), style = "margin-top:-8px; margin-bottom:6px;",
                       shiny::tags$small(shiny::textOutput(ns(base::paste0("preview_", this_key)), inline = TRUE), style = "color:#0E6E8C; font-family:monospace;")
            )
          )
        })
      })

      for (key in ordered_keys) {
        base::local({
          this_key <- key
          input_id <- base::paste0("map_", this_key)
          output_id <- base::paste0("preview_", this_key)
          output[[output_id]] <- shiny::renderText({
            selected_col <- input[[input_id]]
            if (base::is.null(selected_col) || !base::nzchar(selected_col)) return("No column selected.")
            raw_vals <- ds_obj$data_original[[selected_col]] %||% ds_obj$data[[selected_col]]
            if (base::is.null(raw_vals)) return("")
            preview_vals <- utils::head(raw_vals[!base::is.na(raw_vals) & base::nzchar(base::as.character(raw_vals))], 5)
            base::paste0("Preview: ", base::paste(preview_vals, collapse = ", "))
          })
        })
      }

      shiny::showModal(shiny::modalDialog(
        title = shiny::div(shiny::icon("magic"), base::paste("Mapping Wizard:", dataset_name)),
        size = "l", easyClose = FALSE,
        footer = shiny::tagList(
          shiny::modalButton("Cancel", shiny::icon("times")),
          shiny::actionButton(ns("commit_modal_mappings_btn"), "Apply Selected", class = "btn-primary", icon = shiny::icon("check"))
        ),
        ui_header, judgement_note, date_note, shiny::hr(), row_ui,
        shiny::hr(),
        shiny::div(style = "display: flex; align-items: center; white-space: nowrap;",
          shiny::checkboxInput(ns("apply_to_all_similar_modal"), "Apply to other datasets with the same missing keys", value = TRUE, width = "100%")
        )
      ))
    }

    shiny::observeEvent(input$commit_modal_mappings_btn, {
      shiny::req(rv$current_mapping_dataset)
      shinybusy::show_modal_spinner(text = "Applying...")
      base::tryCatch({
        result <- .apply_mappings_safe(dataset_name = rv$current_mapping_dataset, apply_to_similar = base::isTRUE(input$apply_to_all_similar_modal))
        shinybusy::remove_modal_spinner()
        if (base::isTRUE(result$filter_blocked)) {
          .show_filter_blocked_modal(result$missing)
        } else if (base::isTRUE(result$success)) {
          rv$mapping_trigger <- rv$mapping_trigger + 1; shiny::removeModal(); rv$current_mapping_dataset <- NULL
        }
      }, error = function(e) { shinybusy::remove_modal_spinner(); .log_event(base::paste("ERROR:", e$message)); shiny::showNotification(e$message, type = "error", duration = 8)
      })
    })

    # Shown when a filter (location, date, or depth) is switched on in
    # Step 1 but the dataset genuinely has no column that can satisfy it,
    # neither auto-detected nor manually mapped. There's nothing left to
    # try in this wizard at that point; the actual fix is either going
    # back to Step 1 to turn the filter off, or supplying the missing
    # column, so the two response options reflect exactly those choices.
    .show_filter_blocked_modal <- function(missing_keys) {
      filter_labels <- base::unique(base::vapply(missing_keys, function(k) {
        base::switch(k, "latitude" = "location", "longitude" = "location", "depth" = "depth", "date_time_info" = "date/time", k)
      }, character(1)))

      shiny::showModal(shiny::modalDialog(
        title = NULL, size = "m", easyClose = FALSE,
        style = "background-color: #1B2A2E; color: #E8E3D3; border: 2px solid #D98C3D; border-radius: 10px;",
        shiny::div(style = "text-align: center; padding: 6px 4px;",
          shiny::icon("hat-wizard", style = "font-size: 2.6em; color: #D98C3D; margin-bottom: 8px;"),
          shiny::h3("YOU CANNOT PASS!", style = "color: #E8E3D3; font-weight: 800; letter-spacing: 0.03em; margin-bottom: 14px;"),
          shiny::p(style = "font-size: 1.02em;",
            base::sprintf("The %s data you're trying to filter by isn't available in this dataset, and no column has been mapped to it.",
                          base::paste(filter_labels, collapse = " / "))),
          shiny::p(style = "opacity: 0.85; font-size: 0.92em;",
            "Go back to Setup and disable that filter, or return to the wizard and map the missing column.")
        ),
        footer = shiny::tagList(
          shiny::actionButton(ns("filter_blocked_stay_btn"), "Stay and Fix Mapping", class = "btn-outline-light"),
          shiny::actionButton(ns("filter_blocked_goto_setup_btn"), "Go Back to Setup", class = "btn-warning")
        )
      ))
    }

    shiny::observeEvent(input$filter_blocked_stay_btn, {
      shiny::removeModal()
      shiny::req(rv$current_mapping_dataset)
      .show_mapping_modal(rv$current_mapping_dataset)
    })

    shiny::observeEvent(input$filter_blocked_goto_setup_btn, {
      shiny::removeModal()
      rv$current_mapping_dataset <- NULL
      shiny::updateTabsetPanel(session = session_parent, inputId = "main_navbar", selected = "step1")
    })

    .apply_mappings_safe <- function(dataset_name, apply_to_similar) {
      ds_obj <- rv$staging_datasets[[dataset_name]]
      all_keys <- wizard_keys_dynamic()
      blockers <- blocker_keys_dynamic()
      new_mappings <- base::list()

      for (key in all_keys) {
        user_choice <- input[[base::paste0("map_", key)]]
        if (!base::is.null(user_choice) && user_choice != "") new_mappings[[key]] <- user_choice
      }

      # If year, month, and day were all pointed at the exact same raw
      # column, this is a combined date column, not three independently
      # duplicated ones. Collapse it to a single "date" mapping before the
      # general duplicate check below ever sees it, so this specific,
      # legitimate pattern never raises a conflict.
      ymd_keys <- base::c("year", "month", "day")
      if (base::all(ymd_keys %in% base::names(new_mappings))) {
        ymd_cols <- base::unlist(new_mappings[ymd_keys])
        if (base::length(base::unique(ymd_cols)) == 1 && base::nzchar(ymd_cols[1])) {
          new_mappings[ymd_keys] <- NULL
          new_mappings[["date"]] <- ymd_cols[[1]]
          .log_event("INFO", base::sprintf("'%s': Year/Month/Day were all mapped to the same column ('%s'); treating it as a single combined date column.", dataset_name, ymd_cols[[1]]))
        }
      }

      # Tchla is unconditionally required for any analysis. Location, date,
      # and depth are only required because a filter for them was switched
      # on in Step 1, if that filter's data still isn't available after
      # this mapping attempt, that's a different situation worth a more
      # direct prompt than a plain validation error: the fix isn't "map it
      # correctly", it's "this dataset doesn't have this data at all, go
      # disable the filter or supply the column."
      non_date_blockers <- base::setdiff(blockers, "date_time_info")
      still_missing_blockers <- base::setdiff(non_date_blockers, base::names(new_mappings))

      if ("date_time_info" %in% blockers) {
        existing_keys <- base::names(ds_obj$rename_map %||% base::list())
        combined_keys <- base::union(existing_keys, base::names(new_mappings))
        if (!.date_satisfied(combined_keys)) still_missing_blockers <- base::c(still_missing_blockers, "date_time_info")
      }

      filter_driven_missing <- base::intersect(still_missing_blockers, base::c("latitude", "longitude", "depth", "date_time_info"))
      if (base::length(filter_driven_missing) > 0) {
        return(base::list(success = FALSE, filter_blocked = TRUE, missing = filter_driven_missing))
      }

      if (base::length(still_missing_blockers) > 0) {
        readable <- base::vapply(still_missing_blockers, function(k) .display_label(k), character(1))
        base::stop(base::paste0("Cannot save: the following required columns still need mapping: ", base::paste(readable, collapse = ", "), "."))
      }

      if (base::length(new_mappings) == 0) return(base::list(success = TRUE, filter_blocked = FALSE))
      chosen_raw_cols <- base::unlist(new_mappings)
      if (base::any(base::duplicated(chosen_raw_cols))) base::stop("Mapping Conflict: You assigned multiple standard targets to the exact same raw column. Each variable must be mapped uniquely (year/month/day pointed at one shared column is handled automatically and won't trigger this).")

      temp_staging <- rv$staging_datasets
      for(key in base::names(new_mappings)) {
        temp_staging[[dataset_name]]$rename_map[[key]] <- new_mappings[[key]]
        # Every key set through this wizard is, by definition, a human
        # decision, not an alias match, so it's recorded as "manual" here
        # regardless of whether the value happens to match what auto-
        # detection would have guessed anyway.
        temp_staging[[dataset_name]]$rename_source[[key]] <- "manual"
      }

      if(apply_to_similar){
        missing_before <- base::setdiff(all_keys, base::names(ds_obj$rename_map %||% base::list()))
        similar_datasets <- .find_similar_datasets(dataset_name, missing_before)
        for(sim_ds in similar_datasets) for(key in base::names(new_mappings)) {
          temp_staging[[sim_ds]]$rename_map[[key]] <- new_mappings[[key]]
          # Propagated to another dataset via the "apply to similar" option,
          # rather than chosen for it directly, but it's still a value a
          # human confirmed rather than one auto-detection produced, so it
          # is tagged manual here too rather than silently defaulting back
          # to "auto" for datasets that received it this way.
          temp_staging[[sim_ds]]$rename_source[[key]] <- "manual"
        }
      }

      rv$mapping_history[[base::length(rv$mapping_history) + 1]] <- purrr::map(rv$staging_datasets, rlang::duplicate)
      rv$staging_datasets <- temp_staging
      return(base::list(success = TRUE, filter_blocked = FALSE))
    }

    .find_similar_datasets <- function(ref, missing) { base::names(rv$staging_datasets) |> purrr::keep(function(n) { if(n==ref) return(FALSE); s <- .get_mapping_status(rv$staging_datasets[[n]], wizard_keys_dynamic()); base::identical(base::sort(s$Missing[[1]]), base::sort(missing)) }) }

    shiny::observeEvent(input$rollback_mappings_btn, { if (base::length(rv$mapping_history) > 0) { last_idx <- base::length(rv$mapping_history); rv$staging_datasets <- rv$mapping_history[[last_idx]]; rv$mapping_history[[last_idx]] <- NULL; rv$mapping_trigger <- rv$mapping_trigger + 1 } })

    shiny::observe({
      shiny::req(mapping_summary())
      has_blocker_missing <- base::any(base::sapply(mapping_summary()$Missing, function(m) base::any(m %in% blocker_keys_dynamic())))
      if (!has_blocker_missing) shinyjs::enable("commit_all_mappings_btn") else shinyjs::disable("commit_all_mappings_btn")
    })

    shiny::observeEvent(input$commit_all_mappings_btn, {
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
        shiny::showModal(shiny::modalDialog(title = shiny::div(shiny::icon("exclamation-triangle", class = "text-warning"), "Resolution Capabilities Warning"), shiny::div(class = "alert alert-warning", "Some datasets are missing pigments found in your Fm matrix. The following groups will default to 0 unless you go back and map them:"), shiny::div(style = "max-height: 300px; overflow-y: auto;", warning_ui), footer = shiny::tagList(shiny::modalButton("Go Back"), shiny::actionButton(ns("force_commit_btn"), "Acknowledge & Proceed", class = "btn-warning"))))
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
