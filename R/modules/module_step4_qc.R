# ============================================================================
# MODULE: Step 4 - Filter & Clean
# Handles automated quality control and data filtering, applying the rules
# configured in Step 1 to a working copy of each dataset (the original
# import is never modified).
#
# Hooked into the shared report-state system (mark_step_available /
# invalidate_from_step), so re-running QC after a change correctly
# invalidates Step 5 onward for both the tab-lock mechanism and the
# Download Report toggles. Geospatial and depth filters read live from
# rename_map; temporal filtering depends on resolve_datetime_columns()
# (backend_helpers.R) having already populated year/month/day correctly.
# ============================================================================

qcUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h3("Step 4: Filter & Clean"),
    shiny::p("Review the automated quality control and filtering results before grouping."),
    shiny::hr(),

    shiny::fluidRow(
      shiny::column(4,
                    bslib::card(
                      bslib::card_header(shiny::icon("shield-alt"), " Execute Quality Control"),
                      bslib::card_body(
                        shiny::p("Apply the cleaning rules and data filters configured in Step 1."),
                        shiny::uiOutput(ns("active_toggles_ui")),
                        shiny::actionButton(ns("run_qc_btn"), "Clean My Data", class = "btn-primary w-100 fw-bold", icon = shiny::icon("magic"))
                      )
                    )
      ),
      shiny::column(8,
                    bslib::card(
                      bslib::card_header("Quality Control & Filtering Breakdown"),
                      bslib::card_body(DT::DTOutput(ns("qc_summary_table")))
                    )
      )
    )
  )
}

qcServer <- function(id, rv, .log_event, .update_workflow_state, reset_downstream_data, session_parent) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    `%||%` <- function(a, b) if (!base::is.null(a)) a else b

    # Reads session_parent$input directly, not rv$config - shows the
    # checkboxes' true current state on screen, the same source of truth
    # the sync fix below now uses right before running, so this preview
    # can never drift from what a click on "Clean My Data" will actually
    # do (previously, the only way to see whether a toggle "took" was to
    # save it and read the resulting QC log after the fact).
    output$active_toggles_ui <- shiny::renderUI({
      .chip <- function(label, active) {
        shiny::tags$span(class = base::paste("badge me-1 mb-2", if (base::isTRUE(active)) "bg-success" else "bg-secondary"),
                          style = "font-size: 0.8em;", if (base::isTRUE(active)) shiny::icon("check") else shiny::icon("xmark"), " ", label)
      }
      pin <- session_parent$input
      shiny::div(class = "mb-3",
        shiny::div(class = "text-muted small mb-1", "Cleaning rules that will run:"),
        .chip("Duplicates", pin$toggle_handle_duplicates),
        .chip("Blanks -> 0", pin$toggle_handle_nas),
        .chip("Negatives -> 0", pin$toggle_handle_negatives),
        .chip("Empty Samples", pin$toggle_handle_zerosum),
        shiny::div(class = "text-muted small mt-2 mb-1", "Filters that will run:"),
        .chip("Location", pin$toggle_geo_filter),
        .chip("Date", pin$toggle_temporal_filter),
        .chip("Depth", pin$toggle_depth_filter)
      )
    })

    shiny::observeEvent(input$run_qc_btn, {
      if (base::length(rv$staging_datasets) == 0) {
        shiny::showNotification("No data available to clean. Please complete Step 3.", type = "warning")
        return()
      }

      .log_event("QC", "Initiating Quality Control pipeline...")
      shinybusy::show_modal_spinner(text = "Applying cleaning rules and filters...")

      # The four cleaning checkboxes (and the filter toggles) live on Step
      # 1's top-level UI, un-namespaced, while this is a module with its
      # own namespaced `input`. Previously, rv$config$data_cleaning was
      # only ever refreshed from those checkboxes by sync_config_with_ui(),
      # which was called ONLY from the "Save Configuration" and "Load Fm
      # Matrix Files" button handlers in app.R - never from here. That
      # meant checking a box in the UI changed nothing until (and unless)
      # the user separately clicked "Save Configuration" first; clicking
      # "Run QC" ran against whatever rv$config held from before, silently
      # ignoring the checkboxes on screen. Syncing here, right before
      # reading cfg_clean/cfg_filt, makes the visible checkbox state
      # authoritative at the moment it actually matters.
      rv$config <- sync_config_with_ui(rv$config, session_parent$input)

      temp_qc_list <- base::list()
      summary_rows <- base::list()

      cfg_clean <- rv$config$data_cleaning
      cfg_filt <- rv$config$filtering

      for (ds_name in base::names(rv$staging_datasets)) {
        ds <- rv$staging_datasets[[ds_name]]
        df <- ds$data
        rename_map <- ds$rename_map

        initial_n <- base::nrow(df)

        # target_cols is the set of columns the rest of this function treats
        # as pigment data -- duplicates are matched on it, NAs/negatives are
        # cleaned on it, and the zero-pigment-sum row check sums across it.
        # It used to be defined as "every column the metadata regex doesn't
        # catch" (colnames(df) minus a protected_cols list). That's wrong
        # for any real dataset that carries columns beyond what this app
        # recognizes as a pigment: a merged database export (e.g. MAREDAT)
        # routinely has dozens of extra fields - record/event/bottle index
        # numbers, derived pigment sums, free-text provenance columns - that
        # the metadata regex was never written to catch, because it isn't a
        # list of pigments, it's a guess at what ISN'T metadata. Those extra
        # columns got summed into the zero-pigment-sum check right alongside
        # real pigments, so a row with e.g. Database_num = 33848 and every
        # actual pigment at 0 summed to 33848, not 0, and the "remove empty
        # samples" check never fired no matter how many samples were
        # genuinely empty. It also meant duplicate-matching and the NA/
        # negative cleanup were silently running against irrelevant numeric
        # columns (and coercing free-text columns to NA then 0).
        #
        # target_cols is now built the same way Step 3's wizard and Step 6's
        # run_phytoclass_analysis() already resolve pigment columns: from
        # the app's own recognized list (config$general$essential_pigments
        # + "Tchla"), resolved to this dataset's actual column name via
        # rename_map first, falling back to a literal name match for data
        # that's already been through Step 3's rename step.
        essential_keys <- base::c(rv$config$general$essential_pigments, "Tchla")
        target_cols <- base::character(0)
        for (key in essential_keys) {
          if (key %in% base::colnames(df)) {
            target_cols <- base::c(target_cols, key)
          } else if (!base::is.null(rename_map[[key]]) && rename_map[[key]] %in% base::colnames(df)) {
            target_cols <- base::c(target_cols, rename_map[[key]])
          }
        }
        target_cols <- base::unique(target_cols)

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

        # 3. Handle Negatives
        if (base::isTRUE(cfg_clean$enforce_non_negative_pigments$enabled)) {
          df <- df |>
            dplyr::mutate(dplyr::across(dplyr::any_of(target_cols), ~ base::ifelse(base::is.numeric(.x) & .x < 0, 0, .x)))
        }

        # 4. Filter empty samples
        if (base::isTRUE(cfg_clean$handle_zero_pigment_sum$enabled) && base::length(target_cols) > 0) {
          num_df <- df[, target_cols, drop = FALSE] |> dplyr::mutate(dplyr::across(dplyr::everything(), ~ base::suppressWarnings(base::as.numeric(.x))))
          row_sums <- base::rowSums(num_df, na.rm = TRUE)
          df <- df[row_sums > 0, ]
        }
        n_after_zero <- base::nrow(df)
        dropped_zero <- n_after_dup - n_after_zero

        # 5. Geo Filter
        n_before_geo <- base::nrow(df)
        if (base::isTRUE(cfg_filt$geospatial$enabled) && !base::is.null(rename_map$latitude) && !base::is.null(rename_map$longitude)) {
          lat_col <- if ("latitude" %in% base::colnames(df)) "latitude" else rename_map$latitude
          lon_col <- if ("longitude" %in% base::colnames(df)) "longitude" else rename_map$longitude

          min_lat <- base::as.numeric(rv$config$filtering$geospatial$min_latitude %||% -90)
          max_lat <- base::as.numeric(rv$config$filtering$geospatial$max_latitude %||% 90)
          min_lon <- base::as.numeric(rv$config$filtering$geospatial$min_longitude %||% -180)
          max_lon <- base::as.numeric(rv$config$filtering$geospatial$max_longitude %||% 180)

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
            # optional = TRUE matters here, not just suppressWarnings(): if
            # every row in a dataset fails every candidate date format (e.g.
            # a file with no "day" ever populated, or - as surfaced by a
            # stress-test file built specifically to hit this - every row
            # malformed at once), as.Date.character() does not return an
            # all-NA vector, it throws a hard error ("character string is
            # not in a standard unambiguous format"). suppressWarnings()
            # cannot catch an error, only a warning, so that case crashed
            # this observeEvent outright. optional = TRUE tells as.Date() to
            # return NA in that total-failure case too, instead of erroring.
            dplyr::mutate(parsed_date_tmp = base::suppressWarnings(base::as.Date(base::paste(year, month, day, sep="-"), optional = TRUE))) |>
            dplyr::filter(!base::is.na(parsed_date_tmp) & parsed_date_tmp >= start_date & parsed_date_tmp <= end_date) |>
            dplyr::select(-parsed_date_tmp)
        }
        dropped_temp <- n_before_temp - base::nrow(df)

        # 7. Depth Filter
        n_before_depth <- base::nrow(df)
        if (base::isTRUE(cfg_filt$depth$enabled) && !base::is.null(rename_map$depth)) {
          depth_col <- if ("depth" %in% base::colnames(df)) "depth" else rename_map$depth
          min_depth <- base::as.numeric(rv$config$filtering$depth$min_depth %||% 0)
          max_depth <- base::as.numeric(rv$config$filtering$depth$max_depth %||% 10000)

          df <- df |> dplyr::filter(base::as.numeric(.data[[depth_col]]) >= min_depth & base::as.numeric(.data[[depth_col]]) <= max_depth)
        }
        dropped_depth <- n_before_depth - base::nrow(df)

        final_n <- base::nrow(df)

        if (initial_n > 0 && base::isTRUE(cfg_filt$temporal$enabled) && dropped_temp == n_before_temp && n_before_temp > 0) {
          .log_event("WARNING", base::sprintf("Dataset '%s': the date filter removed ALL %d remaining samples. If this is unexpected, confirm the date columns were actually mapped in Step 3 (not left as NA).", ds_name, n_before_temp))
        }

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

      rv$step_status <- mark_step_available(rv$step_status, "step4")
      rv$step_status <- invalidate_from_step(rv$step_status, "step5")
      rv$report_data$step4 <- rv$qc_summary_df

      .update_workflow_state("step5")

      shinybusy::remove_modal_spinner()
      shiny::showNotification("Quality Control & Filtering complete.", type = "message")
    })

    output$qc_summary_table <- DT::renderDT({
      shiny::req(rv$qc_summary_df)
      DT::datatable(rv$qc_summary_df, rownames = FALSE, options = base::list(pageLength = 10, scrollX = TRUE, searching = FALSE, lengthChange = FALSE)) |>
        DT::formatStyle("Final Samples", fontWeight = "bold", color = "#3F7D4F")
    }, server = FALSE)

  })
}
