# ============================================================================
# PROGRESSIVE REPORT BUILDER
#
# Implements the "Download Report" system: as each step completes, its
# output becomes an available, toggleable section of a single downloadable
# report. Sections for steps not yet reached, or whose output has gone
# stale because an upstream step changed (see config_manager.R's
# invalidate_from_step()), are shown but disabled.
#
# The step7 ("Final Package") section covers everything a dedicated final
# export needs: a categorised parameter sheet, per-group Community
# Estimates and F-Matrix sheets, PNG plots, and raw phytoclass output.
# Every other section shares the same category-header styling rather than
# that polish being unique to one section.
#
# Because PNGs and raw .rds files can't live inside a spreadsheet, a
# report that includes step5 (clustering diagnostics) or step7 (the final
# package) needs more than a spreadsheet to hold everything. Every
# download therefore consistently produces a single .zip file containing
# the workbook and, when relevant, Plots/ and Raw_Output/ subfolders, so
# the interface behaves the same way regardless of which sections happen
# to be selected.
# ============================================================================

REPORT_SECTION_LABELS <- base::list(
  setup_information    = "Setup Information (Step 1)",
  datasets_loaded       = "Datasets Loaded (Step 2)",
  column_mapping_log    = "Column Mapping Log (Step 3)",
  quality_control_log   = "Quality Control & Filtering (Step 4)",
  grouping_and_clustering = "Grouping & Clustering, incl. diagnostic plots (Step 5)",
  analysis_results      = "Analysis Results Summary (Step 6)",
  final_visualizations  = "Final Package: full results, plots & raw output (Step 7)"
)

STEP_TO_SECTION <- base::list(
  step1 = "setup_information", step2 = "datasets_loaded", step3 = "column_mapping_log",
  step4 = "quality_control_log", step5 = "grouping_and_clustering",
  step6 = "analysis_results", step7 = "final_visualizations"
)

`%||%` <- function(a, b) if (!base::is.null(a)) a else b

report_section_status <- function(step_status) {
  rows <- base::lapply(base::names(STEP_TO_SECTION), function(step) {
    section_key <- STEP_TO_SECTION[[step]]
    state <- step_status[[step]] %||% "not_reached"
    base::data.frame(
      step = step,
      section_key = section_key,
      label = REPORT_SECTION_LABELS[[section_key]],
      state = state,
      available = base::identical(state, "available"),
      message = base::switch(state,
        "available"   = "Ready to include",
        "stale"       = "Out of date, an earlier step changed since this ran. Re-run this step to refresh it.",
        "not_reached" = "Not completed yet in this session",
        "Not completed yet in this session"
      ),
      stringsAsFactors = FALSE
    )
  })
  base::do.call(base::rbind, rows)
}

render_report_toggle_ui <- function(ns, step_status, current_selection = NULL) {
  status_df <- report_section_status(step_status)
  current_selection <- current_selection %||% base::character(0)

  rows <- base::lapply(base::seq_len(base::nrow(status_df)), function(i) {
    r <- status_df[i, ]
    is_available <- base::isTRUE(r$available)
    checkbox_id <- ns(base::paste0("report_include_", r$section_key))

    shiny::div(
      class = if (is_available) "mb-2" else "mb-2 text-muted",
      style = if (!is_available) "opacity: 0.55;" else "",
      shiny::tagList(
        shinyjs::disabled(
          shiny::checkboxInput(
            checkbox_id,
            label = r$label,
            value = is_available && (r$section_key %in% current_selection || base::length(current_selection) == 0)
          )
        ),
        if (!is_available) shiny::tags$small(shiny::icon("circle-info"), base::paste0(" ", r$message), style = "display:block; margin-left: 24px; margin-top: -8px;")
      )
    )
  })

  shiny::tagList(
    shiny::tags$script(shiny::HTML(base::paste0(
      "setTimeout(function(){",
      base::paste(base::sprintf(
        "$('#%s').prop('disabled', false);",
        ns(base::paste0("report_include_", status_df$section_key[status_df$available]))
      ), collapse = ""),
      "}, 50);"
    ))),
    shiny::tagList(rows)
  )
}

# ----------------------------------------------------------------------------
# Internal helpers shared by every section writer below.
# ----------------------------------------------------------------------------

.add_plain_sheet <- function(wb, header_style, sheet_name, df) {
  if (base::is.null(df) || base::nrow(df) == 0) return(invisible(NULL))
  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeData(wb, sheet_name, df)
  openxlsx::addStyle(wb, sheet_name, header_style, rows = 1, cols = 1:base::ncol(df))
  openxlsx::setColWidths(wb, sheet_name, cols = 1:base::ncol(df), widths = "auto")
}

# Setup Information uses the same categorised look as the final package's
# own parameter sheet, so this styling isn't exclusive to one section.
.add_categorized_sheet <- function(wb, header_style, sheet_name, df, category_col = 1) {
  if (base::is.null(df) || base::nrow(df) == 0) return(invisible(NULL))
  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeData(wb, sheet_name, df)
  openxlsx::addStyle(wb, sheet_name, header_style, rows = 1, cols = 1:base::ncol(df))
  openxlsx::setColWidths(wb, sheet_name, cols = 1:base::ncol(df), widths = "auto")
  cat_rows <- base::which(base::startsWith(base::as.character(df[[category_col]]), "---")) + 1
  if (base::length(cat_rows) > 0) {
    cat_style <- openxlsx::createStyle(textDecoration = "bold", fontColour = "#0056b3")
    for (r in cat_rows) { openxlsx::addStyle(wb, sheet_name, cat_style, rows = r, cols = category_col) }
  }
}

.add_mapping_log_sheet <- function(wb, header_style, sheet_name, log_result) {
  if (base::is.data.frame(log_result)) log_result <- base::list(table = log_result, manual_cells = NULL)
  if (base::is.null(log_result) || base::is.null(log_result$table) || base::nrow(log_result$table) == 0) return(invisible(NULL))
  df <- log_result$table
  manual_cells <- log_result$manual_cells

  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeData(wb, sheet_name, df)
  openxlsx::addStyle(wb, sheet_name, header_style, rows = 1, cols = 1:base::ncol(df))
  openxlsx::setColWidths(wb, sheet_name, cols = 1:base::ncol(df), widths = "auto")

  manual_style <- openxlsx::createStyle(fgFill = "#FFF3CD")
  if (!base::is.null(manual_cells)) {
    for (r in base::seq_len(base::nrow(manual_cells))) {
      for (c in base::seq_len(base::ncol(manual_cells))) {
        if (base::isTRUE(manual_cells[r, c])) {
          openxlsx::addStyle(wb, sheet_name, manual_style, rows = r + 1, cols = c + 1, stack = TRUE)
        }
      }
    }
  }

  legend_row <- base::nrow(df) + 3
  openxlsx::writeData(wb, sheet_name, base::data.frame(Legend = "Highlighted cells (and a trailing '*') were mapped manually in Step 3; plain cells were matched automatically from known aliases. '(not mapped)' means that variable was not present or not required for that dataset."), startRow = legend_row, colNames = FALSE)
}

# ----------------------------------------------------------------------------
# Single source of truth for "everything about one analysis group." Called
# from both the master report's per-group sheets and the standalone
# per-group workbook (.write_standalone_group_workbook, below) - never
# computed independently in two places, which is exactly the pattern that
# caused the QC-sheet duplication this was written to replace.
#
# Works identically whether the group succeeded or failed: result_est/
# result_fmat are NULL on failure (nothing to show), but run_parameters and
# diagnosis are always populated, since "what were the settings" and
# "what happened, including the actual error text" are useful in both
# cases - a clean success is worth confirming at a glance too, not just a
# failure worth explaining.
# ----------------------------------------------------------------------------
.build_group_content <- function(ds_name, ds, config) {
  log <- ds$log_analyzer %||% base::list()
  status <- log$status %||% "Not Run"
  succeeded <- base::identical(status, "Success")

  qc_rules <- base::c(
    if (base::isTRUE(config$data_cleaning$handle_duplicates$enabled)) "Duplicates",
    if (base::isTRUE(config$data_cleaning$handle_pigment_nas$enabled)) "NAs",
    if (base::isTRUE(config$data_cleaning$enforce_non_negative_pigments$enabled)) "Negatives",
    if (base::isTRUE(config$data_cleaning$handle_zero_pigment_sum$enabled)) "Empty Samples"
  )
  filters <- base::c(
    if (base::isTRUE(config$filtering$geospatial$enabled)) "Location",
    if (base::isTRUE(config$filtering$temporal$enabled)) "Date",
    if (base::isTRUE(config$filtering$depth$enabled)) "Depth"
  )
  seed_text <- if (base::isTRUE(base::as.logical(config$phytoclass$use_fixed_seed))) base::as.character(config$phytoclass$fixed_seed) else "Unconstrained (not reproducible run-to-run)"
  mm_text <- if (base::isTRUE(config$phytoclass$use_custom_minmax)) (config$phytoclass$selected_minmax_file %||% "N/A") else "Phytoclass Internal Default"

  run_parameters <- base::data.frame(
    Setting = base::c(
      "--- To Reproduce This Group ---", "Instructions",
      "--- QC & Filtering Applied ---", "Cleaning Rules Active", "Filters Active",
      "--- Phytoclass Parameters ---", "Fm Matrix Used", "Iterations (Niter)", "Cooling Step Size", "Random Seed", "Min/Max Profile"
    ),
    Value = base::c(
      "", "Upload this file's 'Source Data' sheet alone in Step 2, choose 'By Source File' grouping in Step 5, then run Step 6 using the parameters below.",
      "",
      if (base::length(qc_rules) > 0) base::paste(qc_rules, collapse = ", ") else "None",
      if (base::length(filters) > 0) base::paste(filters, collapse = ", ") else "None",
      "",
      log$fm_matrix_used %||% "N/A",
      base::as.character(config$phytoclass$niter %||% NA),
      base::as.character(config$phytoclass$step_size %||% NA),
      seed_text,
      mm_text
    ),
    stringsAsFactors = FALSE
  )

  diagnosis <- base::data.frame(
    Setting = base::c(
      "Status", "Fm Matrix Used", "Mean RMSE", "Mean Condition Number", "Flagged For Review",
      "Excluded Pigments", "Excluded Classes", "Rows Dropped (Zero Pigment Signal)", "Rows Dropped (Zero Tchla)",
      "Error Message"
    ),
    Value = base::c(
      status,
      log$fm_matrix_used %||% "N/A",
      base::as.character(base::round(base::as.numeric(log$mean_rmse %||% NA), 4)),
      base::as.character(base::round(base::as.numeric(log$mean_condnum %||% NA), 2)),
      base::as.character(base::isTRUE(log$flagged_for_review)),
      base::paste(log$excluded_pigments %||% base::character(0), collapse = ", "),
      base::paste(log$excluded_classes %||% base::character(0), collapse = ", "),
      base::as.character(log$rows_dropped_zero_signal %||% 0),
      base::as.character(log$rows_dropped_zero_tchla %||% 0),
      log$error_details$message %||% ""
    ),
    stringsAsFactors = FALSE
  )

  result_est <- NULL
  result_fmat <- NULL
  if (succeeded && !base::is.null(ds$data_final)) {
    clean_output <- ds$data_final |>
      dplyr::select(-dplyr::any_of(base::c("cleaning_status", "duplicate_status", "qc_pass", "filter_status_geo", "filter_status_temporal", "filter_status_depth", "original_row_num", "year", "month", "day", "hour", "minute")))
    base::colnames(clean_output) <- base::make.names(clean_result_colnames(base::colnames(clean_output)), unique = TRUE)
    clean_output <- apply_pigment_display_names(clean_output, config)
    if ("UniqueID" %in% base::names(clean_output)) clean_output <- clean_output |> dplyr::select(UniqueID, dplyr::everything())
    result_est <- clean_output
    if (!base::is.null(ds$f_matrix_final)) result_fmat <- format_fmatrix_for_display(ds$f_matrix_final, config)
  }

  base::list(run_parameters = run_parameters, diagnosis = diagnosis, result_est = result_est, result_fmat = result_fmat, status = status, succeeded = succeeded)
}

# A complete, self-contained workbook for ONE analysis group: source data
# (every original column, not just the app's canonical pigment set),
# run parameters, diagnosis, and results if it succeeded. Deliberately
# generated for every group regardless of success/failure or how many
# other groups exist - this is the "reproduce just this one result"
# artifact, independent of whatever the master report's section toggles
# are currently set to.
.write_standalone_group_workbook <- function(ds_name, ds, source_data, config, out_path) {
  content <- .build_group_content(ds_name, ds, config)
  wb <- openxlsx::createWorkbook()
  header_style <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#D9E1F2", border = "Bottom")

  if (!base::is.null(source_data) && base::nrow(source_data) > 0) {
    .add_plain_sheet(wb, header_style, "Source Data", source_data)
  }
  .add_categorized_sheet(wb, header_style, "Run Parameters", content$run_parameters)
  .add_plain_sheet(wb, header_style, "Diagnosis", content$diagnosis)
  if (!base::is.null(content$result_est)) .add_plain_sheet(wb, header_style, "Community Estimate", content$result_est)
  if (!base::is.null(content$result_fmat)) .add_plain_sheet(wb, header_style, "F-Matrix Used", content$result_fmat)

  openxlsx::saveWorkbook(wb, file = out_path, overwrite = TRUE)
}

# PNGs/raw .rds are separate files in the zip, not workbook sheets, so they
# are never subject to the per-group sheet threshold below - only sheet
# COUNT inside the workbook is the "overwhelming" problem being solved,
# not files sitting in a Plots/ or Raw_Output/ folder.
.write_group_plots_and_raw <- function(ds_name, ds, config, plots_dir, raw_dir) {
  if (base::is.null(ds$data_final)) return(invisible(NULL))
  if (!base::is.null(ds$phytoclass_raw)) {
    base::saveRDS(ds$phytoclass_raw, file = base::file.path(raw_dir, base::paste0("Raw_Phytoclass_Output_", ds_name, ".rds")))
  }
  base::tryCatch({
    ggplot2::ggsave(filename = base::file.path(plots_dir, base::paste0("AreaPlot_", ds_name, ".png")), plot = plot_community_area(ds$data_final, config), width = 10, height = 6, dpi = 300)
    ggplot2::ggsave(filename = base::file.path(plots_dir, base::paste0("BarPlot_", ds_name, ".png")), plot = plot_community_bar(ds$data_final, config), width = 10, height = 6, dpi = 300)
  }, error = function(e) NULL)
  invisible(NULL)
}

# Writes this group's sheets into the MASTER workbook: Source Data (only if
# include_source_data is TRUE - the caller decides that based on whether
# Step 5's own section already wrote it, avoiding the exact duplication
# pattern the QC-sheet fix addressed), Run Parameters, Diagnosis, and
# Community Estimate/F-Matrix if it succeeded. Uses .build_group_content()
# for all of it, so this can never independently drift from what the
# standalone per-group workbook shows for the same group.
.write_group_report_sheets <- function(wb, header_style, ds_name, ds, config, source_data, include_source_data, existing_sheet_names) {
  content <- .build_group_content(ds_name, ds, config)

  .unique_sheet_name <- function(proposed) {
    proposed <- base::substr(proposed, 1, 31)
    counter <- 2
    while (proposed %in% existing_sheet_names) {
      suffix <- base::paste0("_", counter)
      max_base_len <- 31 - base::nchar(suffix)
      proposed <- base::paste0(base::substr(proposed, 1, max_base_len), suffix)
      counter <- counter + 1
    }
    existing_sheet_names <<- base::c(existing_sheet_names, proposed)
    proposed
  }

  if (include_source_data && !base::is.null(source_data) && base::nrow(source_data) > 0) {
    .add_plain_sheet(wb, header_style, .unique_sheet_name(base::paste0("6.", ds_name, ".Source")), source_data)
  }
  .add_categorized_sheet(wb, header_style, .unique_sheet_name(base::paste0("6.", ds_name, ".Params")), content$run_parameters)
  .add_plain_sheet(wb, header_style, .unique_sheet_name(base::paste0("6.", ds_name, ".Diag")), content$diagnosis)
  if (!base::is.null(content$result_est)) .add_plain_sheet(wb, header_style, .unique_sheet_name(base::paste0("6.", ds_name, ".Est")), content$result_est)
  if (!base::is.null(content$result_fmat)) .add_plain_sheet(wb, header_style, .unique_sheet_name(base::paste0("6.", ds_name, ".Fmat")), content$result_fmat)

  existing_sheet_names
}


# ----------------------------------------------------------------------------
# Assembles the report into a temporary working directory (workbook, and
# Plots/ + Raw_Output/ subfolders when relevant sections need them), then
# zips that directory's contents into file_path. Every download goes
# through this same path now, so the interface never has to decide "is
# this one a plain file or a bundle" ahead of time, it always is one.
# ----------------------------------------------------------------------------

compile_progressive_report <- function(report_data, step_status, selected_sections, config, file_path) {
  # Per-group sheets (source data in Step 5; source data/params/diagnosis/
  # results in Step 7) are included in the MASTER workbook only when the
  # group count is at or below this. Above it, the master workbook keeps
  # only the views that already scale regardless of N ("5. Clustering
  # Summary", "6. All Analysis Results") plus a note pointing to
  # Group_Results/, since a 200-sheet workbook is not actually more useful
  # than a 100-file folder for drilling into one specific group. This does
  # NOT affect the standalone per-group workbooks below, which are always
  # generated for every group regardless of this threshold - they exist
  # specifically to be the "look at just this one" mechanism at any scale.
  LARGE_GROUP_THRESHOLD <- 20

  work_dir <- base::tempfile("phytoclassShiny_report_")
  plots_dir <- base::file.path(work_dir, "Plots")
  raw_dir <- base::file.path(work_dir, "Raw_Output")
  group_results_dir <- base::file.path(work_dir, "Group_Results")
  base::dir.create(work_dir, recursive = TRUE)

  wb <- openxlsx::createWorkbook()
  header_style <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#D9E1F2", border = "Bottom")
  existing_sheet_names <- base::character(0)
  any_plots_written <- FALSE
  any_raw_written <- FALSE
  any_group_results_written <- FALSE
  step5_wrote_per_group <- FALSE
  large_group_note <- NULL

  section_available <- function(step) base::identical(step_status[[step]] %||% "not_reached", "available")
  section_wanted <- function(section_key) section_key %in% selected_sections

  n_groups <- base::max(
    base::length(report_data$step5$cluster_datasets %||% base::list()),
    base::length(report_data$step7$analyzed_datasets %||% base::list())
  )
  is_large_run <- n_groups > LARGE_GROUP_THRESHOLD

  if (section_available("step1") && section_wanted("setup_information")) {
    .add_categorized_sheet(wb, header_style, "1. Setup Information", report_data$step1)
  }
  if (section_available("step2") && section_wanted("datasets_loaded")) {
    .add_plain_sheet(wb, header_style, "2. Datasets Loaded", report_data$step2)
  }
  if (section_available("step3") && section_wanted("column_mapping_log")) {
    .add_mapping_log_sheet(wb, header_style, "3. Column Mapping Log", report_data$step3)
  }
  if (section_available("step4") && section_wanted("quality_control_log")) {
    .add_plain_sheet(wb, header_style, "4. Quality Control Log", report_data$step4)
  }
  if (section_available("step5") && section_wanted("grouping_and_clustering")) {
    # Clustered, analysis-ready data, exported under the app's own short
    # pigment codes (not display names) specifically so it can be
    # re-imported directly in a future session without redoing column
    # mapping. See the multi-hour-clustering-then-failed-analysis scenario
    # this was originally built for.
    if (!base::is.null(report_data$step5$cluster_datasets) && !is_large_run) {
      for (cluster_name in base::names(report_data$step5$cluster_datasets)) {
        sheet_name <- base::substr(base::paste0("5.", cluster_name), 1, 31)
        .add_plain_sheet(wb, header_style, sheet_name, report_data$step5$cluster_datasets[[cluster_name]])
        existing_sheet_names <- base::c(existing_sheet_names, sheet_name)
      }
      step5_wrote_per_group <- TRUE
    } else if (is_large_run) {
      large_group_note <- base::sprintf("%d analysis groups detected - per-group sheets were omitted from this workbook to keep it navigable. See Group_Results/ for a complete, self-contained workbook per group.", n_groups)
    }
    .add_plain_sheet(wb, header_style, "5. Clustering Summary", report_data$step5$summary)

    diag <- report_data$step5$diagnostics
    if (!base::is.null(diag)) {
      if (!base::dir.exists(plots_dir)) base::dir.create(plots_dir)
      base::tryCatch({
        if (!base::is.null(diag$pca_plot)) { ggplot2::ggsave(base::file.path(plots_dir, "PCA_Cluster_Map.png"), diag$pca_plot, width = 8, height = 6, dpi = 300); any_plots_written <- TRUE }
        if (!base::is.null(diag$dendro_plot)) { ggplot2::ggsave(base::file.path(plots_dir, "Hierarchical_Dendrogram.png"), diag$dendro_plot, width = 10, height = 6, dpi = 300); any_plots_written <- TRUE }
        if (!base::is.null(diag$elbow_plot)) { ggplot2::ggsave(base::file.path(plots_dir, "Silhouette_Optimization.png"), diag$elbow_plot, width = 8, height = 6, dpi = 300); any_plots_written <- TRUE }
        if (!base::is.null(diag$wss_plot)) { ggplot2::ggsave(base::file.path(plots_dir, "WSS_Elbow_Plot.png"), diag$wss_plot, width = 8, height = 6, dpi = 300); any_plots_written <- TRUE }
      }, error = function(e) NULL)
    }
  }
  if (section_available("step6") && section_wanted("analysis_results")) {
    .add_plain_sheet(wb, header_style, "6. Analysis Results Summary", report_data$step6)
  }
  if (section_available("step7") && section_wanted("final_visualizations")) {
    # The full final package: categorised parameters, per-group results,
    # F-matrices, plots, and raw output.
    payload <- report_data$step7
    if (!base::is.null(payload)) {
      if (!base::dir.exists(plots_dir)) base::dir.create(plots_dir)
      if (!base::dir.exists(raw_dir)) base::dir.create(raw_dir)
      base::dir.create(group_results_dir, showWarnings = FALSE)

      if (!base::is.null(payload$qc_summary_df) && !section_wanted("quality_control_log")) {
        # report_data$step4 and payload$qc_summary_df are both literally
        # rv$qc_summary_df - the same object, not two independently
        # computed summaries. Writing it here only when the standalone
        # Step 4 section wasn't ALSO selected avoids ending up with two
        # sheets ("4. Quality Control Log" and "4. QC & Filtering Log")
        # containing identical data, which previously happened on every
        # "everything selected" download, since that's the default state.
        .add_plain_sheet(wb, header_style, "4. QC & Filtering Log", payload$qc_summary_df)
      }
      if (!base::is.null(payload$session_log)) {
        log_df <- base::data.frame(`Terminal Trace Output` = payload$session_log)
        openxlsx::addWorksheet(wb, "Session Log (All Steps)")
        openxlsx::writeData(wb, "Session Log (All Steps)", log_df)
        openxlsx::setColWidths(wb, "Session Log (All Steps)", cols = 1, widths = 100)
      }

      analyzed <- payload$analyzed_datasets %||% base::list()
      source_data_by_group <- report_data$step5$cluster_datasets %||% base::list()

      unified_results <- base::list()
      for (ds_name in base::names(analyzed)) {
        ds <- analyzed[[ds_name]]
        if (!base::is.null(ds$data_final)) {
          clean <- ds$data_final |>
            dplyr::select(-dplyr::any_of(base::c("cleaning_status", "duplicate_status", "qc_pass", "filter_status_geo", "filter_status_temporal", "filter_status_depth", "original_row_num", "year", "month", "day", "hour", "minute")))
          base::colnames(clean) <- base::make.names(clean_result_colnames(base::colnames(clean)), unique = TRUE)
          clean <- apply_pigment_display_names(clean, config)
          clean$Analysis_Group <- ds_name
          if ("UniqueID" %in% base::names(clean)) clean <- clean |> dplyr::select(UniqueID, Analysis_Group, dplyr::everything())
          unified_results[[ds_name]] <- clean
        }
      }
      if (base::length(unified_results) > 0) {
        global_df <- dplyr::bind_rows(unified_results)
        .add_plain_sheet(wb, header_style, "6. All Analysis Results", global_df)
      }

      # include_source_data: only true when Step 5's own section didn't
      # already write it (and wasn't itself skipped for being a large
      # run) - the same "don't write it twice" logic as the QC sheet fix.
      include_source_data <- !step5_wrote_per_group

      for (ds_name in base::names(analyzed)) {
        ds <- analyzed[[ds_name]]
        source_data <- source_data_by_group[[ds_name]]

        if (!is_large_run) {
          existing_sheet_names <- .write_group_report_sheets(wb, header_style, ds_name, ds, config, source_data, include_source_data, existing_sheet_names)
        }

        # Standalone per-group workbook: always generated, for every group,
        # success or failure, regardless of n_groups. This is what makes a
        # single failed group in an otherwise-huge run something you can
        # hand off and re-run in isolation, instead of needing the whole
        # multi-hundred-row master report to do it.
        base::tryCatch({
          safe_name <- base::make.names(ds_name)
          out_path <- base::file.path(group_results_dir, base::paste0(safe_name, ".xlsx"))
          .write_standalone_group_workbook(ds_name, ds, source_data, config, out_path)
          any_group_results_written <- TRUE
        }, error = function(e) NULL)

        .write_group_plots_and_raw(ds_name, ds, config, plots_dir, raw_dir)
        if (!base::is.null(ds$data_final)) { any_plots_written <- TRUE; any_raw_written <- any_raw_written || !base::is.null(ds$phytoclass_raw) }
      }

      manifest_rows <- base::list(
        base::data.frame(File = "phytoclassShiny_Report.xlsx", Contents = "This workbook: all selected sections, in order.", stringsAsFactors = FALSE),
        base::data.frame(File = "Group_Results/", Contents = "One complete, self-contained workbook per analysis group (source data, run parameters, diagnosis, and results if it succeeded) - always included here regardless of how many groups there are, whether or not the master workbook above includes per-group detail sheets.", stringsAsFactors = FALSE),
        base::data.frame(File = "Plots/", Contents = "PNG area/bar charts per analysis group, plus clustering diagnostics if Step 5 was included.", stringsAsFactors = FALSE),
        base::data.frame(File = "Raw_Output/", Contents = "Raw phytoclass R objects (.rds), one per analysis group, for advanced users continuing analysis in R directly.", stringsAsFactors = FALSE)
      )
      if (!base::is.null(large_group_note)) {
        manifest_rows[[base::length(manifest_rows) + 1]] <- base::data.frame(File = "(Note)", Contents = large_group_note, stringsAsFactors = FALSE)
      }
      manifest_df <- dplyr::bind_rows(manifest_rows)
      .add_plain_sheet(wb, header_style, "7. Export Manifest", manifest_df)

      if (base::exists("save_config") && base::is.function(save_config) && !base::is.null(payload$config_snapshot)) {
        base::tryCatch({ save_config(payload$config_snapshot, base::file.path(work_dir, "config_session.yaml")) }, error = function(e) NULL)
      }
    }
  }

  openxlsx::saveWorkbook(wb, file = base::file.path(work_dir, "phytoclassShiny_Report.xlsx"), overwrite = TRUE)

  if (!any_plots_written && base::dir.exists(plots_dir) && base::length(base::list.files(plots_dir)) == 0) base::unlink(plots_dir, recursive = TRUE)
  if (!any_raw_written && base::dir.exists(raw_dir) && base::length(base::list.files(raw_dir)) == 0) base::unlink(raw_dir, recursive = TRUE)
  if (!any_group_results_written && base::dir.exists(group_results_dir) && base::length(base::list.files(group_results_dir)) == 0) base::unlink(group_results_dir, recursive = TRUE)

  files_to_zip <- base::list.files(work_dir, recursive = TRUE)
  zip::zip(zipfile = file_path, files = files_to_zip, root = work_dir)

  base::unlink(work_dir, recursive = TRUE)
  invisible(file_path)
}
