# ============================================================================
# MODULE: Step 7 - Results & Viewing
#
# Interactive viewing only: graphs, run metrics, and the optimised
# pigment-ratio matrix for a selected group. Downloading happens through
# the global "Download Report" control (top navigation bar); its "Final
# Package" section, implemented in report_builder.R, covers the same data
# shown here plus plots, raw output, and every prior step's results.
# ============================================================================

reportingUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h3("Step 7: View Results"),
    shiny::p(class="section-desc", "Review your final graphs and math scores here. To download your results, use the 'Download Report' control in the top navigation bar, available from any step, its 'Final Package' section includes everything shown here plus plots, raw output, and every prior step's data."),
    shiny::hr(),

    shiny::fluidRow(
      shiny::column(4,
                    bslib::card(
                      bslib::card_header(shiny::icon("search"), " View Graphs"),
                      bslib::card_body(
                        shiny::selectInput(ns("dataset_to_explore"), "Select Group:", choices = NULL),
                        shiny::hr(),
                        shiny::div(class="text-uppercase tracking-wider fw-bold text-secondary small mb-2", "Math Scores"),
                        shiny::verbatimTextOutput(ns("performance_metrics_display")),
                        shiny::uiOutput(ns("smape_diagnostic_note")),
                        shiny::uiOutput(ns("exclusion_note")),
                        shiny::uiOutput(ns("review_flag_badge"))
                      )
                    )
      ),
      shiny::column(8,
                    bslib::card(
                      bslib::card_header(shiny::icon("chart-bar"), " Phytoplankton Community Graphs"),
                      bslib::card_body(
                      shiny::tabsetPanel(
                        shiny::tabPanel("Relative Abundance (Percentage)",
                                        shiny::br(),
                                        shiny::plotOutput(ns("community_area_plot"), height = "500px")
                        ),
                        shiny::tabPanel("Absolute Abundance (Concentration)",
                                        shiny::br(),
                                        shiny::plotOutput(ns("sample_bar_plot"), height = "500px")
                        ),
                        shiny::tabPanel("Optimised Pigment Ratios",
                                        shiny::br(),
                                        shiny::p(class="text-muted small", "The final pigment-to-chlorophyll-", shiny::em("a"), " ratio phytoclass converged on for this group. Zero cells mean that pigment isn't part of this class's signature; non-zero cells are the optimised ratio used to resolve its contribution to each sample."),
                                        DT::DTOutput(ns("fmatrix_table"))
                        )
                      )
                      )
                    )
      )
    )
  )
}

reportingServer <- function(id, rv, .log_event) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    `%||%` <- function(a, b) if (!base::is.null(a)) a else b

    shiny::observe({
      shiny::req(rv$analyzed_datasets)
      ds_with_results <- purrr::keep(rv$analyzed_datasets, ~!base::is.null(.x$data_final))
      shiny::updateSelectInput(session, "dataset_to_explore", choices = base::names(ds_with_results))
    })

    selected_dataset <- shiny::reactive({
      shiny::req(input$dataset_to_explore, rv$analyzed_datasets)
      rv$analyzed_datasets[[input$dataset_to_explore]]
    })

    output$performance_metrics_display <- shiny::renderText({
      shiny::req(selected_dataset())
      log <- selected_dataset()$log_analyzer
      ds_name <- selected_dataset()$name

      phyto_metrics <- base::paste(
        "--- RUN FIT METRICS ---",
        base::paste("Convergence State :", log$status %||% "N/A"),
        base::paste("Selected Baseline :", log$fm_matrix_used %||% "N/A"),
        base::paste("Random Seed       :", log$seed_used %||% "Unconstrained"),
        base::paste("Residual Error    :", base::round(log$mean_rmse %||% NA, 4)),
        base::paste("Condition Matrix  :", base::round(log$mean_condnum %||% NA, 2)),
        base::paste("Symmetric MAPE    :", if (base::is.na(log$mean_smape %||% NA)) "N/A (see note below)" else base::paste0(base::round(log$mean_smape, 2), "%")),
        sep = "\n"
      )

      cluster_metrics <- ""
      if (!base::is.null(rv$cluster_diagnostics) && base::startsWith(ds_name, "Cluster_")) {
        sample_count <- log$rows_input_to_phyto %||% "N/A"
        cluster_metrics <- base::paste(
          "\n--- SEGMENT PROPERTY LOG ---",
          base::paste("Segment Density   :", sample_count),
          base::paste("Elbow Optimum (k) :", rv$cluster_diagnostics$info$optimal_k %||% "N/A"),
          base::paste("Target Groups (k) :", rv$cluster_diagnostics$info$used_k %||% "N/A"),
          sep = "\n"
        )
      }
      base::paste(phyto_metrics, cluster_metrics, sep = "\n")
    })

    output$review_flag_badge <- shiny::renderUI({
      shiny::req(selected_dataset())
      log <- selected_dataset()$log_analyzer
      if (base::isTRUE(log$flagged_for_review)) {
        threshold <- rv$config$phytoclass$rmse_review_threshold %||% 0.1
        shiny::div(class = "alert alert-warning mt-2", style = "font-size:0.85em; padding:6px 10px;",
                   shiny::icon("triangle-exclamation"),
                   base::sprintf(" RMSE (%.4f) exceeds the review threshold (%.2f). Hayward et al. (2023) recommend increasing the iteration limit or step size, or reclustering this group.", log$mean_rmse %||% NA, base::as.numeric(threshold)))
      }
    })

    output$smape_diagnostic_note <- shiny::renderUI({
      shiny::req(selected_dataset())
      log <- selected_dataset()$log_analyzer
      note <- log$smape_diagnostic
      if (!base::is.null(note) && !base::is.na(note)) {
        is_na_result <- base::is.na(log$mean_smape %||% NA)
        shiny::div(class = base::paste("alert mt-2", if (is_na_result) "alert-secondary" else "alert-light border"),
                   style = "font-size:0.8em; padding:6px 10px;",
                   shiny::icon("circle-info"), " ", note)
      }
    })

    output$exclusion_note <- shiny::renderUI({
      shiny::req(selected_dataset())
      log <- selected_dataset()$log_analyzer
      excl_pigments <- log$excluded_pigments
      excl_classes <- log$excluded_classes
      if (base::length(excl_pigments) == 0 && base::length(excl_classes) == 0) return(NULL)

      pigment_labels <- if (base::length(excl_pigments) > 0) pigment_display_name(excl_pigments, rv$config) else base::character(0)

      shiny::div(class = "alert alert-secondary mt-2", style = "font-size:0.8em; padding:6px 10px;",
                 shiny::icon("scissors"),
                 base::sprintf(" This group's reference matrix was trimmed before analysis: %s were absent from every sample in this group%s.",
                               if (base::length(pigment_labels) > 0) base::paste(pigment_labels, collapse = ", ") else "no pigments",
                               if (base::length(excl_classes) > 0) base::paste0(", making ", base::paste(excl_classes, collapse = ", "), " unresolvable and excluding ", base::ifelse(base::length(excl_classes) == 1, "it", "them"), " from this group's results") else ""))
    })

    output$fmatrix_table <- DT::renderDT({
      shiny::req(selected_dataset()$f_matrix_final)
      f_df <- format_fmatrix_for_display(selected_dataset()$f_matrix_final, rv$config)
      shiny::req(f_df)
      numeric_cols <- base::setdiff(base::colnames(f_df), "Phytoplankton_Class")
      DT::datatable(f_df, rownames = FALSE, options = base::list(scrollX = TRUE, pageLength = 10, searching = FALSE)) |>
        DT::formatRound(columns = numeric_cols, digits = 4)
    }, server = FALSE)

    output$community_area_plot <- shiny::renderPlot({
      shiny::req(selected_dataset()$data_final)
      plot_community_area(selected_dataset()$data_final, rv$config)
    })
    output$sample_bar_plot <- shiny::renderPlot({
      shiny::req(selected_dataset()$data_final)
      plot_community_bar(selected_dataset()$data_final, rv$config)
    })
  })
}
