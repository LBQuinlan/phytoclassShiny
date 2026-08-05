# ============================================================================
# MODULE: Step 5 - Grouping Strategy
# Handles sample grouping via file splitting or chemometric pigment
# clustering.
#
# Design notes:
#
#   Clustering distances are computed on explicitly scaled data
#   (base::scale()), matching the scale.=TRUE used for the PCA diagnostic
#   plot shown alongside it, so the plot and the actual grouping describe
#   the same geometry rather than the plot implying a different structure
#   than the one produced.
#
#   K-Means minimizes squared Euclidean distance internally; this is a
#   mathematical property of the algorithm, not a metric that can be
#   swapped. The distance-metric selector is disabled and relabelled when
#   K-Means is selected, since it only meaningfully applies to the two
#   Ward's-based methods.
#
#   Large-dataset failsafe: hierarchical clustering requires a full O(n^2)
#   pairwise distance matrix, and the auto-k silhouette sweep re-runs
#   clustering several times on top of that. Above
#   performance$large_dataset_threshold samples (default 5000), this is
#   disabled automatically: the app switches to K-Means (O(n*k), scales
#   linearly) for the actual cluster assignment, which still runs on the
#   full dataset, while diagnostic plots (PCA, dendrogram,
#   silhouette/elbow curves) are computed on a random representative
#   subsample (default 2000 rows) so they stay fast and memory-safe
#   regardless of total dataset size. This is surfaced to the user via a
#   banner, never silent.
#
#   Group-size status is a three-tier OK (>=20) / Caution (12-19,
#   acceptable for clean data but Hayward et al. 2023 recommend 20+ for
#   noisier datasets) / Warning (<12, insufficient).
#
#   Hooked into the shared report-state system: confirming a strategy
#   marks Step 5 available and stores the clustered/grouped data (using
#   the app's own short pigment codes, not display names, so it can be
#   re-imported directly in a future session) into rv$report_data$step5,
#   independent of whether Step 6 subsequently succeeds.
# ============================================================================

strategyUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h3("Step 5: Grouping Strategy"),
    shiny::p("How should the app group your data before doing the math?", class = "text-muted"),
    shiny::hr(),

    bslib::layout_columns(
      col_widths = c(4, 8),
      bslib::card(
        shiny::h4(shiny::icon("cogs"), " 1. Setup Strategy"),
        shinyWidgets::radioGroupButtons(inputId = ns("grouping_method_input"), label = "Group By:", choices = base::c("By Source File", "By Pigment Cluster"), selected = "By Source File", status = "outline-primary", width = "100%", justified = TRUE),

        shiny::conditionalPanel(condition = base::sprintf("input['%s'] == 'By Source File'", ns("grouping_method_input")), shiny::p(class="text-muted small mt-1", shiny::icon("info-circle"), " Keep each uploaded Excel file separate.")),
        shiny::conditionalPanel(condition = base::sprintf("input['%s'] == 'By Pigment Cluster'", ns("grouping_method_input")), shiny::p(class="text-muted small mt-1", shiny::icon("info-circle"), " Mix files and group samples by pigment profiles.")),

        shiny::conditionalPanel(condition = base::sprintf("input['%s'] == 'By Pigment Cluster'", ns("grouping_method_input")),
                                shiny::hr(), shiny::h5("Clustering Settings"),
                                shiny::uiOutput(ns("large_dataset_banner")),
                                shiny::selectInput(ns("normalization_method_input"), "1. Data to compare:", choices = base::c("Ratio to Tchla", "Raw Data"), selected = "Ratio to Tchla", width="100%"),
                                shiny::selectInput(ns("transformation_method_input"), "2. Transformation:", choices = base::c("Box-Cox", "Log10(x+1)", "None"), selected = "Box-Cox", width="100%"),
                                shiny::selectInput(ns("distance_method_input"), "3. Distance Metric:", choices = base::c("Manhattan", "Euclidean"), selected = "Manhattan", width="100%"),
                                shiny::uiOutput(ns("distance_metric_note")),
                                shiny::selectInput(ns("cluster_method_input"), "4. Algorithm & Pruning:", choices = base::c("Ward's + DynamicTreeCut", "Ward's + Silhouette Cut", "K-Means"), selected = "Ward's + DynamicTreeCut", width="100%"),

                                shiny::conditionalPanel(condition = base::sprintf("input['%s'] != 'Ward\\'s + DynamicTreeCut'", ns("cluster_method_input")),
                                                        shiny::radioButtons(ns("k_determination_mode"), "5. Number of Clusters (k):", choices = base::c("Auto", "Manual"), selected = "Auto", inline = TRUE),
                                                        shiny::conditionalPanel(condition = base::sprintf("input['%s'] == 'Manual'", ns("k_determination_mode")), shiny::numericInput(ns("k_max_input"), "Set 'k':", value = 3, min = 2, max = 20, width="100%"))
                                )
        ),

        shiny::hr(), shiny::h4(shiny::icon("eye"), " 2. Review Groups"),
        shiny::actionButton(ns("preview_strategy_btn"), "Preview Groups", icon = shiny::icon("play"), class = "btn-outline-primary w-100 mb-2"),
        shinyjs::hidden(shiny::actionButton(ns("confirm_strategy_btn"), "Lock in Strategy", icon = shiny::icon("check-double"), class = "btn-success w-100 fw-bold"))
      ),

      shiny::tagList(
        shinyjs::hidden(
          shiny::div(id = ns("preview_results_container"),
                     bslib::layout_columns(
                       col_widths = c(5, 7),
                       bslib::card(
                         shiny::h4(shiny::icon("table"), " Group Sizes"),
                         shiny::div(class = "judgement-note", shiny::icon("circle-info"), " ",
                                    shiny::strong("Caution"), " (fewer than 20 samples) and ", shiny::strong("Warning"),
                                    " (fewer than 12) flag groups where phytoclass's fit is more likely to be unstable or poorly constrained - not an automatic failure, but worth reviewing the group's composition before trusting its results."),
                         DT::DTOutput(ns("cluster_distribution_table"))
                       ),
                       shiny::div(id = ns("viz_card_container"),
                                  bslib::card(shiny::h4(shiny::icon("chart-pie"), " Cluster Graphs"),
                                              shiny::uiOutput(ns("diagnostic_subsample_note")),
                                              shiny::tabsetPanel(id = ns("viz_tabs"),
                                                                 shiny::tabPanel("PCA Map", shiny::plotOutput(ns("pca_plot"), height = "350px")),
                                                                 shiny::tabPanel("Dendrogram", shiny::plotOutput(ns("dendro_plot"), height = "350px")),
                                                                 shiny::tabPanel("Optimization", shiny::plotOutput(ns("elbow_plot"), height = "350px")),
                                                                 shiny::tabPanel("WSS", shiny::plotOutput(ns("wss_plot"), height = "350px"))
                                              )
                                  )
                       )
                     )
          )
        )
      )
    )
  )
}

strategyServer <- function(id, rv, .log_event, .update_workflow_state, session_parent) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    `%||%` <- function(a, b) if (!base::is.null(a)) a else b
    local_env <- shiny::reactiveValues(datasets = base::list(), summary_df = NULL, pca_plot = NULL, dendro_plot = NULL, elbow_plot = NULL, wss_plot = NULL, opt_k_log = "N/A", used_large_dataset_path = FALSE, subsample_n = NULL)

    shiny::observeEvent(input$grouping_method_input, { shinyjs::hide("confirm_strategy_btn"); shinyjs::hide("preview_results_container"); local_env$datasets <- base::list() })

    # Disable the distance-metric selector when K-Means is chosen, since
    # K-Means does not actually use it for cluster assignment.
    shiny::observeEvent(input$cluster_method_input, {
      if (base::identical(input$cluster_method_input, "K-Means")) {
        shinyjs::disable("distance_method_input")
      } else {
        shinyjs::enable("distance_method_input")
      }
    }, ignoreNULL = FALSE)

    output$distance_metric_note <- shiny::renderUI({
      if (base::identical(input$cluster_method_input, "K-Means")) {
        shiny::tags$small(class = "text-muted", shiny::icon("info-circle"), " K-Means always uses Euclidean distance internally; this selector only applies to the two Ward's-based methods, and is disabled here to avoid implying otherwise.")
      }
    })

    # --- BIG DATA FAILSAFE: pre-check before the user even hits Preview ---
    output$large_dataset_banner <- shiny::renderUI({
      shiny::req(rv$master_qc_data)
      n_samples <- base::nrow(rv$master_qc_data)
      threshold <- base::as.numeric(rv$config$performance$large_dataset_threshold %||% 5000)
      if (n_samples > threshold) {
        shiny::div(class = "alert alert-warning", style = "font-size:0.85em; padding: 8px 12px;",
                   shiny::icon("triangle-exclamation"),
                   base::sprintf(" %d samples detected (over the %d-sample safety threshold). Hierarchical clustering (DynamicTreeCut/Silhouette Cut) requires a full pairwise distance matrix that would be too slow and memory-intensive at this scale. K-Means will be used for the actual grouping instead, applied to all %d samples; diagnostic plots below will be computed on a representative subsample so they render quickly.", n_samples, threshold, n_samples)
        )
      }
    })

    shiny::observeEvent(input$preview_strategy_btn, {
      shiny::req(rv$master_qc_data); master_data <- rv$master_qc_data
      if (base::nrow(master_data) == 0) { shiny::showNotification("No data available to group.", type = "error"); return() }

      shinybusy::show_modal_spinner(text = "Rendering strategy previews... This may take a moment for large datasets.")

      base::tryCatch({
        method <- input$grouping_method_input
        .log_event("STRATEGY", base::sprintf("Generating preview: %s", method))
        large_threshold <- base::as.numeric(rv$config$performance$large_dataset_threshold %||% 5000)
        subsample_n <- base::as.numeric(rv$config$performance$diagnostic_subsample_size %||% 2000)

        if (method == "By Source File") {
          shinyjs::hide("viz_card_container")
          # Fallback used only if SourceFile is somehow missing. UniqueID is
          # created as paste(dataset_name, "Row", number, sep="_") -- i.e.
          # "MyFile_Row_00001" -- so the pattern must strip "_Row_00001",
          # not just a trailing number, or the recovered group label ends
          # up as "MyFile_Row" instead of "MyFile".
          split_data <- if ("SourceFile" %in% base::names(master_data)) base::split(master_data, master_data$SourceFile) else base::split(master_data, base::gsub("_Row_[0-9]+$", "", master_data$UniqueID))
          local_datasets <- base::list(); summary_rows <- base::list()
          for (fname in base::names(split_data)) {
            df_sub <- split_data[[fname]]; n_count <- base::nrow(df_sub)
            local_datasets[[fname]] <- base::list(name = fname, data = df_sub, type = "File")
            summary_rows[[base::length(summary_rows) + 1]] <- tibble::tibble(`Group ID` = fname, `Count` = n_count, `Status` = .group_size_status(n_count))
          }
          local_env$datasets <- local_datasets; local_env$summary_df <- dplyr::bind_rows(summary_rows)
          local_env$used_large_dataset_path <- FALSE

        } else if (method == "By Pigment Cluster") {
          shinyjs::show("viz_card_container")
          n_total <- base::nrow(master_data)
          is_large <- n_total > large_threshold
          local_env$used_large_dataset_path <- is_large

          # Column exclusion: deliberately mirrors the regex-based
          # protected_cols logic in module_step4_qc.R rather than keeping a
          # second, independent hardcoded list. The previous literal list
          # here ("Lat", "lat", "Lon", "lon", "Depth") never matched the
          # app's actual canonical column names ("latitude", "longitude",
          # "depth" - confirmed via backend_helpers.R / module_step3's
          # column mapping), so real coordinate values were silently being
          # fed into the pigment transforms below as if they were pigment
          # concentrations. Keeping this identical to Step 4 means the two
          # modules cannot drift apart on what counts as "metadata" again.
          meta_matches <- base::grep("(?i)id|file|row|year|month|day|hour|minute|date|time|station|cruise|zone|sample|site", base::colnames(master_data), value = TRUE)
          geo_matches <- base::grep("(?i)lat|lon|depth", base::colnames(master_data), value = TRUE)
          meta_cols <- base::unique(base::c("UniqueID", "SourceFile", "original_row_num", meta_matches, geo_matches))
          pigment_data <- master_data[ , !(base::names(master_data) %in% meta_cols), drop=FALSE]
          pigment_data[] <- base::lapply(pigment_data, function(x) base::suppressWarnings(base::as.numeric(base::as.character(x))))
          pigment_data <- pigment_data[ , base::colSums(base::is.na(pigment_data)) < base::nrow(pigment_data), drop = FALSE]; pigment_data[base::is.na(pigment_data)] <- 0

          if (input$normalization_method_input == "Ratio to Tchla" && "Tchla" %in% base::names(pigment_data)) {
            safe_tchla <- base::pmax(pigment_data$Tchla, 1e-4)
            pigment_data <- pigment_data / safe_tchla
          }

          # Safety net, not a substitute for the QC toggle: log10(x+1) and
          # Box-Cox both require non-negative input mathematically. If
          # negative values are still present here (e.g. because "Enforce
          # non-negative pigments" was turned off in Step 1), floor them to
          # 0 explicitly and tell the user, rather than letting them
          # silently become NaN inside the transform below.
          n_neg <- base::sum(pigment_data < 0, na.rm = TRUE)
          if (n_neg > 0) {
            .log_event("WARNING", base::sprintf("%d negative pigment value(s) detected going into the '%s' transform (QC 'Enforce non-negative pigments' may be off). Floored to 0 before transformation.", n_neg, input$transformation_method_input))
            pigment_data[pigment_data < 0] <- 0
          }

          if (input$transformation_method_input %in% base::c("Log(x+1)", "Log10(x+1)")) {
            pigment_data <- base::log10(pigment_data + 1)
          } else if (input$transformation_method_input == "Box-Cox") {
            .robust_boxcox <- function(v) {
              base::tryCatch({
                x_shift <- v + 1e-6
                bc <- base::suppressWarnings(MASS::boxcox(stats::lm(x_shift ~ 1), plotit = FALSE))
                lambda <- bc$x[base::which.max(bc$y)]
                if(base::abs(lambda) < 1e-4) return(base::log(x_shift))
                return((x_shift^lambda - 1) / lambda)
              }, error = function(e) {
                # Falls back on x_shift (already floored to >= 1e-6 above),
                # not the raw v. The previous version called log1p(v) here,
                # which is undefined for any v <= -1 - i.e. the exact same
                # values that made MASS::boxcox() throw in the first place
                # would also NaN out on this fallback.
                return(base::log1p(x_shift - 1))
              })
            }
            pigment_data[] <- base::lapply(pigment_data, .robust_boxcox)
          }

          valid_cols <- base::apply(pigment_data, 2, function(x) { v <- stats::var(x, na.rm=TRUE); !base::is.na(v) && v > 1e-8 }); pigment_data <- pigment_data[, valid_cols, drop = FALSE]
          if (base::ncol(pigment_data) < 2 || base::nrow(pigment_data) < 3) { base::stop("Insufficient mathematical variance across samples for clustering.") }

          # Explicit scaling before any distance computation, matching the
          # scale.=TRUE used for the PCA plot below, so both describe the
          # same geometry.
          pigment_data_scaled <- base::scale(pigment_data)
          pigment_data_scaled[!base::is.finite(pigment_data_scaled)] <- 0

          # Effective algorithm: forced to K-Means above the large-dataset
          # threshold, regardless of what the user selected, since the
          # hierarchical path is not safe at this scale. This is surfaced
          # to the user via large_dataset_banner above and logged here.
          effective_cluster_method <- if (is_large) "K-Means" else input$cluster_method_input
          if (is_large && input$cluster_method_input != "K-Means") {
            .log_event("STRATEGY", base::sprintf("Large dataset failsafe engaged: requested '%s' overridden to 'K-Means' for %d samples.", input$cluster_method_input, n_total))
          }

          dist_metric <- if (input$distance_method_input == "Manhattan") "manhattan" else "euclidean"

          # Diagnostic data: the full scaled dataset normally, or a random
          # representative subsample when the large-dataset failsafe is
          # active. This is ONLY for the plots below; the actual cluster
          # assignment further down always uses the full pigment_data_scaled.
          if (is_large) {
            base::set.seed(42)
            diag_idx <- base::sample.int(n_total, base::min(subsample_n, n_total))
            diagnostic_data <- pigment_data_scaled[diag_idx, , drop = FALSE]
            local_env$subsample_n <- base::length(diag_idx)
          } else {
            diagnostic_data <- pigment_data_scaled
            local_env$subsample_n <- NULL
          }
          dist_matrix_diag <- stats::dist(diagnostic_data, method = dist_metric)

          local_env$dendro_plot <- NULL; local_env$elbow_plot <- NULL; local_env$wss_plot <- NULL

          if (effective_cluster_method == "Ward's + DynamicTreeCut") {
            hc <- stats::hclust(dist_matrix_diag, method = "ward.D2")
            min_cluster_size <- base::max(3, base::floor(base::nrow(diagnostic_data) * 0.03))

            diag_clusters <- dynamicTreeCut::cutreeDynamic(dendro = hc, distM = base::as.matrix(dist_matrix_diag), deepSplit = 2, pamStage = TRUE, minClusterSize = min_cluster_size, verbose = 0)
            if (base::any(diag_clusters == 0)) diag_clusters[diag_clusters == 0] <- base::max(diag_clusters) + 1
            k_val <- base::length(base::unique(diag_clusters))
            local_env$opt_k_log <- "DynamicTreeCut (Auto)"
            local_env$dendro_plot <- factoextra::fviz_dend(hc, k = k_val, show_labels = FALSE, rect = TRUE, main = "DynamicTreeCut Pruning")

            # Full-dataset assignment: cut the SAME algorithm's logic applied
            # to the complete distance structure only when feasible (i.e.
            # never for the large-dataset path, where hc above is already
            # subsample-only by construction and is not re-run on full data).
            if (is_large) {
              # Re-fit final assignment with K-Means at the discovered k,
              # since re-running hierarchical clustering on the full,
              # over-threshold dataset is exactly what this failsafe exists
              # to avoid.
              base::set.seed(42)
              clusters <- stats::kmeans(pigment_data_scaled, centers = base::max(2, k_val), nstart = 10)$cluster
            } else {
              clusters <- diag_clusters
            }

          } else {
            max_possible_k <- base::min(20, base::nrow(diagnostic_data) - 1)
            k_user <- if (input$k_determination_mode == "Manual") input$k_max_input else 10
            k_val <- base::max(2, base::min(k_user, max_possible_k))

            if (input$k_determination_mode == "Auto") {
              best_k <- 2; best_sil <- -1
              if (effective_cluster_method == "Ward's + Silhouette Cut") {
                hc_temp <- stats::hclust(dist_matrix_diag, method = "ward.D2")
                for (test_k in 2:k_val) { cl_temp <- stats::cutree(hc_temp, k = test_k); sil <- base::mean(cluster::silhouette(cl_temp, dist_matrix_diag)[, 3]); if (sil > best_sil) { best_sil <- sil; best_k <- test_k } }
              } else {
                for (test_k in 2:k_val) { base::set.seed(42); km_temp <- stats::kmeans(diagnostic_data, centers = test_k, nstart = 10); sil <- base::mean(cluster::silhouette(km_temp$cluster, dist_matrix_diag)[, 3]); if (sil > best_sil) { best_sil <- sil; best_k <- test_k } }
              }
              k_val <- best_k; .log_event("STRATEGY", base::sprintf("Auto-K silhouette detection (%s) optimized at clusters: %d", if (is_large) base::sprintf("on %d-sample subsample", base::nrow(diagnostic_data)) else "on full dataset", k_val))
              local_env$opt_k_log <- "Silhouette (Auto)"
            } else { local_env$opt_k_log <- "Manual" }

            if (effective_cluster_method == "Ward's + Silhouette Cut" && !is_large) {
              hc <- stats::hclust(dist_matrix_diag, method = "ward.D2")
              clusters <- stats::cutree(hc, k = k_val)
              local_env$dendro_plot <- factoextra::fviz_dend(hc, k = k_val, show_labels = FALSE, rect = TRUE, main = "Ward's Hierarchical Dendrogram")
            } else {
              # K-Means (either by user choice, or because the failsafe
              # forced it): always fit on the FULL scaled dataset, this is
              # the one clustering path cheap enough to do that safely.
              base::set.seed(42)
              clusters <- stats::kmeans(pigment_data_scaled, centers = k_val, nstart = 25)$cluster
              if (effective_cluster_method == "Ward's + Silhouette Cut" && is_large) {
                local_env$dendro_plot <- NULL # not meaningful at this scale; omitted rather than misleadingly shown for a subsample
              }
            }

            safe_k_max <- base::min(10, base::nrow(diagnostic_data) - 1)
            hcut_wrapper <- function(x, k) base::list(cluster = stats::cutree(stats::hclust(stats::dist(x, method=dist_metric), method="ward.D2"), k=k))

            local_env$elbow_plot <- factoextra::fviz_nbclust(diagnostic_data, if(effective_cluster_method == "Ward's + Silhouette Cut" && !is_large) hcut_wrapper else stats::kmeans, method = "silhouette", k.max = safe_k_max) + ggplot2::ggtitle(base::sprintf("Silhouette Optimization Curve (Selected k = %d)", k_val))
            local_env$wss_plot <- factoextra::fviz_nbclust(diagnostic_data, if(effective_cluster_method == "Ward's + Silhouette Cut" && !is_large) hcut_wrapper else stats::kmeans, method = "wss", k.max = safe_k_max) + ggplot2::ggtitle("Elbow Method (Within-Cluster Sum of Squares)")
          }

          master_data$ClusterID <- base::paste0("Cluster_", clusters)

          # PCA diagnostic: on the subsample when the failsafe is active,
          # exactly matching whatever data dendro/elbow/silhouette used
          # above, so all diagnostic plots describe the same view of the
          # data as each other.
          pca_res <- stats::prcomp(diagnostic_data, scale. = FALSE)  # already scaled above; scale. = TRUE here would double-scale
          pca_labels <- if (is_large) base::as.factor(clusters[diag_idx]) else base::as.factor(clusters)
          local_env$pca_plot <- factoextra::fviz_pca_ind(pca_res, geom = "point", col.ind = pca_labels, palette = "jco", addEllipses = TRUE, title = base::sprintf("PCA Chemical Communities%s", if (is_large) base::sprintf(" (%d-sample subsample)", base::nrow(diagnostic_data)) else ""))

          split_data <- base::split(master_data, master_data$ClusterID)
          local_datasets <- base::list(); summary_rows <- base::list()
          for (cname in base::names(split_data)) {
            df_sub <- split_data[[cname]]; n_count <- base::nrow(df_sub); local_datasets[[cname]] <- base::list(name = cname, data = df_sub, type = "Cluster")
            summary_rows[[base::length(summary_rows) + 1]] <- tibble::tibble(`Group ID` = cname, `Count` = n_count, `Status` = .group_size_status(n_count))
          }
          local_env$datasets <- local_datasets; local_env$summary_df <- dplyr::bind_rows(summary_rows)
        }

        shinyjs::show("preview_results_container"); shinyjs::show("confirm_strategy_btn")

      }, error = function(e) {
        .log_event("ERROR", base::paste("Strategy preview generation crashed:", e$message))
        shiny::showNotification(base::paste("Preview Failed:", e$message), type = "error", duration = 10)
      }, finally = {
        shinybusy::remove_modal_spinner()
      })
    })

    # Three-tier status: OK (>=20) / Caution (12-19) / Warning (<12)
    .group_size_status <- function(n_count) {
      if (n_count < 12) "Warning (N<12)"
      else if (n_count < 20) "Caution (N<20)"
      else "OK"
    }

    output$diagnostic_subsample_note <- shiny::renderUI({
      if (base::isTRUE(local_env$used_large_dataset_path) && !base::is.null(local_env$subsample_n)) {
        shiny::div(class = "alert alert-info", style = "font-size:0.82em; padding: 6px 10px; margin-bottom: 8px;",
                   shiny::icon("flask"), base::sprintf(" These diagnostics are computed on a random %d-sample subsample for speed. Final group assignment below still covers every sample.", local_env$subsample_n))
      }
    })

    output$cluster_distribution_table <- DT::renderDT({
      shiny::req(local_env$summary_df)
      DT::datatable(local_env$summary_df, rownames=FALSE, options = base::list(pageLength = 10, searching = FALSE, lengthChange = FALSE)) |>
        DT::formatStyle("Status", color = DT::styleEqual(base::c("OK", "Caution (N<20)", "Warning (N<12)"), base::c("#3F7D4F", "#D98C3D", "#B23A48")), fontWeight = 'bold')
    }, server = FALSE)

    output$pca_plot <- shiny::renderPlot({ shiny::req(local_env$pca_plot); local_env$pca_plot })
    output$dendro_plot <- shiny::renderPlot({ shiny::req(local_env$dendro_plot); local_env$dendro_plot })
    output$elbow_plot <- shiny::renderPlot({ shiny::req(local_env$elbow_plot); local_env$elbow_plot })
    output$wss_plot <- shiny::renderPlot({ shiny::req(local_env$wss_plot); local_env$wss_plot })

    shiny::observeEvent(input$confirm_strategy_btn, {
      shiny::req(base::length(local_env$datasets) > 0)
      rv$analysis_datasets <- local_env$datasets
      rv$config$strategy$method <- input$grouping_method_input

      if (input$grouping_method_input == "By Pigment Cluster") {
        rv$cluster_diagnostics <- base::list(
          info = base::list(
            optimal_k = local_env$opt_k_log,
            used_k = base::length(local_env$datasets),
            algorithm = input$cluster_method_input,
            transform = input$transformation_method_input,
            distance = input$distance_method_input,
            large_dataset_path_used = base::isTRUE(local_env$used_large_dataset_path)
          ),
          pca_plot = local_env$pca_plot,
          dendro_plot = local_env$dendro_plot,
          elbow_plot = local_env$elbow_plot,
          wss_plot = local_env$wss_plot
        )
      } else {
        rv$cluster_diagnostics <- NULL
      }

      # Report-state: Step 5 becomes available, Step 6 onward is invalidated
      # since it hasn't run against this (possibly new) grouping yet. The
      # exported data uses short pigment codes deliberately (see
      # report_builder.R), so it can be re-imported directly in a future
      # session without needing column mapping redone.
      rv$step_status <- mark_step_available(rv$step_status, "step5")
      rv$step_status <- invalidate_from_step(rv$step_status, "step6")
      rv$report_data$step5 <- base::list(
        summary = local_env$summary_df,
        cluster_datasets = base::lapply(rv$analysis_datasets, function(d) d$data),
        # Included here (not just in rv$cluster_diagnostics) so the
        # Download Report's grouping_and_clustering section can export
        # these PNGs even if the user never opens Step 7, per-step data
        # belongs with its own step, not borrowed from a later one.
        diagnostics = rv$cluster_diagnostics
      )

      .log_event("STRATEGY", base::sprintf("Locked in %d analysis arrays.", base::length(rv$analysis_datasets)))
      .update_workflow_state("step6")
      shiny::updateTabsetPanel(session = session_parent, inputId = "main_navbar", selected = "step6")
    })
  })
}
