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
        shiny::conditionalPanel(condition = base::sprintf("input['%s'] == 'By Pigment Cluster'", ns("grouping_method_input")), shiny::p(class="text-muted small mt-1", shiny::icon("info-circle"), " Mix all files together and group samples that have similar pigment profiles.")),
        
        shiny::conditionalPanel(condition = base::sprintf("input['%s'] == 'By Pigment Cluster'", ns("grouping_method_input")), 
                                shiny::hr(), shiny::h5("Clustering Settings"), 
                                shiny::selectInput(ns("normalization_method_input"), "1. Data to compare:", choices = base::c("Ratio to Tchla", "Raw Data"), selected = "Ratio to Tchla", width="100%"), 
                                shiny::selectInput(ns("transformation_method_input"), "2. Math transformation:", choices = base::c("Log(x+1)", "None"), selected = "Log(x+1)", width="100%"), 
                                shiny::selectInput(ns("cluster_method_input"), "3. Algorithm:", choices = base::c("Hierarchical (Ward)", "K-Means"), selected = "Hierarchical (Ward)", width="100%"), 
                                shiny::radioButtons(ns("k_determination_mode"), "4. Number of Clusters (k):", choices = base::c("Auto", "Manual"), selected = "Auto", inline = TRUE), 
                                shiny::conditionalPanel(condition = base::sprintf("input['%s'] == 'Manual'", ns("k_determination_mode")), shiny::numericInput(ns("k_max_input"), "Set 'k':", value = 3, min = 2, max = 20, width="100%"))
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
                       bslib::card(shiny::h4(shiny::icon("table"), " Group Sizes"), DT::DTOutput(ns("cluster_distribution_table"))), 
                       shiny::div(id = ns("viz_card_container"), 
                                  bslib::card(shiny::h4(shiny::icon("chart-pie"), " Cluster Graphs"), 
                                              shiny::tabsetPanel(id = ns("viz_tabs"), 
                                                                 shiny::tabPanel("PCA Map", shiny::plotOutput(ns("pca_plot"), height = "350px")), 
                                                                 shiny::tabPanel("Silhouette Method", shiny::plotOutput(ns("elbow_plot"), height = "350px")),
                                                                 shiny::tabPanel("Elbow Plot (WSS)", shiny::plotOutput(ns("wss_plot"), height = "350px"))
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
    local_env <- shiny::reactiveValues(datasets = base::list(), summary_df = NULL, pca_plot = NULL, elbow_plot = NULL, wss_plot = NULL)
    
    shiny::observeEvent(input$grouping_method_input, { shinyjs::hide("confirm_strategy_btn"); shinyjs::hide("preview_results_container"); local_env$datasets <- base::list() })
    
    shiny::observeEvent(input$preview_strategy_btn, {
      shiny::req(rv$master_qc_data); master_data <- rv$master_qc_data
      if (base::nrow(master_data) == 0) { shiny::showNotification("No data available to group.", type = "error"); return() }
      
      method <- input$grouping_method_input
      .log_event("STRATEGY", base::sprintf("Generating preview: %s", method))
      
      if (method == "By Source File") {
        shinyjs::hide("viz_card_container")
        split_data <- if ("SourceFile" %in% base::names(master_data)) base::split(master_data, master_data$SourceFile) else base::split(master_data, base::gsub("_[0-9]+$", "", master_data$UniqueID))
        local_datasets <- base::list(); summary_rows <- base::list()
        for (fname in base::names(split_data)) {
          df_sub <- split_data[[fname]]; n_count <- base::nrow(df_sub)
          local_datasets[[fname]] <- base::list(name = fname, data = df_sub, type = "File")
          summary_rows[[base::length(summary_rows) + 1]] <- tibble::tibble(`Group ID` = fname, `Count` = n_count, `Status` = if(n_count < 12) "Warning (N<12)" else "OK")
        }
        local_env$datasets <- local_datasets; local_env$summary_df <- dplyr::bind_rows(summary_rows)
        
      } else if (method == "By Pigment Cluster") {
        shinyjs::show("viz_card_container")
        meta_cols <- base::c("UniqueID", "SourceFile", "Lat", "Lon", "Depth", "Date", "Time", "Station", "Cruise")
        pigment_data <- master_data[ , !(base::names(master_data) %in% meta_cols), drop=FALSE]
        pigment_data[] <- base::lapply(pigment_data, function(x) base::suppressWarnings(base::as.numeric(base::as.character(x))))
        pigment_data <- pigment_data[ , base::colSums(base::is.na(pigment_data)) < base::nrow(pigment_data), drop = FALSE]; pigment_data[base::is.na(pigment_data)] <- 0
        
        if (input$normalization_method_input == "Ratio to Tchla" && "Tchla" %in% base::names(pigment_data)) pigment_data <- pigment_data / (pigment_data$Tchla + 1e-8) 
        if (input$transformation_method_input == "Log(x+1)") pigment_data <- base::log1p(pigment_data)
        
        valid_cols <- base::apply(pigment_data, 2, function(x) { v <- stats::var(x, na.rm=TRUE); !base::is.na(v) && v > 1e-8 }); pigment_data <- pigment_data[, valid_cols, drop = FALSE]
        if (base::ncol(pigment_data) < 2 || base::nrow(pigment_data) < 3) { shiny::showNotification("Insufficient variance for clustering.", type = "error"); return() }
        
        max_possible_k <- base::min(20, base::nrow(pigment_data) - 1)
        k_user <- if (input$k_determination_mode == "Manual") input$k_max_input else 10
        k_val <- base::max(2, base::min(k_user, max_possible_k))
        dist_matrix <- stats::dist(pigment_data, method = "euclidean")
        
        if (input$k_determination_mode == "Auto") {
          best_k <- 2; best_sil <- -1
          if (input$cluster_method_input == "Hierarchical (Ward)") { 
            hc_temp <- stats::hclust(dist_matrix, method = "ward.D2")
            for (test_k in 2:k_val) { cl_temp <- stats::cutree(hc_temp, k = test_k); sil <- base::mean(cluster::silhouette(cl_temp, dist_matrix)[, 3]); if (sil > best_sil) { best_sil <- sil; best_k <- test_k } }
          } else { 
            for (test_k in 2:k_val) { base::set.seed(42); km_temp <- stats::kmeans(pigment_data, centers = test_k, nstart = 10); sil <- base::mean(cluster::silhouette(km_temp$cluster, dist_matrix)[, 3]); if (sil > best_sil) { best_sil <- sil; best_k <- test_k } } 
          }
          k_val <- best_k; .log_event("STRATEGY", base::sprintf("Auto-K silhouette detection optimized at clusters: %d", k_val))
        }
        
        if (input$cluster_method_input == "Hierarchical (Ward)") { clusters <- stats::cutree(stats::hclust(dist_matrix, method = "ward.D2"), k = k_val)
        } else { base::set.seed(42); clusters <- stats::kmeans(pigment_data, centers = k_val, nstart = 25)$cluster }
        
        master_data$ClusterID <- base::paste0("Cluster_", clusters); pca_res <- stats::prcomp(pigment_data, scale. = TRUE)
        local_env$pca_plot <- factoextra::fviz_pca_ind(pca_res, geom = "point", col.ind = base::as.factor(clusters), palette = "jco", addEllipses = TRUE, title = "PCA Chemical Communities")
        
        hcut_wrapper <- function(x, k) base::list(cluster = stats::cutree(stats::hclust(stats::dist(x), method="ward.D2"), k=k))
        
        local_env$elbow_plot <- factoextra::fviz_nbclust(pigment_data, if(input$cluster_method_input == "Hierarchical (Ward)") hcut_wrapper else stats::kmeans, method = "silhouette") + ggplot2::ggtitle(base::sprintf("Silhouette Optimization Curve (Selected k = %d)", k_val))
        
        # New: WSS (Elbow) Plot generation
        local_env$wss_plot <- factoextra::fviz_nbclust(pigment_data, if(input$cluster_method_input == "Hierarchical (Ward)") hcut_wrapper else stats::kmeans, method = "wss") + ggplot2::ggtitle("Elbow Method (Within-Cluster Sum of Squares)")
        
        split_data <- base::split(master_data, master_data$ClusterID)
        local_datasets <- base::list(); summary_rows <- base::list()
        for (cname in base::names(split_data)) {
          df_sub <- split_data[[cname]]; n_count <- base::nrow(df_sub); local_datasets[[cname]] <- base::list(name = cname, data = df_sub, type = "Cluster")
          summary_rows[[base::length(summary_rows) + 1]] <- tibble::tibble(`Group ID` = cname, `Count` = n_count, `Status` = if(n_count < 12) "Warning (N<12)" else "OK")
        }
        local_env$datasets <- local_datasets; local_env$summary_df <- dplyr::bind_rows(summary_rows)
      }
      shinyjs::show("preview_results_container"); shinyjs::show("confirm_strategy_btn")
    })
    
    output$cluster_distribution_table <- DT::renderDT({ shiny::req(local_env$summary_df); DT::datatable(local_env$summary_df, rownames=FALSE, options = base::list(pageLength = 10, dom = 't')) |> DT::formatStyle("Status", color = DT::styleEqual(c("OK", "Warning (N<12)"), c("#198754", "#dc3545")), fontWeight = 'bold') })
    output$pca_plot <- shiny::renderPlot({ shiny::req(local_env$pca_plot); local_env$pca_plot })
    output$elbow_plot <- shiny::renderPlot({ shiny::req(local_env$elbow_plot); local_env$elbow_plot })
    output$wss_plot <- shiny::renderPlot({ shiny::req(local_env$wss_plot); local_env$wss_plot })
    
    shiny::observeEvent(input$confirm_strategy_btn, { shiny::req(base::length(local_env$datasets) > 0); rv$analysis_datasets <- local_env$datasets; rv$config$strategy$method <- input$grouping_method_input; .log_event("STRATEGY", base::sprintf("Locked in %d analysis arrays.", base::length(rv$analysis_datasets))); .update_workflow_state("step6"); shiny::updateTabsetPanel(session = session_parent, inputId = "main_navbar", selected = "step6") })
  })
}