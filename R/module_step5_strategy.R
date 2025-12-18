# ============================================================================
#
#   _phytoclass_Shiny V1.0 - STEP 5: ANALYSIS STRATEGY
#
#   Description:
#   Helps you decide how to group your data. You can analyze files individually
#   or use the recommended Clustering method to group samples by pigment
#   similarity (following best practices from Hayward et al., 2023).
#
# ============================================================================


# --- UI Function ---
strategyUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Step 5: Grouping Strategy"),
    p("Group similar samples to improve optimization accuracy. Hayward et al. (2023) recommends clustering normalized data to group samples by community structure, not just biomass."),
    hr(),
    
    fluidRow(
      column(10, offset = 1,
             wellPanel(
               h4("1. Data Summary"),
               p("The table below summarizes all samples that passed the QC checks in Step 4."),
               DTOutput(ns("source_data_summary_table"))
             ),
             
             wellPanel(
               h4("2. Select Strategy"),
               fluidRow(
                 column(4,
                        h5(strong("Grouping Method")),
                        radioButtons(ns("analysis_strategy_choice"), NULL,
                                     choices = c("Analyze Files Individually" = "by_source",
                                                 "Cluster Samples (Recommended)" = "by_cluster"),
                                     selected = "by_source"),
                        helpText(icon("info-circle"), "Clustering uses Ratios internally to find groups, but sends Raw Data to the analyzer.")
                 ),
                 column(8,
                        shinyjs::hidden(
                          div(id = ns("clustering_options_div"),
                              h5(strong("Clustering Parameters")),
                              wellPanel(
                                style = "background-color: #e9ecef;",
                                fluidRow(
                                  column(6,
                                         # Default to Hayward's recommendation (Tchla + Log)
                                         selectInput(ns("normalization_method_input"), "1. Clustering Input:", 
                                                     choices = c("Normalize to Tchla (Ratios)" = "tchla", "Raw Concentrations" = "none"), 
                                                     selected = "tchla"),
                                         selectInput(ns("transformation_method_input"), "2. Transformation:", 
                                                     choices = c("Log(x+1)" = "log1p", "None" = "none"), 
                                                     selected = "log1p")
                                  ),
                                  column(6,
                                         selectInput(ns("cluster_method_input"), "3. Algorithm:", 
                                                     choices = c("Hierarchical (ward.D2)" = "hclust", "K-Means" = "kmeans"), 
                                                     selected = "hclust"),
                                         numericInput(ns("k_max_input"), "Target Clusters (k):", value = 5, min = 2, max = 20, step = 1),
                                         helpText("Aim for groups with N > 12 samples.")
                                  )
                                )
                              )
                          )
                        )
                 )
               )
             ),
             
             wellPanel(
               h4("3. Preview & Commit"),
               fluidRow(
                 column(6, actionButton(ns("preview_strategy_btn"), "Preview Groups", class = "btn-primary btn-lg", width = "100%", icon = icon("eye"))),
                 column(6, shinyjs::disabled(
                   actionButton(ns("commit_strategy_btn"), "Confirm Strategy", class = "btn-success btn-lg", width = "100%", icon = icon("check-double"))
                 ))
               )
             ),
             
             wellPanel(
               h4("4. Strategy Preview"),
               uiOutput(ns("strategy_preview_ui"))
             )
      )
    )
  )
}

# --- Server Function ---
strategyServer <- function(id, rv, .log_event, .update_workflow_state, main_session) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    m_rv <- reactiveValues(preview_results = NULL)
    
    observe({
      shinyjs::toggle(id = "clustering_options_div", condition = input$analysis_strategy_choice == "by_cluster")
    })
    
    observeEvent(c(rv$master_qc_data, input$analysis_strategy_choice), {
      m_rv$preview_results <- NULL
      shinyjs::disable("commit_strategy_btn")
    })
    
    output$source_data_summary_table <- renderDT({
      req(rv$master_qc_data)
      summary_df <- rv$master_qc_data %>% dplyr::count(source_dataset, name = "Samples Passing QC")
      datatable(summary_df, options = list(dom = 't', paging = FALSE, searching = FALSE, info = FALSE), rownames = FALSE)
    })
    
    observeEvent(input$preview_strategy_btn, {
      if (is.null(rv$master_qc_data) || nrow(rv$master_qc_data) == 0) {
        showNotification("No data available to preview. Please complete Step 4.", type = "error")
        return()
      }
      .log_event(paste("Action: Previewing analysis strategy:", input$analysis_strategy_choice))
      
      show_modal_spinner(text = "Generating strategy preview...")
      tryCatch({
        if (input$analysis_strategy_choice == "by_source") {
          analysis_groups <- split(rv$master_qc_data, ~rv$master_qc_data$source_dataset) %>%
            purrr::map(~list(name = first(.x$source_dataset), data = .x))
          
          summary_df <- tibble(Group = names(analysis_groups), `Sample Count` = purrr::map_int(analysis_groups, ~nrow(.x$data)))
          summary_df$Status <- ifelse(summary_df$`Sample Count` < 12, "Warning (N<12)", "OK")
          
          m_rv$preview_results <- list(
            type = "by_source", analysis_datasets = analysis_groups,
            summary_table = summary_df
          )
        } else {
          # Perform Clustering
          cluster_res <- .perform_clustering_logic(rv$master_qc_data, input$normalization_method_input, 
                                                   input$transformation_method_input, input$cluster_method_input, 
                                                   input$k_max_input, rv$config)
          
          # IMPORTANT: We pass the RAW DATA back out, just grouped by ClusterID
          analysis_groups <- split(cluster_res$data_with_clusters, ~cluster_res$data_with_clusters$ClusterID) %>%
            purrr::keep(~nrow(.x) > 0) %>%
            purrr::map(~list(name = first(.x$ClusterID), data = .x))
          
          m_rv$preview_results <- list(
            type = "by_cluster", 
            analysis_datasets = analysis_groups, 
            summary_table = cluster_res$cluster_summary,
            pca_plot = cluster_res$pca_plot
          )
        }
        shinyjs::enable("commit_strategy_btn")
        showNotification("Preview generated successfully.", type = "message")
      }, error = function(e) {
        .log_event(paste("ERROR during strategy preview:", e$message))
        showModal(modalDialog(title = "Preview Error", e$message))
        m_rv$preview_results <- NULL; shinyjs::disable("commit_strategy_btn")
      }, finally = {
        remove_modal_spinner()
      })
    })
    
    # --- Clustering Logic (Robust) ---
    .perform_clustering_logic <- function(data, norm_method, trans_method, clust_method, k_max, config) {
      
      # 1. Isolate Pigment Data
      meta_keys <- c("latitude", "longitude", "depth", "year", "month", "day", "date", "time", "internal_source_id", "source_dataset", "UniqueID")
      # Note: We exclude Tchla from 'pigment_cols' for the matrix itself, but we need it for normalization
      
      potential_pigments <- setdiff(names(data), c(meta_keys, "Tchla", "cleaning_status", "duplicate_status", "qc_pass", "filter_status_geo", "filter_status_temporal", "filter_status_depth", "original_row_num"))
      pigment_cols <- potential_pigments[sapply(data[potential_pigments], is.numeric)]
      
      if(length(pigment_cols) < 2) stop("Not enough numeric pigment columns found for clustering.")
      
      # Create working matrix
      work_matrix <- data[, pigment_cols, drop=FALSE]
      work_matrix[is.na(work_matrix)] <- 0 # Ensure no NAs
      
      # 2. Normalization (Hayward: Ratio to Tchla)
      if (norm_method == "tchla") {
        # Robust Divisor: Use Tchla column if valid, else RowSums
        if ("Tchla" %in% names(data) && all(is.numeric(data$Tchla))) {
          div <- data$Tchla
        } else {
          div <- rowSums(work_matrix, na.rm=TRUE)
        }
        
        # Prevent Div/0 crash
        div[div <= 1e-9] <- 1 
        
        work_matrix <- sweep(work_matrix, 1, div, "/")
        
        # Sanity check: Clean up Inf/NaN created by division
        work_matrix[!is.finite(as.matrix(work_matrix))] <- 0
      }
      
      # 3. Transformation (Hayward: Log)
      if (trans_method == "log1p") {
        work_matrix <- log1p(work_matrix)
      }
      
      # 4. Clustering
      dist_mat <- vegan::vegdist(work_matrix, method = "euclidean")
      
      groups <- NULL
      if(clust_method == "hclust") {
        hc <- hclust(dist_mat, method = "ward.D2")
        groups <- cutree(hc, k = min(k_max, nrow(data)-1))
      } else {
        km <- kmeans(work_matrix, centers = min(k_max, nrow(data)-1))
        groups <- km$cluster
      }
      
      # 5. Output
      data$ClusterID <- paste0("Cluster_", groups)
      
      pca <- prcomp(work_matrix, scale. = TRUE)
      p <- fviz_pca_ind(pca, geom.ind = "point", col.ind = as.factor(groups), 
                        addEllipses = TRUE, title = "Cluster Preview (PCA)") + theme_minimal()
      
      summary <- data %>% count(ClusterID, name = "Count")
      summary$Status <- ifelse(summary$Count < 12, "Warning (N<12)", "OK")
      
      return(list(data_with_clusters = data, pca_plot = p, cluster_summary = summary))
    }
    
    output$strategy_preview_ui <- renderUI({
      if (is.null(m_rv$preview_results)) return(tags$div(class = "alert alert-secondary", "Click 'Preview Groups'."))
      
      tagList(
        h5(if(m_rv$preview_results$type == "by_cluster") "Preview of Pigment Clusters" else "Preview of Groups by Source File"),
        DTOutput(ns("preview_summary_table")),
        if(m_rv$preview_results$type == "by_cluster") plotOutput(ns("preview_pca_plot"), height = "500px")
      )
    })
    
    output$preview_summary_table <- renderDT({
      req(m_rv$preview_results$summary_table)
      datatable(m_rv$preview_results$summary_table, options = list(dom = 't', paging = FALSE, searching = FALSE, info = FALSE), rownames = FALSE) %>%
        formatStyle('Status', color = styleEqual(c("OK", "Warning (N<12)"), c("green", "red")), fontWeight = styleEqual(c("OK", "Warning (N<12)"), c("normal", "bold")))
    })
    
    output$preview_pca_plot <- renderPlot({
      req(m_rv$preview_results$pca_plot)
      m_rv$preview_results$pca_plot
    })
    
    observeEvent(input$commit_strategy_btn, {
      req(m_rv$preview_results)
      .log_event("Action: Committing analysis strategy and proceeding to Step 6.")
      
      rv$analysis_datasets <- m_rv$preview_results$analysis_datasets
      
      dataset_names <- names(rv$analysis_datasets)
      updatePickerInput(main_session, "datasets_for_phytoclass_run", choices = dataset_names, selected = dataset_names)
      
      .update_workflow_state("step6")
      updateNavbarPage(main_session, "main_navbar", selected = "step6")
      showNotification("Strategy confirmed. Proceeding to Step 6.", type = "message", duration = 8)
    })
    
  })
}