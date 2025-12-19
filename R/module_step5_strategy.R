# ============================================================================
#
#   _phytoclass_Shiny V1.0 - STEP 5: ANALYSIS STRATEGY
#
#   Description:
#   Helps you decide how to group your data. Remodeled with a "Control Panel"
#   layout for better visual stability between "Source" and "Cluster" modes.
#
# ============================================================================


# --- UI Function ---
strategyUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Step 5: Grouping Strategy"),
    p("Define how samples are grouped for optimization. Clustering is recommended to group samples by community structure."),
    hr(),
    
    # --- BAND 1: DECISION & CONTEXT ---
    fluidRow(
      # Method Selection (Left)
      column(5,
             wellPanel(
               style = "height: 200px;", # Fixed height for alignment
               h4(icon("project-diagram"), "1. Grouping Method"),
               radioButtons(ns("analysis_strategy_choice"), NULL,
                            choices = c("By Source File" = "by_source",
                                        "Cluster Analysis" = "by_cluster"),
                            selected = "by_source"),
               helpText(style = "color: #666; margin-top: 10px;",
                        icon("info-circle"), 
                        "Clustering groups samples by pigment similarity. Source File groups strictly by filename.")
             )
      ),
      
      # Data Summary (Right)
      column(7,
             wellPanel(
               style = "height: 200px; overflow-y: auto;", # Fixed height match
               h4(icon("list"), "Data Context"),
               DTOutput(ns("source_data_summary_table"))
             )
      )
    ),
    
    # --- BAND 2: CONFIGURATION (Dynamic) ---
    # This section adapts based on the selection above
    fluidRow(
      column(12,
             # CLUSTERING CONFIGURATION
             shinyjs::hidden(
               div(id = ns("clustering_options_div"),
                   wellPanel(
                     style = "background-color: #f8f9fa; border-left: 5px solid #007bff;",
                     h4(icon("sliders-h"), "Clustering Configuration"),
                     br(),
                     fluidRow(
                       column(3, 
                              tags$label("1. Input Data"),
                              selectInput(ns("normalization_method_input"), NULL, 
                                          choices = c("Ratio to Tchla" = "tchla", "Raw Data" = "none"), 
                                          selected = "tchla", width = "100%")
                       ),
                       column(3, 
                              tags$label("2. Transformation"),
                              selectInput(ns("transformation_method_input"), NULL, 
                                          choices = c("Log(x+1)" = "log1p", "None" = "none"), 
                                          selected = "log1p", width = "100%")
                       ),
                       column(3, 
                              tags$label("3. Algorithm"),
                              selectInput(ns("cluster_method_input"), NULL, 
                                          choices = c("Hierarchical (Ward)" = "hclust", "K-Means" = "kmeans"), 
                                          selected = "hclust", width = "100%")
                       ),
                       column(3,
                              tags$label("4. Cluster Count (k)"),
                              div(style = "display: flex; align-items: center; gap: 5px;",
                                  radioButtons(ns("k_determination_mode"), NULL,
                                               choices = c("Auto" = "auto", "Manual" = "manual"), 
                                               inline = TRUE, width = "auto"),
                                  div(style = "flex-grow: 1;",
                                      numericInput(ns("k_max_input"), NULL, value = 5, min = 2, max = 20, step = 1, width = "100%")
                                  )
                              )
                       )
                     )
                   )
               )
             ),
             
             # SOURCE FILE MESSAGE (Placeholder to keep UI stable)
             shinyjs::hidden(
               div(id = ns("source_options_div"),
                   wellPanel(
                     style = "background-color: #f8f9fa; border-left: 5px solid #6c757d; text-align: center; padding: 30px;",
                     h4(style="color: #6c757d;", icon("check-circle"), "No configuration needed."),
                     p("Each file uploaded will be treated as a distinct optimization group.")
                   )
               )
             )
      )
    ),
    
    # --- BAND 3: ACTION BAR ---
    wellPanel(
      fluidRow(
        column(6, 
               actionButton(ns("preview_strategy_btn"), "Preview Groups", 
                            class = "btn-primary btn-lg", width = "100%", icon = icon("play"))
        ),
        column(6, 
               shinyjs::disabled(
                 actionButton(ns("commit_strategy_btn"), "Confirm Strategy", 
                              class = "btn-success btn-lg", width = "100%", icon = icon("check"))
               )
        )
      )
    ),
    
    # --- BAND 4: RESULTS PREVIEW ---
    uiOutput(ns("strategy_preview_ui"))
  )
}

# --- Server Function ---
strategyServer <- function(id, rv, .log_event, .update_workflow_state, main_session) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    m_rv <- reactiveValues(preview_results = NULL)
    
    # Logic to toggle the configuration panels based on selection
    observe({
      is_cluster <- input$analysis_strategy_choice == "by_cluster"
      shinyjs::toggle(id = "clustering_options_div", condition = is_cluster)
      shinyjs::toggle(id = "source_options_div", condition = !is_cluster)
    })
    
    # Toggle Input State based on Auto/Manual K
    observe({
      if (input$k_determination_mode == "auto") {
        shinyjs::disable("k_max_input")
      } else {
        shinyjs::enable("k_max_input")
      }
    })
    
    # Reset preview if data or method changes
    observeEvent(c(rv$master_qc_data, input$analysis_strategy_choice), {
      m_rv$preview_results <- NULL
      shinyjs::disable("commit_strategy_btn")
    })
    
    output$source_data_summary_table <- renderDT({
      req(rv$master_qc_data)
      summary_df <- rv$master_qc_data %>% dplyr::count(source_dataset, name = "N_Samples")
      colnames(summary_df)[1] <- "Source File"
      
      datatable(summary_df, options = list(dom = 't', paging = FALSE, searching = FALSE, info = FALSE), 
                rownames = FALSE, class = "cell-border stripe hover compact")
    })
    
    observeEvent(input$preview_strategy_btn, {
      req(rv$master_qc_data)
      if (nrow(rv$master_qc_data) == 0) return()
      
      .log_event(paste("Action: Previewing analysis strategy:", input$analysis_strategy_choice))
      show_modal_spinner(text = "Calculating strategy...")
      
      tryCatch({
        if (input$analysis_strategy_choice == "by_source") {
          # --- FILE BASED ---
          analysis_groups <- split(rv$master_qc_data, ~rv$master_qc_data$source_dataset) %>%
            purrr::map(~list(name = first(.x$source_dataset), data = .x))
          
          summary_df <- tibble(Group = names(analysis_groups), `Sample Count` = purrr::map_int(analysis_groups, ~nrow(.x$data)))
          summary_df$Status <- ifelse(summary_df$`Sample Count` < 12, "Warning (N<12)", "OK")
          
          m_rv$preview_results <- list(type = "by_source", analysis_datasets = analysis_groups, summary_table = summary_df)
          
        } else {
          # --- CLUSTERING BASED ---
          
          # 1. Prepare Data Matrix
          matrix_prep <- .prepare_matrix_for_clustering(rv$master_qc_data, input$normalization_method_input, input$transformation_method_input)
          work_matrix <- matrix_prep$matrix
          
          optimal_k <- NA
          elbow_plot <- NULL
          used_k <- input$k_max_input
          
          # 2. Determine K (Auto vs Manual)
          if (input$k_determination_mode == "auto") {
            elbow_res <- .calculate_elbow(work_matrix)
            optimal_k <- elbow_res$optimal_k
            elbow_plot <- elbow_res$plot
            used_k <- optimal_k
            
            updateNumericInput(session, "k_max_input", value = used_k)
            showNotification(paste("Auto-detected Optimal Clusters:", used_k), type = "message")
          }
          
          # 3. Perform Final Clustering
          cluster_res <- .perform_final_clustering(rv$master_qc_data, work_matrix, input$cluster_method_input, used_k)
          
          # 4. Group data
          analysis_groups <- split(cluster_res$data_with_clusters, ~cluster_res$data_with_clusters$ClusterID) %>%
            purrr::keep(~nrow(.x) > 0) %>%
            purrr::map(~list(name = first(.x$ClusterID), data = .x))
          
          m_rv$preview_results <- list(
            type = "by_cluster", 
            analysis_datasets = analysis_groups, 
            summary_table = cluster_res$cluster_summary,
            pca_plot = cluster_res$pca_plot,
            elbow_plot = elbow_plot, 
            clustering_info = list(optimal_k = optimal_k, used_k = used_k, method = input$cluster_method_input)
          )
        }
        
        shinyjs::enable("commit_strategy_btn")
        
      }, error = function(e) {
        .log_event(paste("ERROR strategy:", e$message))
        showModal(modalDialog(title = "Strategy Error", e$message))
      }, finally = { remove_modal_spinner() })
    })
    
    # --- HELPER 1: Matrix Prep ---
    .prepare_matrix_for_clustering <- function(data, norm_method, trans_method) {
      meta_keys <- c("latitude", "longitude", "depth", "year", "month", "day", "date", "time", "internal_source_id", "source_dataset", "UniqueID")
      potential_pigments <- setdiff(names(data), c(meta_keys, "Tchla", "cleaning_status", "duplicate_status", "qc_pass", "filter_status_geo", "filter_status_temporal", "filter_status_depth", "original_row_num"))
      pigment_cols <- potential_pigments[sapply(data[potential_pigments], is.numeric)]
      
      if(length(pigment_cols) < 2) stop("Not enough numeric pigment columns found.")
      
      work_matrix <- as.matrix(data[, pigment_cols, drop=FALSE])
      work_matrix[!is.finite(work_matrix)] <- 0
      
      if (norm_method == "tchla") {
        div <- if ("Tchla" %in% names(data)) data$Tchla else rowSums(work_matrix, na.rm=TRUE)
        div[div <= 1e-9] <- 1 
        work_matrix <- sweep(work_matrix, 1, div, "/")
        work_matrix[!is.finite(work_matrix)] <- 0
      }
      
      if (trans_method == "log1p") work_matrix <- log1p(work_matrix)
      
      return(list(matrix = work_matrix))
    }
    
    # --- HELPER 2: The Elbow Method (WSS) ---
    .calculate_elbow <- function(mat) {
      vars <- apply(mat, 2, var)
      mat_clean <- mat[, vars > 1e-9, drop=FALSE]
      
      if(ncol(mat_clean) < 1) return(list(optimal_k = 1, plot = NULL))
      
      max_k <- min(15, nrow(mat_clean) - 1)
      if (max_k < 2) return(list(optimal_k = 1, plot = NULL))
      
      wss <- numeric(max_k)
      for (k in 1:max_k) {
        set.seed(123)
        # Wrapped in suppressWarnings() to hide "Quick-TRANSfer" messages.
        # Increased iter.max to 100 to help it converge better silently.
        wss[k] <- suppressWarnings(
          kmeans(mat_clean, centers = k, nstart = 5, iter.max = 100)$tot.withinss
        )
      }
      
      coords <- data.frame(k = 1:max_k, wss = wss)
      x1 <- 1; y1 <- wss[1]
      x2 <- max_k; y2 <- wss[max_k]
      m <- (y2 - y1) / (x2 - x1)
      b_intercept <- y1 - m*x1
      dists <- abs(m * coords$k - coords$wss + b_intercept) / sqrt(m^2 + 1)
      
      optimal_k <- which.max(dists)
      
      p <- ggplot(coords, aes(x = k, y = wss)) +
        geom_line() + geom_point() +
        geom_vline(xintercept = optimal_k, linetype="dashed", color="red") +
        annotate("text", x = optimal_k + 0.5, y = max(wss), label = paste("Optimal k =", optimal_k), color="red", hjust=0) +
        labs(title = "Elbow Method (Optimal k)", x = "Number of Clusters", y = "Within-Cluster Sum of Squares") +
        theme_minimal()
      
      return(list(optimal_k = optimal_k, plot = p))
    }
    
    # --- HELPER 3: Final Clustering ---
    .perform_final_clustering <- function(data, work_matrix, method, k) {
      vars <- apply(work_matrix, 2, var)
      work_matrix_clean <- work_matrix[, vars > 1e-9, drop=FALSE]
      
      if(ncol(work_matrix_clean) == 0) stop("No pigments with varying concentrations found for clustering.")
      
      groups <- NULL
      if(method == "hclust") {
        dist_mat <- vegan::vegdist(work_matrix_clean, method = "euclidean") 
        hc <- hclust(dist_mat, method = "ward.D2")
        groups <- cutree(hc, k = min(k, nrow(data)))
      } else {
        set.seed(123)
        km <- kmeans(work_matrix_clean, centers = min(k, nrow(data)), iter.max = 100)
        groups <- km$cluster
      }
      
      data$ClusterID <- paste0("Cluster_", groups)
      
      pca <- prcomp(work_matrix_clean, scale. = TRUE)
      p <- fviz_pca_ind(pca, geom.ind = "point", col.ind = as.factor(groups), 
                        addEllipses = TRUE, title = "Cluster Preview (PCA)") + theme_minimal()
      
      summary <- data %>% count(ClusterID, name = "Count")
      summary$Status <- ifelse(summary$Count < 12, "Warning (N<12)", "OK")
      
      return(list(data_with_clusters = data, pca_plot = p, cluster_summary = summary))
    }
    
    output$strategy_preview_ui <- renderUI({
      if (is.null(m_rv$preview_results)) return(NULL) # Clean start
      
      if(m_rv$preview_results$type == "by_source") {
        tagList(h4("Preview: Groups by File"), DTOutput(ns("preview_summary_table")))
      } else {
        tagList(
          h4("Preview: Clustering Results"),
          fluidRow(
            column(5, DTOutput(ns("preview_summary_table"))),
            column(7, 
                   tabsetPanel(
                     tabPanel("PCA Map", br(), plotOutput(ns("preview_pca_plot"), height = "400px")),
                     if(!is.null(m_rv$preview_results$elbow_plot)) {
                       tabPanel("Elbow Plot (Auto-k)", br(), plotOutput(ns("preview_elbow_plot"), height = "300px"))
                     } else {
                       tabPanel("Elbow Plot", br(), p("Not available in Manual mode."))
                     }
                   )
            )
          )
        )
      }
    })
    
    output$preview_summary_table <- renderDT({
      req(m_rv$preview_results$summary_table)
      datatable(m_rv$preview_results$summary_table, options = list(dom = 't', paging = FALSE, searching = FALSE, info = FALSE, scrollY="200px"), 
                rownames = FALSE, class = "cell-border stripe hover compact") %>%
        formatStyle('Status', color = styleEqual(c("OK", "Warning (N<12)"), c("green", "red")), fontWeight = styleEqual(c("OK", "Warning (N<12)"), c("normal", "bold")))
    })
    
    output$preview_pca_plot <- renderPlot({ req(m_rv$preview_results$pca_plot); m_rv$preview_results$pca_plot })
    output$preview_elbow_plot <- renderPlot({ req(m_rv$preview_results$elbow_plot); m_rv$preview_results$elbow_plot })
    
    observeEvent(input$commit_strategy_btn, {
      req(m_rv$preview_results)
      .log_event("Action: Committing analysis strategy.")
      
      rv$analysis_datasets <- m_rv$preview_results$analysis_datasets
      
      if(m_rv$preview_results$type == "by_cluster") {
        rv$cluster_diagnostics <- list(
          info = m_rv$preview_results$clustering_info,
          elbow_plot = m_rv$preview_results$elbow_plot
        )
      } else {
        rv$cluster_diagnostics <- NULL
      }
      
      dataset_names <- names(rv$analysis_datasets)
      updatePickerInput(main_session, "datasets_for_phytoclass_run", choices = dataset_names, selected = dataset_names)
      
      .update_workflow_state("step6")
      updateNavbarPage(main_session, "main_navbar", selected = "step6")
      showNotification("Strategy confirmed. Proceed to Step 6.", type = "message", duration = 8)
    })
    
  })
}