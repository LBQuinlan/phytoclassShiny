# ============================================================================
#
#   _phytoclass_Shiny V1.0 - STEP 7: RESULTS & REPORTING
#
#   Description:
#   The final dashboard. Creates graphs, compiles the Master Excel Report,
#   and archives the Session Config for perfect reproducibility.
#
# ============================================================================

# --- UI Function ---
reportingUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Step 7: Results & Export"),
    p("Interactively explore the results for each analysis group, then generate a complete report package."),
    hr(),
    
    fluidRow(
      column(4,
             wellPanel(
               h4("1. Explore Results"),
               selectInput(ns("dataset_to_explore"), "Select Analysis Group to View:", choices = NULL),
               hr(),
               h5(strong("Performance Diagnostics")),
               verbatimTextOutput(ns("performance_metrics_display"))
             ),
             wellPanel(
               h4("2. Export"),
               p("Create a timestamped folder containing clean Result files, the Master Audit Report, and a reusable Config file."),
               actionButton(ns("generate_report_package_btn"), "Download Report Package", class = "btn-primary btn-lg", width = "100%", icon = icon("download")),
               hr(),
               uiOutput(ns("report_package_status_ui"))
             )
      ),
      column(8,
             wellPanel(
               h4("Visualizations"),
               tabsetPanel(
                 tabPanel("Community Composition (100% Area Plot)",
                          br(),
                          plotOutput(ns("community_area_plot"), height = "500px")
                 ),
                 tabPanel("Sample-by-Sample (Biomass Bar Plot)",
                          br(),
                          plotOutput(ns("sample_bar_plot"), height = "500px")
                 )
               )
             )
      )
    )
  )
}

# --- Server Function ---
reportingServer <- function(id, rv, .log_event) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    observe({
      req(rv$analyzed_datasets)
      ds_with_results <- purrr::keep(rv$analyzed_datasets, ~!is.null(.x$data_final))
      updateSelectInput(session, "dataset_to_explore", choices = names(ds_with_results))
    })
    
    selected_dataset <- reactive({
      req(input$dataset_to_explore, rv$analyzed_datasets)
      rv$analyzed_datasets[[input$dataset_to_explore]]
    })
    
    output$performance_metrics_display <- renderText({
      req(selected_dataset())
      
      log <- selected_dataset()$log_analyzer
      ds_name <- selected_dataset()$name
      
      phyto_metrics <- paste(
        "--- Phytoclass Performance ---",
        paste("Status:", log$status %||% "N/A"),
        paste("Fm Matrix Used:", log$fm_matrix_used %||% "N/A"),
        paste("Mean RMSE:", round(log$mean_rmse %||% NA, 4)),
        paste("Mean Condition Num:", round(log$mean_condnum %||% NA, 2)),
        sep = "\n"
      )
      
      cluster_metrics <- ""
      if (!is.null(rv$cluster_diagnostics) && startsWith(ds_name, "Cluster_")) {
        sample_count <- log$rows_input_to_phyto %||% "N/A"
        
        cluster_metrics <- paste(
          "\n--- Clustering Diagnostics ---",
          paste("Samples in Cluster:", sample_count),
          paste("Optimal k (Elbow):", rv$cluster_diagnostics$info$optimal_k %||% "N/A"),
          paste("Used k:", rv$cluster_diagnostics$info$used_k %||% "N/A"),
          sep = "\n"
        )
      }
      
      paste(phyto_metrics, cluster_metrics, sep = "\n")
    })
    
    # --- Helper: Name Cleaner ---
    .clean_names <- function(x) {
      x <- gsub("^Phyto_", "", x)
      x <- gsub("_Abund$", "", x)
      x <- gsub("Phyto_RMSE", "RMSE", x)
      x <- gsub("Phyto_CondNum", "Condition_Number", x)
      return(x)
    }
    
    # --- Helper: Palette Builder ---
    .get_palette <- function(data_classes) {
      config_palette <- rv$config$reporting$plotting$custom_palette
      if (is.null(config_palette)) return(NULL) 
      
      user_colors <- unlist(config_palette)
      final_palette <- c()
      
      for (cls in data_classes) {
        if (cls %in% names(user_colors)) {
          final_palette[cls] <- user_colors[[cls]]
        } else {
          hyphenated <- gsub("\\.", "-", cls) 
          if (hyphenated %in% names(user_colors)) {
            final_palette[cls] <- user_colors[[hyphenated]]
          } else {
            final_palette[cls] <- NA 
          }
        }
      }
      
      missing_entries <- names(final_palette)[is.na(final_palette)]
      if (length(missing_entries) > 0) {
        default_colors <- scales::hue_pal()(length(missing_entries))
        for(i in seq_along(missing_entries)) {
          final_palette[missing_entries[i]] <- default_colors[i]
        }
      }
      
      return(final_palette)
    }
    
    # --- Plotting Helpers ---
    .plot_area <- function(data) {
      long_df <- data %>% 
        dplyr::select(UniqueID, starts_with("Phyto_")) %>%
        dplyr::select(-ends_with("RMSE"), -ends_with("CondNum")) %>%
        pivot_longer(cols = -UniqueID, names_to = "Class", values_to = "Abundance") %>%
        mutate(Class = .clean_names(Class))
      
      unique_classes <- unique(long_df$Class)
      custom_pal <- .get_palette(unique_classes)
      
      p <- ggplot(long_df, aes(x = UniqueID, y = Abundance, fill = Class, group = Class)) +
        geom_area(alpha = 0.8, position = "fill") + 
        scale_y_continuous(labels = scales::percent) +
        theme_minimal() +
        theme(axis.text.x = element_blank()) +
        labs(title = "Community Composition (Relative Abundance)", y = "Contribution to Total Chl a")
      
      if (!is.null(custom_pal)) {
        p <- p + scale_fill_manual(values = custom_pal)
      }
      return(p)
    }
    
    .plot_bar <- function(data) {
      long_df <- data %>% 
        dplyr::select(UniqueID, starts_with("Phyto_")) %>%
        dplyr::select(-ends_with("RMSE"), -ends_with("CondNum")) %>%
        pivot_longer(cols = -UniqueID, names_to = "Class", values_to = "Abundance") %>%
        mutate(Class = .clean_names(Class))
      
      unique_classes <- unique(long_df$Class)
      custom_pal <- .get_palette(unique_classes)
      
      p <- ggplot(long_df, aes(x = UniqueID, y = Abundance, fill = Class)) +
        geom_bar(stat = "identity", position = "stack") +
        theme_minimal() +
        theme(axis.text.x = element_blank()) +
        labs(title = "Sample Biomass (Absolute)", y = "Concentration (ug/L)")
      
      if (!is.null(custom_pal)) {
        p <- p + scale_fill_manual(values = custom_pal)
      }
      return(p)
    }
    
    output$community_area_plot <- renderPlot({
      req(selected_dataset()$data_final)
      .plot_area(selected_dataset()$data_final)
    })
    
    output$sample_bar_plot <- renderPlot({
      req(selected_dataset()$data_final)
      .plot_bar(selected_dataset()$data_final)
    })
    
    # --- REPORT GENERATION ---
    observeEvent(input$generate_report_package_btn, {
      if (length(rv$analyzed_datasets) == 0) {
        showNotification("Please run the analysis in Step 6 first.", type="error")
        return()
      }
      
      base_output_dir <- rv$config$workspace$output_directory %||% "outputs"
      
      # Clean "Session" naming convention
      timestamp <- format(Sys.time(), "%Y-%m-%d_%H%M")
      session_output_dir <- file.path(base_output_dir, paste0("Session_", timestamp))
      plots_output_dir <- file.path(session_output_dir, "Plots")
      
      if (!dir.exists(session_output_dir)) dir.create(session_output_dir, recursive = TRUE)
      if (!dir.exists(plots_output_dir)) dir.create(plots_output_dir)
      
      show_modal_spinner(text = "Generating reports and plots...")
      
      tryCatch({
        wb_master <- createWorkbook()
        
        # 1. Session Info
        addWorksheet(wb_master, "Session Info")
        
        cluster_k_auto <- if(!is.null(rv$cluster_diagnostics)) rv$cluster_diagnostics$info$optimal_k else "N/A"
        cluster_k_used <- if(!is.null(rv$cluster_diagnostics)) rv$cluster_diagnostics$info$used_k else "N/A"
        
        method_raw <- rv$config$clustering$cluster_method %||% "hclust"
        dist_metric <- if(method_raw == "hclust") "Euclidean" else "N/A (K-Means)"
        linkage <- if(method_raw == "hclust") "Ward.D2" else "N/A"
        
        run_time <- if(!is.null(rv$performance_metrics)) paste(rv$performance_metrics$total_time_sec, "s") else "N/A"
        speed <- if(!is.null(rv$performance_metrics)) paste(rv$performance_metrics$time_per_sample, "s") else "N/A"
        
        session_info <- data.frame(
          Parameter = c(
            "Session ID", "Date", 
            "Optimization Iterations (Niter)", "Cooling Step", 
            "Clustering Method", "Distance Metric", "Linkage Method",
            "Optimal K (Elbow)", "Final K Used",
            "Total Runtime", "Avg Speed (sec/sample)",
            "QC Duplicates", "Geo Filter", "Temp Filter"
          ),
          Value = c(
            rv$session_id, as.character(Sys.Date()), 
            rv$config$phytoclass$niter, rv$config$phytoclass$step_size,
            method_raw, dist_metric, linkage,
            as.character(cluster_k_auto), as.character(cluster_k_used),
            run_time, speed,
            rv$config$data_cleaning$handle_duplicates$enabled,
            rv$config$filtering$geospatial$enabled,
            rv$config$filtering$temporal$enabled
          )
        )
        writeData(wb_master, "Session Info", session_info)
        
        # 2. QC Stats
        if (!is.null(rv$qc_summary_df)) {
          addWorksheet(wb_master, "QC Statistics")
          writeData(wb_master, "QC Statistics", rv$qc_summary_df)
        }
        
        # 3. Resolution Warnings
        if (length(rv$resolution_warnings) > 0) {
          warn_df <- stack(rv$resolution_warnings)
          colnames(warn_df) <- c("Issue_Detected", "Dataset_Name")
          warn_df <- warn_df %>%  dplyr::select(Dataset_Name, Issue_Detected)
          addWorksheet(wb_master, "Resolution Warnings")
          writeData(wb_master, "Resolution Warnings", warn_df)
        }
        
        # 4. Session Log
        log_df <- data.frame(Timestamped_Log = rv$session_log)
        addWorksheet(wb_master, "Session Log")
        writeData(wb_master, "Session Log", log_df)
        
        # 5. Full Audit Trail
        audit_list <- list()
        if (length(rv$datasets_processed) > 0) {
          for (n in names(rv$datasets_processed)) {
            d_ann <- rv$datasets_processed[[n]]$data_annotated
            if (!is.null(d_ann)) audit_list[[n]] <- d_ann
          }
          if (length(audit_list) > 0) {
            master_audit_df <- dplyr::bind_rows(audit_list)
            addWorksheet(wb_master, "Triage Log (All Samples)")
            writeData(wb_master, "Triage Log (All Samples)", master_audit_df)
          }
        }
        
        # 6. Unified Results
        unified_results <- list()
        for(ds_name in names(rv$analyzed_datasets)) {
          ds <- rv$analyzed_datasets[[ds_name]]
          if(!is.null(ds$data_final)) {
            clean <- ds$data_final %>%
              dplyr::select(-any_of(c("cleaning_status", "duplicate_status", "qc_pass",
                                      "filter_status_geo", "filter_status_temporal", 
                                      "filter_status_depth", "original_row_num", "year", "month", "day")))
            colnames(clean) <- .clean_names(colnames(clean))
            clean$Analysis_Group <- ds_name
            if("UniqueID" %in% names(clean)) {
              clean <- clean %>% dplyr::select(UniqueID, Analysis_Group, everything())
            }
            unified_results[[ds_name]] <- clean
          }
        }
        
        if(length(unified_results) > 0) {
          global_df <- bind_rows(unified_results)
          addWorksheet(wb_master, "All Samples Combined")
          writeData(wb_master, "All Samples Combined", global_df)
        }
        
        saveWorkbook(wb_master, file = file.path(session_output_dir, "PhytoClass_Master_Report.xlsx"), overwrite = TRUE)
        
        # 7. Plots, Config & Individual Files
        
        # --- ARCHIVE CONFIG FOR REPRODUCIBILITY ---
        # Save as "config_session.yaml" so it can be drag-and-dropped back to root
        save_config(rv$config, file.path(session_output_dir, "config_session.yaml"))
        
        if (!is.null(rv$cluster_diagnostics) && !is.null(rv$cluster_diagnostics$elbow_plot)) {
          ggsave(filename = file.path(plots_output_dir, "Elbow_Plot_Optimization.png"), 
                 plot = rv$cluster_diagnostics$elbow_plot, width = 8, height = 6, dpi = 300)
        }
        
        for(ds_name in names(rv$analyzed_datasets)) {
          ds <- rv$analyzed_datasets[[ds_name]]
          if(!is.null(ds$data_final)) {
            clean_output <- ds$data_final %>%
              dplyr::select(-any_of(c("cleaning_status", "duplicate_status", "qc_pass",
                                      "filter_status_geo", "filter_status_temporal", "filter_status_depth",
                                      "original_row_num", "year", "month", "day")))
            colnames(clean_output) <- .clean_names(colnames(clean_output))
            if("UniqueID" %in% names(clean_output)) clean_output <- clean_output %>% dplyr::select(UniqueID, everything())
            
            write.xlsx(clean_output, file = file.path(session_output_dir, paste0("Result_", ds_name, ".xlsx")))
            
            p_area <- .plot_area(ds$data_final)
            ggsave(filename = file.path(plots_output_dir, paste0("AreaPlot_", ds_name, ".png")), 
                   plot = p_area, width = 10, height = 6, dpi = 300)
            
            p_bar <- .plot_bar(ds$data_final)
            ggsave(filename = file.path(plots_output_dir, paste0("BarPlot_", ds_name, ".png")), 
                   plot = p_bar, width = 10, height = 6, dpi = 300)
          }
        }
        
        .log_event("Success: Final report package generated.")
        output$report_package_status_ui <- renderUI({ 
          p(style = "color:#28a745;", "✅ Data + Plots saved to:", br(), code(session_output_dir)) 
        })
      }, error = function(e) {
        .log_event(paste("ERROR during report generation:", e$message))
        output$report_package_status_ui <- renderUI({ p(style = "color:#dc3545;", "❌ FAILED:", br(), e$message) })
      }, finally = { remove_modal_spinner() })
    })
    
  })
}