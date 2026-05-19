# ============================================================================
# MODULE: Step 7 - Results & Reporting
# Description: The final dashboard. Hardened against OS-level file locks
# and reactive memory leaks.
# ============================================================================

reportingUI <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::h3("Step 7: Results & Download"),
    shiny::p(class="section-desc", "View your final graphs, check the math scores, and download your results."),
    shiny::hr(),
    
    shiny::fluidRow(
      shiny::column(4,
                    shiny::wellPanel(
                      shiny::h4(shiny::icon("search"), " 1. View Graphs"),
                      shiny::selectInput(ns("dataset_to_explore"), "Select Group:", choices = NULL),
                      shiny::hr(),
                      shiny::div(class="text-uppercase tracking-wider fw-bold text-secondary small mb-2", "Math Scores"),
                      shiny::verbatimTextOutput(ns("performance_metrics_display"))
                    ),
                    shiny::wellPanel(
                      shiny::h4(shiny::icon("archive"), " 2. Download Results"),
                      shiny::p(style="font-size:0.88em; color:#555;", "Save all your clean data, calculated abundances, and graphs into a new folder on your computer."),
                      shiny::actionButton(ns("generate_report_package_btn"), "Save Results to Computer", class = "btn-success btn-lg w-100 fw-bold", icon = shiny::icon("download")),
                      
                      shiny::uiOutput(ns("open_folder_ui"))
                    )
      ),
      shiny::column(8,
                    shiny::wellPanel(
                      shiny::h4(shiny::icon("chart-bar"), " Phytoplankton Community Graphs"),
                      shiny::tabsetPanel(
                        shiny::tabPanel("Relative Abundance (Percentage)",
                                        shiny::br(),
                                        shiny::plotOutput(ns("community_area_plot"), height = "500px")
                        ),
                        shiny::tabPanel("Absolute Abundance (Concentration)",
                                        shiny::br(),
                                        shiny::plotOutput(ns("sample_bar_plot"), height = "500px")
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
    
    local_rv <- shiny::reactiveValues(saved_package_path = NULL)
    
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
        base::paste("Residual Error    :", base::round(log$mean_rmse %||% NA, 4)),
        base::paste("Condition Matrix  :", base::round(log$mean_condnum %||% NA, 2)),
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
    
    shiny::observeEvent(input$open_local_directory_btn, {
      shiny::req(local_rv$saved_package_path)
      path <- base::normalizePath(local_rv$saved_package_path, mustWork = FALSE)
      
      if (!base::dir.exists(path)) {
        shiny::showNotification("Target directory reference could not be localized on current mount.", type = "error")
        return()
      }
      
      tryCatch({
        if (.Platform$OS.type == "windows") {
          utils::browseURL(path) 
        } else if (Sys.info()["sysname"] == "Darwin") {
          base::system2("open", args = base::shQuote(path)) 
        } else {
          base::system2("xdg-open", args = base::shQuote(path)) 
        }
        .log_event("SYSTEM", base::paste("Dispatched OS Explorer to path channel:", path))
      }, error = function(e) {
        shiny::showNotification(base::paste("OS Explorer intercept failed:", e$message), type = "warning")
      })
    })
    
    output$open_folder_ui <- shiny::renderUI({
      shiny::req(local_rv$saved_package_path)
      shiny::div(class = "mt-3 p-3 border rounded text-center animate-fade-in", 
                 style = "background-color: #f1f8f5; border-color: #a3cfbb !important;",
                 shiny::p(style = "color: #146c43; font-size: 0.88em; margin-bottom: 12px; font-weight:500;", 
                          shiny::icon("check-circle"), " Results Successfully Saved!"),
                 shiny::actionButton(ns("open_local_directory_btn"), "Open Results Folder", 
                                     class = "btn-outline-success btn-sm w-100 font-monospace", icon = shiny::icon("folder-open"))
      )
    })
    
    .clean_names <- function(x) {
      x <- base::gsub("^Phyto_", "", x)
      x <- base::gsub("_Abund$", "", x)
      x <- base::gsub("Phyto_RMSE", "RMSE", x)
      x <- base::gsub("Phyto_CondNum", "Condition_Number", x)
      return(x)
    }
    
    .get_palette <- function(data_classes) {
      config_palette <- rv$config$reporting$plotting$custom_palette
      if (base::is.null(config_palette)) return(NULL) 
      user_colors <- base::unlist(config_palette)
      final_palette <- base::c()
      for (cls in data_classes) {
        if (cls %in% base::names(user_colors)) {
          final_palette[cls] <- user_colors[[cls]]
        } else {
          hyphenated <- base::gsub("\\.", "-", cls) 
          if (hyphenated %in% base::names(user_colors)) {
            final_palette[cls] <- user_colors[[hyphenated]]
          } else { final_palette[cls] <- NA }
        }
      }
      missing_entries <- base::names(final_palette)[base::is.na(final_palette)]
      if (base::length(missing_entries) > 0) {
        default_colors <- scales::hue_pal()(base::length(missing_entries))
        for(i in base::seq_along(missing_entries)) { final_palette[missing_entries[i]] <- default_colors[i] }
      }
      return(final_palette)
    }
    
    .plot_area <- function(data) {
      long_df <- data |> 
        dplyr::select(UniqueID, tidyselect::starts_with("Phyto_")) |>
        dplyr::select(-tidyselect::ends_with("RMSE"), -tidyselect::ends_with("CondNum")) |>
        tidyr::pivot_longer(cols = -UniqueID, names_to = "Class", values_to = "Abundance") |>
        dplyr::mutate(Class = .clean_names(Class))
      
      unique_classes <- base::unique(long_df$Class)
      custom_pal <- .get_palette(unique_classes)
      
      p <- ggplot2::ggplot(long_df, ggplot2::aes(x = UniqueID, y = Abundance, fill = Class, group = Class)) +
        ggplot2::geom_area(alpha = 0.85, position = "fill") + 
        ggplot2::scale_y_continuous(labels = scales::percent) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank()) +
        ggplot2::labs(title = "Community Composition Matrix Profile", y = "Relative Yield Allocation (TChla)")
      
      if (!base::is.null(custom_pal)) p <- p + ggplot2::scale_fill_manual(values = custom_pal)
      return(p)
    }
    
    .plot_bar <- function(data) {
      long_df <- data |> 
        dplyr::select(UniqueID, tidyselect::starts_with("Phyto_")) |>
        dplyr::select(-tidyselect::ends_with("RMSE"), -tidyselect::ends_with("CondNum")) |>
        tidyr::pivot_longer(cols = -UniqueID, names_to = "Class", values_to = "Abundance") |>
        dplyr::mutate(Class = .clean_names(Class))
      
      unique_classes <- base::unique(long_df$Class)
      custom_pal <- .get_palette(unique_classes)
      
      p <- ggplot2::ggplot(long_df, ggplot2::aes(x = UniqueID, y = Abundance, fill = Class)) +
        ggplot2::geom_bar(stat = "identity", position = "stack", width=1) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank()) +
        ggplot2::labs(title = "Absolute Community Biomass Projections", y = "Concentration Density (ug/L)")
      
      if (!base::is.null(custom_pal)) p <- p + ggplot2::scale_fill_manual(values = custom_pal)
      return(p)
    }
    
    output$community_area_plot <- shiny::renderPlot({ 
      shiny::req(selected_dataset()$data_final)
      .plot_area(selected_dataset()$data_final) 
    })
    output$sample_bar_plot <- shiny::renderPlot({ 
      shiny::req(selected_dataset()$data_final)
      .plot_bar(selected_dataset()$data_final) 
    })
    
    shiny::observeEvent(input$generate_report_package_btn, {
      if (base::length(rv$analyzed_datasets) == 0) { 
        shiny::showNotification("Run the optimization phase prior to packaging.", type="error")
        return() 
      }
      
      shinyjs::disable("generate_report_package_btn")
      shinybusy::show_modal_spinner(text = "Saving Results...")
      
      base_output_dir <- rv$config$workspace$output_directory %||% "phytoclass_output"
      timestamp <- base::format(base::Sys.time(), "%Y-%m-%d_%H%M")
      session_output_dir <- base::file.path(base_output_dir, base::paste0("Session_", timestamp))
      plots_output_dir <- base::file.path(session_output_dir, "Plots")
      
      tryCatch({
        if (!base::dir.exists(session_output_dir)) base::dir.create(session_output_dir, recursive = TRUE)
        if (!base::dir.exists(plots_output_dir)) base::dir.create(plots_output_dir)
        
        wb_master <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb_master, "Session Info")
        
        cluster_k_auto <- if(!base::is.null(rv$cluster_diagnostics)) rv$cluster_diagnostics$info$optimal_k else "N/A"
        cluster_k_used <- if(!base::is.null(rv$cluster_diagnostics)) rv$cluster_diagnostics$info$used_k else "N/A"
        method_raw <- rv$config$strategy$method %||% "hclust"
        dist_metric <- if(method_raw == "By Pigment Cluster") "Euclidean" else "N/A"
        run_time <- if(!base::is.null(rv$performance_metrics)) paste(rv$performance_metrics$total_time_sec, "s") else "N/A"
        
        session_info <- base::data.frame(
          Parameter = c("Session ID", "Date", "Niter Cycles", "Cooling Decay Rate", "Grouping Paradigm", "Distance Framework", "Optimum K Target", "Realized K Grouping"),
          Value = c(rv$session_id, base::as.character(base::Sys.Date()), rv$config$phytoclass$niter, rv$config$phytoclass$step_size, method_raw, dist_metric, base::as.character(cluster_k_auto), base::as.character(cluster_k_used))
        )
        openxlsx::writeData(wb_master, "Session Info", session_info)
        
        if (!base::is.null(rv$qc_summary_df)) { 
          openxlsx::addWorksheet(wb_master, "QC Statistics")
          openxlsx::writeData(wb_master, "QC Statistics", rv$qc_summary_df) 
        }
        
        log_df <- base::data.frame(Timestamped_Log = rv$session_log)
        openxlsx::addWorksheet(wb_master, "Session Log")
        openxlsx::writeData(wb_master, "Session Log", log_df)
        
        unified_results <- base::list()
        for(ds_name in base::names(rv$analyzed_datasets)) {
          ds <- rv$analyzed_datasets[[ds_name]]
          if(!base::is.null(ds$data_final)) {
            clean <- ds$data_final |>
              dplyr::select(-dplyr::any_of(c("cleaning_status", "duplicate_status", "qc_pass", "filter_status_geo", "filter_status_temporal", "filter_status_depth", "original_row_num", "year", "month", "day")))
            base::colnames(clean) <- .clean_names(base::colnames(clean))
            clean$Analysis_Group <- ds_name
            if("UniqueID" %in% base::names(clean)) clean <- clean |> dplyr::select(UniqueID, Analysis_Group, dplyr::everything())
            unified_results[[ds_name]] <- clean
          }
        }
        
        if(base::length(unified_results) > 0) {
          global_df <- dplyr::bind_rows(unified_results)
          openxlsx::addWorksheet(wb_master, "All Samples Combined")
          openxlsx::writeData(wb_master, "All Samples Combined", global_df)
        }
        
        openxlsx::saveWorkbook(wb_master, file = base::file.path(session_output_dir, "PhytoClass_Master_Report.xlsx"), overwrite = TRUE)
        if (base::exists("save_config") && base::is.function(save_config)) save_config(rv$config, base::file.path(session_output_dir, "config_session.yaml"))
        
        if (!base::is.null(rv$cluster_diagnostics) && !base::is.null(rv$cluster_diagnostics$elbow_plot)) {
          ggplot2::ggsave(filename = base::file.path(plots_output_dir, "Elbow_Plot_Optimization.png"), plot = rv$cluster_diagnostics$elbow_plot, width = 8, height = 6, dpi = 300)
        }
        
        for(ds_name in base::names(rv$analyzed_datasets)) {
          ds <- rv$analyzed_datasets[[ds_name]]
          if(!base::is.null(ds$data_final)) {
            clean_output <- ds$data_final |>
              dplyr::select(-dplyr::any_of(c("cleaning_status", "duplicate_status", "qc_pass", "filter_status_geo", "filter_status_temporal", "filter_status_depth", "original_row_num", "year", "month", "day")))
            base::colnames(clean_output) <- .clean_names(base::colnames(clean_output))
            if("UniqueID" %in% base::names(clean_output)) clean_output <- clean_output |> dplyr::select(UniqueID, dplyr::everything())
            
            openxlsx::write.xlsx(clean_output, file = base::file.path(session_output_dir, base::paste0("Result_", ds_name, ".xlsx")))
            ggplot2::ggsave(filename = base::file.path(plots_output_dir, base::paste0("AreaPlot_", ds_name, ".png")), plot = .plot_area(ds$data_final), width = 10, height = 6, dpi = 300)
            ggplot2::ggsave(filename = base::file.path(plots_output_dir, base::paste0("BarPlot_", ds_name, ".png")), plot = .plot_bar(ds$data_final), width = 10, height = 6, dpi = 300)
          }
        }
        
        .log_event("SUCCESS", base::paste("Final report parcel output configured at standard handle mount:", session_output_dir))
        local_rv$saved_package_path <- session_output_dir
        shiny::showNotification("Export Complete! Directory generated successfully.", type = "message")
        
      }, error = function(e) {
        if (base::grepl("Permission denied", e$message) || base::grepl("cannot open file", e$message)) {
          shiny::showModal(shiny::modalDialog(title = "File System Locked", "Permission denied. If you have a previous export file currently open in Excel, please close it and try again.", type = "error"))
        } else {
          shiny::showNotification(base::paste("Export Failed:", e$message), type = "error", duration = 10)
        }
        .log_event("EXPORT FAULT", base::paste("Export failed:", e$message))
      }, finally = { 
        shinybusy::remove_modal_spinner() 
        shinyjs::enable("generate_report_package_btn")
      })
    })
  })
}