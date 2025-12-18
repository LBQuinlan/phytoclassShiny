# ============================================================================
#
#   _phytoclass_Shiny V1.0 - STEP 4: QC SUMMARY
#
#   Description:
#   The visual interface for Quality Control. It runs the checks and displays
#   a summary table showing exactly how many samples passed or failed your
#   cleaning rules.
#
# ============================================================================

# --- UI Function ---
qcUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Step 4: QC & Filtering"),
    p("Run the configured cleaning and filtering rules on all loaded datasets. Review the summary to see how many samples passed before proceeding."),
    hr(),
    
    fluidRow(
      column(10, offset = 1,
             wellPanel(
               h4("1. Execute"),
               div(style = "text-align: center;",
                   actionButton(ns("run_qc_and_aggregate_btn"), "Run QC Pipeline", class = "btn-primary btn-lg", icon = icon("cogs"))
               )
             ),
             
             wellPanel(
               h4("2. QC Audit Summary"),
               p("A breakdown of samples passing and failing each check."),
               DTOutput(ns("qc_summary_table"))
             )
      )
    )
  )
}


# --- Server Function ---
qcServer <- function(id, rv, .log_event, .update_workflow_state, reset_downstream_data) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    observeEvent(input$run_qc_and_aggregate_btn, {
      req(rv$config, length(rv$datasets_processed) > 0)
      
      show_modal_spinner(text = "Running QC pipeline...")
      tryCatch({
        qc_results <- run_qc_pipeline(rv$datasets_processed, rv$config)
        
        rv$datasets_processed <- qc_results$datasets_annotated
        rv$master_qc_data <- qc_results$master_qc_data
        
        if(!is.null(rv$master_qc_data) && nrow(rv$master_qc_data) > 0) {
          .log_event(paste("Success: QC pipeline complete.", nrow(rv$master_qc_data), "samples passed all checks."))
          reset_downstream_data("strategy")
          .update_workflow_state("step5")
          updateNavbarPage(session, "main_navbar", selected = "step5")
        } else {
          .log_event("Warning: No samples passed QC. Workflow halted.")
          showNotification("No samples passed the QC and filtering steps. Please check your data or adjust settings in Step 1.", type="warning", duration = 10)
        }
        
        rv$qc_summary_df <- .generate_qc_summary_df(rv$datasets_processed)
        
      }, error = function(e){ 
        .log_event(paste("ERROR during QC:", e$message))
        showModal(modalDialog(title = "QC Pipeline Error", e$message))
      }, finally = { 
        remove_modal_spinner() 
      })
    })
    
    output$qc_summary_table <- renderDT({
      req(rv$qc_summary_df)
      df <- rv$qc_summary_df
      
      # SAFETY CHECK: Ensure the expected columns exist before formatting
      if (nrow(df) == 0 || !all(c("Failed", "Passed") %in% names(df))) {
        return(datatable(tibble(Status = "No Data Available"), rownames=FALSE, options=list(dom='t')))
      }
      
      datatable(df, rownames = FALSE, options = list(dom = 't', ordering = FALSE, pageLength = 10)) %>%
        formatStyle('Failed', color = styleInterval(0, c('black', '#dc3545')), fontWeight = styleInterval(0, c('normal', 'bold'))) %>%
        formatStyle('Passed', color = '#28a745')
    })
    
    .generate_qc_summary_df <- function(datasets_processed) {
      # Return valid empty structure if no input
      empty_structure <- tibble(Category = character(), Rule = character(), Passed = integer(), Failed = integer())
      if(length(datasets_processed) == 0) return(empty_structure)
      
      all_annotated <- dplyr::bind_rows(lapply(datasets_processed, `[[`, "data_annotated"))
      if(nrow(all_annotated) == 0) return(empty_structure)
      
      total_initial <- sum(purrr::map_int(datasets_processed, ~nrow(.x$data_original)))
      
      summary_list <- list(
        list(Category = "Start", Rule = "Total Samples Loaded", Passed = total_initial, Failed = 0),
        list(Category="Pigment QC", Rule="Missing Pigment Values", Failed=sum(all_annotated$cleaning_status == "Flagged_NA")),
        list(Category="Pigment QC", Rule="Negative Pigment Values", Failed=sum(all_annotated$cleaning_status == "Flagged_Negative")),
        list(Category="Pigment QC", Rule="Zero Pigment Sum", Failed=sum(all_annotated$cleaning_status == "Flagged_ZeroSum")),
        list(Category="Pigment QC", Rule="Duplicate Pigment Profile", Failed=sum(all_annotated$duplicate_status == "Flagged_Duplicate")),
        list(Category="Filtering", Rule="Geospatial Filter", Failed=sum(all_annotated$filter_status_geo %in% c("Flagged_GeoNA", "Flagged_GeoRange"))),
        list(Category="Filtering", Rule="Temporal Filter", Failed=sum(all_annotated$filter_status_temporal %in% c("Flagged_DateNA", "Flagged_DateRange"))),
        list(Category="Filtering", Rule="Depth Filter", Failed=sum(all_annotated$filter_status_depth %in% c("Flagged_DepthNA", "Flagged_DepthRange")))
      )
      
      summary_df <- dplyr::bind_rows(summary_list) %>%
        dplyr::mutate(Passed = ifelse(Category == "Start", Passed, total_initial - Failed))
      
      total_passed <- sum(
        all_annotated$cleaning_status == "Keep" &
          all_annotated$duplicate_status == "Keep_Unique" &
          all_annotated$filter_status_geo == "Keep" &
          all_annotated$filter_status_temporal == "Keep" &
          all_annotated$filter_status_depth == "Keep"
      )
      
      summary_df <- summary_df %>% 
        add_row(Category="Finish", Rule="Total Samples Passing All Checks", Passed=total_passed, Failed=total_initial-total_passed)
      
      return(summary_df)
    }
  })
}