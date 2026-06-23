# ============================================================================
# 00_Setup_Phytoclass.R (One-Time Initialization Script)
# ============================================================================
FORCE_UPDATE_PACKAGES <- FALSE
base::options(repos = base::c(CRAN = "https://cloud.r-project.org/"))

# 0. UNIVERSAL PATH DETECTOR (Stealth Mode to prevent RStudio Popups)
base::tryCatch({
  current_path <- NULL
  rs_pkg <- base::paste0("rstudio", "api")
  if (base::requireNamespace(rs_pkg, quietly = TRUE)) {
    rs_ns <- base::asNamespace(rs_pkg)
    if (rs_ns$isAvailable()) {
      current_path <- rs_ns$getActiveDocumentContext()$path
    }
  }
  if (base::is.null(current_path)) {
    sys_frames <- base::sys.frames()
    for (f in sys_frames) {
      if (!base::is.null(f$ofile)) { current_path <- f$ofile; break }
    }
  }
  if (base::is.null(current_path)) {
    cmd_args <- base::commandArgs(trailingOnly = FALSE)
    file_arg <- base::grep("^--file=", cmd_args, value = TRUE)
    if (base::length(file_arg) > 0) { current_path <- base::sub("^--file=", "", file_arg[1]) }
  }
  if (!base::is.null(current_path) && base::nzchar(current_path)) {
    base::setwd(base::dirname(base::normalizePath(current_path)))
  }
}, error = function(e) NULL)

# 1. PRIMARY R VERSION GATE & UAC FIX
current_r_numeric <- base::paste(base::R.version$major, base::R.version$minor, sep = ".")

if (base::package_version(current_r_numeric) < base::package_version("4.4.0")) {
  base::cat("\n[!] CRITICAL: Your core R software is out of date!\n")
  if (.Platform$OS.type == "windows") {
    base::cat("\n*** IMPORTANT WINDOWS NOTICE ***\n")
    base::cat("The R installer will now open. Please look for a flashing shield icon\n")
    base::cat("on your Windows taskbar. You MUST click it to grant Administrator access!\n")
    base::cat("********************************\n\n")
    
    utils::install.packages("installr")
    installr::updateR(browse_to_download_page = FALSE, silent = FALSE)
    base::stop("R upgrade initiated. Please restart after the wizard finishes.")
  } else {
    base::cat("Please visit https://cran.r-project.org/ to download the latest version of R.\n")
    base::stop("Launch aborted due to outdated R version.")
  }
}

base::cat("\n--- Running Setup & Generating Audit Log ---\n")

# 2. STRICT SANDBOXING (Locking out the user's default system library)
app_lib <- base::file.path(base::getwd(), "app_packages")
if (!base::dir.exists(app_lib)) { base::dir.create(app_lib, recursive = TRUE) }
base::.libPaths(base::c(app_lib, base::.Library))

# 3. UNCONDITIONAL RAM PURGE (Expanded to include hidden rendering engines)
conflict_prone_packages <- base::c(
  "shinybusy", "shinyWidgets", "shinyjs", "DT", "htmlwidgets",
  "bslib", "promises", "htmltools", "jsonlite", "shiny", "rlang"
)
for (ns in conflict_prone_packages) {
  if (ns %in% base::loadedNamespaces()) {
    base::tryCatch({ base::detach(base::paste0("package:", ns), unload = TRUE, character.only = TRUE) }, error = function(e) NULL)
    base::tryCatch({ base::unloadNamespace(ns) }, error = function(e) NULL)
  }
}

# 4. PACKAGE LIST & OS-AGNOSTIC INSTALL (Added jsonlite and htmlwidgets)
required_packages <- base::list(
  "shiny" = "1.10.0", "bslib" = "0.9.0", "shinyjs" = "2.1.0", 
  "shinyWidgets" = "0.9.0", "DT" = "0.33", "htmlwidgets" = "1.6.4",
  "jsonlite" = "1.8.8", "yaml" = "2.3.10", "dplyr" = "1.2.0", 
  "tidyr" = "1.3.1", "readxl" = "1.4.5", "openxlsx" = "4.2.8", 
  "lubridate" = "1.9.4", "digest" = "0.6.37", "rlang" = "1.1.7", 
  "tibble" = "3.3.0", "fs" = "1.6.6", "purrr" = "1.2.1", 
  "scales" = "1.4.0", "vegan" = "2.7-1", "cluster" = "2.1.8.1", 
  "factoextra" = "1.0.7", "ggplot2" = "3.5.2", "MASS" = "7.3-65", 
  "dynamicTreeCut" = "1.63-1", "glue" = "1.8.0", "stringdist" = "0.9.15", 
  "phytoclass" = "2.3.1", "shinybusy" = "0.3.3", "tidyselect" = "1.2.1"
)

# --- 5. INITIAL AUDIT & ENVIRONMENT CHECK ---
installed_pkgs_before <- utils::installed.packages(lib.loc = app_lib)
installed_versions_before <- if (base::nrow(installed_pkgs_before) > 0) {
  # FIX: Correctly mapped to the 'stats' library instead of 'base'
  stats::setNames(installed_pkgs_before[, "Version"], installed_pkgs_before[, "Package"])
} else {
  base::character(0)
}

current_user <- base::Sys.info()["user"]
app_write_access <- base::ifelse(base::file.access(".", 2) == 0, "GRANTED", "DENIED")
lib_write_access <- base::ifelse(base::file.access(app_lib, 2) == 0, "GRANTED", "DENIED")

log_lines <- base::c(
  "==========================================================================",
  "                      PHYTOCLASS SETUP AUDIT LOG                          ",
  "==========================================================================",
  base::paste("Timestamp:     ", base::Sys.time()),
  base::paste("R Version:     ", base::R.version.string),
  base::paste("OS System:     ", base::Sys.info()["sysname"], base::Sys.info()["release"]),
  base::paste("Current User:  ", current_user),
  "--------------------------------------------------------------------------",
  "ENVIRONMENT DIAGNOSTICS:",
  base::paste("App Folder:    ", base::getwd()),
  base::paste(" -> App Write: ", app_write_access),
  base::paste("Local Library: ", app_lib),
  base::paste(" -> Lib Write: ", lib_write_access),
  "--------------------------------------------------------------------------",
  "PACKAGE PROCESSING DETAILS:",
  base::sprintf("%-20s | %-12s | %-12s | %-15s | %-10s", "Package", "Target Ver", "Pre-Run Ver", "Action Taken", "Outcome"),
  "--------------------------------------------------------------------------"
)

# --- 6. INSTALLATION LOOP WITH DEEP LOGGING ---
for (pkg in base::names(required_packages)) {
  target_ver <- required_packages[[pkg]]
  pre_ver <- if (pkg %in% base::names(installed_versions_before)) installed_versions_before[[pkg]] else "None"
  
  needs_update <- FALSE
  action_label <- "None (Up to Date)"
  outcome_label = "Skipped"
  
  if (pre_ver == "None") {
    needs_update <- TRUE
    action_label <- "Install Fresh"
  } else if (base::package_version(pre_ver) < base::package_version(target_ver)) {
    needs_update <- TRUE
    action_label <- "Update Package"
  } else if (FORCE_UPDATE_PACKAGES) {
    needs_update <- TRUE
    action_label <- "Force Reinstall"
  }
  
  if (needs_update) {
    base::cat(base::sprintf("Processing: %s (Target: %s, Found: %s)\n", pkg, target_ver, pre_ver))
    base::tryCatch({
      utils::install.packages(pkg, lib = app_lib, dependencies = TRUE)
      outcome_label <- "SUCCESS"
    }, error = function(e) {
      # Check if the failure is due to a classic Windows file lock
      if (base::grepl("permission denied|cannot open file|lazy-load", e$message, ignore.case = TRUE) && .Platform$OS.type == "windows") {
        outcome_label <- "FAILED: File locked by Windows"
        base::cat(base::sprintf("   [!] OS LOCK DETECTED: Windows is blocking modifications to '%s'.\n", pkg))
        base::cat("       To fix this: Restart RStudio, press Ctrl+Shift+F10 to clear RAM, and re-run this setup script.\n")
      } else {
        outcome_label <- base::paste("FAILED:", e$message)
      }
    })
  }
  
  log_lines <- base::c(log_lines, base::sprintf("%-20s | %-12s | %-12s | %-15s | %-10s", pkg, target_ver, pre_ver, action_label, outcome_label))
}

# --- 7. FINAL INTEGRITY CHECK ---
installed_pkgs_after <- utils::installed.packages(lib.loc = app_lib)
installed_versions_after <- if (base::nrow(installed_pkgs_after) > 0) {
  # FIX: Correctly mapped to the 'stats' library instead of 'base'
  stats::setNames(installed_pkgs_after[, "Version"], installed_pkgs_after[, "Package"])
} else {
  base::character(0)
}

missing_or_outdated <- base::character(0)
log_lines <- base::c(log_lines, "--------------------------------------------------------------------------", "FINAL POST-RUN RECONCILIATION:", "--------------------------------------------------------------------------")

for (pkg in base::names(required_packages)) {
  post_ver <- if (pkg %in% base::names(installed_versions_after)) installed_versions_after[[pkg]] else "Missing"
  target_ver <- required_packages[[pkg]]
  
  status_marker <- "[OK]"
  if (post_ver == "Missing" || base::package_version(post_ver) < base::package_version(target_ver)) {
    status_marker <- "[CRITICAL ERROR]"
    missing_or_outdated <- base::c(missing_or_outdated, pkg)
  }
  
  log_lines <- base::c(log_lines, base::sprintf("%-16s Final Status: %-16s (Installed: %-10s | Required: %s)", status_marker, pkg, post_ver, target_ver))
}

log_lines <- base::c(log_lines, "==========================================================================")
base::writeLines(log_lines, "phytoclassShiny_launch_log.txt")

# Terminal Output to the user
if (base::length(missing_or_outdated) > 0) {
  base::cat("\n[!] SETUP INCOMPLETE: Some packages failed to install. Check 'phytoclassShiny_launch_log.txt' for details.\n")
} else {
  base::cat("\n=======================================================\n")
  base::cat(" [OK] SETUP COMPLETE! Environment is perfectly configured.\n")
  base::cat("=======================================================\n")
  base::cat(" You may now close this window. To open the app, simply\n")
  base::cat(" double-click the 'app.R' file and click 'Run App'.\n\n")
}