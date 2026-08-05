# ============================================================================
# 00_Setup_phytoclassShiny.R (Initialization Script)
# ============================================================================
FORCE_UPDATE_PACKAGES <- FALSE
base::options(repos = base::c(CRAN = "https://cloud.r-project.org/"))

# 0. PATH DETECTOR
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

# 1. R VERSION CHECK
current_r_numeric <- base::paste(base::R.version$major, base::R.version$minor, sep = ".")

if (base::package_version(current_r_numeric) < base::package_version("4.4.0")) {
  base::cat("\n[!] CRITICAL: Outdated R version detected.\n")
  if (.Platform$OS.type == "windows") {
    base::cat("\n*** WINDOWS UPGRADE INITIATED ***\n")
    base::cat("Look for the flashing shield icon on your taskbar and grant Admin access.\n")
    base::cat("*********************************\n\n")
    
    utils::install.packages("installr")
    installr::updateR(browse_to_download_page = FALSE, silent = FALSE)
    base::stop("R upgrade initiated. Restart setup after completion.")
  } else {
    base::cat("Visit https://cran.r-project.org/ to download the latest R version.\n")
    base::stop("Launch aborted: Outdated environment.")
  }
}

base::cat("\n--- Running Setup & Generating Audit Log ---\n")

# 2. SANDBOX INITIALIZATION
app_lib <- base::file.path(base::getwd(), "app_packages")
if (!base::dir.exists(app_lib)) { base::dir.create(app_lib, recursive = TRUE) }
base::.libPaths(base::c(app_lib, base::.Library))

# 3. NAMESPACE PURGE
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

# 4. DEPENDENCY MANIFEST
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
  "phytoclass" = "2.3.1", "shinybusy" = "0.3.3", "tidyselect" = "1.2.1", "zip" = "2.3.0"
)

# 5. ENVIRONMENT AUDIT
installed_pkgs_before <- utils::installed.packages(lib.loc = app_lib)
installed_versions_before <- if (base::nrow(installed_pkgs_before) > 0) {
  stats::setNames(installed_pkgs_before[, "Version"], installed_pkgs_before[, "Package"])
} else {
  base::character(0)
}

current_user <- base::Sys.info()["user"]
app_write_access <- base::ifelse(base::file.access(".", 2) == 0, "GRANTED", "DENIED")
lib_write_access <- base::ifelse(base::file.access(app_lib, 2) == 0, "GRANTED", "DENIED")

log_lines <- base::c(
  "==========================================================================",
  "                 PHYTOCLASSSHINY SETUP AUDIT LOG                          ",
  "==========================================================================",
  base::paste("Timestamp:     ", base::Sys.time()),
  base::paste("R Version:     ", base::R.version.string),
  base::paste("OS System:     ", base::Sys.info()["sysname"], base::Sys.info()["release"]),
  base::paste("Current User:  ", current_user),
  "--------------------------------------------------------------------------",
  "ENVIRONMENT DIAGNOSTICS:",
  base::paste("App Folder:    ", base::dirname(base::getwd())),
  base::paste(" -> App Write: ", app_write_access),
  base::paste("Local Library: ", app_lib),
  base::paste(" -> Lib Write: ", lib_write_access),
  "--------------------------------------------------------------------------",
  "PACKAGE PROCESSING DETAILS:",
  base::sprintf("%-20s | %-12s | %-12s | %-15s | %-10s", "Package", "Target Ver", "Pre-Run Ver", "Action Taken", "Outcome"),
  "--------------------------------------------------------------------------"
)

# 6. INSTALLATION EXECUTION
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

# 7. INTEGRITY VERIFICATION
installed_pkgs_after <- utils::installed.packages(lib.loc = app_lib)
installed_versions_after <- if (base::nrow(installed_pkgs_after) > 0) {
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

# 8. WORKSPACE CONFIGURATION (.Rprofile & .Rproj)
base::cat("\n--- Configuring Isolated Workspace ---\n")

root_dir <- base::dirname(base::getwd())

rprofile_path <- base::file.path(root_dir, ".Rprofile")
rprofile_content <- base::c(
  "# ==========================================================================",
  "# AUTOMATICALLY GENERATED BY PHYTOCLASSSHINY SETUP",
  "# ==========================================================================",
  "Sys.setenv(PHYTOCLASSSHINY_SANDBOX_ACTIVE = 'TRUE')",
  ".libPaths(base::file.path(base::getwd(), 'system', 'app_packages'))",
  "base::cat('\\n[✓] phytoclassShiny Sandbox Active\\n')",
  "",
  "if (interactive() && base::requireNamespace('rstudioapi', quietly = TRUE)) {",
  "  if (rstudioapi::isAvailable()) {",
  "    rstudioapi::navigateToFile('app.R')",
  "    base::cat('\\n[>] app.R loaded. Click \\'Run App\\' in the top right of the editor to start.\\n\\n')",
  "  }",
  "}"
)
base::writeLines(rprofile_content, rprofile_path)
base::cat(" [OK] Security sandbox (.Rprofile) generated successfully.\n")

rproj_files <- base::list.files(path = root_dir, pattern = "\\.Rproj$")
if (base::length(rproj_files) == 0) {
  rproj_path <- base::file.path(root_dir, "phytoclassShiny.Rproj")
  rproj_content <- base::c(
    "Version: 1.0",
    "",
    "RestoreWorkspace: No",
    "SaveWorkspace: No",
    "AlwaysSaveHistory: No",
    "",
    "EnableCodeIndexing: Yes",
    "UseSpacesForTab: Yes",
    "NumSpacesForTab: 2",
    "Encoding: UTF-8"
  )
  base::writeLines(rproj_content, rproj_path)
  base::cat(" [OK] RStudio Project file (phytoclassShiny.Rproj) created.\n")
} else {
  base::cat(base::sprintf(" [OK] Existing project file found: %s\n", rproj_files[1]))
}

if (base::length(missing_or_outdated) > 0) {
  base::cat("\n[!] SETUP INCOMPLETE: Packages failed to install. Check 'system/phytoclassShiny_launch_log.txt'.\n")
} else {
  base::cat("\n=======================================================\n")
  base::cat(" [OK] SETUP COMPLETE! Environment is perfectly configured.\n")
  base::cat("=======================================================\n")
  base::cat(" To open the app, double-click 'LAUNCH_PHYTOCLASSSHINY.bat'\n")
  base::cat(" or open 'phytoclassShiny.Rproj' in RStudio.\n\n")
}