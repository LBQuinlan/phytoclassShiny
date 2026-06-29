@echo off
title phytoclassShiny
echo ========================================================
echo   PHYTOCLASSSHINY ENGINE LAUNCHER
echo ========================================================
cd /d "%~dp0"

:: 1. Locate the R Engine
where Rscript >nul 2>nul
if %ERRORLEVEL% EQU 0 (
    set "RSCRIPT_EXE=Rscript"
    goto :CHECK_ENV
)

for /d %%a in ("C:\Program Files\R\R-*") do (
    if exist "%%a\bin\Rscript.exe" set "RSCRIPT_EXE=%%a\bin\Rscript.exe"
)

if not defined RSCRIPT_EXE (
    echo [CRITICAL ERROR] R is not installed or cannot be found.
    echo Please install R from https://cran.r-project.org/ before running this app.
    pause
    exit /b
)

:CHECK_ENV
:: 2. Verify Sandbox Integrity (Checks the system subfolder)
if not exist "system\app_packages\" goto :RUN_SETUP
if not exist ".Rprofile" goto :RUN_SETUP
goto :LAUNCH_APP

:RUN_SETUP
:: 3. First-Time Environment Construction
echo.
echo   [!] First-time run or incomplete environment detected.
echo   [!] Initializing secure sandbox and downloading required packages...
echo   [!] Please wait. This may take a few minutes depending on your internet connection.
echo.
"%RSCRIPT_EXE%" system\package_installer.R

if not exist ".Rprofile" (
    echo.
    echo [CRITICAL ERROR] Environment setup failed to complete. 
    echo Please check 'system\phytoclassShiny_launch_log.txt' for details.
    pause
    exit /b
)
echo.
echo   [OK] Environment perfectly configured. Proceeding to launch...
echo.

:LAUNCH_APP
:: 4. Start the Application
echo   Please wait... The app will open in your default web browser.
echo   [!] DO NOT CLOSE THIS WINDOW WHILE USING THE APP [!]
echo ========================================================
"%RSCRIPT_EXE%" app.R

:: 5. Keep Window Open Post-Execution
echo.
echo ========================================================
echo   PHYTOCLASSSHINY SESSION ENDED.
echo   You may now safely close this window.
echo ========================================================
pause