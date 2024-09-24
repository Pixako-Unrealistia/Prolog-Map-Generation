@echo off
REM Set the path to the SWI-Prolog installation directory
SET PROLOG_DIR=%~dp0

REM Add SWI-Prolog bin directory to the PATH
SET PATH=%PROLOG_DIR%bin;%PATH%

REM Verify if SWI-Prolog is detected
swipl --version
IF %ERRORLEVEL% NEQ 0 (
    echo SWI-Prolog is not detected. Please check your installation.
) ELSE (
    echo SWI-Prolog is successfully detected.
)

pause
