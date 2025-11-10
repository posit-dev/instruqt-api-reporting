# Instruqt Hours Reporting Launcher
# This script sets the required environment variables and launches the Shiny app

cat("=== Instruqt Hours Reporting ===\n\n")

# Check if API key is set
api_key <- Sys.getenv("INSTRUQT_API_KEY")
if (api_key == "") {
  cat("ERROR: INSTRUQT_API_KEY environment variable is not set\n\n")
  cat("Please set it with:\n")
  cat("  Sys.setenv(INSTRUQT_API_KEY = 'your_api_key_here')\n\n")
  cat("Or run:\n")
  cat("  export INSTRUQT_API_KEY=your_api_key_here\n\n")
  stop("INSTRUQT_API_KEY not set")
}

# Set team slug
Sys.setenv(INSTRUQT_TEAM_SLUG = "posit")

cat("Environment variables configured:\n")
cat("  INSTRUQT_API_KEY: ✓ (set)\n")
cat("  INSTRUQT_TEAM_SLUG:", Sys.getenv("INSTRUQT_TEAM_SLUG"), "\n\n")
cat("Starting Shiny app...\n\n")

# Load and run the app
library(shiny)
runApp("app.R", port = 3838, host = "0.0.0.0")
