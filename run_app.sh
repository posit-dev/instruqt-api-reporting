#!/bin/bash

# Instruqt Analytics Dashboard Launcher
# This script sets the required environment variables and launches the Shiny app

echo "=== Instruqt Analytics Dashboard ==="
echo ""

# Check if API key is already set
if [ -z "$INSTRUQT_API_KEY" ]; then
    echo "ERROR: INSTRUQT_API_KEY environment variable is not set"
    echo ""
    echo "Please set it with:"
    echo "  export INSTRUQT_API_KEY=your_api_key_here"
    echo ""
    exit 1
fi

# Set team slug
export INSTRUQT_TEAM_SLUG=posit

echo "Environment variables configured:"
echo "  INSTRUQT_API_KEY: ✓ (set)"
echo "  INSTRUQT_TEAM_SLUG: $INSTRUQT_TEAM_SLUG"
echo ""
echo "Starting Shiny app..."
echo ""

# Run the Shiny app
Rscript -e "shiny::runApp('app.R', port=3838, host='0.0.0.0')"
