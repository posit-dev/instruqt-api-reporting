# Instruqt Hours Reporting

A simple Shiny dashboard for tracking Instruqt infrastructure hours consumed.

## Features

- **Total Hours Display**: Large, clear display of total infrastructure hours consumed
- **Hours Timeline**: Line chart showing hours consumed over time
- **Daily Breakdown**: Bar chart showing daily hours consumption for the last 30 days
- **Date Range Filter**: Easily adjust the date range to view specific periods

## Setup

### 1. Install R Packages

```r
install.packages(c(
  "shiny",
  "shinydashboard",
  "plotly",
  "dplyr",
  "lubridate",
  "httr",
  "jsonlite"
))
```

### 2. Set Environment Variables

Set your Instruqt API key and team slug:

```bash
export INSTRUQT_API_KEY=your_api_key_here
export INSTRUQT_TEAM_SLUG=posit
```

Or set them in R:

```r
Sys.setenv(INSTRUQT_API_KEY = "your_api_key_here")
Sys.setenv(INSTRUQT_TEAM_SLUG = "posit")
```

**Note**: Find your team slug in your Instruqt dashboard URL: `https://play.instruqt.com/YOUR-TEAM-SLUG/...`

## Running the App

### Option 1: Using the launcher script
```bash
Rscript run_app.R
```

### Option 2: Using the shell script
```bash
./run_app.sh
```

### Option 3: From RStudio
1. Open `app.R`
2. Set environment variables (see above)
3. Click "Run App"

### Option 4: From R console
```r
Sys.setenv(INSTRUQT_TEAM_SLUG = "posit")
library(shiny)
runApp("app.R")
```

Then visit http://localhost:3838 and click "Refresh Data" to load your data.

## Files

- `app.R` - Main Shiny dashboard application
- `instruqt_api.R` - API client for fetching consumption data from Instruqt GraphQL API
- `run_app.R` - R launcher script
- `run_app.sh` - Bash launcher script
- `README.md` - This file

## Usage Tips

1. Use the date range filter in the sidebar to select the time period you want to view
2. Click "Refresh Data" to reload the data after changing the date range
3. The dashboard shows infrastructure hours consumed (sandbox hours)
4. Hover over charts for detailed daily values

## Troubleshooting

**API Key Not Found**
- Ensure `INSTRUQT_API_KEY` is set as an environment variable
- Restart your R session after setting variables

**Data Not Loading**
- Verify your API key is valid
- Check your internet connection
- Ensure `INSTRUQT_TEAM_SLUG` is set correctly

**Slow Performance**
- Reduce date range filter
- The API returns all records at once; expect initial load time

## Support

For Instruqt API issues, consult the [Instruqt API Documentation](https://api-docs.instruqt.com/)

---

Dashboard powered by R Shiny and the Instruqt GraphQL API
