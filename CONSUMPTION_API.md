# Consumption Hours API

## Overview

The Instruqt API provides consumption hours data through the `ConsumptionInsight` GraphQL type. This gives you the actual infrastructure/sandbox runtime hours, which differs from user activity hours in play reports.

## Key Differences

- **Play Reports (`playReports`)**: User activity time (~438 hours for May 11 - Nov 7, 2025)
- **Consumption Insights (`consumption`)**: Infrastructure runtime (~1,184 hours for the same period)

The consumption hours match what you see in the Instruqt dashboard under "Time actual(Actual)".

## Usage

The `get_consumption_hours()` function is now available in `instruqt_api.R`:

```r
# Get consumption hours for a date range
consumption <- get_consumption_hours(
  start_date = "2025-05-11T00:00:00Z",
  end_date = "2025-11-07T23:59:59Z",
  play_type = "ALL"  # Options: "ALL", "NORMAL", "DEVELOPER"
)

# Returns:
# $totalSandboxHours: Total infrastructure hours
# $totalHotStartHours: Total hot start hours
```

## Play Types

- **ALL**: All plays (sandbox + developer mode)
- **NORMAL**: Regular user plays only
- **DEVELOPER**: Developer mode plays only

## Dashboard Integration

The Shiny dashboard now uses the consumption API for the "Hours Consumed" metric, ensuring it matches the Instruqt dashboard numbers.
