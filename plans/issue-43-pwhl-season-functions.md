# Implementation Plan: PWHL inRegularSeason / inPlayoffs / inOffSeason Functions (Issue #43)

**Issue Reference:** [#43: inRegularSeason/Playoff/Offseason functions for PWHL](https://github.com/pbulsink/HockeyModel/issues/43)  
**Related PR / Format Baseline:** [#42: API Cleanup](https://github.com/pbulsink/HockeyModel/issues/42) / PR #59  
**Target Branch:** `fix-43-inRegularSeason-PWHL`  
**Date:** October 7, 2026  

---

## 1. Executive Summary & Objectives

In HockeyModel, the current functions `inRegularSeason()`, `inPlayoffs()`, and `inOffSeason()` only operate on NHL data, directly make HTTP requests to the NHL statistics API (`https://api.nhle.com/stats/rest/en/season`) without error-recovery or fallback, and do not support PWHL.

This plan addresses GitHub Issue #43 by:
1. Converting the exported functions `inRegularSeason()`, `inPlayoffs()`, and `inOffSeason()` into unified front-end functions that accept an optional `league` argument defaulting to both leagues (`league = NULL`), returning a named list `list(nhl = ..., pwhl = ...)` for both leagues or an atomic scalar when a single league is selected.
2. Converting the existing NHL logic into internal functions `.in_regular_season_nhl()`, `.in_playoffs_nhl()`, and `.in_off_season_nhl()` (with aliases `.inRegularSeasonNHL()`, etc.), adding robust `tryCatch` handling around the NHL API hit with a fallback to inspecting `HockeyModel::schedule` to check if `date` is past the last scheduled regular season game or playoff game.
3. Implementing parallel internal functions for PWHL: `.in_regular_season_pwhl()`, `.in_playoffs_pwhl()`, and `.in_off_season_pwhl()` (with aliases `.inRegularSeasonPWHL()`, etc.) that query the PWHL HockeyTech API via `getPWHLSeasons()` and fall back to inspecting `HockeyModel::pwhlSchedule`.
4. Updating existing internal callers (e.g. `.daily_summary_nhl()`, `.daily_summary_pwhl()`, and `.tweet()`) so their conditional checks cleanly evaluate single-league logicals.
5. Providing full unit test coverage and VCR cassette integration for happy paths and fallback scenarios.

---

## 2. API Design & Function Signatures

### 2.1 Public Front-End Functions (Exported)

Following the league resolver pattern established in Issue #42 (`.resolve_frontend_leagues()` and `.simplify_frontend_result()` in `R/frontend-core.R`):

```r
#' Check if date falls in regular season
#'
#' @param date (`Date` or `character(1)`) Date to check. Defaults to [Sys.Date()].
#' @param boolean (`logical(1)`) When `TRUE` (default), returns logical indicator.
#'   When `FALSE`, returns the season ID string (or `FALSE` if not in season).
#' @param league (`character(1)` or `NULL`) League selector:
#'   * `NULL`, `NA`, or `"both"`: Evaluates both leagues and returns a named list (`$nhl`, `$pwhl`).
#'   * `"nhl"`: Evaluates NHL only and returns a single result.
#'   * `"pwhl"`: Evaluates PWHL only and returns a single result.
#' @returns Logical or season ID string for single league, or named list (`nhl`, `pwhl`) for both.
#' @export
inRegularSeason <- function(date = Sys.Date(), boolean = TRUE, league = NULL)

#' Check if date falls in playoffs
#'
#' @param date (`Date` or `character(1)`) Date to check. Defaults to [Sys.Date()].
#' @param boolean (`logical(1)`) When `TRUE` (default), returns logical indicator.
#'   When `FALSE`, returns the season ID string (or `FALSE` if not in playoffs).
#' @param league (`character(1)` or `NULL`) League selector: `NULL`, `NA`, `"both"`, `"nhl"`, or `"pwhl"`.
#' @returns Logical or season ID string for single league, or named list (`nhl`, `pwhl`) for both.
#' @export
inPlayoffs <- function(date = Sys.Date(), boolean = TRUE, league = NULL)

#' Check if date falls in off-season
#'
#' @param date (`Date` or `character(1)`) Date to check. Defaults to [Sys.Date()].
#' @param league (`character(1)` or `NULL`) League selector: `NULL`, `NA`, `"both"`, `"nhl"`, or `"pwhl"`.
#' @returns Logical `TRUE`/`FALSE` for single league, or named list (`nhl`, `pwhl`) for both.
#' @export
inOffSeason <- function(date = Sys.Date(), league = NULL)
```

### 2.2 Internal Functions (Not Exported)

Following Issue #42 snake_case convention with camelCase aliases as requested in Issue #43:

- **NHL Internal Functions:**
  - `.in_regular_season_nhl(date = Sys.Date(), boolean = TRUE, schedule = HockeyModel::schedule)`
  - `.in_playoffs_nhl(date = Sys.Date(), boolean = TRUE, schedule = HockeyModel::schedule)`
  - `.in_off_season_nhl(date = Sys.Date(), schedule = HockeyModel::schedule)`
  - Aliases:
    - `.inRegularSeasonNHL <- .in_regular_season_nhl`
    - `.inPlayoffsNHL <- .in_playoffs_nhl`
    - `.inOffSeasonNHL <- .in_off_season_nhl`

- **PWHL Internal Functions:**
  - `.in_regular_season_pwhl(date = Sys.Date(), boolean = TRUE, schedule = HockeyModel::pwhlSchedule)`
  - `.in_playoffs_pwhl(date = Sys.Date(), boolean = TRUE, schedule = HockeyModel::pwhlSchedule)`
  - `.in_off_season_pwhl(date = Sys.Date(), schedule = HockeyModel::pwhlSchedule)`
  - Aliases:
    - `.inRegularSeasonPWHL <- .in_regular_season_pwhl`
    - `.inPlayoffsPWHL <- .in_playoffs_pwhl`
    - `.inOffSeasonPWHL <- .in_off_season_pwhl`

*Note on `schedule` parameter in internals:* Defaulting to the package dataset while accepting a custom `schedule` parameter allows tests to cleanly test fallback behavior and past-last-game edge cases without modifying package globals or mocking network layers.

---

## 3. Detailed Logic & Fallback Strategy

### 3.1 NHL Implementation

1. **Validation:**
   - Verify `date` with `is.Date(date)`. Abort via `cli::cli_abort("{.arg date} must be a Date or date-like value.")` on invalid input.
   - Convert `date <- as.Date(date)`.
   - Validate `boolean` is a single non-NA logical.

2. **API Attempt:**
   - Wrap the request in `tryCatch(..., error = function(e) NULL)`:
     ```r
     seasons_data <- tryCatch({
       url <- "https://api.nhle.com/stats/rest/en/season"
       resp <- httr2::request(url) |>
         httr2::req_cache(tempdir()) |>
         httr2::req_retry(max_seconds = 120) |>
         httr2::req_perform() |>
         httr2::resp_body_string() |>
         jsonlite::fromJSON()
       resp$data
     }, error = function(e) NULL)
     ```
   - If `!is.null(seasons_data)` and `nrow(seasons_data) > 0`:
     - **Regular Season:**
       - Match rows: `as.Date(seasons_data$startDate) <= date & as.Date(seasons_data$regularSeasonEndDate) >= date`.
       - If matching row found: `if (boolean) TRUE else as.character(match$id)`.
       - If no match found, check if `date` is between known regular season end and season end (which means playoffs) or completely outside: return `FALSE`.
     - **Playoffs:**
       - Match rows: `as.Date(seasons_data$regularSeasonEndDate) <= date & as.Date(seasons_data$endDate) >= date`.
       - If matching row found: `if (boolean) TRUE else as.character(match$id)`.
       - Else: `FALSE`.
     - **Off-Season:**
       - Match rows: `as.Date(seasons_data$startDate) <= date & as.Date(seasons_data$endDate) >= date`.
       - If matching row found: `FALSE` (in season).
       - Else: `TRUE` (off-season).

3. **Schedule Fallback (when API fails or returns NULL):**
   - Inspect `schedule`:
     ```r
     reg_games <- schedule[!is.na(schedule$Date) & schedule$GameType == "R", ]
     playoff_games <- schedule[!is.na(schedule$Date) & schedule$GameType == "P", ]
     ```
   - **Regular Season Fallback:**
     - If `nrow(reg_games) == 0`: return `FALSE`.
     - `min_date <- min(reg_games$Date)`
     - `max_date <- max(reg_games$Date)`
     - If `date > max_date`: past last scheduled regular season game -> return `FALSE`.
     - If `date < min_date`: before regular season start -> return `FALSE`.
     - If `min_date <= date & date <= max_date`: return `if (boolean) TRUE else get_season_id_from_schedule(reg_games)`.
   - **Playoffs Fallback:**
     - If `nrow(playoff_games) > 0`:
       - `min_p <- min(playoff_games$Date)`
       - `max_p <- max(playoff_games$Date)`
       - If `min_p <= date & date <= max_p`: return `if (boolean) TRUE else get_season_id_from_schedule(playoff_games)`.
     - Else: return `FALSE`.
   - **Off-Season Fallback:**
     - If `nrow(schedule) == 0`: return `TRUE`.
     - If `date < min(schedule$Date) || date > max(schedule$Date)`: return `TRUE`.
     - Else: return `FALSE`.

### 3.2 PWHL Implementation

1. **Validation:**
   - Same `is.Date(date)` and `boolean` validation as NHL.

2. **API Attempt:**
   - Call `getPWHLSeasons()` wrapped in `tryCatch(..., error = function(e) NULL)`.
   - Result schema: `id` (integer), `career` (1/0), `playoff` (1/0), `start_date` (Date), `end_date` (Date).
   - If `!is.null(seasons)` and `nrow(seasons) > 0`:
     - **Regular Season (`career == 1 & playoff == 0`):**
       - Match rows: `seasons$career == 1 & seasons$playoff == 0 & seasons$start_date <= date & seasons$end_date >= date`.
       - If match found: `if (boolean) TRUE else as.character(match$id)`.
       - Else: `FALSE`.
     - **Playoffs (`career == 1 & playoff == 1`):**
       - Match rows: `seasons$career == 1 & seasons$playoff == 1 & seasons$start_date <= date & seasons$end_date >= date`.
       - If match found: `if (boolean) TRUE else as.character(match$id)`.
       - Else: `FALSE`.
     - **Off-Season (`career == 1`):**
       - Active season match: `seasons$career == 1 & seasons$start_date <= date & seasons$end_date >= date`.
       - If active match found: `FALSE` (not off-season).
       - Else: `TRUE` (off-season).

3. **Schedule Fallback (when API fails or returns NULL):**
   - Inspect `schedule` (`HockeyModel::pwhlSchedule`):
     ```r
     reg_games <- schedule[!is.na(schedule$Date) & schedule$GameType == "R", ]
     playoff_games <- schedule[!is.na(schedule$Date) & schedule$GameType == "P", ]
     ```
   - **Regular Season Fallback:**
     - If `nrow(reg_games) == 0`: return `FALSE`.
     - If `date > max(reg_games$Date)`: past last scheduled regular season game -> return `FALSE`.
     - If `date < min(reg_games$Date)`: return `FALSE`.
     - If in range: return `if (boolean) TRUE else as.character(max(reg_games$GameID))`.
   - **Playoffs Fallback:**
     - If `nrow(playoff_games) > 0 & date >= min(playoff_games$Date) & date <= max(playoff_games$Date)`:
       return `if (boolean) TRUE else as.character(max(playoff_games$GameID))`.
     - Else: return `FALSE`.
   - **Off-Season Fallback:**
     - If `nrow(schedule) == 0`: return `TRUE`.
     - If `date < min(schedule$Date) || date > max(schedule$Date)`: return `TRUE`.
     - Else: return `FALSE`.

---

## 4. Codebase Call Site Updates

Because `inRegularSeason()`, `inPlayoffs()`, and `inOffSeason()` default to `league = NULL` returning a named list `list(nhl = ..., pwhl = ...)`, any existing caller doing `if (inRegularSeason())` would encounter:
`Error in if (inRegularSeason()) : argument is not interpretable as logical`

We will update the following files to specify the league explicitly:
1. `R/frontend-daily-summary.R`:
   - Line 17: `if (inOffSeason(league = "NHL"))`
   - Line 116: `if (inRegularSeason(league = "NHL"))`
   - Line 209: `if (inRegularSeason(league = "NHL"))`
   - Line 217: `else if (inPlayoffs(league = "NHL"))`
   - Line 225: `if (as.numeric(format(Sys.Date(), "%w")) == 1 && inRegularSeason(league = "NHL"))`
   - Line 233: `if (as.numeric(format(Sys.Date(), "%w")) == 0 && inRegularSeason(league = "NHL"))`
   - Line 239: `if (as.numeric(format(Sys.Date(), "%w")) == 2 && inRegularSeason(league = "NHL"))`
   - In `.daily_summary_pwhl()`: integrate `inOffSeason(league = "PWHL")` for consistency.
2. `R/frontend-social.R`:
   - Line 88: `if (inRegularSeason(league = "NHL"))`

---

## 5. File Layout & Structure

```
HockeyModel/
├── R/
│   ├── frontend-season.R          # Shared front-end functions: inRegularSeason(), inPlayoffs(), inOffSeason()
│   ├── nhl-team-season-lookup.R   # Internal NHL functions: .in_regular_season_nhl(), .in_playoffs_nhl(), .in_off_season_nhl()
│   ├── pwhl-api-fetch.R           # Internal PWHL functions: .in_regular_season_pwhl(), .in_playoffs_pwhl(), .in_off_season_pwhl()
│   ├── frontend-daily-summary.R   # Updated with explicit league = "NHL"
│   └── frontend-social.R          # Updated with explicit league = "NHL"
├── tests/testthat/
│   ├── test-frontend-season.R     # New test suite testing front-end multi-league dispatch & validation
│   ├── test-nhl-team-season-lookup.R # Updated to test NHL internals & fallback
│   └── test-pwhl-api-fetch.R      # Extended to test PWHL internals & fallback
```

---

## 6. Testing Strategy & Test Cases

1. **Front-End Dispatch Tests (`test-frontend-season.R`):**
   - Default `league = NULL` returns named list with `$nhl` and `$pwhl`.
   - `league = "both"` returns named list with `$nhl` and `$pwhl`.
   - `league = "nhl"` returns scalar (logical or character).
   - `league = "pwhl"` returns scalar (logical or character).
   - Case insensitivity: `"NHL"`, `"nhl"`, `"PWHL"`, `"pwhl"`.
   - Invalid `league` throws informative `cli::cli_abort`.
   - Invalid `date` and `boolean` throw informative errors.

2. **NHL Internal & Fallback Tests (`test-nhl-team-season-lookup.R`):**
   - Happy path with VCR cassette `utils`.
   - Fallback path: simulate network error / mock API returning NULL.
   - Schedule with regular season games: date in season returns TRUE; date past last game returns FALSE; date before season returns FALSE.
   - `boolean = FALSE` returns season ID string or FALSE.

3. **PWHL Internal & Fallback Tests (`test-pwhl-api-fetch.R`):**
   - Happy path with VCR cassette `pwhl-seasons`:
     - Regular season date (e.g. `2024-01-15` in 2024 season, `2025-01-15` in 2024-25 season).
     - Playoff date (e.g. `2024-05-15` in 2024 playoffs).
     - Off-season date (e.g. `2024-08-01`).
   - Fallback path: mock `getPWHLSeasons()` failure.
   - Schedule fallback using mock `schedule`:
     - Date between min and max regular season returns TRUE.
     - Date past last scheduled regular season game returns FALSE.
     - Date in playoffs returns TRUE for playoffs, FALSE for regular season.

---

## 7. Execution Checklist

- [ ] 1. Check out branch `fix-43-inRegularSeason-PWHL`.
- [ ] 2. Add failing tests in `tests/testthat/test-frontend-season.R` and `tests/testthat/test-pwhl-api-fetch.R`.
- [ ] 3. Implement PWHL internal functions in `R/pwhl-api-fetch.R`.
- [ ] 4. Refactor NHL internal functions with fallback in `R/nhl-team-season-lookup.R`.
- [ ] 5. Implement shared front-end functions in `R/frontend-season.R`.
- [ ] 6. Update internal callers in `R/frontend-daily-summary.R` and `R/frontend-social.R`.
- [ ] 7. Update existing tests in `test-nhl-team-season-lookup.R`.
- [ ] 8. Format code with `air format .`.
- [ ] 9. Regenerate documentation via `devtools::document()`.
- [ ] 10. Run test suite: `devtools::test(reporter = "check")`.
- [ ] 11. Run package check: `devtools::check(error_on = "warning")`.
- [ ] 12. Update `NEWS.md` under dev heading.
