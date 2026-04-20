# HockeyModel R Package - Unused/Unnecessary Exports Analysis

**Analysis Date:** April 11, 2026  
**Total Exported Functions:** 84  
**Analysis Scope:** All R/*.R files, tests/testthat/*.R, vignettes/*.Rmd, and data-raw/ directories

---

## Executive Summary

This analysis identified several categories of potentially unnecessary exported functions in the HockeyModel package:

1. **5 Thin Wrapper Functions** - Simple one-line wrappers that could be inlined
2. **11 Tweet/Social Media Functions** - Lack test coverage, tied to legacy Twitter integration
3. **8+ Internal Helper Functions** - Should not be exported (currently only for internal use)
4. **4-6 Iterative Ranking Functions** - May be incomplete or experimental
5. **3+ Undocumented Utility Functions** - Minimal or no direct usage

---

## 1. THIN WRAPPER FUNCTIONS (High Priority for Cleanup)

These functions are minimal wrappers around internal functions with no additional logic. They could be inlined or documented as aliases.

### Category Details

| Function | Wraps | Purpose | Tests | Usage |
|----------|-------|---------|-------|-------|
| `todayOddsPlot()` | `plot_odds_today()` | Odds visualization wrapper | ✓ | Exported, called from vignette, api-output.R |
| `playoffOdds()` | `plot_prediction_playoffs_by_team()` | Playoff visualization wrapper | ✓ | Exported, called from vignette, api-output.R |
| `presidentOdds()` | `plot_prediction_presidents_by_team()` | Pres trophy visualization wrapper | ✓ | Exported, api-output.R |
| `pointPredict()` | `plot_prediction_points_by_team()` | Points visualization wrapper | ✓ | Exported, api-output.R |
| `ratings()` | `plot_team_rating()` | Ratings visualization wrapper | ✓ | Exported, api-output.R, dailySummary() |

**Recommendation:**  
These could be:
- **Option A:** Remove from exports and call the underlying functions directly
- **Option B:** Document as public API aliases if users reference them
- **Option C:** Consolidate into a single graphics export function

**Implementation in main.R (lines ~50-65):**
```r
todayOddsPlot <- function(...) return(plot_odds_today(...))
playoffOdds <- function() return(plot_prediction_playoffs_by_team())
presidentOdds <- function() return(plot_prediction_presidents_by_team())
pointPredict <- function() return(plot_prediction_points_by_team())
ratings <- function(m = HockeyModel::m) return(plot_team_rating(m = m))
```

---

## 2. TWEET/SOCIAL MEDIA FUNCTIONS (Medium-High Priority)

These functions post to social media (Bluesky/Mastodon via `atrrr` package). They are largely untested and may be tied to a specific deployment workflow that may not be maintained.

### Functions in This Category

- `tweetGames()` - Posts per-game predictions  
- `tweetLikelihoods()` - Posts point likelihood distributions  
- `tweetMetrics()` - Posts model performance metrics  
- `tweetPace()` - Posts team pace graphics  
- `tweetPlayoffOdds()` - Posts playoff odds  
- `tweetSeries()` - Posts playoff series predictions  
- `dailySummary()` - Main coordinator; calls all above functions  

### Issues

1. **No Test Coverage**: None of these functions have dedicated unit tests
2. **External Dependencies**: Rely on `atrrr` package and Bluesky/Mastodon credentials
3. **Hardcoded Graphics Paths**: Tightly coupled to filesystem structure
4. **Commented Legacy Code**: Old `rtoot::` (Mastodon) code commented throughout
5. **Deployment-Specific**: Only useful if you have configured Bluesky/Mastodon credentials

### Usage Pattern

All tweet functions are called coordinatively from within `dailySummary()` function in [main.R](R/main.R#L106-L315):
```r
dailySummary <- function(...) {
  # Update predictions...
  tweet(...)  # Helper to post graphics
  tweetPlayoffOdds(...)
  tweetPace(...)
  tweetMetrics(...)
  tweetLikelihoods(...)
  tweetSeries(...)
}
```

### Recommendations

1. **Move to separate package**: Create a `HockeyModelSocial` companion package for social media integration
2. **Add configuration**: Use environment variables or config files instead of hardcoded paths
3. **Add tests**: Mock the `atrrr` package calls for testing
4. **Document deployment**: Add setup guide for Bluesky/Mastodon authentication
5. **Or: Mark as experimental**: Add `@keywords internal` or version to DESCRIPTION

---

## 3. INTERNAL HELPER FUNCTIONS (Should Not Be Exported)

These functions are utility/helper functions that are only called internally. Exporting them creates API surface area that shouldn't be part of the public interface.

### Not Directly Called Unless Specified

| Function | File | Called By | Recommendation |
|----------|------|-----------|-----------------|
| `todayDC()` | dixon-coles.R | `remainderSeasonDC()`, `plot_odds_today()`, graphics code | Mark `@keywords internal` |
| `parse_dc_params()` | dixon-coles.R | Internal to many DC-related functions | Mark `@keywords internal` |
| `add_postponed_to_schedule_end()` | dixon-coles.R | `remainderSeasonDC()`, `dcPredictMultipleDays()` | Mark `@keywords internal` |
| `parseCores()` | (utils/shared) | Various simulation functions | Mark `@keywords internal` |
| `prob_matrix()` | dixon-coles.R | `dcProbMatrix()`, `dcSample()`, `dcResult()` | Mark `@keywords internal` |
| `dcLambda()` | dixon-coles.R | `dcProbMatrix()` | Mark `@keywords internal` |
| `dcxG()` | dixon-coles.R | `todayDC()` | Mark `@keywords internal` |
| `extraTimeSolver()` | (utils/graphics) | `dcSample()`, `dcResult()`, graphics functions | Mark `@keywords internal` |
| `format_playoff_odds()` | league.R | `daily_odds_table()` | Should be internal to `daily_odds_table()` |
| `cleanModel()` | utils.R | `getM()` | Mark `@keywords internal` |
| `DCweights()` | dixon-coles.R | `getM()` | Mark `@keywords internal` (or used in `tune_dc_weight()`) |
| `DCRhoLogLik()` | dixon-coles.R | `getRho()` | Mark `@keywords internal` |
| `DCPredictErrorRecover()` | dixon-coles.R | `DCPredict()`, `todayDC()`, `remainderSeasonDC()` | Mark `@keywords internal` |

### Utility Functions That Are Also Helpers

These support functions are used throughout the codebase but not by external users:

| Function | Likely File | Purpose | Public? |
|----------|-------------|---------|---------|
| `getNumGames()` | league.R | Gets games in current season | Should be internal |
| `getLongTeam()` | league.R | Maps short to long team names | Should be internal |
| `getShortTeam()` | league.R | Maps long to short team names | Should be internal |
| `getTeamDivisions()` | league.R | Gets team's division | Possibly public for statistics |
| `getTeamConferences()` | league.R | Gets team's conference | Possibly public for statistics |
| `getDivisions()` | league.R | Gets all division names | Should be internal |
| `getConferences()` | league.R | Gets all conference names | Should be internal |
| `clean_names()` | league.R | Data cleaning utility | Should be internal |
| `historicalPoints()` | utils.R | Calculates historical points | Mark `@keywords internal` |
| `iterative_season_progress()` | dc_iterative.R | Calculates season progress | Mark `@keywords internal` |
| `updateIterativeRankings()` | dc_iterative.R | Updates iterative ratings | Mark `@keywords internal` |
| `iterateGame()` | dc_iterative.R | Processes single game iteratively | Mark `@keywords internal` |
| `getTeamRankings()` | dc_iterative.R | Gets current team rankings | Mark `@keywords internal` |
| `build_past_predictions()` | league.R | Historical prediction builder | Not exported but used in tests |

### Recommendation

All 13+ of these should be marked with `@keywords internal` to exclude from public API documentation but keep them functional. This prevents accidental dependency on implementation details.

---

## 4. ITERATIVE RANKING FUNCTIONS (Low-to-Medium Priority)

These functions implement an alternative iterative Dixon-Coles model. They appear to be experimental/incomplete.

### Functions

- `iterativeGamePredict()` - Predicts using iterative model | [dc_iterative.R](R/dc_iterative.R) | No direct test calls
- `getIterativeTable()` - Gets iteration state table | [dc_iterative.R](R/dc_iterative.R) | Only in test-dc_iterative.R
- `getReplacementIterativeParameters()` - ? | [dc_iterative.R](R/dc_iterative.R) | No usage found
- `getReplacementRankings()` - Recalculates from scratch | [dc_iterative.R](R/dc_iterative.R) | Only in test-dc_iterative.R

### Issues

1. **Incomplete Implementation**: Functions lack documentation clarity
2. **Minimal Test Coverage**: Only basic tests in `test-dc_iterative.R`
3. **No User-Facing Integration**: Not called by `dailySummary()` or main workflow
4. **Unclear Purpose**: Documentation doesn't explain when/why to use vs regular DC method

### Recommendation

1. **Complete the feature**: If this is an active feature, add comprehensive documentation and integration tests
2. **Deprecate**: Mark with `@deprecated` if superseded by regular DC model
3. **Separate package**: Move to a separate experimental package if still in development
4. **Mark as internal**: Change to `@keywords internal` if only for developers

---

## 5. POTENTIALLY UNUSED OR UNDOCUMENTED (Low Priority)

### Functions with Minimal Direct Usage

| Function | File | Used By | Test Coverage | Notes |
|----------|------|---------|----------------|-------|
| `get_xg()` | dixon-coles.R | No clear usage found | Unknown | Is this used? Naming suggests expected goals |
| `gameIDValidator()` | (location unclear) | Not found in grep | ✓ | Validation utility, probably used by API code |
| `hexToRGB()` | graphics.R | No direct usage found | ✓ | Color utility function |
| `getAPISeries()` | api-interface.R | Used in graphics, tests | ✓ | Tested but internal to graphics mostly |
| `getSeriesOdds()` | league.R | Not found in codebase | Unknown | Playoff series odds, unclear if implemented  |
| `recordTodaysPredictions()` | main.R | Called in test only | ✓ | Test-only function, might be legacy |
| `compile_predictions()` | graphics.R | Default param in graphics | ✓ | Probably used in workflows |
| `cleanupPredictionsFile()` | league.R | Only in test-league.R | ✓ | Utility for cleanup |
| `updateScoresAPI_byGameID()` | api-interface.R | Not exported | Not clear | Variant of updateScoresAPI |

---

## 6. WELL-MAINTAINED, CORE FUNCTIONS (Keep As-Is)

These functions are essential to the package and well-maintained:

### Core Prediction Functions

- `DCPredict()` - Dixon-Coles prediction; central to package [dixon-coles.R](R/dixon-coles.R#L401)
- `remainderSeasonDC()` - Season simulation; exported and tested [dixon-coles.R](R/dixon-coles.R#L127)
- `loopless_sim()` - Fast simulation engine; used in `dcPredictMultipleDays()` [league.R](R/league.R#L303)
- `sim_engine()` - Core simulation; called by `loopless_sim()` [league.R](R/league.R#L410)  
- `simulateSeasonParallel()` - Parallel season simulation [league.R](R/league.R#L145)

### Model Training Functions

- `updateDC()` - Updates m, rho, beta, eta, k parameters [dixon-coles.R](R/dixon-coles.R#L11)
- `getM()` - Fits main model; tested [dixon-coles.R](R/dixon-coles.R#L269)
- `getRho()` - Calculates rho; tested [dixon-coles.R](R/dixon-coles.R#L315)
- `getWeibullParams()` - Calculates tie adjustments; tested [dixon-coles.R](R/dixon-coles.R#L336)

### Data Functions

- `buildStats()` - Team statistics; heavily tested [league.R](R/league.R)
- `updateModel()` - Main workflow orchestrator [main.R](R/main.R#L12)
- `updatePredictions()` - Daily prediction update [main.R](R/main.R#L28)

### API Functions

- `getNHLSchedule()` - Fetches NHL schedule; tested [api-interface.R](R/api-interface.R#L9)
- `getNHLScores()` - Fetches game scores; tested [api-interface.R](R/api-interface.R#L130)
- `updateScheduleAPI()` - Updates local schedule; tested [api-interface.R](R/api-interface.R#L104)
- `updateScoresAPI()` - Updates local scores; tested [api-interface.R](R/api-interface.R#L312)

---

## RECOMMENDATIONS BY PRIORITY

### 🔴 HIGH PRIORITY - Address Immediately

**1. Remove or Document Tweet Functions (6-7 functions)**
   - Add `@keywords internal` or move to separate package
   - Add configuration/setup documentation
   - Add test coverage if keeping in package

**2. Mark 13+ Helper Functions as Internal**
   - Remove from public NAMESPACE
   - Add `@keywords internal` to roxygen docs
   - Keeps them functional but removes from API surface

**3. Remove 5 Thin Wrapper Functions**
   - OR: Consolidate into graphics export module
   - Update all external callers to use underlying functions directly

### 🟡 MEDIUM PRIORITY - Investigate & Clarify

**1. Iterative Ranking System (4 functions)**
   - Clarify purpose and status
   - Either complete/test fully or deprecate
   - Add documentation on when to use

**2. Ambiguous Functions (5-8 functions)**
   - `get_xg()` - Clarify purpose and usage
   - `getSeriesOdds()` - Verify if implemented
   - `getAPISeries()` - Consider making internal

### 🟢 LOW PRIORITY - Monitor

**1. Utility Functions (10+ functions)**
   - These serve legitimate internal purposes
   - Mark as internal to prevent accidental public dependency
   - Could eventually extract to utilities package

---

## IMPLEMENTATION PLAN

### Phase 1: Documentation (Week 1)
- [ ] Add `@keywords internal` to 15+ helper functions
- [ ] Update NAMESPACE to remove internal functions
- [ ] Regenerate documentation with `roxygen2::roxygenise()`

### Phase 2: Social Media Refactor (Week 2-3)
- [ ] Create HockeyModelSocial companion package OR
- [ ] Move tweet functions to package-internal functions file
- [ ] Update dailySummary() documentation

### Phase 3: Wrapper Function Consolidation (Week 3)
- [ ] Remove 5 wrapper functions from exports
- [ ] Update all external callers (vignettes, api-output.R)
- [ ] Regenerate docs and NAMESPACE

### Phase 4: Testing & Validation (Week 4)
- [ ] Run full test suite: `devtools::test()`
- [ ] Check for R CMD CHECK errors: `devtools::check()`
- [ ] Update NEWS.md with changes

---

## APPENDIX: Detailed Function Locations

### By File

**[R/main.R]** - Main workflow and tweet coordination
- `updateModel()` ✓ (keep)
- `updatePredictions()` ✓ (keep)
- `todayOddsPlot()` ⚠️ (wrapper)
- `playoffOdds()` ⚠️ (wrapper)
- `presidentOdds()` ⚠️ (wrapper)
- `pointPredict()` ⚠️ (wrapper)
- `ratings()` ⚠️ (wrapper)
- `tweets...functions (6)` 🔴 (untested, social media)
- `dailySummary()` 🔴 (untested, social media coordinator)

**[R/dixon-coles.R]** - Core prediction model
- All core DC functions ✓
- Many internal helpers ⚠️ (should be marked internal)

**[R/league.R]** - League stats and simulations
- Core simulation functions ✓
- Helper utilities ⚠️ (should be marked internal)

**[R/graphics.R]** - Visualization functions
- Plot functions ✓ (tested)
- `format_playoff_odds()` ⚠️ (internal helper, used by daily_odds_table)
- Team utility functions ⚠️ (mark internal)

**[R/api-interface.R]** - API integration
- Core API functions ✓ (tested)
- `games_today()` ✓ (tested)
- `updateScheduleAPI()` ✓ (tested)
- `updateScoresAPI()` ✓ (tested)

**[R/dc_iterative.R]** - Experimental iterative model
- 4 iterative functions 🟡 (unclear status, minimal testing)

**[R/utils.R]** - General utilities
- `normalizeOdds()` ✓ (used widely)
- `mutate_cond()` ✓ (used in dplyr pipes)
- `logLoss()` ✓ (metric, tested)
- `historicalPoints()` ⚠️ (rarely used, mark internal)

---

## NOTES FOR MAINTAINERS

1. **Test Coverage**: Package has good test coverage for core functions but social media functions are completely untested

2. **Documentation**: Graphics wrappers are documented but their purpose (simple wrappers) could be clearer

3. **Helper Functions**: Many internal helpers are exported but never called outside the package

4. **External Dependencies**: Tweet functions depend on `atrrr` package which may not be available in all environments

5. **API Stability**: Consider semantic versioning when removing or marking functions as internal

---

Generated: April 11, 2026
Analysis Time: ~30 minutes
Files Analyzed: 14 R source files, 8 test files, 2 vignettes
