# HockeyModel Code Review & Fix Plan

**Date:** September 24, 2026  
**Status:** In Progress

---

## Summary

Multi-subagent code review identified issues across daily posting workflow, Dixon-Coles model correctness, season simulation performance, and API robustness. Four critical issues fixed; remaining work categorized by priority and domain.

---

## ✅ COMPLETED FIXES

### 1. ✅ getRho() Optimization Direction (CRITICAL)
- **Issue:** Minimizing log-likelihood instead of maximizing it
- **Root Cause:** Passed raw log-likelihood to `optim()` which minimizes; should negate
- **Fix Applied:**
  - Changed objective to return `-DCRhoLogLik(...)` 
  - Updated method to Brent with bounds `[-0.5, 0.5]` (goalmodel convention)
  - Added convergence check with warning
  - Updated test bounds to allow full `[-0.5, 0.5]` range
- **Tests Added:** 
  - `getRho produces valid rho in [-0.5, 0.5]`
  - `getRho maximizes likelihood (not minimizes)`
- **Impact:** Critical—systematically improves all DC model predictions

### 2. ✅ Away OT Probability Bug (CRITICAL)
- **Issue:** `dcResult()` and `dcExpandedOdds()` use `otwinnerprob[1]` (home) for away OT win
- **Root Cause:** Copy-paste typo; line 355/438 should use `otwinnerprob[2]`
- **Fix Applied:** Changed away OT probability calculation to use correct `otwinnerprob[2]`
- **Tests Added:**
  - `dcResult uses correct away OT probability [2]`
  - `dcExpandedOdds uses correct away OT probability [2]`
  - `dcResult and dcExpandedOdds use otwinnerprob[2] consistently`
- **Impact:** Corrects asymmetric OT/SO probability allocations

### 3. ✅ Probability Matrix Validation Gap (Issue 1.2, CRITICAL)
- **Issue:** No checks that `prob_matrix()`/`dcSample()`/`dcResult()`/`dcExpandedOdds()`
  probability vectors were valid; a large Weibull tie-enhancement (`k`) could
  push the diagonal sum above 1, which flipped off-diagonal win/loss cells
  negative during renormalization (observed magnitudes up to -0.34 for extreme
  inputs). The tau adjustment could also push a low-goal cell slightly
  negative near the edges of `rho`'s range.
- **Fix Applied:**
  - `prob_matrix()` now clamps small negative tau-adjustment artifacts in the
    2×2 low-goal block before renormalizing, and caps the diagonal (with a
    warning) if the tie-enhanced diagonal sum reaches or exceeds 1, leaving a
    small positive remainder for win/loss outcomes instead of dividing by a
    negative renormalization factor.
  - Added `validateProbMatrix()` (in `validation-helpers.R`): checks all
    values are finite and within tolerance of `[0, 1]`, clamps small
    out-of-range noise, renormalizes to sum to exactly 1, and errors if
    violations exceed tolerance (indicating a real bug rather than floating
    point/boundary noise).
  - `prob_matrix()`, `dcSample()`, `dcResult()`, and `dcExpandedOdds()` all
    validate their probability output/inputs through this helper;
    `dcSample()`'s old ad hoc `pm2[pm2 < 0] <- 1e-8` patch was removed in favor
    of the shared, stricter validation.
- **Tests Added:**
  - `prob_matrix never returns negative probabilities for extreme inputs`
  - `prob_matrix warns when the tie-enhanced diagonal exceeds 1`
  - `dcResult and dcExpandedOdds remain valid for extreme inputs`
- **Impact:** Critical—eliminates a class of invalid/negative probabilities
  that could silently corrupt sampling and downstream simulation for extreme
  (but reachable) lambda/mu/rho/k combinations.

### 4. ✅ League Mismatch in NHL Daily Workflow (Issue 2.1)
- **File:** `frontend-daily-summary.R` (`.daily_summary_nhl()`)
- **Issue:** `updateModel()` and `updatePredictions()` were called without
  `league = "NHL"`, so both NHL and PWHL parameters were refit/returned even
  though the rest of the function assumes NHL-only structure.
- **Fix Applied:** Both calls now pass `league = "NHL"` explicitly.
- **Impact:** NHL daily workflow no longer refits/mixes PWHL data unnecessarily.

### 5. ✅ Broken `tweet()` Call in `dailySummary()` (Issue 2.2)
- **Files:** `frontend-daily-summary.R`, `frontend-social.R`
- **Issue:** `tweet(graphic_dir, delay = delay, graphic_dir = graphic_dir)`
  passed `graphic_dir` positionally into `tweet()`'s unused `games` parameter,
  then again by name; `schedule` was also unused in the function body.
- **Fix Applied:** Removed the unused `games` and `schedule` parameters from
  `tweet()`; the call site now uses named arguments only:
  `tweet(graphic_dir = graphic_dir, delay = delay)`.
- **Impact:** Call is now unambiguous; no dead parameters remain on `tweet()`.

### 6. ✅ `tweetPace()` Ignores `delay` Parameter (Issue 2.4)
- **File:** `frontend-social.R`
- **Issue:** Per-team post loop logged the supplied `delay` but slept on a
  freshly-drawn `stats::runif(1, min = 1, max = 3) * 60` instead.
- **Fix Applied:** The loop now sleeps on the supplied `delay`, matching the
  logged message.
- **Impact:** `delay` is now respected consistently for team posts.

---

## 🚨 REMAINING ISSUES BY PRIORITY

### TIER 1: CRITICAL (Correctness)

#### Issue 1.3: GLM Fitting Starting-Value Robustness
- **File:** `nhl-dixon-coles-fit.R:44-68` (getM function)
- **Note:** The `+0` in `Team + Opponent + Home + 0` is deliberate: it drops the
  intercept so every team gets its own attack/defence coefficient, rather than
  fixing an arbitrary reference team (e.g. Anaheim Ducks) to 0 as `glm()` would
  otherwise do. This is documented in an inline comment at line 60 and should
  not be changed.
- **Investigation:** `Team` and `Opponent` are always built from the same
  home/away union of teams (`df.indep`'s `Team` column is `c(HomeTeam,
  AwayTeam)` and `Opponent` is `c(AwayTeam, HomeTeam)`), so their factor levels
  are always identical. This guarantees the design matrix for
  `Team + Opponent + Home + 0` is always full rank with exactly `2 * n_teams`
  columns — matching `rep(0.01, length(unique(df.indep$Team)) * 2)` — even
  when a team appears in only home or only away rows. Confirmed via
  simulation (both edge cases) that `glm()` never produces `NA` (aliased)
  coefficients.
- **Status:** ✅ No bug found; the starting-vector length always matches model
  rank by construction. Added a regression test locking in this invariant.
- **Test Added:** `getM starting values match model rank`

---

### TIER 2: IMPORTANT (Daily Workflow)

#### Issue 2.3: Silent Social Posting Failures
- **Files:** `frontend-daily-summary.R:67-112`, `frontend-social.R:19-70`
- **Problem:** `try()` blocks suppress errors without structured reporting
- **Effect:** Failed posts appear successful; errors lost
- **Fix:** 
  - Capture `try()` result and log/report explicitly
  - Consider `tryCatch()` with condition handling
  - Provide user-facing error summary
- **Test:** Verify failed posts are reported, not silently swallowed
- **Status:** ✅ Fixed. Added `.safe_post()` (wraps a single `atrrr::post()` call
  in `tryCatch()`, emits `cli::cli_warn()` immediately on failure, and returns
  a structured `list(description, success, error)`) and
  `.summarize_post_results()` (aggregates a batch into a `data.frame` and
  warns with a failure count/description list) in `frontend-social.R`. Every
  bare `try(atrrr::post(...))` call site in `frontend-social.R` and
  `frontend-daily-summary.R` now goes through `.safe_post()`; the exported
  `tweet*()` functions return their post summary (invisibly) instead of
  `NULL`, and `.daily_summary_nhl()` combines every sub-call's summary into
  one `data.frame` and emits a final warning if any posts failed.

#### Issue 2.5: `tweetPlayoffOdds()` Dead Code
- **File:** `frontend-social.R:518-521`
- **Problem:** `status` variable assigned but never used
- **Fix:** Remove dead code or document why it exists
- **Test:** None needed (cleanup only)

---

### TIER 3: ROBUSTNESS (API & Edge Cases)

#### Issue 3.1: API Response Inconsistencies in `getNHLSchedule()`
- **File:** `nhl-api-fetch.R:23-60`
- **Problem:** No validation of `teamColours`, `site`, or `site$games` structure before access
- **Fix:** Add schema validation; provide fallback/error if missing
- **Test:** Test with malformed API responses

#### Issue 3.2: Date Handling in `games_today()`
- **File:** `nhl-api-fetch.R:98-134`
- **Problem:** Can fail if requested date absent from API response; uses `[[1]]` without bounds check
- **Fix:** Validate response contains the date; handle gracefully if missing
- **Test:** Test with missing/future dates

#### Issue 3.3: `all_games` Parameter Ignored
- **File:** `nhl-api-fetch.R:93-101`
- **Problem:** Documented to filter postponed/in-progress games, but always returns all
- **Fix:** Implement actual filtering logic
- **Test:** Verify `all_games = FALSE` filters correctly

#### Issue 3.4: Broken `gameIDs = NULL` Handling
- **File:** `nhl-api-fetch.R:177-180`
- **Problem:** When `gameIDs = NULL`, code subsets `gameIDs` itself (creating empty vector) instead of deriving from schedule
- **Fix:** Extract game IDs from schedule when `gameIDs = NULL`
- **Test:** Test with `gameIDs = NULL`
- **Status:** ✅ Fixed

#### Issue 3.5: Progress Bar Not Advanced on Failures
- **File:** `nhl-api-fetch.R:204-249`
- **Problem:** `pb$tick()` only called after final games; progress incomplete on errors
- **Fix:** Tick progress bar for each game, including failures
- **Test:** Verify progress completes even with API failures

#### Issue 3.6: Unsafe Global Connection Cleanup
- **File:** `nhl-api-fetch.R:354`
- **Problem:** `closeAllConnections()` closes unrelated code's connections
- **Fix:** Close only connections opened by this function
- **Test:** Verify no side effects on other R code
- **Status:** ✅ Fixed (removed the call entirely; observed live that it closed the interactive console's own stdout connection during a test run)

#### Issue 3.7: Unreachable Shootout Detection
- **File:** `nhl-api-fetch.R:264-269`
- **Problem:** `OTStatus <= 3 ~ ""` and `OTStatus > 3 ~ "OT"` occur before `OTStatus == 5 & GameType == "R" ~ "SO"`
- **Effect:** Shootout branch unreachable
- **Fix:** Reorder case_when conditions; put specific cases first
- **Test:** Test SO detection with real SO data
- **Status:** ✅ Fixed

#### Issue 3.8: Mixed Data Types in `OTStatus`
- **File:** `nhl-api-fetch.R:228-231, 264-280`
- **Problem:** `OTStatus` mixes `""` (character) with numeric values; causes warnings/NA
- **Fix:** Use consistent type (numeric or character) throughout
- **Test:** Verify no type coercion warnings

#### Issue 3.9: Missing `case_when()` Fallback
- **File:** `nhl-api-fetch.R:264-280`
- **Problem:** No fallback branch; unrecognized status/score combinations produce NA
- **Fix:** Add `.default = NA` or explicit error for unexpected values
- **Test:** Test with unexpected OTStatus values
- **Status:** ✅ Fixed (added explicit `.default = NA` branches plus `cli_abort()` checks that error on any resulting NA in `OTStatus` or `Result`)

#### Issue 3.10: Tied Score Handling Without Validation
- **File:** `nhl-api-fetch.R:272-280`
- **Problem:** Tied final scores (Result = 0.5) included without checking if game is SO or incomplete
- **Fix:** Validate game is actually tied/shootout before assigning 0.5
- **Test:** Test tied score detection
- **Status:** ✅ Fixed (modern NHL games are never a final tie; `getNHLScores()` now errors on any tied final score instead of assigning `Result = 0.5`)

#### Issue 3.11: xG Request After Potential Score Fetch Failure
- **File:** `nhl-api-fetch.R:253-285`
- **Problem:** xG request proceeds even if `getNHLScores()` failed; joining NULL with xG may fail
- **Fix:** Validate scores exist before fetching xG; handle gracefully
- **Test:** Test with missing scores
- **Status:** ✅ Fixed (xG lookup is now driven off `scores$GameID`, after validation, and is skipped entirely when no scores were retrieved)

#### Issue 3.12: Fragile xG Column Detection
- **File:** `nhl-api-fetch.R:374-395`
- **Problem:** Assumes exactly one "home" and "away" row; missing/duplicate rows produce silent errors
- **Fix:** Validate row count and columns before processing
- **Test:** Test with malformed NST responses

#### Issue 3.13: Unsafe Cache Lookup with Shell `grep`
- **File:** `nhl-api-fetch.R:311-319`
- **Problem:** Shell `grep` on file paths unsafe with spaces or special characters
- **Fix:** Use R's `list.files()` or similar instead
- **Test:** Test with spaces/special chars in paths

#### Issue 3.14: Hardcoded Playoff Win Count
- **File:** `nhl-api-fetch.R:715-717`
- **Problem:** Playoff series hardcoded to 4 wins; doesn't generalize to format changes
- **Fix:** Make configurable or derive from series data
- **Test:** Document assumption; plan for future format changes
- **Status:** ✅ Fixed (`getAPISeries()` gained a `wins_required = 4` parameter; the API doesn't report series format so this is a configurable default rather than a derived value)

#### Issue 3.15: Hardcoded Field Names Throughout
- **Files:** `nhl-api-fetch.R` (many locations)
- **Problem:** Schedule, score, playoff parsing all depend on fixed API field names
- **Effect:** Any schema change breaks fetches silently
- **Fix:** 
  - Centralize field name mappings
  - Add validation that expected fields exist
  - Fail fast if schema changes
- **Test:** Test with missing/renamed fields

#### Issue 3.16: Hardcoded Playoff Series Letter Mapping
- **File:** `nhl-api-fetch.R:704-713, 733-742`
- **Problem:** Assumes series are `A–Z`; breaks for localized or renamed identifiers
- **Fix:** Handle arbitrary series identifiers; don't assume letter mapping
- **Test:** Test with non-letter series IDs
- **Status:** ✅ Fixed (`Series` is now returned as an opaque character column instead of being mapped through `which(LETTERS == x)`)

---

### TIER 4: PERFORMANCE (Simulation)

#### Issue 4.1: O(G²) GameID Lookup in `sim_engine()`
- **File:** `nhl-league-simulation.R:417-421`
- **Problem:** Loop filters `all_season[all_season$GameID == g, ]` for every game (O(G²))
- **Complexity:** O(G²) instead of O(G)
- **Fix:** 
  - Iterate by row index instead: `for (i in seq_len(nrow(all_season))) { row <- all_season[i, ] }`
  - Or precompute vectors outside loop
- **Test:** Benchmark large simulations; should see speedup

#### Issue 4.2: Large Intermediate `long_season` Data Frame
- **File:** `nhl-league-simulation.R:441-448`
- **Problem:** Creates ~2*G*S row table (every team every game every sim); high memory/copy overhead
- **Complexity:** O(G*S) memory; repeated copies during dplyr operations
- **Fix:**
  - Accumulate team statistics in S×T matrices (points, wins, results)
  - Only materialize final output if caller requests raw results
  - Use matrix rowsums or integer indexing for aggregation
- **Test:** Compare memory usage on large S; profile before/after

#### Issue 4.3: Redundant `extraTimeSolver()` Calls in Sequential Branch
- **File:** `nhl-league-simulation.R:132-133`
- **Problem:** Called twice per simulation in sequential path; parallel path calls once before loop
- **Fix:** Compute once for all simulations in both branches
- **Test:** Verify same OT/SO probabilities used consistently

#### Issue 4.4: Inefficient Chunk Division in `loopless_sim()`
- **File:** `nhl-league-simulation.R:258-259, 335-350`
- **Problem:** 
  - `nsims <- floor(nsims / cores)` loses remainder
  - `seq_along(1:(cores * 100))` launches `cores * 100` tasks regardless of actual work
  - Final chunk count may not sum to requested S
- **Fix:** Distribute exactly S simulations into ~equal chunks; verify sum = S
- **Test:** Verify `sum(chunk_sizes) == requested_nsims` for various values

#### Issue 4.5: Per-Simulation Data-Frame Copying
- **Files:** `nhl-league-simulation.R:74, 130` (and `sim_engine()`)
- **Problem:** Copy `odds_table` for every iteration; most columns invariant
- **Fix:** Store invariant columns once; generate only result vectors per sim
- **Test:** Profile memory allocation per simulation

#### Issue 4.6: Repeated `rbind()` Accumulation
- **File:** `nhl-dixon-coles-workflow.r:253-254`
- **Problem:** Loop calls `rbind()` repeatedly on growing table (O(N²) copies)
- **Fix:** Accumulate frames in list; call `bind_rows()` once
- **Test:** Benchmark with many games; should see speedup

#### Issue 4.7: Repeated Parameter Parsing
- **File:** `nhl-dixon-coles-predict.R:28, 152, 191, 332, 415` (and elsewhere)
- **Problem:** `parse_dc_params()` called repeatedly in inner loops
- **Fix:** Parse once at public function boundary; pass parsed params to internals
- **Test:** Verify no re-parsing in hot loops

#### Issue 4.8: Expensive Odds Generation Loop
- **File:** `nhl-dixon-coles-workflow.r:237-255`
- **Problem:** Loops by date; each calls `todayDC()` which loops all games and calls `DCPredict()`
- **Complexity:** O(F * M²) where F = future games, M = max goal count
- **Fix:**
  - Vectorize model prediction over all future games
  - Batch `prob_matrix()` calls
  - Collect daily frames in list before `bind_rows()`
- **Test:** Benchmark odds generation with many future games

#### Issue 4.9: Large Parallel Serialization Overhead
- **File:** `nhl-league-simulation.r:67-71` (simulateSeasonParallel)
- **Problem:** One task per simulation; many small objects serialized to workers
- **Fix:** 
  - Chunk simulations (e.g., 10 sims per task)
  - Return only summary statistics unless raw results requested
  - Use explicit RNG strategy for reproducibility
- **Test:** Benchmark with 1000+ simulations; compare task overhead

#### Issue 4.10: Stats Accumulation Always Materializes Output
- **File:** `nhl-league-simulation.r:179-204, 357-395`
- **Problem:** Always returns full results table even if caller only needs summaries
- **Fix:** Make raw results optional; accumulate mean/min/max/quantiles online
- **Test:** Compare output sizes; add `return_raw = FALSE` option

---

### TIER 5: DOCUMENTATION & TESTING

#### Issue 5.1: Weibull Tie Adjustment Not in Standard DC Model
- **File:** `nhl-dixon-coles-fit.r:135-189`
- **Problem:** Custom Weibull diagonal enhancement; not in original Dixon-Coles
- **Fix:**
  - Document as custom extension with rationale
  - Consider joint likelihood estimation instead of heuristic fit
  - Validate improvement over standard model
- **Test:** Compare predictions with/without Weibull; log-loss on held-out data

#### Issue 5.2: Logistic Weighting Not Standard Dixon-Coles
- **File:** `nhl-dixon-coles-fit.r:257-288`
- **Problem:** Uses logistic time decay, not exponential (standard Dixon-Coles)
- **Fix:**
  - Document as deliberate extension
  - Provide rationale or benchmark against exponential
  - Make configurable
- **Test:** Compare exp vs logistic weighting; choose based on validation

#### Issue 5.3: Missing Documentation on DC/OT/SO Extensions
- **File:** `nhl-dixon-coles-predict.r` (general)
- **Problem:** Model estimates regulation scores; converts draws to OT/SO outcomes using fixed probabilities (0.6858606, 0.3141394)
- **Fix:**
  - Document these are empirical NHL observations
  - Add validation these are still accurate by season/era
  - Plan recalibration
- **Test:** Periodically validate against actual historical OT/SO records

#### Issue 5.4: Reference Document Review Incomplete
- **Files:** `reference/dc.pdf`, `reference/rs.pdf`
- **Problem:** Could not extract text; comparison against references incomplete
- **Fix:** Review PDFs against current implementation; document any deviations
- **Test:** Cross-reference all DC model assumptions

#### Issue 5.5: Constants Documentation Gap
- **File:** `constants.R:52-58`
- **Problem:** `DC_NU_PWHL` documented as 2, actual value is 5
- **Fix:** Correct documentation or explain discrepancy
- **Test:** Audit all constants for accuracy

---

## Implementation Roadmap

### Phase 1: Critical Correctness (Now)
- ✅ Fix rho optimization (DONE)
- ✅ Fix away OT probability (DONE)
- ✅ Add probability validation (Issue 1.2, DONE)
- ✅ Verify GLM starting-value/rank robustness (Issue 1.3, DONE — no bug found; regression test added)

### Phase 2: Daily Workflow (Next)
- ✅ Fix league parameter passing (Issue 2.1, DONE)
- ✅ Fix tweet() call (Issue 2.2, DONE)
- ✅ Fix tweetPace() delay parameter (Issue 2.4, DONE)
- [ ] Improve error reporting (Issue 2.3)
- [ ] Remove tweetPlayoffOdds() dead code (Issue 2.5)

### Phase 3: API Robustness (Following)
- [ ] Validate API response schemas (Issues 3.1, 3.2, 3.3, etc.)
- [ ] Implement proper error handling
- [ ] Add comprehensive tests for edge cases
- [ ] Fix hardcoded field dependencies (Issue 3.15)

### Phase 4: Performance Optimization (Later)
- [ ] Remove O(G²) lookup (Issue 4.1)
- [ ] Replace long_season with matrices (Issue 4.2)
- [ ] Fix chunk division (Issue 4.4)
- [ ] Vectorize odds generation (Issue 4.8)
- [ ] Profile and optimize hot loops

### Phase 5: Documentation & Validation (Ongoing)
- [ ] Document all custom model extensions
- [ ] Validate custom weighting schemes
- [ ] Audit constants against documentation
- [ ] Cross-reference with papers/goalmodel

---

## Testing Strategy

### Regression Tests (Already Added)
- ✅ getRho bounds and maximization
- ✅ Away OT probability consistency
- ✅ DCPredict draw allocation
- ✅ Probability matrix validation under extreme inputs (Issue 1.2)

### Recommended New Tests
- GLM identification and stability
- API response schema validation
- Edge cases: missing dates, malformed responses, empty schedules
- Chunk distribution summing to requested count
- OT/SO probability calibration against historical data

---

## Notes

- All fixes assume the package's public API remains stable
- Performance optimizations should be benchmarked before/after
- API changes should trigger immediate validation updates
- DC model extensions should be formally documented with rationale
