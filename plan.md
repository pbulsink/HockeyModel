# HockeyModel Code Review & Fix Plan

**Date:** September 24, 2026  
**Status:** Phase 1-3 Complete; Phase 4-5 Outstanding

---

## Summary

Multi-subagent code review identified issues across daily posting workflow, Dixon-Coles model correctness, season simulation performance, and API robustness. Phases 1–3 (Tier 1–3) now complete; Phases 4–5 (Tier 4–5) remain outstanding.

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
- **File:** `frontend-social.R` (tweetPlayoffOdds)
- **Problem:** `status` variable assigned but never used
- **Fix:** Removed the unused `status` definition; the inline `text` argument in the `.safe_post()` call is the canonical version.
- **Test:** None needed (cleanup only)
- **Status:** ✅ Fixed

---

### TIER 3: ROBUSTNESS (API & Edge Cases)

#### Issue 3.1: API Response Inconsistencies in `getNHLSchedule()`
- **File:** `nhl-api-fetch.R:23-60`
- **Problem:** No validation of `teamColours`, `site`, or `site$games` structure before access
- **Fix:** Add schema validation; provide fallback/error if missing
- **Status:** ✅ Fixed. Added checks for `teamColours` data frame structure, `site` list structure, and `site$games` presence before access. Returns NULL for malformed responses.

#### Issue 3.2: Date Handling in `games_today()`
- **File:** `nhl-api-fetch.R:98-134`
- **Problem:** Can fail if requested date absent from API response; uses `[[1]]` without bounds check
- **Fix:** Validate response contains the date; handle gracefully if missing
- **Status:** ✅ Fixed. Added validation that API response has `gameWeek` structure. Uses `sapply()` to safely find matching date; returns informative message if date not found.

#### Issue 3.3: `all_games` Parameter Ignored
- **File:** `nhl-api-fetch.R:93-101`
- **Problem:** Documented to filter postponed/in-progress games, but always returns all
- **Fix:** Implement actual filtering logic
- **Status:** ✅ Fixed. When `all_games = FALSE`, function now filters out postponed and rescheduled games.

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
- **Status:** ✅ Fixed. Progress bar now ticks immediately after trying to fetch each game, before validation checks, so failures don't prevent progress.

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
- **Status:** ✅ Fixed. `OTStatus` now starts as `NA_integer_` (numeric) when created, then converted to character during the case_when pipeline. Avoids mixed-type issues during data frame construction.

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
- **Status:** ✅ Fixed. Added validation that NST report has required columns; checks for exactly one home and one away row; errors with descriptive messages if structure is invalid.

#### Issue 3.13: Unsafe Cache Lookup with Shell `grep`
- **File:** `nhl-api-fetch.R:311-319`
- **Problem:** Shell `grep` on file paths unsafe with spaces or special characters
- **Fix:** Use R's `list.files()` or similar instead
- **Status:** ✅ Fixed. Replaced shell `grep` with R-native `read.csv()` + `dplyr::filter()` within a `tryCatch()`. Safer, cross-platform, and more robust to malformed cache files.

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
- **Status:** ✅ Fixed. Added `.NHL_API_FIELDS` constants list at top of `nhl-api-fetch.R` as centralized schema reference. All critical functions (`getNHLSchedule()`, `games_today()`, `getNHLScores()`, `get_xg()`, `getAPISeries()`) now validate expected fields exist before access and error with descriptive messages on schema mismatch. Future field access changes can reference the constant list for consistency.

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
- **Status:** ✅ Fixed. Pre-extracted the per-game columns (`HomeWin`, `HomeOT`, `HomeSO`, `AwaySO`, `AwayOT`, `AwayWin`, `Result`) once outside the loop and iterate by row index (`for (i in seq_len(season_length))`) instead of re-scanning `all_season` by `GameID` on every iteration. RNG draw order and per-game logic are unchanged; output is byte-identical to the old code (verified via golden comparison). Benchmark: G=16000 went from ~10.2s to ~0.77s (near-linear in G). Also fixed a latent `dplyr::select()` `.data$` deprecation (now string names) that surfaced once tests exercise `sim_engine()`. Added two tests: `sim_engine preserves played results and samples unplayed games` and `sim_engine handles a full-season-sized schedule`.

#### Issue 4.2: Large Intermediate `long_season` Data Frame
- **File:** `nhl-league-simulation.R:441-448`
- **Problem:** Creates ~2*G*S row table (every team every game every sim); high memory/copy overhead
- **Complexity:** O(G*S) memory; repeated copies during dplyr operations
- **Fix:**
  - Accumulate team statistics in S×T matrices (points, wins, results)
  - Only materialize final output if caller requests raw results
  - Use matrix rowsums or integer indexing for aggregation
- **Test:** Compare memory usage on large S; profile before/after
- **Status:** ✅ Fixed. `sim_engine()` no longer materializes the 2*G*S row `long_season` data frame. Per-game result vectors (length `nsims` each) are collected in `home_res` / `away_res` lists, then aggregated into per-team totals via a `sum_mask()` helper that sums boolean masks over the games each team plays (home and away). The final `all_results` data frame is the same S*T rows as before, so downstream `dplyr::group_by()`/`rank()` logic is unchanged. All 12 sim_engine tests pass; 8 unrelated pre-existing failures in graphics/API/PWHL tests remain.

#### Issue 4.3: Redundant `extraTimeSolver()` Calls in Sequential Branch
- **File:** `nhl-league-simulation.R:132-133`
- **Problem:** Called twice per simulation in sequential path; parallel path calls once before loop
- **Fix:** Compute once for all simulations in both branches
- **Test:** Verify same OT/SO probabilities used consistently
- **Status**: Fixed in PR #55

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
- **Status:** Fixed in PR #57

#### Issue 4.6: Repeated `rbind()` Accumulation
- **File:** `nhl-dixon-coles-workflow.r:253-254`
- **Problem:** Loop calls `rbind()` repeatedly on growing table (O(N²) copies)
- **Fix:** Accumulate frames in list; call `bind_rows()` once
- **Test:** Benchmark with many games; should see speedup
- **Status:** Fixed in PR #54

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
- **Status:** Fixed in PR #56

#### Issue 4.9: Large Parallel Serialization Overhead
- **File:** `nhl-league-simulation.r:67-71` (simulateSeasonParallel)
- **Problem:** One task per simulation; many small objects serialized to workers
- **Fix:** 
  - Chunk simulations (e.g., 10 sims per task)
  - Return only summary statistics unless raw results requested
  - Use explicit RNG strategy for reproducibility
- **Test:** Benchmark with 1000+ simulations; compare task overhead
- **Status:** Fixed in PR #57

#### Issue 4.10: Stats Accumulation Always Materializes Output
- **File:** `nhl-league-simulation.r:179-204, 357-395`
- **Problem:** Always returns full results table even if caller only needs summaries
- **Fix:** Make raw results optional; accumulate mean/min/max/quantiles online
- **Test:** Compare output sizes; add `return_raw = FALSE` option
- **Status** Won't Fix, intended behavior.

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
- ✅ Improve error reporting (Issue 2.3, DONE)
- ✅ Remove tweetPlayoffOdds() dead code (Issue 2.5, DONE)

### Phase 3: API Robustness (✅ COMPLETE)
- ✅ Validate API response schemas (Issues 3.1, 3.2, 3.3)
- ✅ Implement proper error handling (Issues 3.5, 3.8)
- ✅ Add comprehensive validation (Issues 3.12, 3.13)
- ✅ Fix hardcoded field dependencies (Issue 3.15)
- ✅ All 16 Tier 3 issues resolved

### Phase 4: Performance Optimization (Later)
- ✅ Remove O(G²) lookup (Issue 4.1, DONE)
- ✅ Replace long_season with per-game result lists (Issue 4.2, DONE)
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
