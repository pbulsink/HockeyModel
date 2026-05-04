# Dead Code & Unused Function Audit

This document lists functions that are never called (or only called from tests),
and dead/unreachable code blocks across the R package.
Use it as a review checklist — tick off each item once you have verified and removed it.

---

## `R/utils.R`

### Unused functions

- [ ] **`gId()` / `is_valid_gameId()`** (lines 577–588) — `@keywords internal` helpers
  superseded by the exported `gameIDValidator()`. Neither is called anywhere in the
  package. `is_valid_gameId` is just `Vectorize(gId)`, and `gameIDValidator` already
  handles vectorised input.

- [ ] **`historicalPoints()`** (line 44) — called only from
  `tests/testthat/test-utils.R`. No production code uses it.

- [ ] **`formatPredsForHockeyVisContest()`** (line 245) — called only from
  `tests/testthat/test-utils.R`. No production code uses it.

- [ ] **`mutate_cond()`** (line 74) — exported (`@export`) but never called from
  any production code in `R/`. Only exercised in `tests/testthat/test-utils.R`.
  Either add real usage, remove the export, or drop it entirely.

---

## `R/dixon-coles.R`

### Unused functions

- [ ] **`DCweights_old()`** (line 1038) — old `DCweights` implementation, fully
  replaced by the current `DCweights()`. Never called anywhere. Safe to delete.

---

## `R/dc_iterative.R`

### Unused functions

- [ ] **`saveIterativePredictions()`** (line 631) — `@keywords internal`, never
  called anywhere. Saves iterative predictions to disk, but nothing in the package
  invokes it.

- [ ] **`readIterativePredictions()`** (line 655) — `@keywords internal`, never
  called anywhere. Paired with `saveIterativePredictions()`; both are dead.

- [ ] **`iterativeDailyUpdate()`** (line 842) — `@keywords internal`, never
  called anywhere. Intended to orchestrate the iterative model update pipeline, but
  `getIterativeTable()` is the actual entry point used everywhere.

- [ ] **`iterativeOddsTable()`** (line 861) — called only from
  `tests/testthat/test-dc_iterative.R`. No production code uses it.

### Dead code in active functions

- [ ] **Wasted computation in `optimizeIterative_WL_Internal()`** (line 502) —
  `acc` and `rocauc` are computed but the function always returns only `ll`:
  `return(list(ll, acc, rocauc)[1])`. The extra metric calculations are dead work.

- [ ] **Wasted computation in `optimizeIterative_XG_Internal()`** (line 559) —
  `r2` and `mse` are computed but the function always returns only `rmse`:
  `return(list(rmse, r2, mse)[1])`. Same pattern.

---

## `R/api-interface.R`

### Unused functions

- [ ] **`updateScoresAPI_byGameID()`** (line 437) — `@keywords internal`, never
  called. `updateScoresAPI()` handles all score updates without this helper.

- [ ] **`validateWins()`** (line 759) — defined but never called. Parses playoff
  series status strings, but `getAPISeries()` (the only natural caller) never
  invokes it.

### Developer-local hardcoded path (not strictly dead, but broken on any other machine)

- [ ] **`load_or_get_nst()`** (line 292) — uses a hardcoded `~/Documents/natstattrick.csv`
  path and `system2("grep", ...)`. Called by `get_xg()` (line 346). Will silently
  fail on any machine other than the original developer's. Needs either a proper data
  injection approach or removal if xG via NST is no longer supported.

---

## `R/api-output.R`

### Stub plumber routes that always return `NULL`

- [ ] **`/cup-odds`** (lines 47–52) — route body is `NULL`. Either implement or remove.

- [ ] **`/pace-graphic`** (lines 62–67) — route body is `NULL`. Either implement or remove.

- [ ] **`/points-distribution`** (lines 69–75) — route body is `NULL`. Either implement or remove.

---

## `R/data.R`

### Unused functions

- [ ] **`buildTeamColours()`** (line 239) — `@keywords internal`, never called from
  any package code. Developer utility to regenerate the `teamColours` data object.
  If it is still needed for data-raw workflows, move it to `data-raw/`; otherwise remove.

---

## `R/pwhl-api-interface.R`

### Unused functions

- [ ] **`buildPWHLTeamColours()`** (line 520) — `@keywords internal`, never called.
  Same pattern as `buildTeamColours()` above. Move to `data-raw/` or remove.

---

## `R/main.R`

### Commented-out dead code (`rtoot` calls)

The `rtoot::post_toot()` calls throughout the social-media posting functions have
been replaced by `atrrr::post()` but the old calls were left in as comments rather
than deleted. Sixteen instances spread across:

- [ ] Lines 148, 172, 196 — inside `tweet()`
- [ ] Line 285, 302, 334 — inside `tweetPace()` / `tweetLikelihoods()`
- [ ] Line 563, 595 — inside `tweetGames()` or equivalent
- [ ] Line 613 — tweet helper
- [ ] Line 670 — inside series-tweet helper
- [ ] Line 762, 807 — inside `tweetPlayoffOdds()` area
- [ ] Line 864 — another tweet function
- [ ] Lines 948, 990, 995 — end of file tweet functions

---

## `R/league.R`

### Large commented-out old playoff seeding logic

- [ ] **Lines 1724–1910** — ~186 lines of fully commented-out `if/else if/else`
  blocks using the old `completedSeries`/`currentSeries` seeding approach.
  The live code immediately above these comments uses the new `single_series_solver()`
  approach. This dead block is pure comment noise and can be removed.

---

## Summary counts

| Category | Count |
|---|---|
| Functions never called in production | 10 |
| Functions called only from tests | 4 |
| Dead/wasted computation blocks | 2 |
| Unimplemented stub API routes | 3 |
| Commented-out `rtoot` call sites | 16 |
| Commented-out old seeding logic (lines) | ~186 |
