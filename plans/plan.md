# HockeyModel Code Review & Fix Plan

**Date:** September 24, 2026  
**Status:** Phase 1-3 Complete; Phase 4-5 Outstanding

---

## Summary

Multi-subagent code review identified issues across daily posting workflow, Dixon-Coles model correctness, season simulation performance, and API robustness. Phases 1–3 (Tier 1–3) now complete; Phases 4–5 (Tier 4–5) remain outstanding.

---

## 🚨 REMAINING ISSUES BY PRIORITY

### TIER 1: CRITICAL (Correctness)

---

### TIER 2: IMPORTANT (Daily Workflow)

---

### TIER 3: ROBUSTNESS (API & Edge Cases)

#### Issue 3.15: Hardcoded Field Names Throughout
- **Files:** `nhl-api-fetch.R` (many locations)
- **Problem:** Schedule, score, playoff parsing all depend on fixed API field names
- **Effect:** Any schema change breaks fetches silently
- **Fix:** 
  - Centralize field name mappings
  - Add validation that expected fields exist
  - Fail fast if schema changes
- **Status:** ✅ Fixed. Added `.NHL_API_FIELDS` constants list at top of `nhl-api-fetch.R` as centralized schema reference. All critical functions (`getNHLSchedule()`, `games_today()`, `getNHLScores()`, `get_xg()`, `getAPISeries()`) now validate expected fields exist before access and error with descriptive messages on schema mismatch. Future field access changes can reference the constant list for consistency.

---

### TIER 4: PERFORMANCE (Simulation)

#### Issue 4.7: Repeated Parameter Parsing
- **File:** `nhl-dixon-coles-predict.R:28, 152, 191, 332, 415` (and elsewhere)
- **Problem:** `parse_dc_params()` called repeatedly in inner loops
- **Fix:** Parse once at public function boundary; pass parsed params to internals
- **Test:** Verify no re-parsing in hot loops


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
