# Repository structure

Standard R package layout (`HockeyModel`, GPL-3). Most top-level directories other than `R/`, `man/`, `tests/`, `data/` are excluded from the build via `.Rbuildignore`.

```
HockeyModel/
├── AGENTS.md              Agent entry point (build-ignored)
├── DESCRIPTION, NAMESPACE, LICENSE, NEWS.md, README.md
├── HockeyModel.Rproj, codecov.yml, .lintr
├── R/                     Package source (see below)
├── man/                   roxygen2-generated Rd files; never edit by hand
├── data/                  Exported .rda datasets (see below)
├── data-raw/              Dataset/report source material (build-ignored)
├── tests/testthat/        testthat tests, one test-<R file>.R per R file
├── vignettes/             basic-usage.Rmd, model-description.Rmd
├── docs/                  pkgdown site output plus docs/agent-instructions/
├── plans/                 Working plans and notes (build-ignored)
├── reference/             Reference papers (PDFs) for the model
├── prediction_results/    Daily prediction snapshots, YYYY-MM-DD-predictions.RDS
├── cache/                 Cached API responses (.rds, hashed filenames; git-ignored)
├── 20252026.csv, 20262027.csv   Season data files (build-ignored)
├── .github/               Workflows, issue templates, skills
├── .posit/, .kilo/, .vscode/    Tool/editor configuration
```

## `R/` by area

Convention: one file is roughly one exported function (or one tightly related group).

| Area | Files |
|------|-------|
| NHL data | `nhl-api-fetch.R`, `nhl-team-season-lookup.R` |
| NHL model | `nhl-dixon-coles-fit.R`, `nhl-dixon-coles-predict.R`, `nhl-dixon-coles-workflow.R` |
| NHL season/playoffs | `nhl-league-simulation.R`, `nhl-league-standings.R`, `nhl-league-playoffs.R`, `nhl-league-predictions-log.R` |
| Tuning | `optimize-model-parameters.R`, `tune_weights.R` |
| Evaluation | `metrics.R`, `validation-helpers.R` |
| PWHL | `pwhl-api-fetch.R`, `pwhl-model-core.R`, `pwhl-daily-summary.R` |
| Graphics and tables | `graphics-odds.R`, `graphics-pace.R`, `graphics-plots.R`, `graphics-predictions.R`, `graphics-tables.R`, `graphics-utils.R`, `gt_image_export.R` |
| Frontend (daily/social workflow) | `frontend-core.R`, `frontend-daily-summary.R`, `frontend-plots.R`, `frontend-season.R`, `frontend-social.R` |
| Shared | `constants.R`, `utils-export.R`, `utils-pipe.R`, `package-setup.R`, `data.R` (dataset docs), `sysdata.rda` (internal data) |

## `data/`

| Group | Objects |
|-------|---------|
| NHL schedule/results | `schedule`, `scores` |
| NHL model parameters | `beta`, `eta`, `k`, `m`, `rho` |
| PWHL | `pwhlSchedule`, `pwhlScores`, `pwhl_beta`, `pwhl_eta`, `pwhl_k`, `pwhl_m`, `pwhl_rho`, `pwhlTeamColours` |
| Presentation | `teamColours` |
| Examples/testing | `example_predictions`, `example_raw_predictions`, `summary_results_testing` |

Documented in `R/data.R`; regenerate via `usethis::use_data()` rather than editing.

## `tests/testthat/`

- Test files mirror `R/` names (`test-<name>.R`).
- Helpers: `helper-network.R`, `helper-pwhl.R`.
- `_vcr/` holds recorded HTTP cassettes (vcr), `_snaps/` snapshot output; `_problems/` and `Rplots.pdf` are stray artefacts.
- Run: `devtools::test(reporter = "check")`; single file: `devtools::test(filter = "name", reporter = "check")`.

## `.github/`

- `workflows/`: `R-CMD-check.yaml`, `test-coverage.yaml`, `build_netlify.yaml` (pkgdown), `pr-commands.yaml`, `copilot-setup-steps.yml`.
- `skills/`: `cmd-check`, `create-issue`, `document`, `github`, `implement-issue`, `r-code`, `search-code`, `tdd-workflow` (each with a `SKILL.md`).
- `ISSUE_TEMPLATE/`.

## `data-raw/`

Odds CSVs (`20212022odds.csv`, `20222023odds.csv`, `dailyodds.csv`, `old_dailyodds.csv`), `logos/`, and Rmd sources (`gamedate.Rmd`, `todays_predictions.Rmd`).

## Related agent docs

- [model-nhl](model-nhl.md), [model-pwhl](model-pwhl.md), [graphics-products](graphics-products.md)
- NEWS rules: `news.md` (not yet written)
