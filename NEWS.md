# HockeyModel (development version)

* `save_gt_as_png_ragg()` now saves `gt` tables as PNG using `ragg`, removing the need for `webshot2`/`chromote` (#35).
* Added PWHL support: new `pwhlTeamColours`, `pwhlSchedule`, and `pwhlScores` datasets plus `getPWHLSeasons()`, `getCurrentPWHLSeason()`, `getPWHLSchedule()`, `getPWHLScores()`, `pwhl_games_today()`, `updatePWHLScheduleAPI()`, and `updatePWHLScoresAPI()` functions to fetch and maintain PWHL data via the HockeyTech API (#32).
* `dailyPWHLSummary()` now provides a one-liner daily workflow for PWHL, matching `dailySummary()` for NHL: fetches schedule and scores, fits `updatePWHLDC()` model parameters (`pwhl_m`, `pwhl_rho`, `pwhl_beta`, `pwhl_eta`, `pwhl_k`), generates today's odds plot and table via `pwhl_plot_odds_today()` and `pwhl_daily_odds_table()`, posts season-wide playoff-qualification odds from `pwhl_loopless_sim()`, and posts to social media (#32).
