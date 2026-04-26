# HockeyModel (development version)

* `save_gt_as_png_ragg()` now saves `gt` tables as PNG using `ragg`, removing the need for `webshot2`/`chromote` (#35).
* Added PWHL support: new `pwhlTeamColours`, `pwhlSchedule`, and `pwhlScores` datasets plus `getPWHLSeasons()`, `getCurrentPWHLSeason()`, `getPWHLSchedule()`, `getPWHLScores()`, `pwhl_games_today()`, `updatePWHLScheduleAPI()`, and `updatePWHLScoresAPI()` functions to fetch and maintain PWHL data via the HockeyTech API (#32).
