# HockeyModel

<!-- badges: start -->
[![R-CMD-check](https://github.com/pbulsink/HockeyModel/workflows/R-CMD-check/badge.svg)](https://github.com/pbulsink/HockeyModel/actions)
[![Codecov test coverage](https://codecov.io/gh/pbulsink/HockeyModel/branch/master/graph/badge.svg)](https://codecov.io/gh/pbulsink/HockeyModel?branch=master)
[![Netlify Status](https://api.netlify.com/api/v1/badges/1ededd90-c1c2-44cc-8bed-e3d5cea3a284/deploy-status)](https://bulsink.ca)
<!-- badges: end -->

This is a model to predict NHL game winners & their likely performance for the rest of the season.

Read more about the package at [https://pbulsink.github.io/HockeyModel](https://pbulsink.github.io/HockeyModel)

Current predictions are below, and are always posted on twitter at [@BulsinkBot](https://www.twitter.com/BulsinkB).

## Team Ranking
<img src="https://github.com/pbulsink/HockeyModel/raw/master/prediction_results/graphics/current_rating.png" alt="Team Rankings">

## Today's Games
<img src="https://github.com/pbulsink/HockeyModel/raw/master/prediction_results/graphics/today_odds.png" alt="Today's Games">

## Total Point Predictions
<img src="https://github.com/pbulsink/HockeyModel/raw/master/prediction_results/graphics/point_predict.png" alt="Total Point Predictions">

## Point Liklihood Ranges
<img src="https://raw.githubusercontent.com/pbulsink/HockeyModel/master/prediction_results/graphics/pace/westlikelihood.png" width="425" alt="West Coast Team Point Liklihood"/> <img src="https://raw.githubusercontent.com/pbulsink/HockeyModel/master/prediction_results/graphics/pace/eastlikelihood.png" width="425" alt="East Coast Team Point Likelihood"/>

## Playoff Odds
<img src="https://github.com/pbulsink/HockeyModel/raw/master/prediction_results/graphics/playoff_odds.png" alt="Playoff Odds">

## President's Trophy Odds
<img src="https://github.com/pbulsink/HockeyModel/raw/master/prediction_results/graphics/president_odds.png" alt="President's Trophy Odds">

# Recent Changes
- Implemented Ridges plot for expected end-of-season points per team.
- Lots of teams have blue or red primary colours. Implemented new algorithm to use an alternate colour for one or both teams if the primary colours are too similar.
- Backend code changes for ease of editing/updating.
- Added playoff changes for each round and winning the cup, including posting to Twitter daily in March and April. Odds are posted as a table image (such as the Eastern Conference below):
<img src="https://github.com/pbulsink/HockeyModel/raw/master/prediction_results/graphics/east_playoff_odds.png" alt="Eastern Conference Playoff Progression Odds">

# To Do

- Twitter user [@MOCallanain](https://www.twitter.com/MOCallanain) highlighted that the predicted tie rate is < ~0.2, when in actuality it's higher. Likely due to teams playing for loser point, ~~can we parameterize the model to include a tie boost? Diagonal enhanced metric should work - calculate like the DC 0/1 goal enhancement. Model currently re-scales to increase odds to a reasonable amount.~~ DONE
- Currently, only scores are used for model generation. Moneypuck has an expansive expected goals model available for download and updated regularly (see http://moneypuck.com/data.htm). Deriving the team performance by expected goals instead of actual could reduce the impact of luck on expected future performance.
- ~~Switch to NHL API for scores and schedule~~ DONE
- Twitter user [@joseph__ii](https://www.twitter.com/joseph__ii) picked up on a quirk of the OT/SO odds assignment (see https://twitter.com/joseph__ii/status/1357785234285109248). Try rebalance with league or teams' OT performance measure?
