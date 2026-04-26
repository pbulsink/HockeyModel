# Data work. Includes updates of data (daily)

## Datasets Documentation ##

#' Historical NHL and WHA scores.
#'
#' A dataset containing historical NHL and WHA scores.
#'
#' @format A data frame with  variables:
#' \describe{
#'   \item{Date}{Date of the game}
#'   \item{AwayTeam}{Name of visiting team}
#'   \item{AwayGoals}{Number of goals for the visiting team}
#'   \item{HomeTeam}{Name of the home team}
#'   \item{HomeGoals}{Number of goals for the home team}
#'   \item{OTStatus}{The OT status of the game (none, OT, SO, 2OT, 3OT, ...)}
#'   \item{GameID}{Unique GameID for each game}
#'   \item{GameType}{Game Type for each game}
#'   \item{GameStatus}{Game Status - 'Final'}
#'   \item{Result}{A numerically coded result for Hoome Team. Win=1, OTWin = 0.75, SOWin = 0.6, Tie = 0.5, SOLoss = 0.4, OTLoss = 0.25, Loss = 0}
#'   \item{HomexG}{Home Team's xG}
#'   \item{AwayxG}{Away Team's xG}
#'   \item{AwayCF}{Advanced Stats Properties}
#'   \item{AwayCFpk}{Advanced Stats Properties}
#'   \item{AwayCFpp}{Advanced Stats Properties}
#'   \item{AwayG}{Advanced Stats Properties}
#'   \item{AwayGpk}{Advanced Stats Properties}
#'   \item{AwayGpp}{Advanced Stats Properties}
#'   \item{AwayxGpk}{Advanced Stats Properties}
#'   \item{AwayxGpp}{Advanced Stats Properties}
#'   \item{HomeCF}{Advanced Stats Properties}
#'   \item{HomeCFpk}{Advanced Stats Properties}
#'   \item{HomeCFpp}{Advanced Stats Properties}
#'   \item{HomeG}{Advanced Stats Properties}
#'   \item{HomeGpk}{Advanced Stats Properties}
#'   \item{HomeGpp}{Advanced Stats Properties}
#'   \item{HomexGpk}{Advanced Stats Properties}
#'   \item{HomexGpp}{Advanced Stats Properties}
#' }
#' @source \url{http://www.hockey-reference.com/}
"scores"

#' Schedule for current season.
#'
#' A dataframe with the schedule (no scores included) for current season.
#'
#' @format A data frame with  variables:
#' \describe{
#'   \item{Date}{Date of the game played/to be played}
#'   \item{AwayTeam}{Name of the visiting team}
#'   \item{HomeTeam}{Name of the home team}
#'   \item{GameID}{Unique GameID for each game}
#'   \item{GameStatus}{Game Status}
#'   \item{GameType}{Game Type for each game}
#' }
#' @source \url{http://www.hockey-reference.com/}
"schedule"

#' Dixon Coles' `m`
#'
#' Model fit for each team's attack and defend strength, as well as home ice advantage.
#'
#' @format a glm model fit
"m"

#' Dixon Coles' `rho`
#'
#' Rho for low score correction to Dixon Coles. Should be around -0.25
#'
#' @format a single numerical value usually around -0.25
"rho"

#' Beta
#'
#' Beta for adjustment of tie games in the model. Beta is the shape parameter (also known as the Weibull 'slope') for the Weibull distribution, optimized to fit the ties data. Should be around 2
#'
#' @format a single numerical value usually around 2
"beta"

#' Eta
#'
#' Eta for adjustment of tie games in the model. Eta is the Weibull 'scale' parameter, the distribution is multiplied by the diagonal to get it to better estimate tie game odds. Should be around 3
#'
#' @format a single numerical value usually around 3
"eta"

#' k
#'
#' k for adjustment of tie games in the model. k is a multiplication value to the Weibull multiplier to the diagonal to get it to better estimate tie game odds. Should be around 5 or 6
#'
#' @format a single numerical value usually around 5 or 6
"k"


#' Team Colours
#' Hex and RGB team colours (primary & secondary where applicable), and path to team logos
#'
#' @format a data frame of colours & logos
"teamColours"


#' PWHL Team Colours
#'
#' Primary and secondary hex colours, team short codes, hashtags, and PWHL team
#' IDs for all six PWHL teams.
#'
#' @format A data frame with variables:
#' \describe{
#'   \item{Team}{Full team name}
#'   \item{Hex}{Primary colour hex code}
#'   \item{AltHex}{Secondary colour hex code}
#'   \item{Hashtag}{Official social media hashtag}
#'   \item{ShortCode}{Three-letter (or two-letter) team abbreviation}
#'   \item{Division}{League division (currently all `"PWHL"`)}
#'   \item{Conference}{League conference (currently all `"PWHL"`)}
#'   \item{PWHLID}{Integer team ID used by the HockeyTech API}
#' }
#' @source \url{https://www.thepwhl.com/}
"pwhlTeamColours"


#' PWHL Schedule
#'
#' Schedule for the current PWHL season (no scores included).
#'
#' @format A data frame with variables:
#' \describe{
#'   \item{Date}{Date the game is/was played}
#'   \item{HomeTeam}{Name of the home team}
#'   \item{AwayTeam}{Name of the visiting team}
#'   \item{GameID}{Unique PWHL game ID}
#'   \item{GameType}{`"R"` for regular season, `"P"` for playoff}
#'   \item{GameStatus}{`"Scheduled"` or `"Final"`}
#' }
#' @source \url{https://www.thepwhl.com/}
"pwhlSchedule"


#' PWHL Scores
#'
#' Historical PWHL game scores.
#'
#' @format A data frame with variables:
#' \describe{
#'   \item{Date}{Date the game was played}
#'   \item{HomeTeam}{Name of the home team}
#'   \item{AwayTeam}{Name of the visiting team}
#'   \item{GameID}{Unique PWHL game ID}
#'   \item{HomeGoals}{Goals scored by the home team}
#'   \item{AwayGoals}{Goals scored by the away team}
#'   \item{OTStatus}{Overtime status: `""` (regulation), `"OT"`, or `"SO"`}
#'   \item{GameType}{`"R"` for regular season, `"P"` for playoff}
#'   \item{GameStatus}{`"Final"`}
#' }
#' @source \url{https://www.thepwhl.com/}
"pwhlScores"


#' PWHL Dixon-Coles model `m`
#'
#' Fitted GLM capturing each PWHL team's attack and defence strength plus home
#' ice advantage. Populated by [updatePWHLDC()].
#'
#' @format a glm model fit, or `NULL` when not yet fitted
"pwhl_m"


#' PWHL Dixon-Coles `rho`
#'
#' Low-score correction parameter for the PWHL model. Populated by
#' [updatePWHLDC()].
#'
#' @format a single numeric value, or `NULL` when not yet fitted
"pwhl_rho"


#' PWHL `beta`
#'
#' Weibull shape parameter for PWHL tie-game enhancement. Populated by
#' [updatePWHLDC()].
#'
#' @format a single numeric value, or `NULL` when not yet fitted
"pwhl_beta"


#' PWHL `eta`
#'
#' Weibull scale parameter for PWHL tie-game enhancement. Populated by
#' [updatePWHLDC()].
#'
#' @format a single numeric value, or `NULL` when not yet fitted
"pwhl_eta"


#' PWHL `k`
#'
#' Weibull multiplier for PWHL tie-game enhancement. Populated by
#' [updatePWHLDC()].
#'
#' @format a single numeric value, or `NULL` when not yet fitted
"pwhl_k"


#' Iterative Parameters
#' Parameters for iterative Dixon-Coles method
#'
#' @format a list of lists of parameters for win/loss and xG versions
"iterativeParameters"

#' Iterative Rankings
#' Current rankings for iterative Dixon-Coles methods
#'
#' @format a list of data frames and a date value of the last rankings update
"iterativeRankings"

#' Summary Results (Testing)
#' A testing dataset from 2021 presesason. Not for regular use.
#'
#' @format a data frame
"summary_results_testing"

#' Example Predictions
#' A example 'predictions' compiled for each team's change in predictions over the 2020-2021 season (Contains predictions from 2021-01-12 to 2021-05-08)
#'
#' @format a tibble
"example_predictions"

#' Example Raw Predictions
#' A example 'raw predictions' compiled for testing purposes
#'
#' @format a tibble
"example_raw_predictions"

#' Rebuild `teamColours` package data from source CSV
#'
#' @returns `NULL` (invisibly). Writes updated data when `usethis` is
#'   available.
#' @keywords internal
buildTeamColours <- function() {
  teamColours <- utils::read.csv(
    "./data-raw/logos/team_colours.csv",
    stringsAsFactors = FALSE
  )
  teamlist <- unique(teamColours$Team)
  teamColours$Logo <- file.path(
    "./data-raw",
    "logos",
    paste0(tolower(gsub(" ", "_", teamlist)), ".gif")
  )
  if (requireNamespace("usethis")) {
    usethis::use_data(teamColours, overwrite = TRUE)
  } else {
    warning(
      "Can't write teamcolours to file, usethis package must be installed."
    )
  }
}
