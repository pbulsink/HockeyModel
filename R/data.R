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


#' Iterative Parameters
#' Parameters for iterative Dixon-Coles method
#'
#' @format a list of lists of parameters for win/loss and xG versions
'iterativeParameters'

#' Iterative Rankings
#' Current rankings for iterative Dixon-Coles methods
#'
#' @format a list of data frames and a date value of the last rankings update
'iterativeRankings'

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

buildTeamColours <- function(){
  teamColours <- utils::read.csv("./data-raw/logos/team_colours.csv", stringsAsFactors = FALSE)
  teamlist<-unique(teamColours$Team)
  teamColours$Logo <- file.path("./data-raw", "logos", paste0(tolower(gsub(" ", "_", teamlist)), ".gif"))
  if(requireNamespace('usethis')){
    usethis::use_data(teamColours, overwrite = TRUE)
  } else {
    warning("Can't write teamcolours to file, usethis package must be installed.")
  }
}
