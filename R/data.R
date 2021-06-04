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
#'   \item{GameType}{Game Type for each game}
#' }
#' @source \url{http://www.hockey-reference.com/}
"schedule"

#' NHL Divisions
#'
#' A list of divisions with teams in each vector.
#' Normally:
#' list(Atlantic = c("Boston Bruins", "Buffalo Sabres", "Detroit Red Wings", "Florida Panthers", "Montreal Canadiens", "Ottawa Senators", "Tampa Bay Lightning", "Toronto Maple Leafs"), Central = c("Colorado Avalanche", "Chicago Blackhawks", "Dallas Stars", "Minnesota Wild", "Nashville Predators", "St. Louis Blues", "Winnipeg Jets"), Metropolitan = c("Carolina Hurricanes", "Columbus Blue Jackets", "Philadelphia Flyers", "Pittsburgh Penguins", "New Jersey Devils", "New York Islanders", "New York Rangers", "Washington Capitals"), Pacific = c("Anaheim Ducks", "Arizona Coyotes", "Calgary Flames", "Edmonton Oilers", "Los Angeles Kings", "San Jose Sharks", "Vancouver Canucks", "Vegas Golden Knights"))
#'
#' 2020-2021:
#' list(North = c("Calgary Flames", "Edmonton Oilers", "Montreal Canadiens", "Ottawa Senators", "Toronto Maple Leafs", "Vancouver Canucks", "Winnipeg Jets"), West = c("Anaheim Ducks", "Arizona Coyotes", "Colorado Avalanche", "Los Angeles Kings", "Minnesota Wild", "San Jose Sharks", "St. Louis Blues", "Vegas Golden Knights"), Central = c("Carolina Hurricanes", "Chicago Blackhawks", "Columbus Blue Jackets", "Dallas Stars", "Detroit Red Wings", "Florida Panthers", "Nashville Predators", "Tampa Bay Lightning"), East = c("Boston Bruins", "Buffalo Sabres", "New Jersey Devils", "New York Islanders", "New York Rangers", "Philadelphia Flyers", "Pittsburgh Penguins", "Washington Capitals"))
#'
#' @format A list of vectors:
#' \describe{
#'   \item{Atlantic}{Teams in the Atlantic division}
#'   \item{Central}{Teams in the Central division}
#'   \item{Metropolitan}{Teams in the Metropolitan division}
#'   \item{Pacific}{Teams in the Pacific division}
#' }
#' @source \url{http://www.nhl.com/}
"nhl_divisions"

#' NHL Conferences
#'
#' A list of Conferences with teams in each vector.
#'
#' @format A list of vectors:
#' \describe{
#'   \item{East}{Teams in the East conference}
#'   \item{West}{Teams in the West conference}
#' }
#' @source \url{http://www.nhl.com/}
"nhl_conferences"


#' Dixon Coles' `m`
#'
#' Model fit for each team's attack and defend strength, as well as home ice advantage.
#'
#' @format a glm model fit
"m"

#' Dixon Coles' `rho`
#'
#' Rho for low score correction to Dixon Coles
#'
#' @format a single numerical value
"rho"

#' Team Colours
#' Hex and RGB team colours (primary & secondary where applicable), and path to team logos
#'
#' @format a data frame of colours & logos
"teamColours"

buildTeamColours <- function(){
  teamColours <- utils::read.csv("./data-raw/logos/team_colours.csv", stringsAsFactors = FALSE)
  teamlist<-unique(teamColours$Team)
  teamColours$Logo <- file.path("./data-raw", "logos", paste0(tolower(gsub(" ", "_", teamlist)), ".gif"))
  usethis::use_data(teamColours, overwrite = TRUE)
}
