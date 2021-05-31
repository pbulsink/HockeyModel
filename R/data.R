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
#'   \item{League}{NHL or WHA}
#'   \item{Tie}{Boolean, Tie or Not}
#'   \item{Winner}{Name of the winning team}
#'   \item{Loser}{Name of the losing team}
#'   \item{Result}{Coded home win = 0, visitor win = 1}
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

#' Update Schedule
#'
#' @description DEPRECIATED: USE \code{\link{updateScheduleAPI}} Updates the stored schedule if any games are rescheduled due to any event. Adds playoff games as the schedules are released
#'
#' @param data_dir The data storage directory
#' @return schedule
#'
#' @export
updateSchedule <- function(data_dir = "./data-raw"){
  message("Depreciated, use 'updateScheduleAPI'")
  new_schedule<-HockeyScrapR::getSchedule(data_dir = data_dir, from_date=as.Date(getCurrentSeasonStartDate()))
  new_schedule<-new_schedule[,c('Date', 'Visitor', 'Home')]
  new_schedule<-data.frame("Date" = new_schedule$Date, "HomeTeam" = new_schedule$Home, "AwayTeam" = new_schedule$Visitor)
  new_schedule$Date <- as.Date(new_schedule$Date)
  new_schedule<-new_schedule[!(new_schedule$Date > as.Date('2020-03-11') & new_schedule$Date < as.Date('2020-04-05')),]
  if(nrow(new_schedule) != nrow(schedule)){
    schedule <- new_schedule
    suppressMessages(usethis::use_data(schedule, overwrite = TRUE))
  }
  return(new_schedule)
}

#' Update Scores
#'
#' @description DEPRECIATED: USE \code{\link{updateScoresAPI}} Updates the scores of all finished games.
#'
#' @param data_dir The data storage directory
#' @param last_playoffs Trigger to pass to \code{\link[HockeyScrapR]{updateScores}}
#'
#' @return score
#'
#' @export
updateScores <- function(data_dir = "./data-raw", last_playoffs = FALSE){
  message("Depreciated, use 'updateScoresAPI'")
  new_scores<-HockeyScrapR::updateScores(score_data = HockeyModel::scores, data_dir = data_dir, sleep=0, playoffs = TRUE, last_playoffs = last_playoffs)
  if(nrow(new_scores) != nrow(scores)){
    #new_scores$AwayTeam<-factor(new_scores$AwayTeam, levels = levels(scores$AwayTeam))
    #new_scores$HomeTeam<-factor(new_scores$HomeTeam, levels = levels(scores$HomeTeam))
    scores<-new_scores
    suppressMessages(usethis::use_data(scores, overwrite = TRUE))
  }
  return(new_scores)
}

