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

#' Historical NHL scores with Advanced Stats
#'
#' A dataset containing historical NHL and WHA scores.
#'
#' @format A data frame with  variables
#' @source \url{http://www.hockey-reference.com/}
"advanced_scores"

#' Schedule for 2018-2019.
#'
#' A dataframe with the schedule (no scores included) for 2018-2019.
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

#' Elo History
#'
#' Elo rankings for each team through time.
#'
#' @format A sparse dataframe with columns for each historical and active team (WHL and NHL) and each row a date
"elos"

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
#' @description Updates the stored schedule if any games are rescheduled due to any event. Adds playoff games as the schedules are released
#'
#' @param data_dir The data storage directory
#' @return True, if successful update or validation, `schedule` is a built in data
#'
#' @export
updateSchedule <- function(data_dir = "./data-raw/"){
  new_schedule<-HockeyScrapR::getSchedule(data_dir = data_dir, from_date=as.Date("2018-10-01"))
  new_schedule<-new_schedule[,c('Date', 'Visitor', 'Home')]
  new_schedule<-data.frame("Date" = new_schedule$Date, "HomeTeam" = new_schedule$Home, "AwayTeam" = new_schedule$Visitor)
  new_schedule$Date <- as.Date(new_schedule$Date)
  if(nrow(new_schedule) != nrow(schedule)){
    schedule <- new_schedule
    suppressMessages(usethis::use_data(schedule, overwrite = TRUE))
  }
  return(new_schedule)
}

#' Update Scores
#'
#' @description Updates the scores of all finished games.
#'
#' @param data_dir The data storage directory
#' @return True, if successful update or validation, `scores` is a built in data
#'
#' @export
updateScores <- function(data_dir = "./data-raw/"){
  new_scores<-HockeyScrapR::updateScores(score_data = HockeyModel::scores, data_dir = data_dir, sleep=0, playoffs = TRUE, last_playoffs = TRUE)
  if(nrow(new_scores) != nrow(scores)){
    #new_scores$AwayTeam<-factor(new_scores$AwayTeam, levels = levels(scores$AwayTeam))
    #new_scores$HomeTeam<-factor(new_scores$HomeTeam, levels = levels(scores$HomeTeam))
    scores<-new_scores
    suppressMessages(usethis::use_data(scores, overwrite = TRUE))
  }
  return(new_scores)
}

#' Update Series
#'
#' @description Updates the saved series data as manually adjusted in the df in the function
#'
#' @param series series, if provided, else update function.
#'
#' @return series df.
#' @export
updateSeries<-function(series = NULL){
  if(is.null(series)){
    series<-data.frame('HomeTeam' = c("Boston Bruins", "San Jose Sharks"),
                       'AwayTeam' = c("Carolina Hurricanes", "St. Louis Blues"),
                       'HomeWins' = c(0,0),
                       'AwayWins' = c(0,0),
                       stringsAsFactors = FALSE)
  }
  usethis::use_data(series, overwrite = TRUE)
  return(series)
}

#' Playoff Series
#'
#' Manually Updated Playoff Series Status.
#'
#' @format A data frame
"series"
