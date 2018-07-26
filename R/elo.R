#Hosts elo specific functions. Many copied from my work from pbulsink.github.io
#see https://fivethirtyeight.com/features/how-we-calculate-nba-elo-ratings/
# Season reversion = 1505
# New Teams = 1300
# k = MOV * K where K is set and k is for the game. K = 20 for NFL, NBA, less for NHL?
# Margin of VictoryNBA = ((MOV + 3)^0.8)/(7.5 + 0.006 * elodiff) where elodiff = abs(elohome - eloaway including home court adv.)
# MOV NFL = ln(MOV+1) * (2.2/(elodiff)*0.001 + 2.2)
# regression NFL = 1/3 to mean
# regression NBA = 1/4 to mean
#see https://fivethirtyeight.com/features/introducing-nfl-elo-ratings/
#see
# Home adv = 35 pts
# K (no MOV) = 8

updateELO <- function(){

}

plotELO <- function(){NULL}

todayELO <- function(){NULL}

playoffELO <- function(){NULL}

#' Calculate the win chance percent for HomeTeam
#'
#' @param home_rank The ranking of the Home Team.
#' @param away_rank The ranking of the Away Team.
#' @param h_adv The home advantage (in ranking points). Default: 0
#'
#' @return A number between 0 and 1 corresponding to the win chances of the Home Team.
#' @keywords internal
predictEloResult <- function(home_rank, away_rank, h_adv=0) {
  return(1/(1 + (10^((away_rank - (home_rank+h_adv))/400))))
}

newRankings<-function(home, away, result, mov, h_adv, k=8){
  #result is in set [0, 0.5, 1]
  if (!is.null(mov)){
    elodiff <- away - (home+h_adv)
    k<-k*((mov + 3)^0.8)/(7.5 + 0.006 * elodiff)
  }
  d_rank<-k*(result - predictEloResult(home, away))
  return(c(home+d_rank, away-d_rank))
}


