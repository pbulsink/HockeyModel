#Hosts main functions
#Calls to data and preciction calculations, produce figures & tables

update <- function(){NULL} #update scores, etc.

todayOdds <- function(){
  elo <- NULL #eloToday()
  dc <- NULL #dcToday()

  multi <- NULL #multiToday()
}

playoffOdds <- function(){
  elo <- NULL #eloToday()
  dc <- NULL #dcToday()

  multi <- NULL #multiToday()
}

tweet <- function(){NULL}

dailyOummary <- function(){
  today <- todayOdds()
  playoff <- playoffOdds()

  tweet()
}
