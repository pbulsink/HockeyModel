#league stuff: tables, standings, stats, etc.

#' Bulid Stats Table
#'
#' @param scores scores to use to build stats table
#'
#' @return a stats table (as tibble)
#' @export
buildStats<-function(scores = HockeyModel::scores){
  scores<-droplevels(scores)
  teamlist <- sort(unique(c(as.character(scores$HomeTeam), as.character(scores$AwayTeam))))
  tmp1<-scores %>%
    dplyr::group_by(!!dplyr::sym('HomeTeam')) %>%
    dplyr::summarise(
      GP = length(!!dplyr::sym('Result')),
      W = sum(!!dplyr::sym('Result') == 1),
      OTW = sum(!!dplyr::sym('Result') == 0.75),
      SOW = sum(!!dplyr::sym('Result') == 0.6),
      OTL = sum(!!dplyr::sym('Result') == 0.25),
      SOL = sum(!!dplyr::sym('Result') == 0.4),
      L = sum(!!dplyr::sym('Result') == 0),
      P = !!dplyr::sym('W')*2 + !!dplyr::sym('OTW')*2 + !!dplyr::sym('SOW')*2 + !!dplyr::sym('OTL') + !!dplyr::sym('SOL')
      ) %>%
    dplyr::ungroup()
  tmp2<-scores %>%
    dplyr::group_by(!!dplyr::sym('AwayTeam')) %>%
    dplyr::summarise(
      GP = length(!!dplyr::sym('Result')),
      W = sum(!!dplyr::sym('Result') == 0),
      OTW = sum(!!dplyr::sym('Result') == 0.25),
      SOW = sum(!!dplyr::sym('Result') == 0.4),
      OTL = sum(!!dplyr::sym('Result') == 0.75),
      SOL = sum(!!dplyr::sym('Result') == 0.6),
      L = sum(!!dplyr::sym('Result') == 1),
      P = !!dplyr::sym('W')*2 + !!dplyr::sym('OTW')*2 + !!dplyr::sym('SOW')*2 + !!dplyr::sym('OTL') + !!dplyr::sym('SOL')
      ) %>%
    dplyr::ungroup()

  team_stats<-tibble::tibble(
    Team=teamlist,
    GP = tmp1$GP + tmp2$GP,
    Points = tmp1$P + tmp2$P,
    W = tmp1$W + tmp2$W,
    L = tmp1$L + tmp2$L,
    OTL = tmp1$OTL + tmp2$OTL,
    OTW = tmp1$OTW + tmp2$OTW,
    SOL = tmp1$SOL + tmp2$SOL,
    SOW = tmp1$SOW + tmp2$SOW
    )

  nhl_conferences <- HockeyModel::nhl_conferences
  nhl_divisions <- HockeyModel::nhl_divisions

  team_stats$Rank <- rank(-team_stats$Points, ties.method = 'random')

  team_stats$ConfRank <- 0
  team_stats$ConfRank[team_stats$Team %in% nhl_conferences$East] <- rank(-team_stats$Points[team_stats$Team %in% nhl_conferences$East], ties.method = 'random')
  team_stats$ConfRank[team_stats$Team %in% nhl_conferences$West] <- rank(-team_stats$Points[team_stats$Team %in% nhl_conferences$West], ties.method = 'random')

  team_stats$DivRank <- 0
  team_stats$DivRank[team_stats$Team %in% nhl_divisions$Atlantic] <- rank(-team_stats$Points[team_stats$Team %in% nhl_divisions$Atlantic], ties.method = 'random')
  team_stats$DivRank[team_stats$Team %in% nhl_divisions$Central] <- rank(-team_stats$Points[team_stats$Team %in% nhl_divisions$Central], ties.method = 'random')
  team_stats$DivRank[team_stats$Team %in% nhl_divisions$Metropolitan] <- rank(-team_stats$Points[team_stats$Team %in% nhl_divisions$Metropolitan], ties.method = 'random')
  team_stats$DivRank[team_stats$Team %in% nhl_divisions$Pacific] <- rank(-team_stats$Points[team_stats$Team %in% nhl_divisions$Pacific], ties.method = 'random')

  team_stats$Playoffs <- 0
  #Top three from each division
  team_stats$Playoffs[team_stats$DivRank <= 3] <- 1
  for(i in 1:16){
    if(sum(team_stats[team_stats$Team %in% nhl_conferences$East, 'Playoffs'])<8){
      team_stats[(team_stats$Team %in% nhl_conferences$East & team_stats$ConfRank == i), 'Playoffs'] <- 1
    } else {
      #Enough playoff teams
      break
    }
  }
  for(i in 1:16){
    if(sum(team_stats[team_stats$Team %in% nhl_conferences$West, 'Playoffs'])<8){
      team_stats[(team_stats$Team %in% nhl_conferences$West & team_stats$ConfRank == i), 'Playoffs'] <- 1
    } else {
      #Enough playoff teams
      break
    }
  }

  return(team_stats)
}
