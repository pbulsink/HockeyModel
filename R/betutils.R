#bet_utils


compare_market_line <- function(predictions, betdiff=0.05){
  stopifnot(betdiff>0)
  stopifnot(betdiff<1)
  market <- utils::read.csv("~/markets.csv")
  market <- market[, c("Game_Id", "away_odds_final", "home_odds_final")]
  colnames(market)<-c("GameID", "AwayOdds", "HomeOdds")
  predictions<-predictions[,c("GameID", "HomeWin", "Result")]

  markets <- dplyr::left_join(predictions, market, by="GameID")

  markets$AwayWin<-1-markets$HomeWin

  markets$AwayOddsDecimal<-american_to_percent(markets$AwayOdds)
  markets$HomeOddsDecimal<-american_to_percent(markets$HomeOdds)

  markets$HomeBet<-ifelse(markets$HomeWin > markets$HomeOddsDecimal*(1+betdiff), 1, 0)
  markets$AwayBet<-ifelse(markets$AwayWin > markets$AwayOddsDecimal*(1+betdiff), 1, 0)

  markets$BetResult<-0

  for (g in 1:nrow(markets)){
    if(!is.na(markets[g,]$HomeBet)){
      if(markets[g,]$HomeBet == 1 & markets[g,]$Result > 0.5){
        markets[g,]$BetResult <- max(100, markets[g,]$HomeOdds)
        next
      } else if(markets[g,]$HomeBet == 1 & markets[g,]$Result < 0.5){
        markets[g,]$BetResult <- min(-100, markets[g,]$HomeOdds)
        next
      }
    }
    if (!is.na(markets[g,]$AwayBet)){
      if(markets[g,]$AwayBet == 1 & markets[g,]$Result < 0.5){
        markets[g,]$BetResult <- max(100, markets[g,]$AwayOdds)
        next
      } else if(markets[g,]$AwayBet == 1 & markets[g,]$Result > 0.5){
        markets[g,]$BetResult <- min(-100, markets[g,]$AwayOdds)
        next
      }
    }
  }

  return(list("HomeBets" = sum(markets$HomeBet, na.rm = T),
              "AwayBets" = sum(markets$AwayBet, na.rm = T),
              "HomeBetResults" = sum(markets[markets$HomeBet == 1,]$BetResult, na.rm = T),
              "AwayBetResults" = sum(markets[markets$AwayBet == 1,]$BetResult, na.rm = T),
              "HomeUnits" = sum(markets[markets$HomeBet == 1,]$BetResult, na.rm = T)/sum(markets$HomeBet, na.rm = T),
              "AwayUnits" = sum(markets[markets$AwayBet == 1,]$BetResult, na.rm = T)/sum(markets$AwayBet, na.rm = T),
              "TotalBets" = sum(markets$HomeBet, na.rm=T)+sum(markets$AwayBet, na.rm = T),
              "TotalResults" = sum(markets$BetResult, na.rm = T),
              "TotalUnits" = sum(markets$BetResult, na.rm = T)/(sum(markets$AwayBet, na.rm = T) + sum(markets$HomeBet, na.rm = T))))

}

american_to_percent<-function(american_odds){
  atp<-function(american_odds){
    return(ifelse(is.na(american_odds), NA,
                  ifelse(american_odds>0,
                         100 / (american_odds+100),
                         (american_odds * -1)/((american_odds*-1)+100))))
  }
  atp_v<-Vectorize(atp)

  return(atp_v(american_odds))
}

american_won<-function(american_odds){
  aw<-function(american_odds){
    return(ifelse(is.na(american_odds), NA,
                  ifelse(american_odds>0,
                         100+american_odds,
                         100+100*(100/(american_odds*-1)))))
  }
  aw_v<-Vectorize(aw)
  return(aw_v(american_odds))
}

compare_market_line_xg <- function(predictions, overbetdiff=0.5, underbetdiff=0.5, juice=0.02, use_available_mxg=TRUE){
  market<-utils::read.csv("~/markets.csv")
  if(use_available_mxg){
    market<-market[,c("Game_Id", "Total_pregame", "over_price", "under_price", "total_w_formula")]
    colnames(market)<-c("GameID", "Totals", "OverOdds", "UnderOdds", "ImpliedxG")
  } else {
    market<-market[,c("Game_Id", "Total_pregame", "over_price", "under_price")]
    colnames(market)<-c("GameID", "Totals", "OverOdds", "UnderOdds")
    market$OverOddsDecimal<-american_to_percent(market$OverOdds)
    market$UnderOddsDecimal<-american_to_percent(market$UnderOdds)
    market$NoJuiceUnder<-market$UnderOddsDecimal-((1-(market$OverOddsDecimal + market$UnderOddsDecimal))/2)
    market$ImpliedxGPoisson<-totals_to_xg(market$Totals, market$NoJuiceUnder)
    market[,c("OverOddsDecimal", "UnderOddsDecimal", "NoJuiceUnder")]<-NULL
  }

  #mix predictions into the market data
  markets <- dplyr::left_join(predictions, market, by="GameID")
  markets$AwayWin<-1-markets$HomeWin

  #Figure out where we bet
  markets$OverBet<-ifelse(markets$TotalxGPred > markets$ImpliedxG+overbetdiff, 1, 0)
  markets$UnderBet<-ifelse(markets$TotalxGPred < markets$ImpliedxG-underbetdiff, 1, 0)

  markets$BetResult<-0

  #Figure out if we win
  for (g in 1:nrow(markets)){
    if(!is.na(markets[g,]$OverBet)){
      if(markets[g,]$OverBet == 1 & markets[g,]$TotalGoals > markets[g,]$Totals){
        markets[g,]$BetResult <- max(100, markets[g,]$OverOdds)
        next
      } else if(markets[g,]$OverBet == 1 & markets[g,]$TotalGoals < markets[g,]$Totals){
        markets[g,]$BetResult <- min(-100, markets[g,]$OverOdds)
        next
      }
    }
    if (!is.na(markets[g,]$UnderBet)){
      if(markets[g,]$UnderBet == 1 & markets[g,]$TotalGoals < markets[g,]$Totals){
        markets[g,]$BetResult <- max(100, markets[g,]$UnderOdds)
        next
      } else if(markets[g,]$UnderBet == 1 & markets[g,]$TotalGoals > markets[g,]$Totals){
        markets[g,]$BetResult <- min(-100, markets[g,]$UnderOdds)
        next
      }
    }
  }


  return(list("OverBets" = sum(markets$OverBet, na.rm = T),
              "UnderBets" = sum(markets$UnderBet, na.rm = T),
              "OverBetResults" = sum(markets[markets$OverBet == 1,]$BetResult, na.rm = T),
              "UnderBetResults" = sum(markets[markets$UnderBet == 1,]$BetResult, na.rm = T),
              "OverUnits" = sum(markets[markets$OverBet == 1,]$BetResult, na.rm = T)/sum(markets$OverBet, na.rm = T),
              "UnderUnits" = sum(markets[markets$UnderBet == 1,]$BetResult, na.rm = T)/sum(markets$UnderBet, na.rm = T),
              "TotalBets" = sum(markets$OverBet, na.rm=T)+sum(markets$UnderBet, na.rm = T),
              "TotalResults" = sum(markets$BetResult, na.rm = T),
              "TotalUnits" = sum(markets$BetResult, na.rm = T)/(sum(markets$UnderBet, na.rm = T) + sum(markets$OverBet, na.rm = T))))

}

totals_to_xg<-function(under, prob){
  ttx<-function(under, prob){
    if(is.na(under) | is.na(prob)){
      return(NA)
    }
    #set up under and over functions for optimizing. Note both adjust by 0.1 to ensure that u6.0 doesn't try to calculate at 6, but up to 5 and above 7
    obj_under <- function(expg, under_prob, under){
      return(abs(sum(stats::dpois(1:floor(under-0.1), lambda=expg)) - under_prob))
    }
    obj_over <- function(expg, under_prob, under){
      return(abs(sum(stats::dpois(ceiling(under+0.1):20, lambda=expg))-(1-under_prob)))
    }
    return(mean(c(stats::optimize(obj_under, interval = c(under-1, under+1), under_prob=prob, under=under)$minimum,
                  stats::optimize(obj_over, interval = c(under-1, under+1), under_prob=prob, under=under)$minimum)))
  }
  ttx_v<-Vectorize(ttx)
  return(ttx_v(under, prob))
}


percent_to_odd<-function(percent){
  pto<-function(percent){
    decimal<-1/percent
    if(decimal < 2){
      return(as.integer((-100) / (decimal - 1)))
    } else {
      return(as.integer((decimal - 1) * 100))
    }
  }
  pto_v<-Vectorize(pto)
  return(pto_v(percent))
}
