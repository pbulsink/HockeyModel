#Dixon-Coles using the goalmodel package
#set:
# xi = 0.00426
# rs = TrUE
# dc = TRUE
# hfa = TRUE
# rho = -0.2715189

#TODO: add goalmodel to dependencies


prepScoresForGM<-function(gmScores = HockeyModel::scores, xi = 0.00426){
  gmScores$weight <- goalmodel::weights_dc(gmScores$Date, xi = xi)
  gmScores<-gmScores[gmScores$weight > 1e-7,] # this trims to about 10 years of data with xi at 0.00426
}

trainGMModel<-function(gmScores=NULL){
  if(is.null(gmScores)){
    gmScores<-prepScoresForGM()
  }

  gm_mod<-goalmodel::goalmodel(goals1 = gmScores$HomeGoals,
                               goals2 = gmScores$AwayGoals,
                               team1 = gmScores$HomeTeam,
                               team2 = gmScores$AwayTeam,
                               weights = gmScores$weight,
                               rs = TRUE,
                               hfa = TRUE
                               )

  gm_mod_dc<-goalmodel::goalmodel(goals1 = gmScores$HomeGoals,
                               goals2 = gmScores$AwayGoals,
                               team1 = gmScores$HomeTeam,
                               team2 = gmScores$AwayTeam,
                               weights = gmScores$weight,
                               dc = TRUE,
                               rs = TRUE,
                               hfa = TRUE,
                               fixed_params = gm_mod$parameters
                               )

  gm_mod_dc$parameters$rho<-HockeyModel::rho

  return(gm_mod_dc)
}


