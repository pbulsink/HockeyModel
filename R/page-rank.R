modelPageRank<-function(scores=HockeyModel::scores, currentDate = Sys.Date()){
  edges<-data.frame(from = c(as.character(scores$HomeTeam), as.character(scores$AwayTeam)),
                    to = c(as.character(scores$AwayTeam), as.character(scores$HomeTeam)),
                    Goals = c(scores$HomeGoals-scores$AwayGoals,scores$AwayGoals-scores$HomeGoals),
                    AtHome = c(rep(TRUE, nrow(scores)), rep(FALSE, nrow(scores))),
                    stringsAsFactors = FALSE)
  # weight = age factor (see Dixon Coles) * team goals scored - Home ice advantage of 0.3333 goals
  edges$weight <- PageRankWeights(c(scores$Date, scores$Date), currentDate = currentDate, xi = 0.05)*(edges$Goals-edges$AtHome*0.33333)
  edges<-edges[edges$weight > 1e-5,]
  prGraph<-igraph::graph_from_data_frame(d = edges, directed = TRUE)
  prModel<-igraph::page_rank(graph = prGraph, directed = TRUE, weights = NULL)

}
