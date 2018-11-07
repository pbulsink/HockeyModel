f<-function(){
  sc<-scores[scores$Date > as.Date("2018-10-01"),]

teamlist<-unique(c(as.character(sc$HomeTeam), as.character(sc$AwayTeam)))
p<-readRDS("./prediction_results/2018-10-03-predictions.RDS")

#TODO: SOFTCODE
q<-readRDS("./prediction_results/2018-11-04-predictions.RDS")


for (team in teamlist){
  colour = teamColours[teamColours$Team == team, 'Hex']
  teamscores<-sc[sc$HomeTeam == team | sc$AwayTeam == team, c('AwayTeam','HomeTeam','Result')]
  teamscores[teamscores$AwayTeam == team, 'Result'] <- 1-teamscores[teamscores$AwayTeam == team, 'Result']
  teamscores$Venue<-'Home'
  teamscores[teamscores$AwayTeam == team, 'Venue'] <- 'Away'
  teamscores$Points<-ceiling(2*teamscores$Result)
  teamscores$cPoints<-cumsum(teamscores$Points)
  teamscores$GameNum<-1:nrow(teamscores)
  ngames<-nrow(teamscores)
  cp<-tail(teamscores$cPoints, 1)

  pteam<-p[p$Team == team, ]
  ppoints<-pteam$meanPoints
  maxp<-pteam$maxPoints
  minp<-pteam$minPoints
  qteam<-q[q$Team == team, ]
  qpoints<-qteam$meanPoints
  maxq<-qteam$maxPoints
  minq<-qteam$minPoints

  ggplot2::ggplot(teamscores, ggplot2::aes(x = GameNum, y = cPoints, colour = Venue)) +
    ggplot2::geom_point() +
    ggplot2::scale_x_continuous(limits = c(0, 82)) +
    ggplot2::scale_y_continuous(limits = c(0, 164)) +
    ggplot2::theme_bw() +
    ggplot2::ggtitle('Points Pace', subtitle = paste0(team, ' Expected Points: ', round(qpoints, 1), "\nChart by @BulsinkB"))+
    ggplot2::ylab('Points') +
    ggplot2::xlab('Game Number') +
    ggplot2::geom_segment(x = 0, y = 0, xend = 82, yend = ppoints, alpha = 0.05, colour = 'grey')+
    ggplot2::geom_segment(x = 0, y = 0, xend = 82, yend = maxp, alpha = 0.05, colour = 'grey')+
    ggplot2::geom_segment(x = 0, y = 0, xend = 82, yend = minp, alpha = 0.05, colour = 'grey')+
    ggplot2::geom_segment(x = ngames, y = cp, xend = 82, yend = qpoints, alpha = 0.2, colour = colour)+
    ggplot2::geom_segment(x = ngames, y = cp, xend = 82, yend = maxq, alpha = 0.2, colour = colour)+
    ggplot2::geom_segment(x = ngames, y = cp, xend = 82, yend = minq, alpha = 0.2, colour = colour)

}
}
