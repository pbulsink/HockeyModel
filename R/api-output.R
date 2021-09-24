#* Daily Game Odds
#* @get /odds
function() {
todayDC(include_xG = TRUE)
}


#* Get a graphical table of daily odds
#* @serializer png
#* @get /odds-table
function() {
  daily_odds_table()
}


#* Get an image with daily odds
#* @serializer png
#* @get /odds-graphic
function() {
  todayOddsPlot()
}


#* Get an image of playoff odds
#* @serializer png
#* @get /playoff-graphic
function(){
  HockeyModel::playoffOdds()
}

#* Get an image of President's Trophy odds
#* @serializer png
#* @get /president-graphic
function() {
  presidentOdds()
}


#* Get an image with Point Predictions
#* @serializer png
#* @get /points-graphic
function() {
  pointPredict()
}


#* Get an image with Cup Odds
#* @serializer png
#* @get /cup-odds
function() {
  NULL
}

#* @serializer png
#* @get /series-odds
function() {
  NULL
}


#* Get a pace image
#* @serializer png
#* @get /pace-graphic
function() {
  NULL
}


#* Get an image with potential points distribuition
#* @serializer png
#* @get /points-distribution
function() {
  NULL
}

#* Get an image with team ratings
#* @serializer png
#* @get /ratings
function() {
  params<-parse_dc_params(NULL)
  ratings(m=params$m)
}
