#* Daily Game Odds
#* @serializer json
#* post /odds
function() {
  odds<-todayDC(include_xG = TRUE)
  if(is.null(odds)){
    return("")
  } else {
    return(odds)
  }
}


#* Get a graphical table of daily odds
#* @serializer png
#* post /odds-table
function() {
  daily_odds_table()
}


#* Get an image with daily odds
#* @serializer png
#* post /odds-graphic
function() {
  todayOddsPlot()
}


#* Get an image of playoff odds
#* @serializer png
#* post /playoff-graphic
function(){
  playoffOdds()
}

#* Get an image of President's Trophy odds
#* @serializer png
#* post /president-graphic
function() {
  presidentOdds()
}


#* Get an image with Point Predictions
#* @serializer png
#* post /points-graphic
function() {
  pointPredict()
}


#* Get an image with Cup Odds
#* @serializer png
#* post /cup-odds
function() {
  NULL
}

#* @serializer png
#* post /series-odds
function() {
  NULL
}


#* Get a pace image
#* @serializer png
#* post /pace-graphic
function() {
  NULL
}


#* Get an image with potential points distribuition
#* @serializer png
#* post /points-distribution
function() {
  NULL
}

#* Get an image with team ratings
#* @serializer png
#* post /ratings
function() {
  params<-parse_dc_params(NULL)
  ratings(m=params$m)
}
