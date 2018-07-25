# Data work. Includes updates of data (daily)

updateSchedule <- function(data_dir = "./data-raw"){
  new_schedule<-HockeyScrapR::get_schedule(data_dir = data_dir, from_date=as.Date("2018-10-01"))
  if(nrow(new_schedule) > nrow(schedule)){
    schedule <- new_schedule
    devtools::use_data(schedule, overwrite = TRUE)
  }
}

updateScores <- function(data_dir = "./data-raw"){
  new_scores<-HockeyScrapR::updateScores(score_data = scores, data_dir = data_dir)
  if(nrow(new_scores) > nrow(scores)){
    scores<-new_scores
    devtools::use_data(scores, overwrite = TRUE)
  }
}
