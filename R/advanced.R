#' Prepare Advanced Data
#'
#' @param advanced_scores  an Advanced Scores frame from HockeyScrapR
#' @param long whether to return long (one team per row) or wide (both teams per row) data
#'
#' @return a data frame
#' @export
prepareAdvancedData <- function(advanced_scores = HockeyModel::advanced_scores, long = TRUE) {
  advanced_scores$Season <- as.factor(vGetSeason(advanced_scores$Date))
  advanced_scores$AtHome <- TRUE
  ngames<-nrow(advanced_scores)
  as2 <- advanced_scores %>%
    dplyr::group_by(Season) %>%
    dplyr::mutate(GameNumber = 1:dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Playoffs = ifelse(Date > "2017-08-01",
                                    ifelse(GameNumber > 1271, TRUE, FALSE),
                                    ifelse(GameNumber > 1230, TRUE, FALSE)),
                  HomeSVp = HomeSaves/AwayShots,
                  HomeSHp = 1 - AwaySaves/HomeShots,
                  AwaySVp = AwaySaves/HomeShots,
                  AwaySHp = 1 - HomeSaves/AwayShots,
                  HomePts = ceiling(2 * Result),
                  AwayPts = ceiling(2 * (1 - Result)),
                  HomeCFp = HomeCloseCF/(HomeCloseCF + AwayCloseCF),
                  AwayCFp = AwayCloseCF/(HomeCloseCF + AwayCloseCF),
                  HomePDO = HomeSVp + HomeSHp, AwayPDO = AwaySVp + AwaySHp,
                  HomeShiftLngth = HomeTOI/HomeShifts,
                  AwayShiftLngth = AwayTOI/AwayShifts)

  as3 <- dplyr::data_frame(Date = c(as2$Date, as2$Date),
                           AtHome = c(as2$AtHome, !as2$AtHome),
                           Season = as.factor(c(as.character(as2$Season), as.character(as2$Season))),
                           Playoffs = c(as2$Playoffs, as2$Playoffs),
                           SeasonGameNumber = c(as2$GameNumber, as2$GameNumber),
                           Team = as.factor(c(as.character(as2$HomeTeam), as.character(as2$AwayTeam))),
                           Result = c(as2$Result, 1-as2$Result),
                           Goals = c(as2$HomeGoals, as2$AwayGoals),
                           AllCF = c(as2$HomeAllCF, as2$AwayAllCF),
                           AllCA = c(as2$AwayAllCF, as2$HomeAllCF),
                           AllCFp = c(as2$HomeAllCF/(as2$HomeAllCF+as2$AwayAllCF), as2$AwayAllCF/(as2$HomeAllCF+as2$AwayAllCF)),
                           CloseCF = c(as2$HomeCloseCF, as2$AwayCloseCF),
                           CloseCA = c(as2$AwayCloseCF, as2$HomeCloseCF),
                           CloseCFp = c(as2$HomeCloseCF/(as2$HomeCloseCF+as2$AwayCloseCF), as2$AwayCloseCF/(as2$HomeCloseCF+as2$AwayCloseCF)),
                           EvenCF = c(as2$HomeEvenCF, as2$AwayEvenCF),
                           EvenCA = c(as2$AwayEvenCF, as2$HomeEvenCF),
                           EvenCFp = c(as2$HomeEvenCF/(as2$HomeEvenCF+as2$AwayEvenCF), as2$AwayCloseCF/(as2$HomeEvenCF+as2$AwayEvenCF)),
                           Hits = c(as2$HomeHits, as2$AwayHits),
                           Blocks = c(as2$HomeBlocks, as2$AwayBlocks),
                           ShotsF = c(as2$HomeShots, as2$AwayShots),
                           ShotsA = c(as2$AwayShots, as2$HomeShots),
                           PIM = c(as2$HomePIM, as2$AwayPIM),
                           EV = c(as2$HomeEV, as2$AwayEV),
                           PP = c(as2$HomePP, as2$AwayPP),
                           SH = c(as2$HomeSH, as2$AwaySH),
                           Saves = c(as2$HomeSaves, as2$AwaySaves),
                           Shifts = c(as2$HomeShifts, as2$AwayShifts),
                           TOI = c(as2$HomeTOI, as2$AwayTOI),
                           SVp = c(as2$HomeSVp, as2$AwaySVp),
                           SHp = c(as2$HomeSHp, as2$AwaySHp),
                           Pts = c(as2$HomePts, as2$AwayPts),
                           PDO = c(as2$HomePDO, as2$AwayPDO),
                           ShftLngth = c(as2$HomeShiftLngth, as2$AwayShiftLngth)) %>%
    dplyr::arrange(Date, SeasonGameNumber) %>%
    dplyr::group_by(Team) %>%
    dplyr::mutate(last.game = dplyr::lag(Pts, 1, default = 0),
                  CEF.20 = zoo::rollapplyr(EvenCF, 20, mean, partial = TRUE, na.rm = TRUE, fill = 'extend'),
                  CEA.20 = zoo::rollapplyr(EvenCA, 20, mean, partial = TRUE, na.rm = TRUE, fill = 'extend'),
                  CEp.20 = zoo::rollapplyr(EvenCFp, 20, mean, patial = TRUE, na.rm = TRUE, fill = 'extend'),
                  CCF.20 = zoo::rollapplyr(CloseCF, 20, mean, partial = TRUE, na.rm = TRUE, fill = 'extend'),
                  CCA.20 = zoo::rollapplyr(CloseCA, 20, mean, partial = TRUE, na.rm = TRUE, fill = 'extend'),
                  CCp.20 = zoo::rollapplyr(CloseCFp, 20, mean, patial = TRUE, na.rm = TRUE, fill = 'extend'),
                  CAF.20 = zoo::rollapplyr(AllCF, 20, mean, partial = TRUE, na.rm = TRUE, fill = 'extend'),
                  CAA.20 = zoo::rollapplyr(AllCA, 20, mean, partial = TRUE, na.rm = TRUE, fill = 'extend'),
                  CAp.20 = zoo::rollapplyr(AllCFp, 20, mean, patial = TRUE, na.rm = TRUE, fill = 'extend'),
                  hits.20 = zoo::rollapplyr(Hits, 20, mean, partial = TRUE, na.rm = TRUE, fill = 'extend'),
                  shotF.20 = zoo::rollapplyr(ShotsF, 20, mean, partial = TRUE, na.rm = TRUE, fill = 'extend'),
                  shotA.20 = zoo::rollapplyr(ShotsA, 20, mean, partial = TRUE, na.rm = TRUE, fill = 'extend'),
                  blocks.20 = zoo::rollapplyr(Blocks, 20, mean, partial = TRUE, na.rm = TRUE, fill = 'extend'),
                  shp.20 = zoo::rollapplyr(SHp, 20, mean, partial = TRUE, na.rm = TRUE, fill = 'extend'),
                  sv.20 = zoo::rollapplyr(Saves, 20, mean, partial = TRUE, na.rm = TRUE, fill = 'extend'),
                  svp.20 = zoo::rollapplyr(SVp, 20, mean, partial = TRUE, na.rm = TRUE, fill = 'extend'),
                  PDO.20 = zoo::rollapplyr(PDO, 20, mean, partial = TRUE, na.rm = TRUE, fill = 'extend'),
                  ShftLngth.20 = zoo::rollapplyr(Hits, 20, mean, partial = TRUE, na.rm = TRUE, fill = 'extend'),
                  days.since.last = pmin(21, Date - dplyr::lag(Date, 1, default = as.Date("1900-01-01"))),
                  days.till.next = pmin(21, dplyr::lead(Date, 1, default = as.Date("2100-01-01"))-Date)) %>%
    dplyr::mutate(last.20 = zoo::rollapplyr(Pts, 20, sum, partial = TRUE, na.rm = TRUE, fill = 0)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Season, Team) %>%
    dplyr::mutate(TeamGameNumber = 1:dplyr::n()) %>%
    dplyr::ungroup() %>%
    data.frame()

  #A few games have 0 Corsi stats, fill with 0.5 CF %
  as3[is.nan(as3$AllCFp), 'AllCFp'] <- 0.5
  as3[is.nan(as3$CloseCFp), 'CloseCFp'] <- 0.5
  as3[is.nan(as3$EvenCFp), 'EvenCFp'] <- 0.5

  as3$Result6<-as.factor(as.character(as3$Result))
  levels(as3$Result6) <- c('Loss','OT Loss','SO Loss', 'SO Win', 'OT Win', 'Win')

  as3$Result3<-round((as3$Result-0.5)*2.5)
  as3$Result3<-as.factor(as.character(as3$Result3))
  levels(as3$Result3) <- c('Loss','Draw','Win')

  as3$Result2<-round(as3$Result)
  as3$Result2<-as.factor(as.character(as3$Result2))
  levels(as3$Result2) <- c('Loss','Win')

  if(long){
    return(as3)
  } else {
    as_home<-as3[1:ngames,]
    as_away<-as3[(ngames+1):(2*ngames),]
    nms<-colnames(as_home)
    as4<-as3[,c('Date','Season','Playoffs','SeasonGameNumber','Result')]
    tm_specific<-c(6,8:57)
    nms_home<-paste0('home.',nms[tm_specific])
    nms_away<-paste0('away.',nms[tm_specific])
    colnames(as_home)[tm_specific]<-nms_home
    colnames(as_away)[tm_specific]<-nms_away
    as4<-cbind(as4, as_home[,tm_specific], as_away[,tm_specific])

    return(as4)
  }

}

caretTrain<-function(adv = prepareAdvancedData()){
  inTraining <- caret::createDataPartition(adv$Result, p = .80, list = FALSE)
  training <- adv[ inTraining,]
  testing  <- adv[-inTraining,]
  folds<-caret::groupKFold(training$Season, length(unique(training$Season)))
  fitControl<-caret::trainControl(method = 'cv',
                                  index = folds,
                                  number = length(unique(adv$Season)),
                                  #repeats = 10,
                                  preProcOptions = c('center','scale'),
                                  savePredictions = 'final')#,
                                  #summaryFunction = ModelMetrics::mlogLoss)
  gbmGrid <- expand.grid(interaction.depth = c(1,3,5,7,9),
                          n.trees = (1:30)*50,
                          shrinkage = 0.1,
                          n.minobsinnode = 20)
  cl<-parallel::makeCluster(parallel::detectCores()-1)
  doSNOW::registerDoSNOW(cl)
  model_list <- caretEnsemble::caretList(Result ~
                                           AtHome +
                                           CAp.20 +
                                           CEp.20 +
                                           CCp.20 +
                                           PDO.20 +
                                           last.20 +
                                           days.till.next +
                                           days.since.last +
                                           TeamGameNumber +
                                           last.game +
                                           blocks.20 +
                                           hits.20 +
                                           shp.20 +
                                           svp.20 +
                                           ShftLngth.20 +
                                           SeasonGameNumber+
                                           0,
                                         data = training,
                                         trControl = fitControl,
                                         methodList = c('glm','gbm',"rpart", "avNNet", 'pcr', 'pcaNNet', 'rf'))

  greedy_ensemble <- caretEnsemble::caretEnsemble(model_list,
                                                  trControl = caret::trainControl(number = 2)
                                                  )
  summary(greedy_ensemble)
  model_preds <- lapply(model_list, predict, newdata=testing)
  model_preds <- data.frame(model_preds)
  model_preds$Result<-testing$Result
  model_combo <- caret::train(Result ~ ., data = model_preds, method = 'glmnet', trControl = caret::trainControl(method = 'repeatedcv', number = 10, repeats = 10))
  summary(model_combo)
  gbm_ensemble <- caretEnsemble::caretStack(model_list,
                                            method = 'gbm',
                                            trControl = caret::trainControl(method = 'boot',
                                                                            number = 10,
                                                                            savePredictions = 'final',
                                                                            classProbs = TRUE
                                                                            )
                                            )
  summary(gbm_ensemble)

  model_preds <- lapply(model_list, predict, newdata=testing, type="prob")
  model_preds <- lapply(model_preds, function(x) x[,"Win"])
  model_preds <- data.frame(model_preds)
  ens_preds <- predict(greedy_ensemble, newdata=testing, type="prob")
  model_preds$ensemble <- ens_preds
  caTools::colAUC(model_preds, testing$Result3)

  model_preds3 <- model_preds
  model_preds3$ensemble <- predict(gbm_ensemble, newdata=testing, type="prob")
  caTools::colAUC(model_preds3, testing$Result3)
  #library(caret); library(gbm)
  caret::varImp(greedy_ensemble)


  gbm.fit <- caret::train(Result2 ~ Team +
                            AtHome +
                            CAp.20 +
                            CEp.20 +
                            CCp.20 +
                            PDO.20 +
                            last.20 +
                            days.till.next +
                            days.since.last +
                            TeamGameNumber +
                            last.game +
                            blocks.20 +
                            hits.20 +
                            shp.20 +
                            svp.20 +
                            ShftLngth.20 +
                            SeasonGameNumber +
                            0,
                   data = training,
                   method = "gbm",
                   trControl = fitControl,
                   tuneGrid = gbmGrid,
                   metric = 'logLoss')#,
                   #verbose = FALSE)
  parallel::stopCluster(cl)
  gc()

  cl<-parallel::makeCluster(parallel::detectCores()-1)
  doSNOW::registerDoSNOW(cl)
  adaboost.fit<-caret::train(Result2 ~
                               AtHome +
                               CAp.20 +
                               CEp.20 +
                               CCp.20 +
                               PDO.20 +
                               last.20 +
                               days.till.next +
                               days.since.last +
                               TeamGameNumber +
                               last.game +
                               blocks.20 +
                               hits.20 +
                               shp.20 +
                               svp.20 +
                               ShftLngth.20 +
                               SeasonGameNumber +
                               0,
                             data = training,
                             method = "adaboost",
                             trControl = fitControl)#,
                             #verbose = FALSE)
  parallel::stopCluster(cl)
  gc()
  cl<-parallel::makeCluster(parallel::detectCores()-1)
  doSNOW::registerDoSNOW(cl)
  svm.fit <- caret::train(Result2 ~
                            AtHome +
                            CAp.20 +
                            CEp.20 +
                            CCp.20 +
                            PDO.20 +
                            last.20 +
                            days.till.next +
                            days.since.last +
                            TeamGameNumber +
                            last.game +
                            blocks.20 +
                            hits.20 +
                            shp.20 +
                            svp.20 +
                            ShftLngth.20 +
                            SeasonGameNumber +
                            0,
                          data = training,
                          method = "svmLinearWeights",
                          trControl = fitControl)#,
                          #verbose = FALSE)
  parallel::stopCluster(cl)
  gc()
  cl<-parallel::makeCluster(parallel::detectCores()-1)
  doSNOW::registerDoSNOW(cl)
  nn.fit <- caret::train(Result2 ~
                            AtHome +
                            CAp.20 +
                            CEp.20 +
                            CCp.20 +
                            PDO.20 +
                            last.20 +
                            days.till.next +
                            days.since.last +
                            TeamGameNumber +
                            last.game +
                            blocks.20 +
                            hits.20 +
                            shp.20 +
                            svp.20 +
                            ShftLngth.20 +
                            SeasonGameNumber +
                            0,
                          data = training,
                          method = "avNNet",
                          trControl = fitControl)#,
                         #verbose = FALSE)
  parallel::stopCluster(cl)
  gc()
  cl<-parallel::makeCluster(parallel::detectCores()-1)
  doSNOW::registerDoSNOW(cl)
  bayesglm.fit <- caret::train(Result2 ~
                             AtHome +
                             CAp.20 +
                             CEp.20 +
                             CCp.20 +
                             PDO.20 +
                             last.20 +
                             days.till.next +
                             days.since.last +
                             TeamGameNumber +
                             last.game +
                             blocks.20 +
                             hits.20 +
                             shp.20 +
                             svp.20 +
                             ShftLngth.20 +
                             SeasonGameNumber +
                             0,
                           data = training,
                           method = "bayesglm",
                           trControl = fitControl)#,
                           #verbose = FALSE)
  parallel::stopCluster(cl)
  gc()
  cl<-parallel::makeCluster(parallel::detectCores()-1)
  doSNOW::registerDoSNOW(cl)
  nbayes.fit <- caret::train(Result2 ~
                               AtHome +
                               CAp.20 +
                               CEp.20 +
                               CCp.20 +
                               PDO.20 +
                               last.20 +
                               days.till.next +
                               days.since.last +
                               TeamGameNumber +
                               last.game +
                               blocks.20 +
                               hits.20 +
                               shp.20 +
                               svp.20 +
                               ShftLngth.20 +
                               SeasonGameNumber +
                               0,
                             data = training,
                             method = "nb",
                             trControl = fitControl)#,
                             #verbose = FALSE)

  parallel::stopCluster(cl)
  gc()
}
