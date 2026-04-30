# Backtest xG blending vs goal-only model
#'
#' @description Runs a backtest over a date range, re-fitting model params each day using historical data up to that day, and compares goal-only, xG-only, and blended predictions using the fitted weight.
#'
#' @param startDate First date to evaluate (as of predictions for that date)
#' @param endDate Last date to evaluate
#' @param scores Historical scores data frame (must contain HomeTeam, AwayTeam, Date, GameID, Result, HomexG, AwayxG)
#' @param schedule Schedule data frame (must contain Date, HomeTeam, AwayTeam, GameID)
#' @param min_games_for_xg_weight Minimum historical games required to fit xG blending weight
#' @param maxgoal max goals to use in dcProbMatrix
#'
#' @return A list with metrics (logloss & accuracy for each method) and per-game predictions
#' @export
backtest_xg_blend <- function(
  startDate = as.Date(getSeasonStartDate()),
  endDate = Sys.Date(),
  scores = HockeyModel::scores,
  schedule = HockeyModel::schedule,
  min_games_for_xg_weight = 30,
  maxgoal = 10
) {
  stopifnot(is.Date(startDate))
  stopifnot(is.Date(endDate))

  all_dates <- seq.Date(from = as.Date(startDate), to = as.Date(endDate), by = "day")

  preds <- data.frame(
    Date = as.Date(character()),
    GameID = numeric(),
    HomeTeam = character(),
    AwayTeam = character(),
    Actual = numeric(),
    Pred_Goal = numeric(),
    Pred_xG = numeric(),
    Pred_Blend = numeric(),
    stringsAsFactors = FALSE
  )

  for (d in all_dates) {
    # Historical data up to, but not including d
    hist_scores <- scores[scores$Date < d, ]
    if (nrow(hist_scores) < 5) next

    # limit to recent history similarly to getM
    hist_scores <- hist_scores[hist_scores$Date > (d - 4000), ]

    params <- updateDC(scores = hist_scores, currentDate = d, save_data = FALSE, min_games_for_xg_weight = min_games_for_xg_weight)
    w <- 0
    if (!is.null(params$xg_weight)) w <- params$xg_weight

    # games on date d
    todays <- schedule[schedule$Date == d, ]
    if (nrow(todays) == 0) next

    for (i in seq_len(nrow(todays))) {
      g <- todays[i, ]
      home <- g$HomeTeam
      away <- g$AwayTeam
      gid <- g$GameID

      pg <- tryCatch(DCPredict(home, away, params = params, maxgoal = maxgoal, scores = hist_scores, use_xg = FALSE, xg_weight = 0), error = function(e) NULL)
      pxg <- tryCatch(DCPredict(home, away, params = params, maxgoal = maxgoal, scores = hist_scores, use_xg = TRUE, xg_weight = 1), error = function(e) NULL)
      pbl <- tryCatch(DCPredict(home, away, params = params, maxgoal = maxgoal, scores = hist_scores, xg_weight = w), error = function(e) NULL)

      # helper to compute Home.WL probability from odds vector
      homeWL <- function(p) {
        if (is.null(p) || any(is.na(p))) return(NA_real_)
        ph <- p[1]; pd <- p[2]; pa <- p[3]
        denom <- ph + pa
        if (!is.finite(denom) || denom <= 0) {
          return(ph)
        } else {
          return((ph / denom) * pd + ph)
        }
      }

      actual_row <- scores[scores$GameID == gid, ]
      actual_val <- NA
      if (nrow(actual_row) > 0) actual_val <- as.numeric(actual_row$Result[1])

      preds <- rbind(preds, data.frame(
        Date = as.Date(d),
        GameID = gid,
        HomeTeam = home,
        AwayTeam = away,
        Actual = actual_val,
        Pred_Goal = homeWL(pg),
        Pred_xG = homeWL(pxg),
        Pred_Blend = homeWL(pbl),
        stringsAsFactors = FALSE
      ))
    }
  }

  # drop missing actuals
  preds <- preds[!is.na(preds$Actual), ]
  if (nrow(preds) == 0) return(list(metrics = NULL, predictions = preds))

  actual_bin <- as.numeric(preds$Actual > 0.5)

  metrics <- list(
    LogLoss_Goal = logLoss(preds$Pred_Goal, actual_bin),
    LogLoss_xG = logLoss(preds$Pred_xG, actual_bin),
    LogLoss_Blend = logLoss(preds$Pred_Blend, actual_bin),
    Accuracy_Goal = accuracy(preds$Pred_Goal, actual_bin),
    Accuracy_xG = accuracy(preds$Pred_xG, actual_bin),
    Accuracy_Blend = accuracy(preds$Pred_Blend, actual_bin)
  )

  return(list(metrics = metrics, predictions = preds))
}
