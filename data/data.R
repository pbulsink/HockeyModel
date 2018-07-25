#' Historical NHL and WHA scores.
#'
#' A dataset containing historical NHL and WHA scores.
#'
#' @format A data frame with  variables:
#' \describe{
#'   \item{Date}{Date of the game}
#'   \item{Visitor}{Name of visiting team}
#'   \item{VisitorGoals}{Number of goals for the visiting team}
#'   \item{Home}{Name of the home team}
#'   \item{HomeGoals}{Number of goals for the home team}
#'   \item{OTStatus}{The OT status of the game (none, OT, SO, 2OT, 3OT, ...)}
#'   \item{League}{NHL or WHA}
#'   \item{Tie}{Boolean, Tie or Not}
#'   \item{Winner}{Name of the winning team}
#'   \item{Loser}{Name of the losing team}
#'   \item{Result}{Coded home win = 0, visitor win = 1}
#'   ...
#' }
#' @source \url{http://www.hockey-reference.com/}
"scores"

#' Schedule for 2018-2019.
#'
#' A dataframe with the schedule (no scores included) for 2018-2019.
#'
#' @format A data frame with  variables:
#' \describe{
#'   \item{price}{price, in US dollars}
#'   \item{carat}{weight of the diamond, in carats}
#'   ...
#' }
#' @source \url{http://www.hockey-reference.com/}
"schedule"
