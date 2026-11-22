# NHL league standings/points table construction

#' Rank teams by the NHL standings tiebreakers
#'
#' @description Orders `nrow(df)` teams using the official NHL criteria —
#'   points, point percentage (points / games played), wins, regulation wins
#'   (`ROW`), and regulation/OT/SO wins (`ROSW`) — with a random draw as the
#'   final tie-break so exact ties get a stable-per-run order. Shared by
#'   `buildStats()` and `sim_engine()` so both simulation engines rank teams
#'   identically (#71).
#'
#' @param df (`data.frame`) One row per team, with columns `Points`, `GP`,
#'   `W`, `OTW`, and `SOW`.
#' @return (`integer`) A vector of ranks, one per row, where 1 is best.
#' @keywords internal
.rank_by_tiebreakers <- function(df) {
  points <- df$Points
  point_percent <- points / df$GP
  w <- df$W
  row_wins <- w + df$OTW
  rosow <- w + df$OTW + df$SOW
  order(order(
    -points,
    -point_percent,
    -w,
    -row_wins,
    -rosow,
    # random draw only breaks exact ties, so tied teams don't always rank in input order
    stats::runif(nrow(df))
  ))
}

#' Bulid Stats Table
#'
#' @param scores scores to use to build stats table
#'
#' @return a stats table (as tibble)
#' @keywords internal
buildStats <- function(scores) {
  scores <- droplevels(scores)
  teamlist <- sort(unique(c(
    as.character(scores$HomeTeam),
    as.character(scores$AwayTeam)
  )))

  # remainderSeasonDC(nsims=10, cores=1, scores=scor, schedule = sched, regress = TRUE) testing passes results instead of home & Away goals
  if ("HomeGoals" %in% colnames(scores)) {
    tmp1 <- scores |>
      dplyr::group_by(.data$HomeTeam) |>
      dplyr::summarise(
        GP = dplyr::n(),
        W = sum(.data$HomeGoals > .data$AwayGoals & .data$OTStatus == ""),
        OTW = sum(.data$HomeGoals > .data$AwayGoals & .data$OTStatus == "OT"),
        SOW = sum(.data$HomeGoals > .data$AwayGoals & .data$OTStatus == "SO"),
        OTL = sum(.data$HomeGoals < .data$AwayGoals & .data$OTStatus == "OT"),
        SOL = sum(.data$HomeGoals < .data$AwayGoals & .data$OTStatus == "SO"),
        L = sum(.data$HomeGoals < .data$AwayGoals & .data$OTStatus == ""),
        P = as.numeric(
          .data$W * 2 + .data$OTW * 2 + .data$SOW * 2 + .data$OTL + .data$SOL
        )
      ) |>
      dplyr::ungroup()
    tmp2 <- scores |>
      dplyr::group_by(.data$AwayTeam) |>
      dplyr::summarise(
        GP = dplyr::n(),
        W = sum(.data$AwayGoals > .data$HomeGoals & .data$OTStatus == ""),
        OTW = sum(.data$AwayGoals > .data$HomeGoals & .data$OTStatus == "OT"),
        SOW = sum(.data$AwayGoals > .data$HomeGoals & .data$OTStatus == "SO"),
        OTL = sum(.data$AwayGoals < .data$HomeGoals & .data$OTStatus == "OT"),
        SOL = sum(.data$AwayGoals < .data$HomeGoals & .data$OTStatus == "SO"),
        L = sum(.data$AwayGoals < .data$HomeGoals & .data$OTStatus == ""),
        P = as.numeric(
          .data$W * 2 + .data$OTW * 2 + .data$SOW * 2 + .data$OTL + .data$SOL
        )
      ) |>
      dplyr::ungroup()
  } else if ("Result" %in% colnames(scores)) {
    tmp1 <- scores |>
      dplyr::group_by(.data$HomeTeam) |>
      dplyr::summarise(
        GP = dplyr::n(),
        W = sum(.data$Result == 1),
        OTW = sum(.data$Result == 0.75),
        SOW = sum(.data$Result == 0.60),
        SOL = sum(.data$Result == 0.40),
        OTL = sum(.data$Result == 0.25),
        L = sum(.data$Result == 0),
        P = as.numeric(
          .data$W * 2 + .data$OTW * 2 + .data$SOW * 2 + .data$OTL + .data$SOL
        )
      ) |>
      dplyr::ungroup()
    tmp2 <- scores |>
      dplyr::group_by(.data$AwayTeam) |>
      dplyr::summarise(
        GP = dplyr::n(),
        W = sum(.data$Result == 0),
        OTW = sum(.data$Result == 0.25),
        SOW = sum(.data$Result == 0.40),
        SOL = sum(.data$Result == 0.60),
        OTL = sum(.data$Result == 0.75),
        L = sum(.data$Result == 1),
        P = as.numeric(
          .data$W * 2 + .data$OTW * 2 + .data$SOW * 2 + .data$OTL + .data$SOL
        )
      ) |>
      dplyr::ungroup()
  } else {
    stop("Scores must contain home & away goal info or result info.")
  }

  team_stats <- data.frame(
    Team = teamlist,
    GP = tmp1$GP + tmp2$GP,
    Points = tmp1$P + tmp2$P,
    W = tmp1$W + tmp2$W,
    L = tmp1$L + tmp2$L,
    OTL = tmp1$OTL + tmp2$OTL,
    OTW = tmp1$OTW + tmp2$OTW,
    SOL = tmp1$SOL + tmp2$SOL,
    SOW = tmp1$SOW + tmp2$SOW,
    stringsAsFactors = FALSE
  )

  team_stats <- team_stats |>
    dtplyr::lazy_dt() |>
    dplyr::mutate(
      Rank = .rank_by_tiebreakers(tibble::tibble(
        Points = .data$Points,
        GP = .data$GP,
        W = .data$W,
        OTW = .data$OTW,
        SOW = .data$SOW
      )), # shared NHL tiebreakers; random only breaks exact ties (#71)
      Conf = .getTeamConferences(.data$Team), # convenience data, dropped later
      Div = .getTeamDivisions(.data$Team)
    ) |>
    dplyr::group_by(.data$Conf) |>
    dplyr::mutate(ConfRank = rank(.data$Rank, ties.method = "random")) |>
    dplyr::ungroup() |>
    dplyr::group_by(.data$Div) |>
    dplyr::mutate(DivRank = rank(.data$Rank, ties.method = "random")) |>
    dplyr::ungroup() |>
    dplyr::mutate(Playoffs = ifelse(.data$DivRank <= 3, 1, 0)) |>
    dplyr::group_by(.data$Conf, .data$Playoffs) |>
    dplyr::mutate(
      Playoffs = ifelse(
        .data$Rank %in% utils::head(sort(.data$Rank), 2),
        1,
        .data$Playoffs
      )
    ) |> # Mark the two best-ranked non-divisional teams per conference as wildcards (#70)
    dplyr::ungroup() |>
    dplyr::select(-"Conf", -"Div") |>
    tibble::as_tibble()

  return(team_stats)
}
