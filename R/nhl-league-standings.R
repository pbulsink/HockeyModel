# NHL league standings/points table construction

#' Rank teams by the NHL standings tiebreakers
#'
#' @description Orders `nrow(df)` teams using the official criteria — points,
#'   point percentage (points / games played), regulation wins (`ROW`),
#'   regulation/OT/SO wins (`ROSW`), and head-to-head points among tied clubs
#'   (#94) — with a random draw as the final tie-break so exact ties get a
#'   stable-per-run order. Shared by `buildStats()` and `sim_engine()` so both
#'   simulation engines rank teams identically (#71); the PWHL engine uses it
#'   too, since the PWHL procedure follows the same criterion order.
#'
#' @param df (`data.frame`) One row per team, with columns `Points`, `GP`,
#'   `W` (regulation wins), `OTW`, and `SOW`. A `Team` column is also required
#'   when head-to-head points must be computed from `games`.
#' @param games (`data.frame` or `NULL`) One row per game between teams in
#'   `df`, with columns `HomeTeam`, `AwayTeam`, `Date`, and the points each
#'   side earned in that game under the relevant league's scoring system
#'   (`home_points`, `away_points`). When supplied, head-to-head points are
#'   used to separate teams tied on every other criterion: for a two-way tie
#'   this is points earned against each other (excluding the first meeting at
#'   the venue that hosted the extra game when the number of meetings is odd),
#'   and for larger ties the percentage of available points among themselves.
#' @param h2h (`numeric` or `NULL`) Pre-computed head-to-head values, one per
#'   row of `df`. Used as-is in place of `games`; supplied by `sim_engine()`,
#'   which sums per-game result vectors directly to avoid materializing a
#'   games-by-sims table. Ignored when `NULL`.
#' @return (`integer`) A vector of ranks, one per row, where 1 is best.
#' @keywords internal
.rank_by_tiebreakers <- function(df, games = NULL, h2h = NULL) {
  n <- nrow(df)
  points <- df$Points
  point_percent <- points / df$GP
  w <- df$W
  row_wins <- w + df$OTW
  rosow <- w + df$OTW + df$SOW

  # H2H only separates teams tied on every criterion above, so the group key
  # (the five criteria) partitions the teams into independent tie groups. A
  # factor over all rows is used so each row keeps its own index even when
  # several teams share an identical key.
  if (is.null(h2h)) {
    h2h <- rep(0, n)
  }
  if (!is.null(games)) {
    grp <- factor(paste(points, df$GP, w, row_wins, rosow, sep = "\r"))
    gi <- match(games$HomeTeam, df$Team)
    gj <- match(games$AwayTeam, df$Team)
    for (g in split(seq_len(n), grp)) {
      if (length(g) < 2L) {
        next
      }
      members <- df$Team[g]

      # Games played between pairs of clubs inside this tie group.
      idx <- seq_len(nrow(games))[
        vapply(
          seq_len(nrow(games)),
          function(k) {
            games$HomeTeam[k] %in% members && games$AwayTeam[k] %in% members
          },
          logical(1)
        )
      ]

      earned <- rep(0, length(g)) # H2H points per member so far
      # Outer stops at length-1: seq(n+1, n) returns c(n+1, n), not empty.
      for (ti in seq_len(length(members) - 1L)) {
        for (tj in seq(ti + 1L, length(members))) {
          pi <- g[ti]
          pj <- g[tj]
          pair_idx <- idx[(gi == pi & gj == pj) | (gi == pj & gj == pi)]
          if (length(pair_idx) == 0L) {
            next
          }

          # Odd number of meetings: drop the first game played in the city
          # that hosted the extra meeting (NHL/PWHL procedure). A single
          # meeting is dropped entirely, leaving both sides at 0 H2H points.
          if (length(pair_idx) %% 2L == 1L) {
            venue <- games$HomeTeam[pair_idx] # a game's "city" is its home team's
            extra_city <- names(which.max(table(venue)))
            first_at_extra <- pair_idx[venue == extra_city]
            if (length(first_at_extra) > 0L) {
              pair_idx <- setdiff(pair_idx, min(first_at_extra))
            }
          }

          for (k in pair_idx) {
            h <- games$HomeTeam[k]
            earned[ti] <- earned[ti] +
              ifelse(
                h == members[ti],
                games$home_points[k],
                games$away_points[k]
              )
            earned[tj] <- earned[tj] +
              ifelse(
                h == members[ti],
                games$away_points[k],
                games$home_points[k]
              )
          }
        }
      }

      # Percentage of available points among themselves for multi-way ties;
      # two-way groups compare raw points directly (equal denominators). The
      # denominator is the actual points on offer in each meeting, so this is
      # correct under any scoring system.
      if (length(g) > 2L) {
        available <- rep(0, length(g))
        for (k in idx) {
          a <- match(games$HomeTeam[k], members)
          b <- match(games$AwayTeam[k], members)
          total_k <- games$home_points[k] + games$away_points[k]
          available[a] <- available[a] + total_k
          available[b] <- available[b] + total_k
        }
        earned[available > 0] <- earned[available > 0] /
          available[available > 0]
      }

      h2h[g] <- earned
    }
  }

  order(order(
    -points,
    -point_percent,
    -w,
    -row_wins,
    -rosow,
    -h2h,
    # random draw only breaks exact ties, so tied teams don't always rank in input order
    stats::runif(n)
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

  # Per-game points for the head-to-head tiebreaker (#94); NHL games always
  # award 2 points in total, so the away side's share is the complement.
  if ("HomeGoals" %in% colnames(scores)) {
    home_points <- ifelse(
      scores$HomeGoals > scores$AwayGoals,
      2L,
      ifelse(scores$OTStatus != "", 1L, 0L)
    )
  } else {
    home_points <- ifelse(
      scores$Result >= 0.6,
      2L,
      ifelse(scores$Result > 0, 1L, 0L)
    )
  }
  # Odd-meeting exclusion orders games chronologically; row order is the time
  # proxy when no dates are supplied (synthetic fixtures).
  game_date <- if (!is.null(scores$Date)) scores$Date else seq_len(nrow(scores))
  h2h_games <- data.frame(
    HomeTeam = as.character(scores$HomeTeam),
    AwayTeam = as.character(scores$AwayTeam),
    Date = game_date,
    home_points = home_points,
    away_points = 2L - home_points
  )

  # Ranked before the lazy pipeline: the tiebreaker reads whole columns and
  # returns a per-row vector, which dtplyr's scalar mutate can't recycle.
  team_stats$Rank <- .rank_by_tiebreakers(team_stats, games = h2h_games)

  team_stats <- team_stats |>
    dtplyr::lazy_dt() |>
    dplyr::mutate(
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
