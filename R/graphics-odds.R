# Graphics: game and playoff series odds plots

#' Plot Today's Odds
#'
#' @param today The day's odds to plot. Default today.
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#' @param schedule HockeyModel::schedule or a custom value
#' @param teamColours HockeyModel::teamColours or a custom value
#' @param league League identifier. `"NHL"` (default) or `"PWHL"`. When
#'   `"PWHL"`, the CSV file is not written and PWHL-specific labels are used.
#'
#' @return a ggplot image of odds
#'
#' @export
plot_odds_today <- function(
  today = Sys.Date(),
  params = NULL,
  schedule = HockeyModel::schedule,
  teamColours = HockeyModel::teamColours,
  league = "NHL"
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg ggplot2} is required. Install it with {.code install.packages('ggplot2')}."
    )
  }
  params <- parse_dc_params(params)
  todayodds <- todayDC(today = today, params, schedule = schedule)
  if (is.null(todayodds)) {
    return(NULL)
  }
  todayodds$HomeWinOT <- todayodds$AwayWinOT <- 0

  # add odds for each team in OT/SO
  for (g in seq_len(nrow(todayodds))) {
    todayodds$HomeWinOT[g] <- extraTimeSolver(
      home_win = todayodds$HomeWin[g],
      away_win = todayodds$AwayWin[g],
      draw = todayodds$Draw[g]
    )[2]
    todayodds$AwayWinOT[g] <- extraTimeSolver(
      home_win = todayodds$HomeWin[g],
      away_win = todayodds$AwayWin[g],
      draw = todayodds$Draw[g]
    )[3]
  }

  if (nrow(todayodds) > 0) {
    todayodds$GameID <- as.numeric(todayodds$GameID)
    if (league != "PWHL") {
      utils::write.csv(
        todayodds,
        file = paste0("./", getCurrentSeason8(), ".csv"),
        row.names = FALSE,
        append = TRUE
      )
    }
  }
  todayodds$GameID <- NULL
  # Melt data to work with ggplot
  # melted<-reshape2::melt(todayodds, id.vars = c('HomeTeam', 'AwayTeam'))
  melted <- tidyr::pivot_longer(
    todayodds,
    cols = c("HomeWin", "AwayWin", "HomeWinOT", "AwayWinOT", "Draw"),
    names_to = "variable",
    values_to = "value"
  )
  melted$variable <- factor(
    x = melted$variable,
    levels = c("AwayWin", "AwayWinOT", "Draw", "HomeWinOT", "HomeWin"),
    ordered = TRUE
  )
  melted <- melted[melted$variable != "Draw", ]

  if (league == "PWHL") {
    teamColours <- HockeyModel::pwhlTeamColours
  }

  melted$alpha <- 1
  melted$colour <- ""
  for (i in seq_len(nrow(melted))) {
    melted[i, ]$alpha <- ifelse(
      melted[i, ]$variable %in% c("HomeWin", "AwayWin"),
      yes = 1,
      no = 0.7
    )
    tc <- getTeamColours(
      home = melted[i, ]$HomeTeam,
      away = melted[i, ]$AwayTeam,
      teamColours = teamColours
    )
    melted[i, ]$colour <- ifelse(
      melted[i, ]$variable %in% c("HomeWin", "HomeWinOT"),
      yes = tc$home,
      no = tc$away
    )
  }

  # Prepare instructions to read
  text_home <- grid::textGrob(
    "Home Win",
    gp = grid::gpar(fontsize = 10),
    hjust = 0
  )
  text_away <- grid::textGrob(
    "Away Win",
    gp = grid::gpar(fontsize = 10),
    hjust = 1
  )
  otlabel.y <- todayodds[nrow(todayodds), "HomeWin"] +
    todayodds[nrow(todayodds), "Draw"] / 2
  text_ot <- grid::textGrob(
    "OT/SO Decision",
    gp = grid::gpar(fontsize = 10),
    hjust = 0.5
  )

  # build plot
  # p<-ggplot2::ggplot(melted[melted$variable %in% c('HomeWin','HomeWinOT', 'AwayWinOT', 'AwayWin'),],
  p <- ggplot2::ggplot(
    melted,
    ggplot2::aes(
      y = .data$value,
      x = .data$HomeTeam,
      group = .data$variable
    )
  ) +
    ggplot2::geom_bar(
      stat = "identity",
      position = "fill",
      fill = melted$colour,
      alpha = melted$alpha,
      colour = "white"
    ) +
    ggplot2::labs(
      x = "",
      y = "Result Odds",
      title = if (league == "PWHL") {
        "Predictions for Today's PWHL Games"
      } else {
        "Predictions for Today's NHL Games"
      },
      subtitle = paste0("Games played on ", Sys.Date()),
      caption = paste0("P. Bulsink (@bot.bulsink.ca) | ", Sys.Date())
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white"),
      panel.border = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.margin = ggplot2::unit(c(2, 1, 1, 1), "lines")
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(add = 0.3),
      breaks = c(0, 0.5, 1)
    ) +
    ggplot2::annotate(
      "text",
      x = todayodds$HomeTeam,
      y = -.01,
      hjust = 1,
      label = todayodds$HomeTeam
    ) +
    ggplot2::annotate(
      "text",
      x = todayodds$HomeTeam,
      y = 1.01,
      hjust = 0,
      label = todayodds$AwayTeam
    ) +
    ggplot2::annotate(
      "label",
      x = todayodds$HomeTeam,
      y = 0.01,
      hjust = 0,
      label = format(round(todayodds$HomeWin, 3), nsmall = 3)
    ) +
    ggplot2::annotate(
      "label",
      x = todayodds$HomeTeam,
      y = .99,
      hjust = 1,
      label = format(round(todayodds$AwayWin, 3), nsmall = 3)
    ) +
    ggplot2::annotate(
      "label",
      x = todayodds$HomeTeam,
      y = todayodds$HomeWin + todayodds$HomeWinOT - 0.01,
      hjust = 1,
      label = format(round(todayodds$HomeWinOT, 3), nsmall = 3)
    ) +
    ggplot2::annotate(
      "label",
      x = todayodds$HomeTeam,
      y = todayodds$HomeWin + todayodds$HomeWinOT + 0.01,
      hjust = 0,
      label = format(round(todayodds$AwayWinOT, 3), nsmall = 3)
    ) +
    ggplot2::coord_flip()

  return(p)
}


#' Plot Today's Playoff Series Odds
#'
#' @param series A data frame of home team, away team, home wins, away wins
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#' @param teamColours HockeyModel::teamColours or a custom value
#' @param league League identifier. `"NHL"` (default) or `"PWHL"`. When
#'   `"PWHL"`, PWHL-specific labels are used.
#'
#' @return a ggplot image of odds
#'
#' @export
plot_playoff_series_odds <- function(
  series = getAPISeries(),
  params = NULL,
  teamColours = HockeyModel::teamColours,
  league = "NHL"
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg ggplot2} is required. Install it with {.code install.packages('ggplot2')}."
    )
  }
  params <- parse_dc_params(params)
  series <- series[, c("HomeTeam", "AwayTeam", "HomeWins", "AwayWins")]
  series$HomeOdds <- purrr::pmap_dbl(
    series,
    function(HomeTeam, AwayTeam, HomeWins, AwayWins, ...) {
      playoffWin(HomeTeam, AwayTeam, HomeWins, AwayWins, params = params)
    }
  )
  series$AwayOdds <- 1 - series$HomeOdds
  series2 <- series
  # For now, drop won games:
  series2$HomeWins <- series2$AwayWins <- NULL

  # Melt data to work with ggplot
  # melted<-reshape2::melt(series2, id.vars = c('HomeTeam', 'AwayTeam'))
  melted <- tidyr::pivot_longer(
    series2,
    cols = c("HomeOdds", "AwayOdds"),
    names_to = "variable",
    values_to = "value"
  )
  melted$variable <- factor(
    x = melted$variable,
    levels = c("AwayOdds", "HomeOdds"),
    ordered = TRUE
  )
  # melted$HomeTeam <- factor(x = melted$HomeTeam, levels = melted$HomeTeam[1:(length(melted$HomeTeam)/2)], ordered = TRUE)
  melted$colour <- ""

  for (i in seq_len(nrow(melted))) {
    tc <- getTeamColours(
      home = melted[i, ]$HomeTeam,
      away = melted[i, ]$AwayTeam,
      teamColours = teamColours
    )
    melted[i, ]$colour <- ifelse(
      melted[i, ]$variable == "HomeOdds",
      yes = tc$home,
      no = tc$away
    )
  }

  # build plot
  p <- ggplot2::ggplot(
    melted,
    ggplot2::aes(
      y = .data$value,
      x = .data$HomeTeam,
      group = .data$variable
    )
  ) +
    ggplot2::geom_bar(
      stat = "identity",
      position = "fill",
      fill = melted$colour,
      colour = "white"
    ) +
    ggplot2::labs(
      x = "",
      y = "Series Odds",
      title = if (league == "PWHL") {
        "Predictions for PWHL Playoff Series"
      } else {
        "Predictions for NHL Playoff Series"
      },
      subtitle = paste0(
        "Before Games on ",
        Sys.Date(),
        ". Number of wins in brackets."
      ),
      caption = paste0("P. Bulsink (@bot.bulsink.ca) | ", Sys.Date())
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white"),
      panel.border = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.margin = ggplot2::unit(c(2, 1, 1, 1), "lines")
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(add = 0.3),
      breaks = c(0, 0.5, 1)
    ) +
    ggplot2::annotate(
      "text",
      x = series$HomeTeam,
      y = -.01,
      hjust = 1,
      label = series$HomeTeam
    ) +
    ggplot2::annotate(
      "text",
      x = series$HomeTeam,
      y = 1.01,
      hjust = 0,
      label = series$AwayTeam
    ) +
    ggplot2::annotate(
      "label",
      x = series$HomeTeam,
      y = 0.01,
      hjust = 0,
      label = paste0(
        format(round(series$HomeOdds, 3), nsmall = 3),
        " (",
        series$HomeWins,
        ")"
      )
    ) +
    ggplot2::annotate(
      "label",
      x = series$HomeTeam,
      y = .99,
      hjust = 1,
      label = paste0(
        format(round(series$AwayOdds, 3), nsmall = 3),
        " (",
        series$AwayWins,
        ")"
      )
    ) +
    ggplot2::coord_flip()

  return(p)
}
