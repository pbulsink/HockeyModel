# Graphics: team/division pace-vs-prediction plots

#' Plot Pace By Division
#'
#' @param graphic_dir Graphics Directory
#' @param subdir Subdirectory for pace graphics
#' @param prediction_dir Directory for predictions
#' @param scores HockeyModel::scores
#'
#' @return ggplot graphic of team pace vs. predicted
#' @export
plot_pace_by_division <- function(
  graphic_dir = getOption("HockeyModel.graphics.path"),
  subdir = "pace",
  prediction_dir = getOption("HockeyModel.prediction.path"),
  scores = HockeyModel::scores
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg ggplot2} is required. Install it with {.code install.packages('ggplot2')}."
    )
  }
  sc <- scores[scores$Date >= as.Date(getSeasonStartDate()), ]
  sc <- sc[sc$GameType == "R", ]

  # Get old predictions
  p <- readRDS(file.path(
    prediction_dir,
    paste0(getSeasonStartDate(), "-predictions.RDS")
  ))

  if (!dir.exists(file.path(graphic_dir, subdir))) {
    dir.create(file.path(graphic_dir, subdir), recursive = TRUE)
  }

  ngames <- getNumGames()

  teamlist <- unique(c(as.character(sc$HomeTeam), as.character(sc$AwayTeam)))

  teampoints <- as.list(rep(NA, length(teamlist)))
  names(teampoints) <- teamlist

  teamPerformance <- data.frame("GameNum" = 0:ngames)

  for (team in teamlist) {
    teamscores <- sc[
      sc$HomeTeam == team | sc$AwayTeam == team,
      c("AwayTeam", "HomeTeam", "Result", "GameID")
    ]
    teamscores[teamscores$AwayTeam == team, "Result"] <- 1 -
      teamscores[teamscores$AwayTeam == team, "Result"]
    teamscores$Venue <- "Home"
    teamscores[teamscores$AwayTeam == team, "Venue"] <- "Away"
    teamscores$Points <- ceiling(2 * teamscores$Result)
    teamscores$cPoints <- cumsum(teamscores$Points)
    teamscores$GameNum <- seq_len(nrow(teamscores))
    teamscores$xPoints <- teamscores$GameNum *
      (p[p$Team == team, ]$meanPoints / ngames)
    teamscores$xDiff <- teamscores$cPoints - teamscores$xPoints
    teampoints[[team]] <- max(teamscores$cPoints)
    teamscores <- teamscores[, c("GameNum", "xDiff")]
    teamscores <- tibble::add_row(teamscores, "GameNum" = 0, "xDiff" = 0)
    names(teamscores) <- c("GameNum", team)
    teamPerformance <- dplyr::left_join(
      teamPerformance,
      teamscores,
      by = "GameNum"
    )
  }

  games_played <- max(which(
    rowSums(teamPerformance[, 2:ncol(teamPerformance)], na.rm = TRUE) != 0
  ))

  teamPerformance <- teamPerformance[
    teamPerformance$GameNum <= (games_played + 1),
  ]

  teamPerformance <- tidyr::pivot_longer(
    teamPerformance,
    !.data$GameNum,
    names_to = "Team",
    values_to = "PointDiff"
  )

  teamColours <- HockeyModel::teamColours
  # Build and trim team colours for plot
  teamColoursList <- as.vector(teamColours$Hex)
  names(teamColoursList) <- teamColours$Team
  teamColoursList <- teamColoursList[names(teamColoursList) %in% teamlist]

  teamPerformance$label <- NA_character_

  for (team in teamlist) {
    pointdiff <- teamPerformance[
      teamPerformance$Team == team &
        teamPerformance$GameNum ==
          max(teamPerformance[
            teamPerformance$Team == team & !is.na(teamPerformance$PointDiff),
            "GameNum"
          ]),
      "PointDiff"
    ]
    lab <- paste0(
      getShortTeam(team),
      " - ",
      teampoints[team],
      " pts. (",
      ifelse(pointdiff > 0, "+", ""),
      round(pointdiff, 1),
      ")"
    )
    teamPerformance[
      teamPerformance$Team == team &
        teamPerformance$GameNum ==
          max(teamPerformance[
            teamPerformance$Team == team & !is.na(teamPerformance$PointDiff),
            "GameNum"
          ]),
      "label"
    ] <- lab
  }

  teamPerformance$Div <- getTeamDivisions(teamPerformance$Team)

  for (division in unique(teamPerformance$Div)) {
    # tl<-teamlist[teamlist %in% unlist(HockeyModel::nhl_divisions[division])]
    tp <- teamPerformance[teamPerformance$Div == division, ]

    plt <- ggplot2::ggplot(
      tp,
      ggplot2::aes(x = .data$GameNum, y = .data$PointDiff, colour = .data$Team)
    ) +
      # ggplot2::geom_line(na.rm = TRUE) +
      ggplot2::geom_smooth(span = 0.2, na.rm = TRUE, se = FALSE) +
      ggplot2::geom_hline(yintercept = 0) +
      ggplot2::coord_cartesian(
        xlim = c(0, max(teamPerformance$GameNum, 12)),
        clip = "off"
      ) +
      ggplot2::labs(
        title = "Points vs. Predicted at Season Start",
        subtitle = paste(division, "Division Teams"),
        x = "Game Number",
        y = "Points Above/Below Predicted",
        caption = paste0("P. Bulsink (@bot.bulsink.ca) | ", Sys.Date())
      ) +
      ggplot2::scale_colour_manual(values = teamColoursList) +
      ggplot2::scale_x_continuous(
        breaks = seq(from = 0, to = max(teamPerformance$GameNum), by = 5)
      ) + # , expand = ggplot2::expansion(mult = c(0, .1)))+
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "none",
        plot.margin = ggplot2::unit(c(1, 7, 1, 1), "lines")
      )
    if (requireNamespace("ggrepel", quietly = TRUE)) {
      plt <- plt +
        ggrepel::geom_label_repel(
          ggplot2::aes(label = .data$label),
          direction = "y",
          na.rm = TRUE,
          segment.alpha = 0,
          hjust = 0.5,
          xlim = c(
            max(teamPerformance$GameNum, 12),
            max(teamPerformance$GameNum, 12) +
              max(teamPerformance$GameNum, 12) * .18
          )
        )
    }

    grDevices::png(
      filename = file.path(
        graphic_dir,
        subdir,
        paste0(tolower(gsub(" ", "_", division)), "_pace.png")
      ),
      width = 11,
      height = 8.5,
      units = "in",
      res = 300
    )
    print(plt)
    while (grDevices::dev.cur() != 1) {
      grDevices::dev.off()
    }
  }
  return(plt)
}


#' Plot Pace by team
#'
#' @param graphic_dir The graphics directory
#' @param subdir The pace subdirectory in graphics
#' @param prediction_dir The predictions directory
#' @param scores The HockeyModel::scores object, or custom scores in the same format.
#'
#' @export
plot_pace_by_team <- function(
  graphic_dir = getOption("HockeyModel.graphics.path"),
  subdir = "pace",
  prediction_dir = getOption("HockeyModel.prediction.path"),
  scores = HockeyModel::scores
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg ggplot2} is required. Install it with {.code install.packages('ggplot2')}."
    )
  }
  sc <- scores[scores$Date >= as.Date(getSeasonStartDate()), ]

  teamlist <- unique(c(as.character(sc$HomeTeam), as.character(sc$AwayTeam)))

  # Get old and most recent predictions
  p <- readRDS(file.path(
    prediction_dir,
    paste0(getSeasonStartDate(), "-predictions.RDS")
  ))

  pdates <- get_prediction_dates(prediction_dir)
  if (length(pdates) == 0L) {
    cli::cli_abort("No prediction files found in {.path {prediction_dir}}.")
  }
  lastp <- max(pdates)
  q <- readRDS(file.path(prediction_dir, paste0(lastp, "-predictions.RDS")))

  if (!dir.exists(file.path(graphic_dir, subdir))) {
    dir.create(file.path(graphic_dir, subdir), recursive = TRUE)
  }
  numgames <- getNumGames()

  teamColours <- HockeyModel::teamColours

  for (team in teamlist) {
    colour <- teamColours[teamColours$Team == team, "Hex"]
    teamscores <- sc[
      sc$HomeTeam == team | sc$AwayTeam == team,
      c("AwayTeam", "HomeTeam", "Result")
    ]
    teamscores[teamscores$AwayTeam == team, "Result"] <- 1 -
      teamscores[teamscores$AwayTeam == team, "Result"]
    teamscores$Venue <- "Home"
    teamscores[teamscores$AwayTeam == team, "Venue"] <- "Away"
    teamscores$Points <- ceiling(2 * teamscores$Result)
    teamscores$cPoints <- cumsum(teamscores$Points)
    teamscores$GameNum <- seq_len(nrow(teamscores))
    ngames <- nrow(teamscores)
    cp <- utils::tail(teamscores$cPoints, 1)

    pteam <- p[p$Team == team, ]
    ppoints <- pteam$meanPoints
    maxp <- pteam$meanPoints + 2 * (pteam$sdPoints)
    minp <- pteam$meanPoints - 2 * (pteam$sdPoints)
    qteam <- q[q$Team == team, ]
    qpoints <- qteam$meanPoints
    maxq <- qteam$meanPoints + 2 * (qteam$sdPoints)
    minq <- qteam$meanPoints - 2 * (qteam$sdPoints)

    plt <- ggplot2::ggplot(
      teamscores,
      ggplot2::aes(
        x = .data$GameNum,
        y = .data$cPoints,
        colour = .data$Venue
      )
    ) +
      ggplot2::geom_point() +
      ggplot2::scale_x_continuous(limits = c(0, numgames)) +
      ggplot2::scale_y_continuous(limits = c(0, numgames * 2)) +
      ggplot2::labs(
        x = "Game Number",
        y = "Points",
        title = "Points Pace",
        subtitle = paste0(
          team,
          " Expected Points: ",
          format(round(qpoints, 1), nsmall = 1)
        ),
        caption = paste0("P. Bulsink (@bot.bulsink.ca) | ", Sys.Date())
      ) +
      ggplot2::theme_minimal() +
      ggplot2::geom_segment(
        x = 0,
        y = 0,
        xend = numgames,
        yend = ppoints,
        alpha = 0.2,
        colour = "grey"
      ) +
      ggplot2::geom_segment(
        x = 0,
        y = 0,
        xend = numgames,
        yend = maxp,
        alpha = 0.2,
        colour = "grey"
      ) +
      ggplot2::geom_segment(
        x = 0,
        y = 0,
        xend = numgames,
        yend = minp,
        alpha = 0.2,
        colour = "grey"
      ) +
      ggplot2::geom_segment(
        x = ngames,
        y = cp,
        xend = numgames,
        yend = qpoints,
        alpha = 0.2,
        colour = colour
      ) +
      ggplot2::geom_segment(
        x = ngames,
        y = cp,
        xend = numgames,
        yend = maxq,
        alpha = 0.2,
        colour = colour
      ) +
      ggplot2::geom_segment(
        x = ngames,
        y = cp,
        xend = numgames,
        yend = minq,
        alpha = 0.2,
        colour = colour
      )

    grDevices::png(
      filename = file.path(
        graphic_dir,
        subdir,
        paste0(tolower(gsub(" ", "_", team)), ".png")
      ),
      width = 11,
      height = 8.5,
      units = "in",
      res = 300
    )
    print(plt)
    while (grDevices::dev.cur() != 1) {
      grDevices::dev.off()
    }
  }
}
