# Graphics: single game, point likelihood, and team rating plots

#' Plot single game expected goals
#'
#' @param home The Home Team
#' @param away The Away Team
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#' @param maxgoal the max number of goals to predict. Plot a few less.
#'
#' @return a ggplot object
#' @export
plot_game <- function(home, away, params = NULL, maxgoal = 10) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg ggplot2} is required. Install it with {.code install.packages('ggplot2')}."
    )
  }
  params <- parse_dc_params(params)
  # Expected goals home
  lambda <- try(
    stats::predict(
      params$m,
      data.frame(Home = 1, Team = home, Opponent = away),
      type = "response"
    ),
    TRUE
  )

  # Expected goals away
  mu <- try(
    stats::predict(
      params$m,
      data.frame(Home = 0, Team = away, Opponent = home),
      type = "response"
    ),
    TRUE
  )

  # fix errors
  if (!is.numeric(lambda)) {
    lambda <- DCPredictErrorRecover(
      team = home,
      opponent = away,
      homeiceadv = TRUE
    )
  }
  if (!is.numeric(mu)) {
    mu <- DCPredictErrorRecover(
      team = away,
      opponent = home,
      homeiceadv = FALSE
    )
  }

  probability_matrix <- dcProbMatrix(
    home = home,
    away = away,
    params = params,
    maxgoal = maxgoal
  )

  goals <- data.frame(Goals = c(0:maxgoal), Home = 0, Away = 0)

  goals$Away <- colSums(probability_matrix) *
    1 /
    sum(colSums(probability_matrix))
  goals$Home <- rowSums(probability_matrix) *
    1 /
    sum(rowSums(probability_matrix))

  # goals<-reshape2::melt(goals, id = "Goals", variable.name = "Team", value.name = "Density")
  goals <- tidyr::pivot_longer(
    goals,
    cols = c("Home", "Away"),
    names_to = "Team",
    values_to = "Density"
  )
  tc <- getTeamColours(home = home, away = away)
  plotcolors <- c(tc$home, tc$away)

  home_hjust <- 1 - (mu > lambda)

  odds <- DCPredict(home = home, away = away)

  p <- ggplot2::ggplot(
    data = goals,
    ggplot2::aes(x = .data$Goals, y = .data$Density, fill = .data$Team)
  ) +
    ggplot2::geom_area(position = "identity", alpha = 0.6) +
    ggplot2::geom_vline(xintercept = mu, linetype = "dashed") +
    ggplot2::geom_vline(xintercept = lambda, linetype = "dashed") +
    ggplot2::scale_x_continuous(limits = c(0, 8)) +
    ggplot2::scale_fill_manual(labels = c(home, away), values = plotcolors) +
    ggplot2::scale_color_manual(labels = c(home, away), values = plotcolors) +
    ggplot2::annotate(
      geom = "label",
      x = mu,
      y = 0.0,
      label = paste0(
        away,
        "\nPredicted Goals:",
        format(round(mu, 2), nsmall = 2)
      ),
      hjust = home_hjust,
      vjust = 0
    ) +
    ggplot2::annotate(
      geom = "label",
      x = lambda,
      y = 0.0,
      label = paste0(
        home,
        "\nPredicted Goals:",
        format(round(lambda, 2), nsmall = 2)
      ),
      hjust = 1 - home_hjust,
      vjust = 0
    ) +
    ggplot2::labs(
      x = "Predicted Team Goals",
      y = "Odds",
      title = "Predicted Goals",
      subtitle = paste0(
        away,
        " at ",
        home,
        " on ",
        Sys.Date(),
        "\nWin Odds - Away: ",
        format(round(odds[[3]], 3), nsmall = 3),
        " - Home: ",
        format(round(odds[[1]], 3), nsmall = 3),
        " - OT/SO: ",
        format(round(odds[[2]], 3), nsmall = 3)
      ),
      caption = paste0("P. Bulsink (@bot.bulsink.ca) | ", Sys.Date())
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.background = ggplot2::element_rect(
        fill = "white",
        colour = "white"
      ),
      legend.position = c(0.85, 0.85)
    )

  ## ALT:
  ## data <- tibble(x=c(0:9,0:9), team=c(rep("Home", 10), rep("Away", 10)))
  ## data$y <- NA
  ## data$y[1:10]<-dpois(0:9, 3.8)
  ## data$y[11:20]<-dpois(0:9, 2.7)
  ## ggplot()+
  ##   geom_col(data = data[1:10,], mapping = aes(x = x, y = y, fill=team), alpha = .3) +
  ##   geom_col(data = data[11:20,], mapping = aes(x = x, y = y, fill=team), alpha = .3) +
  ##   geom_vline(xintercept = 2.7) + geom_vline(xintercept = 3.8) +
  ##   annotate(geom="label", x = 2.7, y=0, label="Away\nxG=2.7") + annotate(geom = "label", x = 3.8, y = 0, label="Home\nxG=3.8") +
  ##   xlab("Goals") + ylab("Odds") + ggtitle("Expected Goals", subtitle = "Fake Game 2019-12-19") + scale_x_continuous(breaks = c(0:9))
  return(p)
}

#' Team Point Predict Plot
#'
#' @param preds Raw predictions to generate ggridges point likelyhood plot.
#' @param savefiles Whether to save files to disk
#' @param graphic_dir Directory to save plot images
#' @param subdir Subdirectory to save plot images
#'
#' @return plot(s) in a list, named for conference(s) in use at the time.
#' @export
plot_point_likelihood <- function(
  preds = NULL,
  graphic_dir = getOption("HockeyModel.graphics.path"),
  subdir = "pace",
  savefiles = TRUE
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg ggplot2} is required. Install it with {.code install.packages('ggplot2')}."
    )
  }
  if (!requireNamespace("ggridges", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg ggridges} is required. Install it with {.code install.packages('ggridges')}."
    )
  }
  if (is.null(preds)) {
    # Try this:
    preds <- loopless_sim(nsims = 1e4)$raw_results
  }

  preds$Conf <- getTeamConferences(preds$Team)

  conferences <- unique(preds$Conf)

  teamColours <- HockeyModel::teamColours
  teamColoursList <- as.vector(teamColours$Hex)
  names(teamColoursList) <- teamColours$Team

  p <- list()

  # sort the likelihood plots by points
  teamsorted <- preds |>
    dplyr::group_by(.data$Team) |>
    dplyr::summarise(mean.points = mean(.data$Points)) |>
    dplyr::arrange(.data$mean.points) |>
    dplyr::pull(.data$Team)

  preds$Team <- factor(preds$Team, levels = teamsorted)

  for (conf in conferences) {
    conf_preds <- preds[preds$Conf == conf, ]

    conf_colourslist <- teamColoursList[
      names(teamColoursList) %in% conf_preds$Team
    ]

    plot <- ggplot2::ggplot(
      conf_preds,
      ggplot2::aes(x = .data$Points, y = .data$Team, fill = .data$Team)
    ) +
      ggridges::geom_density_ridges(
        rel_min_height = 0.01,
        quantile_lines = TRUE,
        quantiles = 2,
        alpha = .6,
        from = 40,
        to = 130
      ) +
      ggplot2::scale_fill_manual(values = conf_colourslist) +
      ggplot2::labs(
        x = "Predicted Point Likelyhood",
        y = "",
        title = paste0(
          "Point Likelyhoods for ",
          conf,
          " Conference - ",
          getCurrentSeason8()
        ),
        caption = paste0("P. Bulsink (@bot.bulsink.ca) | ", Sys.Date())
      ) +
      ggridges::theme_ridges(grid = TRUE) +
      ggplot2::theme(
        legend.position = "none",
        panel.grid.major.y = ggplot2::element_line(size = .1, color = "grey")
      )

    p[[conf]] <- plot

    if (savefiles) {
      grDevices::png(
        filename = file.path(
          graphic_dir,
          subdir,
          paste0(tolower(conf), "likelihood.png")
        ),
        width = 11,
        height = 8.5,
        units = "in",
        res = 300
      )
      print(plot)
      while (grDevices::dev.cur() != 1) {
        grDevices::dev.off()
      }
    }
  }
  return(p)
}

#' Plot Team Rating
#'
#' @description Produces a plot of offensive and defensive ratings of teams, 0 centred (not scaled).
#'
#' @param m HockeyModel::m. Pass [HockeyModel::pwhl_m] for PWHL.
#' @param teamlist select a subset of teams, if desired
#' @param league League identifier. `"NHL"` (default) or `"PWHL"`. When
#'   `"PWHL"`, PWHL team colours and title are used.
#'
#' @return a ggplot2 plot
#' @export
plot_team_rating <- function(
  m = HockeyModel::m,
  teamlist = NULL,
  league = "NHL"
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg ggplot2} is required. Install it with {.code install.packages('ggplot2')}."
    )
  }
  if (is.null(teamlist)) {
    teamlist <- as.character(unique(m$data$Team))
  }
  # Note: invert defence because positive is better defence makes more sense
  team_params <- data.frame(
    Attack = as.numeric(m$coefficients[seq_along(teamlist)]),
    Defence = c(
      0,
      -m$coefficients[(length(teamlist) + 1):(length(teamlist) * 2 - 1)]
    ),
    Team = sort(teamlist)
  )

  # Standardize data
  team_params$Attack <- (team_params$Attack - mean(team_params$Attack)) /
    stats::sd(team_params$Attack)
  team_params$Defence <- (team_params$Defence - mean(team_params$Defence)) /
    stats::sd(team_params$Defence)

  # Build and trim team colours for plot
  colours_source <- if (league == "PWHL") {
    HockeyModel::pwhlTeamColours
  } else {
    HockeyModel::teamColours
  }
  teamColoursList <- as.vector(colours_source$Hex)
  names(teamColoursList) <- colours_source$Team
  teamColoursList <- teamColoursList[names(teamColoursList) %in% teamlist]

  p <- ggplot2::ggplot(
    team_params,
    ggplot2::aes(
      x = .data$Attack,
      y = .data$Defence,
      color = .data$Team,
      label = .data$Team
    )
  ) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey", linewidth = 1) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey", linewidth = 1) +
    ggplot2::geom_point() +
    ggplot2::scale_colour_manual(values = teamColoursList) +
    ggplot2::labs(
      x = "Offence",
      y = "Defence",
      title = if (league == "PWHL") {
        "Current PWHL Team Offence & Defence Ratings"
      } else {
        "Current NHL Team Offence & Defence Ratings"
      },
      subtitle = paste0("As of ", Sys.Date()),
      caption = paste0("P. Bulsink (@bot.bulsink.ca) | ", Sys.Date())
    ) +
    ggplot2::theme_minimal() +
    ggplot2::coord_cartesian(
      xlim = c(
        -max(abs(team_params$Attack)) + 0.1,
        max(abs(team_params$Attack)) + 0.1
      ),
      ylim = c(
        -max(abs(team_params$Defence)) + 0.1,
        max(abs(team_params$Defence)) + 0.1
      )
    ) +
    ggplot2::annotate(
      "label",
      x = -max(abs(team_params$Attack)),
      y = -max(abs(team_params$Defence)),
      hjust = 0,
      vjust = 0,
      label = "Bad"
    ) +
    ggplot2::annotate(
      "label",
      x = max(abs(team_params$Attack)),
      y = max(abs(team_params$Defence)),
      hjust = 1,
      vjust = 1,
      label = "Good"
    ) +
    ggplot2::annotate(
      "label",
      x = -max(abs(team_params$Attack)),
      y = max(abs(team_params$Defence)),
      hjust = 0,
      vjust = 1,
      label = "Calm"
    ) +
    ggplot2::annotate(
      "label",
      x = max(abs(team_params$Attack)),
      y = -max(abs(team_params$Defence)),
      hjust = 1,
      vjust = 0,
      label = "Frantic"
    ) +
    ggplot2::theme(legend.position = "none")

  if (requireNamespace("ggrepel", quietly = TRUE)) {
    p <- p +
      ggrepel::geom_text_repel(force = 2, max.iter = 5000)
  }

  return(p)
}
