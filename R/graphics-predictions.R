# Graphics: prediction trend plots by team (points, playoffs, president's trophy)

#' Order prediction facet levels
#'
#' @param divisions (`character`) Division values present in the plot data.
#' @returns (`character`) Facet levels with NHL divisions in their usual order
#'   followed by any other divisions.
#' @keywords internal
.prediction_facet_levels <- function(divisions) {
  nhl_divisions <- c("Pacific", "Central", "Metropolitan", "Atlantic")
  c(
    nhl_divisions[nhl_divisions %in% divisions],
    setdiff(unique(divisions), nhl_divisions)
  )
}


#' Plot Predicted Points
#'
#' @param all_predictions the compiled predictions
#' @param past_days number of past days to include on the plot. Default: a fortnight
#' @param teamColours HockeyModel::teamColours or a custom value
#'
#' @return a ggplot object
#' @export
plot_prediction_points_by_team <- function(
  all_predictions = compile_predictions(),
  past_days = 14,
  teamColours = HockeyModel::teamColours
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg ggplot2} is required. Install it with {.code install.packages('ggplot2')}."
    )
  }
  if (!requireNamespace("ggforce", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg ggforce} is required. Install it with {.code install.packages('ggforce')}."
    )
  }
  # Trim predictions to fit plot
  all_predictions$predictionDate <- as.Date(all_predictions$predictionDate)
  lastdate <- max(all_predictions$predictionDate)
  firstdate <- lastdate - past_days
  all_predictions <- all_predictions[
    all_predictions$predictionDate >= firstdate,
  ]

  # extract constants
  teams <- unique(all_predictions$Team)
  dates <- as.Date(unique(all_predictions$predictionDate))
  # Get division
  all_predictions$Division <- getTeamDivisions(
    all_predictions$Team,
    teamColours = teamColours
  )
  # Set divisions to logical order
  all_predictions$facet <- factor(
    x = all_predictions$Division,
    levels = .prediction_facet_levels(all_predictions$Division)
  )
  # make team label appear properly later with ggrepel
  all_predictions$label <- ifelse(
    all_predictions$predictionDate == max(all_predictions$predictionDate),
    as.character(paste0(
      getShortTeam(all_predictions$Team, teamColours = teamColours),
      "\n",
      round(all_predictions$meanPoints, digits = 0)
    )),
    NA_character_
  )

  # Build and trim team colours for plot
  teamColoursList <- as.vector(teamColours$Hex)
  names(teamColoursList) <- teamColours$Team
  teamColoursList <- teamColoursList[names(teamColoursList) %in% teams]

  # make plot
  p <- ggplot2::ggplot(
    data = all_predictions,
    ggplot2::aes(
      x = .data$predictionDate,
      y = .data$meanPoints,
      colour = .data$Team
    )
  ) +
    ggforce::geom_bspline() +
    ggplot2::facet_wrap(
      ~facet,
      ncol = length(unique(all_predictions$Division))
    ) +
    ggplot2::scale_x_date(expand = ggplot2::expansion(mult = c(0, .33))) +
    ggplot2::scale_colour_manual(values = teamColoursList) +
    ggplot2::labs(
      x = "Date",
      y = "Points",
      title = paste0("Predicted Points Over the Past ", past_days, " Days"),
      caption = paste0("P. Bulsink (@bot.bulsink.ca) | ", Sys.Date())
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")

  if (requireNamespace("ggrepel", quietly = TRUE)) {
    p <- p +
      ggrepel::geom_label_repel(
        ggplot2::aes(label = .data$label),
        direction = "y",
        na.rm = TRUE,
        segment.alpha = 0,
        hjust = 0.5,
        xlim = c(lastdate, NA)
      )
  }

  return(p)
}

#' Plot Playoff Odds
#'
#' @param all_predictions the compiled predictions
#' @param past_days number of past days to include on the plot. Default: a fortnight
#' @param teamColours HockeyModel::teamColours or a custom value
#'
#' @return a ggplot object
#' @export
plot_prediction_playoffs_by_team <- function(
  all_predictions = compile_predictions(),
  past_days = 14,
  teamColours = HockeyModel::teamColours
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg ggplot2} is required. Install it with {.code install.packages('ggplot2')}."
    )
  }
  if (!requireNamespace("ggforce", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg ggforce} is required. Install it with {.code install.packages('ggforce')}."
    )
  }
  # Trim predictions to fit plot
  all_predictions$predictionDate <- as.Date(all_predictions$predictionDate)
  lastdate <- max(all_predictions$predictionDate)
  firstdate <- lastdate - past_days
  all_predictions <- all_predictions[
    all_predictions$predictionDate >= firstdate,
  ]

  # extract constants
  teams <- unique(all_predictions$Team)
  # Get division
  all_predictions$Division <- getTeamDivisions(
    all_predictions$Team,
    teamColours = teamColours
  )
  # Set divisions to logical order
  all_predictions$facet <- factor(
    x = all_predictions$Division,
    levels = .prediction_facet_levels(all_predictions$Division)
  )
  # make team label appear properly later with ggrepel
  playoff_odds <- all_predictions[
    all_predictions$predictionDate == lastdate,
  ]$Playoffs
  label <- format(
    round(playoff_odds * 100, digits = 0),
    nsmall = 0,
    trim = TRUE
  )
  label[label == "100" & playoff_odds > 0.999] <- "~100"
  label[label == "100" & playoff_odds != 1] <- ">99.5"
  label[playoff_odds == 1] <- "100"
  label[label == "0" & playoff_odds < 0.001] <- "~0"
  label[label == "0" & playoff_odds != 0] <- "<0.5"
  label[playoff_odds == 0] <- "0"

  label <- paste0(
    getShortTeam(
      all_predictions[all_predictions$predictionDate == lastdate, ]$Team,
      teamColours = teamColours
    ),
    "\n",
    label,
    "%",
    sep = ""
  )

  all_predictions$label <- NA_character_
  all_predictions[all_predictions$predictionDate == lastdate, ]$label <- label

  # Build and trim team colours for plot
  teamColoursList <- as.vector(teamColours$Hex)
  names(teamColoursList) <- teamColours$Team
  teamColoursList <- teamColoursList[names(teamColoursList) %in% teams]

  # make plot
  p <- ggplot2::ggplot(
    data = all_predictions,
    ggplot2::aes(
      x = .data$predictionDate,
      y = .data$Playoffs,
      colour = .data$Team
    )
  ) +
    ggforce::geom_bspline() +
    # ggplot2::geom_line() +
    ggplot2::facet_wrap(
      ~facet,
      ncol = length(unique(all_predictions$Division))
    ) +
    ggplot2::scale_x_date(expand = ggplot2::expansion(mult = c(0, .33))) +
    ggplot2::scale_colour_manual(values = teamColoursList) +
    ggplot2::labs(
      x = "Date",
      y = "Playoff Odds",
      title = paste0("Playoff Odds Over the Past ", past_days, " Days"),
      caption = paste0("P. Bulsink (@bot.bulsink.ca) | ", Sys.Date())
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")

  if (requireNamespace("ggrepel", quietly = TRUE)) {
    p <- p +
      ggrepel::geom_label_repel(
        ggplot2::aes(label = .data$label),
        direction = "y",
        na.rm = TRUE,
        segment.alpha = 0,
        hjust = 0.5,
        xlim = c(lastdate, NA),
        max.iter = 1000
      )
  }

  return(p)
}

#' Plot President's Trophey Odds
#'
#' @param all_predictions the compiled predictions
#' @param past_days number of past days to include on the plot. Default: a fortnight
#' @param minimum Minimum chance at pres trophy to plot. Cleans up significantly.
#' @param teamColours HockeyModel::teamColours or a custom value
#'
#' @return a ggplot object
#' @export
plot_prediction_presidents_by_team <- function(
  all_predictions = compile_predictions(),
  past_days = 14,
  minimum = 0.01,
  teamColours = HockeyModel::teamColours
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg ggplot2} is required. Install it with {.code install.packages('ggplot2')}."
    )
  }
  if (!requireNamespace("ggforce", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg ggforce} is required. Install it with {.code install.packages('ggforce')}."
    )
  }
  # Trim predictions to fit plot
  all_predictions$predictionDate <- as.Date(all_predictions$predictionDate)
  lastdate <- max(all_predictions$predictionDate)
  firstdate <- lastdate - past_days
  all_predictions <- all_predictions[
    all_predictions$predictionDate >= firstdate,
  ]

  rankedTeams <- unname(unlist(all_predictions[
    (all_predictions$predictionDate == lastdate &
      all_predictions$Presidents > minimum),
    "Team"
  ]))

  all_predictions <- all_predictions[all_predictions$Team %in% rankedTeams, ]
  # extract constants
  teams <- unique(all_predictions$Team)
  dates <- as.Date(unique(all_predictions$predictionDate))
  # Get division
  all_predictions$Division <- getTeamDivisions(
    all_predictions$Team,
    teamColours = teamColours
  )
  # Set divisions to logical order
  all_predictions$facet <- factor(
    x = all_predictions$Division,
    levels = .prediction_facet_levels(all_predictions$Division)
  )
  # make team label appear properly later with ggrepel
  all_predictions$label <- ifelse(
    all_predictions$predictionDate == max(all_predictions$predictionDate),
    as.character(paste0(
      getShortTeam(all_predictions$Team, teamColours = teamColours),
      "\n",
      signif(all_predictions$Presidents * 100, digits = 2),
      "%"
    )),
    NA_character_
  )

  # Build and trim team colours for plot
  teamColoursList <- as.vector(teamColours$Hex)
  names(teamColoursList) <- teamColours$Team
  teamColoursList <- teamColoursList[names(teamColoursList) %in% teams]

  # make plot
  p <- ggplot2::ggplot(
    data = all_predictions,
    ggplot2::aes(
      x = .data$predictionDate,
      y = .data$Presidents,
      colour = .data$Team
    )
  ) +
    ggforce::geom_bspline() +
    ggplot2::facet_wrap(
      ~facet,
      ncol = length(unique(all_predictions$Division))
    ) +
    ggplot2::scale_x_date(expand = ggplot2::expansion(mult = c(0, .33))) +
    ggplot2::scale_colour_manual(values = teamColoursList) +
    ggplot2::labs(
      x = "Date",
      y = "President's Trophy Odds",
      title = paste0(
        "President's Trophy Odds Over the Past ",
        past_days,
        " Days"
      ),
      caption = paste0("P. Bulsink (@bot.bulsink.ca) | ", Sys.Date()),
      subtitle = paste0(
        "Teams with < ",
        round(minimum * 100, 2),
        "% odds hidden for simplicity"
      )
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")

  if (requireNamespace("ggrepel", quietly = TRUE)) {
    p <- p +
      ggrepel::geom_label_repel(
        ggplot2::aes(label = .data$label),
        direction = "y",
        na.rm = TRUE,
        segment.alpha = 0,
        hjust = 0.5,
        xlim = c(lastdate, NA)
      )
  }

  return(p)
}
