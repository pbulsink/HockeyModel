# Graphics: gt tables for playoff odds, daily odds, and series odds

#' Format Playoff Odds
#'
#' @description Takes a playoff odds table and returns a gt table
#'
#' @param playoff_odds a playoff odds data frame with columns Team, Make_Playoffs, Win_First_Round, Win_Second_Round, Win_Conference, Win_Cup
#' @param caption_text Additional text to prepend to " Playoff Odds" in table title. E.g. 'Eastern Conference' if only eastteams sent in.
#' @param trim Whether to drop teams that have 0 chance of making playoffs. Default true (ignored for PWHL)
#' @param trimcup Whether to drop teams that have 0 chance of winning cup. Default false (ignored for PWHL)
#' @param league League identifier. `"NHL"` (default) or `"PWHL"`. When
#'   `"PWHL"`, only `Make_Playoffs`, `Make_Finals`, and `Win_Cup` columns are
#'   formatted; team logos are included using the PWHL logo set.
#'
#' @return a gt table
#' @export
format_playoff_odds <- function(
  playoff_odds,
  caption_text = "",
  trim = TRUE,
  trimcup = FALSE,
  league = "NHL"
) {
  if (!requireNamespace("gt", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg gt} is required. Install it with {.code install.packages('gt')}."
    )
  }
  if (!requireNamespace("scales", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg scales} is required. Install it with {.code install.packages('scales')}."
    )
  }

  if (league == "PWHL") {
    teamColours <- HockeyModel::pwhlTeamColours
    table_title <- paste(caption_text, "Playoff Odds")
  } else {
    teamColours <- HockeyModel::teamColours
    table_title <- paste(caption_text, "NHL Playoff Odds")
  }

  if (league == "PWHL") {
    playoff_odds <- playoff_odds |>
      dplyr::arrange(
        dplyr::desc(.data$Win_Cup),
        dplyr::desc(.data$Make_Finals),
        dplyr::desc(.data$Make_Playoffs),
        .data$Team
      )
    playoff_odds_gt <- playoff_odds |>
      dplyr::select(
        .data$Team,
        .data$Make_Playoffs,
        .data$Make_Finals,
        .data$Win_Cup,
        .data$meanPoints,
        .data$meanRank
      ) |>
      tibble::add_column("block" = "  ", .before = 1) |>
      tibble::add_column("image" = playoff_odds$Team, .after = 1) |>
      gt::gt() |>
      gt::tab_header(
        title = table_title,
        subtitle = paste0(
          "Generated ",
          Sys.Date(),
          " | P. Bulsink (@bot.bulsink.ca)"
        )
      ) |>
      gt::cols_label(
        "block" = " ",
        "image" = " ",
        "Make_Playoffs" = "Make Playoffs",
        "Make_Finals" = "Make Finals",
        "Win_Cup" = "Win Cup",
        "meanPoints" = "Mean Points",
        "meanRank" = "Mean Rank"
      ) |>
      gt::data_color(
        columns = c("Make_Playoffs", "Make_Finals", "Win_Cup"),
        fn = scales::col_numeric(c("#fefffe", "#3ccc3c"), domain = c(0, 1))
      ) |>
      gt::fmt_percent(
        columns = c("Make_Playoffs", "Make_Finals", "Win_Cup"),
        drop_trailing_zeros = FALSE
      ) |>
      gt::fmt_number(columns = c("meanPoints", "meanRank"), decimals = 1) |>
      gt::tab_options(heading.align = "left")
  } else {
    playoff_odds <- playoff_odds |>
      dplyr::arrange(
        dplyr::desc(.data$Win_Cup),
        dplyr::desc(.data$Win_Conference),
        dplyr::desc(.data$Win_Second_Round),
        dplyr::desc(.data$Win_First_Round),
        dplyr::desc(.data$Make_Playoffs),
        .data$Team
      )

    if (trim) {
      playoff_odds <- playoff_odds |>
        dplyr::filter(.data$Make_Playoffs > 0)
    }
    if (trimcup) {
      playoff_odds <- playoff_odds |>
        dplyr::filter(.data$Win_Cup > 0)
    }

    playoff_odds_gt <- playoff_odds |>
      tibble::add_column("block" = "  ", .before = 1) |>
      tibble::add_column("image" = "", .after = 1) |>
      dplyr::mutate("image" = .data$Team) |>
      gt::gt() |>
      gt::tab_header(
        title = table_title,
        subtitle = paste0(
          "Generated ",
          Sys.Date(),
          " | P. Bulsink (@bot.bulsink.ca)"
        )
      ) |>
      gt::cols_label(
        "block" = " ",
        "image" = " ",
        "Make_Playoffs" = "Make Playoffs",
        "Win_First_Round" = "Win First Round",
        "Win_Second_Round" = "Win Second Round",
        "Win_Conference" = "Win Conference",
        "Win_Cup" = "Win Cup"
      ) |>
      gt::data_color(
        columns = 4:8,
        fn = scales::col_numeric(c("#fefffe", "#3ccc3c"), domain = c(0, 1))
      ) |>
      gt::fmt_percent(columns = 4:8, drop_trailing_zeros = FALSE) |>
      gt::tab_options(heading.align = "left")
  }

  for (i in seq_len(nrow(playoff_odds))) {
    fallback_logo <- if (league == "PWHL") "pwhl.png" else "nhl.png"
    playoff_odds_gt <- playoff_odds_gt |>
      gt::tab_style(
        style = gt::cell_fill(
          color = teamColours[teamColours$Team == playoff_odds$Team[i], "Hex"]
        ),
        locations = gt::cells_body(columns = "block", rows = i)
      ) |>
      gt::text_transform(
        locations = gt::cells_body(columns = "image", rows = i),
        fn = function(x) {
          gt::local_image(
            filename = ifelse(
              file.exists(file.path(
                getOption("HockeyModel.data.path"),
                "logos",
                paste0(tolower(gsub(" ", "_", x)), ".png")
              )),
              file.path(
                getOption("HockeyModel.data.path"),
                "logos",
                paste0(tolower(gsub(" ", "_", x)), ".png")
              ),
              file.path(
                getOption("HockeyModel.data.path"),
                "logos",
                fallback_logo
              )
            ),
            height = "30px"
          )
        }
      )
  }

  return(playoff_odds_gt)
}


#' Today Odds Table
#'
#' @description Returns a gt table of odds for today's games (or games for a supplied date)
#'
#' @param today A date for games to create a table. Defaults to today.
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#' @param schedule Schedule, or HockeyModel Schedule
#' @param league League identifier. `"NHL"` (default) or `"PWHL"`. When
#'   `"PWHL"`, PWHL team colours and logos are used and the title is set to
#'   `"PWHL Game Odds"`.
#'
#' @return a gt table
#' @export
daily_odds_table <- function(
  today = Sys.Date(),
  params = NULL,
  schedule = HockeyModel::schedule,
  league = "NHL"
) {
  if (!requireNamespace("gt", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg gt} is required. Install it with {.code install.packages('gt')}."
    )
  }
  if (!requireNamespace("scales", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg scales} is required. Install it with {.code install.packages('scales')}."
    )
  }
  params <- parse_dc_params(params)
  todayodds <- todayDC(
    today = as.Date(today),
    params = params,
    schedule = schedule
  )
  if (is.null(todayodds)) {
    return(NULL)
  }
  todayodds$HomexG <- NA
  todayodds$AwayxG <- NA

  for (g in seq_len(nrow(todayodds))) {
    xg <- dcxG(
      home = todayodds$HomeTeam[g],
      away = todayodds$AwayTeam[g],
      params = params
    )
    todayodds$HomexG[g] <- xg$home
    todayodds$AwayxG[g] <- xg$away
    todayodds[g, c("HomeWin", "AwayWin")] <- normalizeOdds(todayodds[
      g,
      c("HomeWin", "AwayWin")
    ])
  }

  if (league == "PWHL") {
    teamColours <- HockeyModel::pwhlTeamColours
    table_title <- "PWHL Game Odds"
    include_images <- TRUE
  } else {
    teamColours <- HockeyModel::teamColours
    table_title <- "NHL Game Odds"
    include_images <- TRUE
  }

  todayodds_tbl <- todayodds |>
    dplyr::select(
      .data$HomeTeam,
      .data$HomexG,
      .data$HomeWin,
      .data$AwayWin,
      .data$AwayxG,
      .data$AwayTeam
    ) |>
    tibble::add_column("homeblock" = "  ", .before = 1) |>
    tibble::add_column("awayblock" = "  ")

  if (include_images) {
    todayodds_tbl <- todayodds_tbl |>
      tibble::add_column("homeimage" = todayodds$HomeTeam, .after = 1) |>
      tibble::add_column(
        "awayimage" = todayodds$AwayTeam,
        .before = "awayblock"
      ) |>
      dplyr::mutate(
        "homeimage" = .data$HomeTeam,
        "awayimage" = .data$AwayTeam
      )
  }

  todayodds_gt <- todayodds_tbl |>
    gt::gt() |>
    gt::tab_header(
      title = table_title,
      subtitle = paste0(
        "For games ",
        today,
        " | P. Bulsink (@bot.bulsink.ca)"
      )
    ) |>
    gt::tab_spanner(
      label = "Home",
      columns = c("HomeTeam", "HomexG", "HomeWin")
    ) |>
    gt::tab_spanner(
      label = "Away",
      columns = c("AwayWin", "AwayxG", "AwayTeam")
    ) |>
    gt::cols_label(
      "homeblock" = " ",
      "HomexG" = "xG",
      "HomeWin" = "Win",
      "HomeTeam" = "Team",
      "AwayxG" = "xG",
      "AwayWin" = "Win",
      "AwayTeam" = "Team",
      "awayblock" = " "
    ) |>
    gt::data_color(
      columns = c("HomeWin", "AwayWin"),
      fn = scales::col_numeric(
        palette = c("#cc3c3c", "#ffffff", "#3c3ccc"),
        domain = c(0, 1)
      )
    ) |>
    gt::fmt_percent(columns = c("HomeWin", "AwayWin"), decimals = 1) |>
    gt::fmt_number(
      columns = c("HomexG", "AwayxG"),
      drop_trailing_zeros = FALSE,
      decimals = 2
    ) |>
    gt::tab_options(
      heading.align = "left",
      table.border.bottom.color = "white",
      table.border.top.color = "white"
    )

  if (include_images) {
    todayodds_gt <- todayodds_gt |>
      gt::cols_label("homeimage" = " ", "awayimage" = " ")
  }

  for (i in seq_len(nrow(todayodds))) {
    todayodds_gt <- todayodds_gt |>
      gt::tab_style(
        style = gt::cell_fill(
          color = teamColours[
            teamColours$Team == todayodds$HomeTeam[i],
            "Hex"
          ]
        ),
        locations = gt::cells_body(columns = "homeblock", rows = i)
      ) |>
      gt::tab_style(
        style = gt::cell_fill(
          color = teamColours[
            teamColours$Team == todayodds$AwayTeam[i],
            "Hex"
          ]
        ),
        locations = gt::cells_body(columns = "awayblock", rows = i)
      )
    if (include_images) {
      todayodds_gt <- todayodds_gt |>
        gt::text_transform(
          locations = gt::cells_body(columns = "homeimage", rows = i),
          fn = function(x) {
            gt::local_image(
              filename = ifelse(
                file.exists(file.path(
                  getOption("HockeyModel.data.path"),
                  "logos",
                  paste0(tolower(gsub(" ", "_", x)), ".png")
                )),
                file.path(
                  getOption("HockeyModel.data.path"),
                  "logos",
                  paste0(tolower(gsub(" ", "_", x)), ".png")
                ),
                file.path(
                  getOption("HockeyModel.data.path"),
                  "logos",
                  paste0(tolower(league), ".png")
                )
              ),
              height = "30px"
            )
          }
        ) |>
        gt::text_transform(
          locations = gt::cells_body(columns = "awayimage", rows = i),
          fn = function(x) {
            gt::local_image(
              filename = ifelse(
                file.exists(file.path(
                  getOption("HockeyModel.data.path"),
                  "logos",
                  paste0(tolower(gsub(" ", "_", x)), ".png")
                )),
                file.path(
                  getOption("HockeyModel.data.path"),
                  "logos",
                  paste0(tolower(gsub(" ", "_", x)), ".png")
                ),
                file.path(
                  getOption("HockeyModel.data.path"),
                  "logos",
                  paste0(tolower(league), ".png")
                )
              ),
              height = "30px"
            )
          }
        )
    }
  }

  return(todayodds_gt)
}


#' Series Odds Table
#'
#' @description Returns a gt table of odds for each active playoff series.
#'
#' @param series A data frame with columns HomeTeam, AwayTeam, HomeWins, and
#'   AwayWins describing the current playoff series. Defaults to
#'   [getAPISeries()].
#' @param params The named list containing m, rho, beta, eta, and k. See
#'   [updateDC] for information on the params list.
#' @param league League identifier. `"NHL"` (default) or `"PWHL"`. When
#'   `"PWHL"`, PWHL team colours and logos are used and the title is set to
#'   `"PWHL Playoff Series Odds"`.
#'
#' @return a gt table
#' @export
series_odds_table <- function(
  series = getAPISeries(),
  params = NULL,
  league = "NHL"
) {
  if (!requireNamespace("gt", quietly = TRUE)) {
    cli::cli_abort(
      c(
        "Package {.pkg gt} is required by {.fn series_odds_table}.",
        "i" = "Install it with {.code install.packages('gt')}."
      )
    )
  }
  if (!requireNamespace("scales", quietly = TRUE)) {
    cli::cli_abort(
      c(
        "Package {.pkg scales} is required by {.fn series_odds_table}.",
        "i" = "Install it with {.code install.packages('scales')}."
      )
    )
  }

  if (!is.data.frame(series)) {
    cli::cli_abort("{.arg series} must be a data frame.")
  }

  required_cols <- c("HomeTeam", "AwayTeam", "HomeWins", "AwayWins")
  missing_cols <- setdiff(required_cols, names(series))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "{.arg series} must contain the required columns used by {.fn series_odds_table}.",
      "x" = "Missing columns: {.val {missing_cols}}"
    ))
  }

  invalid_cols <- character(0)
  if (!is.character(series$HomeTeam)) {
    invalid_cols <- c(invalid_cols, "HomeTeam (expected character)")
  }
  if (!is.character(series$AwayTeam)) {
    invalid_cols <- c(invalid_cols, "AwayTeam (expected character)")
  }
  if (!is.numeric(series$HomeWins)) {
    invalid_cols <- c(invalid_cols, "HomeWins (expected numeric/integer)")
  }
  if (!is.numeric(series$AwayWins)) {
    invalid_cols <- c(invalid_cols, "AwayWins (expected numeric/integer)")
  }
  if (length(invalid_cols) > 0) {
    cli::cli_abort(c(
      "{.arg series} has required columns with invalid types.",
      "x" = "Invalid columns: {.val {invalid_cols}}"
    ))
  }

  if ("Status" %in% names(series)) {
    series <- series[series$Status == "Ongoing", , drop = FALSE]
  }

  params <- parse_dc_params(params)

  series <- series[, required_cols]
  series$HomeOdds <- mapply(
    function(home, away, hw, aw) {
      playoffWin(home, away, hw, aw, params = params)
    },
    series$HomeTeam,
    series$AwayTeam,
    series$HomeWins,
    series$AwayWins
  )
  series$AwayOdds <- 1 - series$HomeOdds

  if (league == "PWHL") {
    teamColours <- HockeyModel::pwhlTeamColours
    table_title <- "PWHL Playoff Series Odds"
    series_note <- gt::md("*Best of 5 game series*")
    include_images <- TRUE
  } else {
    teamColours <- HockeyModel::teamColours
    table_title <- "NHL Playoff Series Odds"
    series_note <- gt::md("*Best of 7 game series*")
    include_images <- TRUE
  }

  # Resolve a team name to its local logo path (falls back to nhl or pwhl logo)
  team_logo_path <- function(team_name, league) {
    candidate <- file.path(
      getOption("HockeyModel.data.path"),
      "logos",
      paste0(tolower(gsub(" ", "_", team_name)), ".png")
    )
    ifelse(
      file.exists(candidate),
      candidate,
      file.path(
        getOption("HockeyModel.data.path"),
        "logos",
        paste0(tolower(league), ".png")
      )
    )
  }

  series_tbl <- series |>
    tibble::add_column("homeblock" = "  ", .before = 1) |>
    tibble::add_column("awayblock" = "  ")

  if (include_images) {
    series_tbl <- series_tbl |>
      tibble::add_column("homeimage" = series$HomeTeam, .after = 1) |>
      tibble::add_column(
        "awayimage" = series$AwayTeam,
        .before = "awayblock"
      ) |>
      dplyr::mutate(
        "homeimage" = .data$HomeTeam,
        "awayimage" = .data$AwayTeam
      )
  }

  series_gt <- series_tbl |>
    gt::gt() |>
    gt::tab_header(
      title = table_title,
      subtitle = paste0(
        "Generated ",
        Sys.Date(),
        " | P. Bulsink (@bot.bulsink.ca)"
      )
    ) |>
    gt::tab_spanner(
      label = "Home",
      columns = c("HomeTeam", "HomeWins", "HomeOdds")
    ) |>
    gt::tab_spanner(
      label = "Away",
      columns = c("AwayOdds", "AwayWins", "AwayTeam")
    ) |>
    gt::cols_label(
      "homeblock" = " ",
      "HomeTeam" = "Team",
      "HomeWins" = "Wins",
      "HomeOdds" = "Series Odds",
      "AwayOdds" = "Series Odds",
      "AwayWins" = "Wins",
      "AwayTeam" = "Team",
      "awayblock" = " "
    ) |>
    gt::data_color(
      columns = c("HomeOdds", "AwayOdds"),
      fn = scales::col_numeric(
        palette = c("#cc3c3c", "#ffffff", "#3c3ccc"),
        domain = c(0, 1)
      )
    ) |>
    gt::fmt_percent(columns = c("HomeOdds", "AwayOdds"), decimals = 1) |>
    gt::tab_options(
      heading.align = "left",
      table.border.bottom.color = "white",
      table.border.top.color = "white"
    ) |>
    gt::tab_source_note(gt::md(series_note))

  if (include_images) {
    series_gt <- series_gt |>
      gt::cols_label("homeimage" = " ", "awayimage" = " ")
  }

  for (i in seq_len(nrow(series))) {
    series_gt <- series_gt |>
      gt::tab_style(
        style = gt::cell_fill(
          color = teamColours[
            teamColours$Team == series$HomeTeam[i],
            "Hex"
          ]
        ),
        locations = gt::cells_body(columns = "homeblock", rows = i)
      ) |>
      gt::tab_style(
        style = gt::cell_fill(
          color = teamColours[
            teamColours$Team == series$AwayTeam[i],
            "Hex"
          ]
        ),
        locations = gt::cells_body(columns = "awayblock", rows = i)
      )
    if (include_images) {
      series_gt <- series_gt |>
        gt::text_transform(
          locations = gt::cells_body(columns = "homeimage", rows = i),
          fn = function(x) {
            gt::local_image(
              filename = team_logo_path(x, league),
              height = "30px"
            )
          }
        ) |>
        gt::text_transform(
          locations = gt::cells_body(columns = "awayimage", rows = i),
          fn = function(x) {
            gt::local_image(
              filename = team_logo_path(x, league),
              height = "30px"
            )
          }
        )
    }
  }

  return(series_gt)
}
