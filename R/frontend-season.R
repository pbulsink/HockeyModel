#' Check if a date falls in the regular season
#'
#' @description Unified front-end for checking whether `date` falls within a
#'   league's regular season. By default both leagues are evaluated and a named
#'   list is returned; pass `league = "nhl"` or `league = "pwhl"` to evaluate a
#'   single league and get back a scalar result.
#'
#' @param date (`Date` or `character(1)`) Date to check. Defaults to [Sys.Date()].
#' @param boolean (`logical(1)`) When `TRUE` (default), returns a logical
#'   indicator. When `FALSE`, returns the season ID string if in the regular
#'   season, else `FALSE`.
#' @param league (`character(1)` or `NULL`) League selector:
#'   * `NULL`, `NA`, or `"both"`: Evaluates both leagues and returns a named list
#'     with elements `$nhl` and `$pwhl`.
#'   * `"nhl"`: Evaluates NHL only and returns a single result.
#'   * `"pwhl"`: Evaluates PWHL only and returns a single result.
#'
#' @returns For a single league, `logical(1)` when `boolean = TRUE`, or the
#'   season ID string / `FALSE` when `boolean = FALSE`. For both leagues, a named
#'   list with elements `$nhl` and `$pwhl`.
#'
#' @examples
#' inRegularSeason(date = as.Date("2018-12-02"), league = "nhl")
#' inRegularSeason(date = as.Date("2024-01-15"), league = "pwhl", boolean = FALSE)
#' @export
inRegularSeason <- function(
  date = Sys.Date(),
  boolean = TRUE,
  league = NULL
) {
  leagues <- .resolve_frontend_leagues(league)

  result <- list(
    nhl = if ("NHL" %in% leagues) {
      .in_regular_season_nhl(date = date, boolean = boolean)
    },
    pwhl = if ("PWHL" %in% leagues) {
      .in_regular_season_pwhl(date = date, boolean = boolean)
    }
  )

  .simplify_frontend_result(result, leagues)
}

#' Check if a date falls in the playoffs
#'
#' @description Unified front-end for checking whether `date` falls within a
#'   league's playoffs. By default both leagues are evaluated and a named list
#'   is returned; pass `league = "nhl"` or `league = "pwhl"` to evaluate a single
#'   league and get back a scalar result.
#'
#' @param date (`Date` or `character(1)`) Date to check. Defaults to [Sys.Date()].
#' @param boolean (`logical(1)`) When `TRUE` (default), returns a logical
#'   indicator. When `FALSE`, returns the season ID string if in the playoffs,
#'   else `FALSE`.
#' @param league (`character(1)` or `NULL`) League selector:
#'   * `NULL`, `NA`, or `"both"`: Evaluates both leagues and returns a named list
#'     with elements `$nhl` and `$pwhl`.
#'   * `"nhl"`: Evaluates NHL only and returns a single result.
#'   * `"pwhl"`: Evaluates PWHL only and returns a single result.
#'
#' @returns For a single league, `logical(1)` when `boolean = TRUE`, or the
#'   season ID string / `FALSE` when `boolean = FALSE`. For both leagues, a named
#'   list with elements `$nhl` and `$pwhl`.
#'
#' @examples
#' inPlayoffs(date = as.Date("2019-05-01"), league = "nhl")
#' inPlayoffs(date = as.Date("2024-05-15"), league = "pwhl", boolean = FALSE)
#' @export
inPlayoffs <- function(
  date = Sys.Date(),
  boolean = TRUE,
  league = NULL
) {
  leagues <- .resolve_frontend_leagues(league)

  result <- list(
    nhl = if ("NHL" %in% leagues) {
      .in_playoffs_nhl(date = date, boolean = boolean)
    },
    pwhl = if ("PWHL" %in% leagues) {
      .in_playoffs_pwhl(date = date, boolean = boolean)
    }
  )

  .simplify_frontend_result(result, leagues)
}

#' Check if a date falls in the off-season
#'
#' @description Unified front-end for checking whether `date` falls within a
#'   league's off-season (i.e. not in the regular season or playoffs). By default
#'   both leagues are evaluated and a named list is returned; pass
#'   `league = "nhl"` or `league = "pwhl"` to evaluate a single league and get
#'   back a scalar result.
#'
#' @param date (`Date` or `character(1)`) Date to check. Defaults to [Sys.Date()].
#' @param league (`character(1)` or `NULL`) League selector:
#'   * `NULL`, `NA`, or `"both"`: Evaluates both leagues and returns a named list
#'     with elements `$nhl` and `$pwhl`.
#'   * `"nhl"`: Evaluates NHL only and returns a single result.
#'   * `"pwhl"`: Evaluates PWHL only and returns a single result.
#'
#' @returns For a single league, `logical(1)`. For both leagues, a named list
#'   with elements `$nhl` and `$pwhl`.
#'
#' @examples
#' inOffSeason(date = as.Date("2019-08-01"), league = "nhl")
#' inOffSeason(date = as.Date("2024-08-01"), league = "pwhl")
#' @export
inOffSeason <- function(date = Sys.Date(), league = NULL) {
  leagues <- .resolve_frontend_leagues(league)

  result <- list(
    nhl = if ("NHL" %in% leagues) {
      .in_off_season_nhl(date = date)
    },
    pwhl = if ("PWHL" %in% leagues) {
      .in_off_season_pwhl(date = date)
    }
  )

  .simplify_frontend_result(result, leagues)
}
