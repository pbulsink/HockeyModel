# Package lifecycle hooks and environment/option setup

#' Resolve a safe worker-core count
#'
#' @param cores (`integer(1)` or `NULL`) Requested core count.
#' @returns (`integer(1)`) Core count constrained to available resources.
#' @keywords internal
parseCores <- function(cores) {
  cores <- cores
  if (is.null(cores)) {
    if (!requireNamespace("parallel", quietly = TRUE)) {
      message(
        "Parallel package must be installed to use multi-core processing."
      )
      cores <- 1
    } else {
      cores <- parallel::detectCores()
    }
  } else {
    if (!requireNamespace("parallel", quietly = TRUE)) {
      message(
        "Parallel package must be installed to use multi-core processing."
      )
      cores <- 1
    } else if (!is.numeric(cores) || cores %% 1 != 0 || cores <= 0) {
      message("Cores must be a integer number")
      cores <- 1
    } else if (cores > parallel::detectCores()) {
      cores <- parallel::detectCores()
    }
  }
  return(cores)
}

#' Initialize package options on load
#'
#' @param libname (`character(1)`) Library path provided by R.
#' @param pkgname (`character(1)`) Package name provided by R.
#' @returns `NULL` (invisibly).
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.HockeyModel <- NULL
  if (requireNamespace("devtools", quietly = TRUE)) {
    packagefile <- NULL
    packagefile <- tryCatch(devtools::package_file(), error = function(e) {
      return(NULL)
    })
    if (is.null(packagefile)) {
      op.HockeyModel <- NULL
    } else {
      op.HockeyModel <- list(
        HockeyModel.prediction.path = file.path(
          devtools::package_file(),
          "prediction_results"
        ),
        HockeyModel.graphics.path = file.path(
          devtools::package_file(),
          "prediction_results",
          "graphics"
        ),
        HockeyModel.data.path = file.path(devtools::package_file(), "data-raw")
      )
    }
  }
  if (is.null(op.HockeyModel)) {
    op.HockeyModel <- list(
      HockeyModel.prediction.path = file.path(
        path.expand("~"),
        "HockeyModel",
        "prediction_results"
      ),
      HockeyModel.graphics.path = file.path(
        path.expand("~"),
        "HockeyModel",
        "prediction_results",
        "graphics"
      ),
      HockeyModel.data.path = file.path(
        path.expand("~"),
        "HockeyModel",
        "data-raw"
      )
    )
  }

  toset <- !(names(op.HockeyModel) %in% names(op))
  if (any(toset)) {
    options(op.HockeyModel[toset])
  }

  invisible()
}

#' List valid prediction file dates
#'
#' @description Returns a sorted vector of `Date` objects for every
#' `YYYY-MM-DD-predictions.RDS` file found in `dir`. Subdirectories and any
#' other files (e.g. graphics, PWHL subdirectory) are ignored.
#'
#' @param dir Directory to search. Defaults to
#'   `getOption("HockeyModel.prediction.path")`.
#'
#' @return A (possibly empty) sorted `Date` vector.
#' @keywords internal
get_prediction_dates <- function(
  dir = getOption("HockeyModel.prediction.path")
) {
  if (is.null(dir) || !dir.exists(dir)) {
    cli::cli_abort("Prediction directory does not exist: {.path {dir}}")
  }
  filelist <- list.files(path = dir, full.names = FALSE)
  filelist <- filelist[
    grepl("^\\d{4}-\\d{2}-\\d{2}-predictions\\.RDS$", filelist)
  ]
  dates <- as.Date(substr(filelist, 1, 10))
  sort(dates[!is.na(dates)])
}


#' Print package startup message on attach
#'
#' @param libname (`character(1)`) Library path provided by R.
#' @param pkgname (`character(1)`) Package name provided by R.
#' @returns `NULL` (invisibly).
#' @keywords internal
.onAttach <- function(libname, pkgname) {
  msgtext <- paste0(
    "HockeyModel package loaded.\nUsing ",
    getOption("HockeyModel.prediction.path"),
    ' as prediction path.\nTo change path, set option("HockeyModel.prediction.path" = [new path]).\n',
    "This can be done interactively or using .RProfile to save your preference."
  )
  packageStartupMessage(msgtext)
}
