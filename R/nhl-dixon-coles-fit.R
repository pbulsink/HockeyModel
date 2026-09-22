# Dixon-Coles model fitting: parameter estimation (m, rho, weibull), weighting, and helpers

#' Fast Dixon-Coles model fitting 'm'.
#'
#' @description Produces a DC model
#'
#' @param scores the historical scores to evaluate
#' @param currentDate (for date weight adjustment)
#' @param xi (`double(1)`) Logistic slope for within-season time decay.
#'   Defaults to [DC_XI_NHL].
#' @param upsilon (`double(1)`) Logistic midpoint (days) for within-season time
#'   decay.  Defaults to [DC_UPSILON_NHL].
#' @param nu (`double(1)`) Cross-season discounting exponent.  `0` (the default
#'   [DC_NU_NHL]) disables cross-season discounting.  Pass `DC_NU_PWHL` (or a
#'   custom value) to discount older seasons for leagues with high year-to-year
#'   roster volatility.
#'
#' @export
#' @return a model 'm' of Dixon-Coles' type parameters.
getM <- function(
  scores = HockeyModel::scores,
  currentDate = Sys.Date(),
  xi = DC_XI_NHL,
  upsilon = DC_UPSILON_NHL,
  nu = DC_NU_NHL
) {
  # stopifnot(is.Date(currentDate))
  currentDate <- as.Date(currentDate)

  scores <- scores[scores$Date >= (currentDate - 4000), ] # auto-trim to ~11 years of data, past then the model doesn't get better, just bigger

  # Derive per-season start dates when cross-season discounting is active
  season_start_dates <- if (nu != 0) derive_season_starts(scores$Date) else NULL

  weights <- DCweights(
    dates = scores$Date,
    currentDate = currentDate,
    xi = xi,
    upsilon = upsilon,
    nu = nu,
    season_start_dates = season_start_dates
  )

  df.indep <- data.frame(
    Date = c(scores$Date, scores$Date),
    GameID = c(scores$GameID, scores$GameID),
    Weight = c(weights, weights),
    Team = as.factor(c(
      as.character(scores$HomeTeam),
      as.character(scores$AwayTeam)
    )),
    Opponent = as.factor(c(
      as.character(scores$AwayTeam),
      as.character(scores$HomeTeam)
    )),
    Goals = c(scores$HomeGoals, scores$AwayGoals),
    Home = c(rep(1, nrow(scores)), rep(0, nrow(scores)))
  )
  # df.indep <- df.indep[df.indep$Weight > 1e-8,]
  # Using a (+0) to remove intercept and give a value for each team instead of assuming 'Anaheim Ducks' = 0 (reference)
  m <- stats::glm(
    Goals ~ Team + Opponent + Home + 0,
    data = df.indep,
    weights = df.indep$Weight,
    family = stats::poisson(link = log),
    start = rep(0.01, length(unique(df.indep$Team)) * 2),
    model = FALSE
  )
  m <- cleanModel(m) # reduce M size
  return(m)
}

#' Generate a 'rho' factor for low scoring games
#'
#' @param m result from getM
#' @param scores the historical scores to evaluate
#'
#' @return a numeric value (typically -0.5 to 0)
#' @export
getRho <- function(m = HockeyModel::m, scores = HockeyModel::scores) {
  if (is.null(m)) {
    m <- getM(scores)
  }
  scores <- scores[scores$GameID %in% unique(m$data$GameID), ]
  expected <- stats::fitted(m)
  home.expected <- as.vector(expected[seq_len(nrow(scores))])
  away.expected <- as.vector(expected[(nrow(scores) + 1):(nrow(scores) * 2)])
  weights <- m$data$Weight[seq_len(nrow(scores))]

  DCoptimRhoFn.fast <- function(par) {
    rho <- par[1]
    DCRhoLogLik(
      y1 = scores$HomeGoals,
      y2 = scores$AwayGoals,
      mu = home.expected,
      lambda = away.expected,
      rho = rho,
      weights = weights
    )
  }

  res <- stats::optim(
    par = c(-0.1),
    fn = DCoptimRhoFn.fast,
    # control = list(fnscale = -1),
    method = "BFGS"
  )
  return(res$par)

  # of course, res$par is rho. Ranges from -0.2779 for last decade, -0.175 for 20152016 or 0.09 fo the whole league's history
}

#' Get Weibull Params
#'
#' @description Weibull Params determine is the diagonal enhancement for goals, helping to more accurately predict tie games.
#'
#' @param m HockeyModel::m
#' @param rho HockeyMoel::rho
#' @param scores HockeyModel::scores
#'
#' @return a [beta] and [eta] and [k] value as a list
#' @export
getWeibullParams <- function(
  m = HockeyModel::m,
  rho = HockeyModel::rho,
  scores = HockeyModel::scores
) {
  if (is.null(m)) {
    m <- getM(scores)
  }
  if (is.null(rho)) {
    rho <- getRho(m = m, scores = scores)
  }

  # Have to grab the 'low score' of each tie game, then add one for modelling purposes (Weibull @ x=0 is 0)
  scores <- scores |>
    dplyr::filter(.data$GameID %in% unique(m$data$GameID)) |>
    dplyr::mutate(
      "weight" = m$data$Weight[seq_len(dplyr::n())],
      "mu" = stats::fitted(m)[seq_len(dplyr::n())],
      "lambda" = stats::fitted(m)[(dplyr::n() + 1):(dplyr::n() * 2)]
    ) |>
    dplyr::mutate(
      "Goals" = dplyr::case_when(
        .data$Result == 0.25 ~ .data$HomeGoals + 1,
        .data$Result == 0.4 ~ .data$HomeGoals + 1,
        .data$Result == 0.5 ~ .data$HomeGoals + 1,
        .data$Result == 0.6 ~ .data$AwayGoals + 1,
        .data$Result == 0.75 ~ .data$AwayGoals + 1,
        TRUE ~ NA_real_
      )
    )
  DCoptimTheta.fast <- function(par) {
    beta <- par[1]
    eta <- par[2]
    k <- par[3]
    # This gets multiplied by two because each tie game has 2 teams getting x goals, whereas non-tie games where either team get x goals get pulled in.
    goalratios <- purrr::map_dbl(
      1:max(scores$Goals, na.rm = TRUE),
      function(x) {
        (nrow(scores[scores$Goals == x & !is.na(scores$Goals), ]) /
          nrow(scores[!is.na(scores$Goals), ])) /
          (nrow(scores[
            (scores$HomeGoals == x | scores$AwayGoals == x) &
              is.na(scores$Goals),
          ]) /
            nrow(scores[is.na(scores$Goals), ]))
      }
    ) *
      2
    weibulldist <- stats::dweibull(
      1:max(scores$Goals, na.rm = TRUE),
      shape = beta,
      scale = eta
    ) *
      k
    return(sum((goalratios - weibulldist)^2))
  }

  res <- stats::optim(
    par = c(3, 1, 6),
    fn = DCoptimTheta.fast,
    lower = c(1e-6, 1e-6, -2),
    upper = c(10, 10, 100),
    # control = list(fnscale=-1),
    method = "L-BFGS-B"
  )

  return(list("beta" = res$par[1], "eta" = res$par[2], "k" = res$par[3]))
}

#' Compute one Dixon-Coles low-score adjustment factor
#'
#' @param xx (`integer(1)`) Home goals.
#' @param yy (`integer(1)`) Away goals.
#' @param lambda (`double(1)`) Home expected goals.
#' @param mu (`double(1)`) Away expected goals.
#' @param rho (`double(1)`) Low-score dependence parameter.
#' @returns (`double(1)`) Tau scaling factor for one score pair.
#' @keywords internal
tau_singular <- function(xx, yy, lambda, mu, rho) {
  if (xx == 0 && yy == 0) {
    return(1 - (lambda * mu * rho))
  } else if (xx == 0 && yy == 1) {
    return(1 + (lambda * rho))
  } else if (xx == 1 && yy == 0) {
    return(1 + (mu * rho))
  } else if (xx == 1 && yy == 1) {
    return(1 - rho)
  } else {
    return(1)
  }
}

#' Tau function
#' @description Used in Dixon-Coles's to adjust low scores
#'
#' @param xx Homeoal value for adjustment factor calculation
#' @param yy Away Goal value for adjustment factor calculation
#' @param lambda Home goal expected for adjustment factor calculation
#' @param mu Away Goal expected for adjustment factor calculation
#' @param rho the factor for adjustment calculations
#'
#' @export
tau <- Vectorize(tau_singular, c("xx", "yy", "lambda", "mu"))

#' Compute sigmoid time-decay weights for historical matches
#'
#' @description Computes per-game weights as the product of two components:
#'
#'   1. **Sigmoid within-season decay** — games closer to `currentDate` receive
#'      higher weight.  Controlled by `xi` (slope) and `upsilon` (midpoint in
#'      days).
#'
#'   2. **Cross-season multiplier** *(optional)* — each game's weight is
#'      multiplied by `1 / (1 + s^nu)`, where `s` is the number of complete
#'      seasons between the game and the current season (s = 0 for the current
#'      season, 1 for the previous season, etc.).  Larger `nu` discounts older
#'      seasons more aggressively.  Set `nu = 0` (the default) to disable this
#'      component.
#'
#' @param dates (`Date`) Match dates.
#' @param currentDate (`Date`) Reference date for weighting.
#' @param xi (`double(1)`) Logistic slope (within-season decay speed).
#' @param upsilon (`double(1)`) Logistic midpoint (days). Weights are ≈0.5 at
#'   this age.
#' @param nu (`double(1)`) Cross-season exponent.  `nu = 0` (default) disables
#'   cross-season discounting.  `nu = 2` is a reasonable starting value for
#'   leagues with high year-to-year roster volatility (e.g. PWHL).
#' @param season_start_dates (`Date` or `NULL`) A sorted vector of season start
#'   dates used to determine which season each game belongs to.  Required when
#'   `nu != 0`; if `NULL` and `nu != 0` an error is raised.  Obtain from
#'   [derive_season_starts()].
#'
#' @returns (`double`) Weight per input date in `[0, 1]`.
#' @keywords internal
DCweights <- function(
  dates,
  currentDate = Sys.Date(),
  xi = DC_XI_NHL,
  upsilon = DC_UPSILON_NHL,
  nu = DC_NU_NHL,
  season_start_dates = NULL
) {
  datediffs <- dates - as.Date(currentDate)
  datediffs <- as.numeric(datediffs * -1)
  # Component 1: sigmoid within-season decay
  w <- 1 - 1 / (1 + exp(-xi * (datediffs - upsilon)))
  w[datediffs <= 0] <- 0 # Future dates should have zero weights

  # Component 2: cross-season multiplier (skipped when nu == 0)
  if (nu != 0) {
    if (is.null(season_start_dates) || length(season_start_dates) == 0) {
      cli::cli_abort(
        "{.arg season_start_dates} must be supplied when {.arg nu} != 0."
      )
    }
    sorted_starts <- as.numeric(sort(as.Date(season_start_dates)))
    # Index of the season containing currentDate (1-based)
    current_idx <- findInterval(as.numeric(as.Date(currentDate)), sorted_starts)
    # Index of the season containing each game date
    game_idx <- findInterval(as.numeric(as.Date(dates)), sorted_starts)
    # s = how many seasons back; clamped to >= 0
    s <- pmax(current_idx - game_idx, 0L)
    w <- w * (1 / (1 + s^nu))
  }

  return(w)
}

#' Evaluate Dixon-Coles log-likelihood for a rho value
#'
#' @param y1 (`integer`) Home goals.
#' @param y2 (`integer`) Away goals.
#' @param lambda (`double`) Home expected goals.
#' @param mu (`double`) Away expected goals.
#' @param rho (`double(1)`) Dependence parameter.
#' @param weights (`double` or `NULL`) Optional per-match weights.
#' @returns (`double(1)`) Summed (optionally weighted) log-likelihood.
#' @keywords internal
DCRhoLogLik <- function(y1, y2, lambda, mu, rho = 0, weights = NULL) {
  # rho=0, independence y1 home goals y2 away goals mu:expected Home, lambda: expected Away
  t <- tau(y1, y2, lambda, mu, rho)
  loglik <- log(t) + log(stats::dpois(y1, lambda)) + log(stats::dpois(y2, mu))
  if (is.null(weights)) {
    return(sum(loglik, na.rm = TRUE))
  } else {
    return(sum(loglik * weights, na.rm = TRUE))
  }
}

#' Derive season start dates from a vector of game dates
#'
#' @description Groups game dates by "season year" — the calendar year of the
#'   season's end — and returns the first game date in each season.  Games
#'   played in or after `season_month_cutoff` (default August) are assigned to
#'   the **following** season year (e.g. a game in October 2024 belongs to the
#'   2024-25 season, season year 2025).
#'
#' @param dates (`Date`) Vector of game dates.
#' @param season_month_cutoff (`integer(1)`) Month number (inclusive) from
#'   which a new season is considered to begin.  Games in this month or later
#'   are assigned to the next calendar year.  Default `8L` (August).
#'
#' @returns A sorted `Date` vector with one entry per season, each being the
#'   first game date of that season.
#' @keywords internal
derive_season_starts <- function(dates, season_month_cutoff = 8L) {
  dates <- as.Date(dates)
  year <- as.integer(format(dates, "%Y"))
  month <- as.integer(format(dates, "%m"))
  # Games in or after the cutoff month start a new season (season year = year+1)
  season_year <- ifelse(month >= season_month_cutoff, year + 1L, year)
  # First game date per season year (unname to drop tapply's season-year names)
  starts <- tapply(as.numeric(dates), season_year, min)
  sort(as.Date(unname(starts), origin = "1970-01-01"))
}

#' Normalize nested Dixon-Coles parameter lists
#'
#' @param params (`list` or `NULL`) Candidate parameters, optionally nested
#'   under a `params` element.
#' @param defaults (`list` or `NULL`) Default values for `m`, `rho`, `beta`,
#'   `eta`, and `k`. When `NULL`, NHL package-level defaults are used.
#' @returns (`list`) Named list containing `m`, `rho`, `beta`, `eta`, and `k`.
#' @keywords internal
parse_dc_params <- function(params = NULL, defaults = NULL) {
  while ("params" %in% names(params)) {
    params <- params$params
  }

  if (is.null(defaults)) {
    defaults <- list(
      m = HockeyModel::m,
      rho = HockeyModel::rho,
      beta = HockeyModel::beta,
      eta = HockeyModel::eta,
      k = HockeyModel::k
    )
  }

  list(
    m = if ("m" %in% names(params)) params$m else defaults$m,
    rho = if ("rho" %in% names(params)) params$rho else defaults$rho,
    beta = if ("beta" %in% names(params)) params$beta else defaults$beta,
    eta = if ("eta" %in% names(params)) params$eta else defaults$eta,
    k = if ("k" %in% names(params)) params$k else defaults$k
  )
}
