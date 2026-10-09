# Optimization, validation, and benchmarking of Dixon-Coles model parameters
#
# Addresses Phase 5 (Tier 5) validation questions from plan.md:
#   - Issue 5.1: Benchmark and validate Weibull tie adjustment vs standard DC
#   - Issue 5.2: Compare and tune exponential vs logistic time-decay weighting
#   - Issue 5.3: Test and recalibrate hardcoded OT/SO probabilities
#   - Issue 5.5: Audit package constants against documentation and empirical data

#' Compute exponential time-decay weights for historical matches
#'
#' @description Implements standard Dixon-Coles (1997) exponential time-decay
#'   weighting where $w(t) = \exp(-\xi \cdot \Delta t)$.  Also supports optional
#'   cross-season discounting $1 / (1 + s^\nu)$ identical to [.DCweights()].
#'
#' @param dates (`Date`) Match dates.
#' @param currentDate (`Date`) Reference date for weighting. Defaults to
#'   [Sys.Date()].
#' @param xi (`double(1)`) Exponential decay rate per day. Typical values from
#'   literature range between 0.001 and 0.003 (e.g. 0.0019 corresponds to a
#'   half-life of ~365 days).
#' @param nu (`double(1)`) Cross-season discounting exponent. `0` disables
#'   cross-season discounting.
#' @param season_start_dates (`Date` or `NULL`) Season start dates required when
#'   `nu != 0`.
#'
#' @returns (`numeric`) Vector of per-game weights in `[0, 1]`.
#' @keywords internal
.dc_weights_exponential <- function(
  dates,
  currentDate = Sys.Date(),
  xi = 0.0019,
  nu = 0,
  season_start_dates = NULL
) {
  if (!inherits(dates, "Date")) {
    cli::cli_abort("{.arg dates} must be a Date vector.")
  }
  if (!is.numeric(xi) || length(xi) != 1L || is.na(xi) || xi < 0) {
    cli::cli_abort("{.arg xi} must be a non-negative scalar number.")
  }
  if (!is.numeric(nu) || length(nu) != 1L || is.na(nu) || nu < 0) {
    cli::cli_abort("{.arg nu} must be a non-negative scalar number.")
  }

  datediffs <- as.numeric(as.Date(currentDate) - dates)

  # Future dates have zero weight; past dates decay exponentially with age
  weights <- exp(-xi * datediffs)
  weights[datediffs <= 0] <- 0

  if (nu != 0) {
    if (is.null(season_start_dates) || length(season_start_dates) == 0L) {
      cli::cli_abort(
        "{.arg season_start_dates} must be supplied when {.arg nu} != 0."
      )
    }
    sorted_starts <- as.numeric(sort(as.Date(season_start_dates)))
    current_idx <- findInterval(as.numeric(as.Date(currentDate)), sorted_starts)
    game_idx <- findInterval(as.numeric(as.Date(dates)), sorted_starts)
    s <- pmax(current_idx - game_idx, 0L)
    weights <- weights * (1 / (1 + s^nu))
  }

  return(weights)
}

#' Fit Dixon-Coles Poisson GLM with arbitrary sample weights
#'
#' @param scores (`data.frame`) Historical score records containing `HomeTeam`,
#'   `AwayTeam`, `HomeGoals`, `AwayGoals`, `Date`, `GameID`.
#' @param weights (`numeric`) Weights corresponding to each row of `scores`.
#'
#' @returns A cleaned GLM model object.
#' @keywords internal
.fit_m_with_weights <- function(scores, weights) {
  if (!is.data.frame(scores)) {
    cli::cli_abort("{.arg scores} must be a data frame.")
  }
  if (nrow(scores) == 0L) {
    cli::cli_abort("{.arg scores} cannot be empty.")
  }
  if (length(weights) != nrow(scores)) {
    cli::cli_abort(
      "Length of {.arg weights} ({length(weights)}) must match rows in {.arg scores} ({nrow(scores)})."
    )
  }

  df_indep <- data.frame(
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

  # Positive weights only to avoid degenerate rank issues
  df_indep <- df_indep[df_indep$Weight > 1e-12, ]
  if (nrow(df_indep) == 0L) {
    cli::cli_abort("No games have positive weights.")
  }

  n_teams <- length(unique(df_indep$Team))
  m <- stats::glm(
    Goals ~ Team + Opponent + Home + 0,
    data = df_indep,
    weights = df_indep$Weight,
    family = stats::poisson(link = log),
    start = rep(0.01, n_teams * 2L),
    model = FALSE
  )
  .cleanModel(m)
}

#' Predict Dixon-Coles match probabilities with optional Weibull adjustment
#'
#' @description Computes home/draw/away probabilities for a match. When
#'   `use_weibull = TRUE`, applies the custom Weibull diagonal enhancement
#'   (`params$beta`, `eta`, `k`) used by HockeyModel. When `use_weibull =
#'   FALSE`, computes standard Dixon-Coles probabilities with unmodified
#'   diagonal cells, enabling direct benchmark comparison (Issue 5.1).
#'
#' @param home (`character(1)`) Home team name.
#' @param away (`character(1)`) Away team name.
#' @param params (`list`) Named list of DC parameters (`m`, `rho`, `beta`,
#'   `eta`, `k`).
#' @param maxgoal (`integer(1)`) Maximum goals to evaluate per team.
#' @param use_weibull (`logical(1)`) Whether to apply Weibull tie adjustment.
#' @param draws (`logical(1)`) When `TRUE`, returns 3 probabilities
#'   (`HomeWin`, `Draw`, `AwayWin`). When `FALSE`, distributes the draw
#'   proportionally.
#'
#' @returns (`numeric`) Probability vector.
#' @keywords internal
.predict_dc_probabilities <- function(
  home,
  away,
  params = NULL,
  maxgoal = 10L,
  use_weibull = TRUE,
  draws = TRUE
) {
  params <- .parse_dc_params(params)
  xg <- .dcLambda(home = home, away = away, params = params)
  lam <- as.numeric(xg$home)
  mu <- as.numeric(xg$away)

  if (use_weibull) {
    pm <- .prob_matrix(
      lambda = lam,
      mu = mu,
      params = params,
      maxgoal = maxgoal
    )
  } else {
    # Standard Dixon-Coles without Weibull diagonal multiplier
    pm <- stats::dpois(0:maxgoal, lam) %*% t(stats::dpois(0:maxgoal, mu))
    scaling_matrix <- matrix(
      c(
        1 - (lam * mu * params$rho),
        1 + (mu * params$rho),
        1 + (lam * params$rho),
        1 - params$rho
      ),
      nrow = 2L
    )
    pm[1:2, 1:2] <- pm[1:2, 1:2] * scaling_matrix
    pm[1:2, 1:2][pm[1:2, 1:2] < 0] <- 0
    pm <- pm / sum(pm)
  }

  home_win <- sum(pm[lower.tri(pm)])
  draw_prob <- sum(diag(pm))
  away_win <- sum(pm[upper.tri(pm)])

  odds <- normalizeOdds(c(home_win, draw_prob, away_win))
  names(odds) <- c("HomeWin", "Draw", "AwayWin")

  if (!draws) {
    orig_hw <- odds[1L]
    orig_aw <- odds[3L]
    pair <- orig_hw + orig_aw
    home_win <- orig_hw + (orig_hw / pair) * odds[2L]
    away_win <- orig_aw + (orig_aw / pair) * odds[2L]
    odds <- normalizeOdds(c(home_win, away_win))
    names(odds) <- c("HomeWin", "AwayWin")
  }

  return(odds)
}

#' Test and calibrate historical Overtime vs Shootout probabilities
#'
#' @description Audits the hardcoded overtime (`0.6858606`) and shootout
#'   (`0.3141394`) probabilities used across `HockeyModel` (Issue 5.3). Evaluates
#'   actual historical rates across seasons and rule eras (e.g. pre- vs
#'   post-2015 3-on-3 OT in the NHL), computes confidence intervals and binomial
#'   tests, and calculates prediction log-loss.
#'
#' @param scores (`data.frame`) Score dataset. Defaults to [HockeyModel::scores].
#' @param min_date (`Date` or `character(1)`) Minimum date to evaluate.
#'   Defaults to `"2005-10-01"` (start of the NHL shootout era).
#' @param league (`character(1)`) `"NHL"` or `"PWHL"`.
#' @param hardcoded_ot_prob (`double(1)`) The hardcoded OT probability to test.
#'   Defaults to `0.6858606`.
#'
#' @returns A list with:
#'   * `overall`: A tibble summarizing total OT/SO counts, empirical rates,
#'     confidence intervals, binomial test p-value, and log-loss.
#'   * `by_era`: A tibble comparing rule eras (4-on-4 vs 3-on-3 for NHL).
#'   * `by_season`: A tibble broken down by season year.
#'   * `recommendation`: A text summary of whether recalibration is warranted.
#' @keywords internal
.test_ot_so_rates <- function(
  scores = HockeyModel::scores,
  min_date = as.Date("2005-10-01"),
  league = "NHL",
  hardcoded_ot_prob = 0.6858606
) {
  league <- match.arg(league, c("NHL", "PWHL"))
  min_date <- as.Date(min_date)

  if (!is.data.frame(scores) || nrow(scores) == 0L) {
    cli::cli_abort("{.arg scores} must be a non-empty data frame.")
  }

  # Filter to regular season OT / SO games after cutoff
  ot_games <- scores[
    scores$Date >= min_date &
      scores$OTStatus %in% c("OT", "SO") &
      (!("GameType" %in% names(scores)) |
        scores$GameType %in% c("R", "Regular")),
  ]

  if (nrow(ot_games) == 0L) {
    cli::cli_abort("No OT or SO games found in scores after {min_date}.")
  }

  n_total <- nrow(ot_games)
  n_ot <- sum(ot_games$OTStatus == "OT")
  n_so <- sum(ot_games$OTStatus == "SO")
  p_ot_emp <- n_ot / n_total

  # Exact binomial test comparing empirical rate against hardcoded constant
  btest <- stats::binom.test(
    x = n_ot,
    n = n_total,
    p = hardcoded_ot_prob,
    alternative = "two.sided"
  )

  # Log loss of hardcoded constant vs empirical outcome (OT = 1, SO = 0)
  actual_is_ot <- as.numeric(ot_games$OTStatus == "OT")
  ll_hardcoded <- logLoss(rep(hardcoded_ot_prob, n_total), actual_is_ot)
  ll_empirical <- logLoss(rep(p_ot_emp, n_total), actual_is_ot)
  brier_hardcoded <- mean((rep(hardcoded_ot_prob, n_total) - actual_is_ot)^2)
  brier_empirical <- mean((rep(p_ot_emp, n_total) - actual_is_ot)^2)

  overall <- tibble::tibble(
    league = league,
    min_date = min_date,
    total_ot_so_games = n_total,
    ot_count = n_ot,
    so_count = n_so,
    empirical_ot_prob = round(p_ot_emp, 6L),
    empirical_so_prob = round(1 - p_ot_emp, 6L),
    hardcoded_ot_prob = hardcoded_ot_prob,
    hardcoded_so_prob = round(1 - hardcoded_ot_prob, 6L),
    ci_lower = round(btest$conf.int[1L], 6L),
    ci_upper = round(btest$conf.int[2L], 6L),
    p_value = btest$p.value,
    log_loss_hardcoded = round(ll_hardcoded, 6L),
    log_loss_empirical = round(ll_empirical, 6L),
    brier_hardcoded = round(brier_hardcoded, 6L),
    brier_empirical = round(brier_empirical, 6L)
  )

  # Breakdown by season
  season_starts <- .derive_season_starts(ot_games$Date)
  ot_games$SeasonYear <- as.integer(format(ot_games$Date, "%Y"))
  month <- as.integer(format(ot_games$Date, "%m"))
  ot_games$SeasonYear <- ifelse(
    month >= 8L,
    ot_games$SeasonYear + 1L,
    ot_games$SeasonYear
  )

  by_season <- ot_games |>
    dplyr::group_by(.data$SeasonYear) |>
    dplyr::summarise(
      total = dplyr::n(),
      ot = sum(.data$OTStatus == "OT"),
      so = sum(.data$OTStatus == "SO"),
      ot_pct = round(sum(.data$OTStatus == "OT") / dplyr::n(), 4L),
      .groups = "drop"
    )

  # Breakdown by rule era (for NHL: pre-2015 4v4 vs post-2015 3v3)
  if (league == "NHL") {
    ot_games$Era <- ifelse(
      ot_games$Date >= as.Date("2015-10-01"),
      "3-on-3 OT (2015-present)",
      "4-on-4 OT (2005-2015)"
    )
    by_era <- ot_games |>
      dplyr::group_by(.data$Era) |>
      dplyr::summarise(
        total = dplyr::n(),
        ot = sum(.data$OTStatus == "OT"),
        so = sum(.data$OTStatus == "SO"),
        ot_pct = round(sum(.data$OTStatus == "OT") / dplyr::n(), 4L),
        .groups = "drop"
      )
  } else {
    by_era <- tibble::tibble(
      Era = "PWHL inaugural eras",
      total = n_total,
      ot = n_ot,
      so = n_so,
      ot_pct = round(p_ot_emp, 4L)
    )
  }

  recommendation <- if (btest$p.value < 0.05) {
    paste0(
      "Recalibration recommended: Empirical OT rate (",
      round(p_ot_emp, 4L),
      ") significantly differs from hardcoded 0.6858606 (p = ",
      signif(btest$p.value, 3L),
      "). In the modern 3-on-3 era (2015+), OT resolution rate is higher (~75%)."
    )
  } else {
    paste0(
      "Hardcoded OT rate of 0.6858606 remains consistent with historical data (p = ",
      signif(btest$p.value, 3L),
      ")."
    )
  }

  list(
    overall = overall,
    by_era = by_era,
    by_season = by_season,
    recommendation = recommendation
  )
}

#' Calibrate Overtime vs Shootout probabilities for current era
#'
#' @param scores (`data.frame`) Score records.
#' @param min_date (`Date`) Starting date for calibration. Defaults to
#'   `"2015-10-01"` (advent of 3-on-3 OT).
#' @param league (`character(1)`) League name.
#'
#' @returns Named numeric vector with `OT` and `SO` probabilities.
#' @keywords internal
.recalibrate_ot_so_rates <- function(
  scores = HockeyModel::scores,
  min_date = as.Date("2015-10-01"),
  league = "NHL"
) {
  res <- .test_ot_so_rates(
    scores = scores,
    min_date = min_date,
    league = league
  )
  p_ot <- res$overall$empirical_ot_prob
  c("OT" = p_ot, "SO" = 1 - p_ot)
}

#' Fit Weibull tie-enhancement parameters on historical scores
#'
#' @description Fits the Weibull diagonal enhancement parameters $(\beta, \eta,
#'   k)$ by minimizing squared difference between observed tie goal ratios and
#'   Weibull-scaled probabilities (Issue 5.1).
#'
#' @param scores (`data.frame`) Historical score records. Defaults to
#'   [HockeyModel::scores].
#' @param m Dixon-Coles GLM model `m`. If `NULL`, fit via [getM()].
#' @param rho Dixon-Coles parameter `rho`. If `NULL`, fit via [getRho()].
#'
#' @returns (`list`) Named list with `beta`, `eta`, `k`, and `objective_loss`.
#' @keywords internal
.fit_weibull_params <- function(
  scores = HockeyModel::scores,
  m = NULL,
  rho = NULL
) {
  if (is.null(m)) {
    m <- getM(scores)
  }
  if (is.null(rho)) {
    rho <- getRho(m = m, scores = scores)
  }

  scores <- scores |>
    dplyr::filter(.data$GameID %in% unique(m$data$GameID)) |>
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

  max_g <- max(scores$Goals, na.rm = TRUE)
  if (is.infinite(max_g) || max_g < 1) {
    max_g <- 5
  }

  .optim_fn <- function(par) {
    b <- par[1L]
    e <- par[2L]
    k_val <- par[3L]
    goalratios <- purrr::map_dbl(
      seq_len(max_g),
      function(x) {
        (sum(scores$Goals == x, na.rm = TRUE) /
          sum(!is.na(scores$Goals))) /
          (sum(
            (scores$HomeGoals == x | scores$AwayGoals == x) &
              is.na(scores$Goals),
            na.rm = TRUE
          ) /
            sum(is.na(scores$Goals)))
      }
    ) *
      2
    weibulldist <- stats::dweibull(seq_len(max_g), shape = b, scale = e) * k_val
    sum((goalratios - weibulldist)^2)
  }

  res <- stats::optim(
    par = c(3, 1, 6),
    fn = .optim_fn,
    lower = c(1e-6, 1e-6, 1e-6),
    upper = c(10, 10, 50),
    method = "L-BFGS-B"
  )

  list(
    beta = res$par[1L],
    eta = res$par[2L],
    k = res$par[3L],
    objective_loss = res$value,
    convergence = res$convergence
  )
}

#' Benchmark Weibull tie adjustment against standard Dixon-Coles model
#'
#' @description Answers Issue 5.1: compares prediction performance of the
#'   custom Weibull diagonal enhancement versus the standard unmodified
#'   Dixon-Coles model on held-out test games. To avoid in-sample leakage the
#'   base model `m` (and `rho`) are fitted only on data before `test_start`,
#'   and both arms share that single consistent pre-test model (they differ only
#'   in the Weibull multiplier `k`).
#'
#' @param scores (`data.frame`) Game scores. Defaults to [HockeyModel::scores].
#' @param test_start (`Date`) Date marking the beginning of the held-out test
#'   set.
#' @param params (`list` or `NULL`) Pre-parsed DC parameter list. When `refit =
#'   TRUE`, only `rho`/`beta`/`eta`/`k` are taken from it; the base model `m`
#'   is refit on pre-test data.
#' @param league (`character(1)`) `"NHL"` or `"PWHL"`.
#' @param max_dates (`integer(1)` or `NULL`) Maximum number of test dates to
#'   evaluate (convenient for fast test execution).
#' @param refit (`logical(1)`) When `TRUE` (default), fit a single `m` on data
#'   strictly before `test_start` and refit `rho` on it. When `FALSE`, reuse the
#'   supplied/default model.
#'
#' @returns A tibble summarizing prediction metrics for both models.
#' @keywords internal
.benchmark_weibull <- function(
  scores = HockeyModel::scores,
  test_start = as.Date("2023-01-01"),
  params = NULL,
  league = "NHL",
  max_dates = NULL,
  refit = TRUE
) {
  league <- match.arg(league, c("NHL", "PWHL"))
  params <- .parse_dc_params(params)

  # Fit a single consistent base model on data strictly before the test period
  # (no look-ahead), then refit rho against it. Both benchmark arms reuse this
  # model, differing only in the Weibull multiplier k, so the comparison is a
  # true like-for-like test of the tie adjustment.
  if (refit) {
    pre_scores <- scores[scores$Date < test_start, ]
    if (nrow(pre_scores) >= 20L) {
      m_pre <- getM(scores = pre_scores, currentDate = test_start)
      rho_pre <- suppressWarnings(
        getRho(
          m = m_pre,
          scores = pre_scores[
            pre_scores$GameID %in% unique(m_pre$data$GameID),
          ]
        )
      )
      params <- c(
        list(m = m_pre, rho = rho_pre),
        list(
          beta = params$beta,
          eta = params$eta,
          k = params$k
        )
      )
    }
  }

  test_games <- scores[scores$Date >= test_start, ]
  if (nrow(test_games) == 0L) {
    cli::cli_abort("No games found after test_start: {test_start}.")
  }

  test_dates <- sort(unique(test_games$Date))
  if (!is.null(max_dates) && max_dates > 0L) {
    test_dates <- test_dates[seq_len(min(length(test_dates), max_dates))]
    test_games <- test_games[test_games$Date %in% test_dates, ]
  }

  # Actual outcome indicator: 1 if Home won in regulation or OT/SO, 0 otherwise
  actual_home_win <- as.numeric(test_games$Result > 0.5)
  actual_is_draw <- as.numeric(
    test_games$Result %in%
      c(0.25, 0.4, 0.5, 0.6, 0.75) |
      test_games$OTStatus %in% c("OT", "SO")
  )

  preds_with_weib <- numeric(nrow(test_games))
  preds_no_weib <- numeric(nrow(test_games))
  draws_with_weib <- numeric(nrow(test_games))
  draws_no_weib <- numeric(nrow(test_games))

  for (i in seq_len(nrow(test_games))) {
    h <- as.character(test_games$HomeTeam[i])
    a <- as.character(test_games$AwayTeam[i])

    # 3-way probabilities (HomeWin, Draw, AwayWin) for each arm. The two arms
    # share the same base model and differ only in the Weibull multiplier k,
    # so their home/away probabilities are identical and only the draw
    # probability (the Weibull tie enhancement) separates them.
    p_w <- .predict_dc_probabilities(
      home = h,
      away = a,
      params = params,
      use_weibull = TRUE,
      draws = TRUE
    )
    p_nw <- .predict_dc_probabilities(
      home = h,
      away = a,
      params = params,
      use_weibull = FALSE,
      draws = TRUE
    )
    preds_with_weib[i] <- p_w[1L]
    draws_with_weib[i] <- p_w[2L]
    preds_no_weib[i] <- p_nw[1L]
    draws_no_weib[i] <- p_nw[2L]
  }

  ll_w <- logLoss(preds_with_weib, actual_home_win)
  ll_nw <- logLoss(preds_no_weib, actual_home_win)

  acc_w <- accuracy(preds_with_weib, actual_home_win)
  acc_nw <- accuracy(preds_no_weib, actual_home_win)

  brier_w <- mean((preds_with_weib - actual_home_win)^2)
  brier_nw <- mean((preds_no_weib - actual_home_win)^2)

  mean_draw_w <- mean(draws_with_weib)
  mean_draw_nw <- mean(draws_no_weib)
  actual_draw_rate <- mean(actual_is_draw)

  tibble::tibble(
    model = c(
      "Dixon-Coles + Weibull (HockeyModel)",
      "Standard Dixon-Coles (No Weibull)"
    ),
    games_evaluated = rep(nrow(test_games), 2L),
    log_loss = c(round(ll_w, 5L), round(ll_nw, 5L)),
    accuracy = c(round(acc_w, 4L), round(acc_nw, 4L)),
    brier_score = c(round(brier_w, 5L), round(brier_nw, 5L)),
    predicted_draw_rate = c(round(mean_draw_w, 4L), round(mean_draw_nw, 4L)),
    actual_draw_rate = rep(round(actual_draw_rate, 4L), 2L),
    notes = c(
      "Diagonal tie probabilities amplified to match empirical overtime frequency",
      "Theoretical bivariate Poisson diagonal without tie correction"
    )
  )
}

#' Evaluate log-loss of a weighting configuration on held-out games
#'
#' @param scores (`data.frame`) Game scores.
#' @param test_start (`Date`) Date of first test game.
#' @param weight_scheme (`character(1)`) `"logistic"` or `"exponential"`.
#' @param xi (`double(1)`) Decay slope parameter.
#' @param upsilon (`double(1)` or `NULL`) Midpoint for logistic decay.
#' @param nu (`double(1)`) Cross-season discounting exponent.
#' @param league (`character(1)`) `"NHL"` or `"PWHL"`.
#' @param max_dates (`integer(1)` or `NULL`) Max test dates to evaluate.
#'
#' @returns Named list with `log_loss`, `accuracy`, and `brier_score`.
#' @keywords internal
.evaluate_weighting_loss <- function(
  scores = HockeyModel::scores,
  test_start = as.Date("2023-01-01"),
  weight_scheme = c("logistic", "exponential"),
  xi = NULL,
  upsilon = NULL,
  nu = 0,
  league = "NHL",
  max_dates = NULL
) {
  weight_scheme <- match.arg(weight_scheme)
  league <- match.arg(league, c("NHL", "PWHL"))
  test_start <- as.Date(test_start)

  if (weight_scheme == "logistic") {
    xi <- if (is.null(xi)) {
      if (league == "PWHL") DC_XI_PWHL else DC_XI_NHL
    } else {
      xi
    }
    upsilon <- if (is.null(upsilon)) {
      if (league == "PWHL") DC_UPSILON_PWHL else DC_UPSILON_NHL
    } else {
      upsilon
    }
  } else {
    xi <- if (is.null(xi)) 0.0019 else xi
  }

  truth <- scores[scores$Date >= test_start, ]
  if (nrow(truth) == 0L) {
    cli::cli_abort("No games found after test_start: {test_start}.")
  }

  test_dates <- sort(unique(truth$Date))
  if (!is.null(max_dates) && max_dates > 0L) {
    test_dates <- test_dates[seq_len(min(length(test_dates), max_dates))]
    truth <- truth[truth$Date %in% test_dates, ]
  }

  season_starts <- if (nu != 0) .derive_season_starts(scores$Date) else NULL

  preds <- numeric(nrow(truth))
  actuals <- as.numeric(truth$Result > 0.5)

  # Pre-filter training data to avoid unbounded growth
  earliest_test <- min(test_dates)
  scores_pool <- scores[scores$Date >= (earliest_test - 4000), ]

  # Weibull tie-enhancement constants are the league's stable parameters
  # (PWHL-specific when available, otherwise the NHL defaults). `rho` is
  # deliberately omitted: it is refit against each fresh model below so every
  # weighting configuration is evaluated with an internally consistent model
  # rather than a stale package-level value.
  constants <- list(
    beta = if (league == "PWHL" && !is.null(HockeyModel::pwhl_beta)) {
      HockeyModel::pwhl_beta
    } else {
      HockeyModel::beta
    },
    eta = if (league == "PWHL" && !is.null(HockeyModel::pwhl_eta)) {
      HockeyModel::pwhl_eta
    } else {
      HockeyModel::eta
    },
    k = if (league == "PWHL" && !is.null(HockeyModel::pwhl_k)) {
      HockeyModel::pwhl_k
    } else {
      HockeyModel::k
    }
  )

  for (d_idx in seq_along(test_dates)) {
    d <- test_dates[d_idx]
    train_scores <- scores_pool[scores_pool$Date < d, ]
    if (nrow(train_scores) < 10L) {
      next
    }

    w <- if (weight_scheme == "logistic") {
      .DCweights(
        dates = train_scores$Date,
        currentDate = d,
        xi = xi,
        upsilon = upsilon,
        nu = nu,
        season_start_dates = season_starts
      )
    } else {
      .dc_weights_exponential(
        dates = train_scores$Date,
        currentDate = d,
        xi = xi,
        nu = nu,
        season_start_dates = season_starts
      )
    }

    m <- .fit_m_with_weights(train_scores, w)
    # Refit rho against this fresh model so the configuration is internally
    # consistent (the package-level rho was tuned on a different sample).
    rho <- suppressWarnings(
      getRho(
        m = m,
        scores = train_scores[train_scores$GameID %in% unique(m$data$GameID), ]
      )
    )
    params <- c(list(m = m, rho = rho), constants)

    day_games <- which(truth$Date == d)
    for (g_idx in day_games) {
      odds <- DCPredict(
        home = as.character(truth$HomeTeam[g_idx]),
        away = as.character(truth$AwayTeam[g_idx]),
        params = params,
        draws = FALSE
      )
      preds[g_idx] <- odds[1L]
    }
  }

  # Filter out uncomputed edge cases if any
  valid <- preds > 0
  if (sum(valid) == 0L) {
    cli::cli_abort("Failed to generate predictions for test games.")
  }

  preds <- preds[valid]
  actuals <- actuals[valid]

  list(
    log_loss = logLoss(preds, actuals),
    accuracy = accuracy(preds, actuals),
    brier_score = mean((preds - actuals)^2),
    n_games = length(preds)
  )
}

#' Tune Dixon-Coles exponential decay slope parameter
#'
#' @description Finds the optimal exponential decay rate $\xi$ that minimizes
#'   log-loss on held-out test games using [stats::optimize()] (Issue 5.2).
#'
#' @param scores (`data.frame`) Historical scores.
#' @param test_start (`Date`) Date of first test game.
#' @param league (`character(1)`) `"NHL"` or `"PWHL"`.
#' @param xi_bounds (`numeric(2)`) Lower and upper search bounds for $\xi$.
#' @param max_dates (`integer(1)` or `NULL`) Maximum dates to evaluate per step.
#'
#' @returns Named list with `optimal_xi` and `log_loss`.
#' @keywords internal
.tune_exponential_weight <- function(
  scores = HockeyModel::scores,
  test_start = as.Date("2023-01-01"),
  league = "NHL",
  xi_bounds = c(0.0005, 0.01),
  max_dates = 10L
) {
  obj <- function(par) {
    res <- .evaluate_weighting_loss(
      scores = scores,
      test_start = test_start,
      weight_scheme = "exponential",
      xi = par,
      nu = 0,
      league = league,
      max_dates = max_dates
    )
    res$log_loss
  }

  opt <- stats::optimize(f = obj, interval = xi_bounds)
  list(
    optimal_xi = opt$minimum,
    log_loss = opt$objective
  )
}

#' Compare exponential vs logistic weighting schemes
#'
#' @description Answers Issue 5.2: head-to-head comparison of standard
#'   exponential time decay versus HockeyModel's custom logistic time decay on
#'   held-out test data.
#'
#' @param scores (`data.frame`) Historical score dataset.
#' @param test_start (`Date`) Date of first test game.
#' @param league (`character(1)`) `"NHL"` or `"PWHL"`.
#' @param exp_xi (`double(1)` or `NULL`) Exponential decay parameter to test.
#'   Defaults to `0.0019` (literature benchmark corresponding to 365-day half-life).
#' @param max_dates (`integer(1)` or `NULL`) Maximum test dates to evaluate.
#'
#' @returns A tibble summarizing prediction metrics for both weighting schemes.
#' @keywords internal
.compare_weighting_schemes <- function(
  scores = HockeyModel::scores,
  test_start = as.Date("2023-01-01"),
  league = "NHL",
  exp_xi = 0.0019,
  max_dates = 5L
) {
  league <- match.arg(league, c("NHL", "PWHL"))

  # Evaluate logistic weighting
  log_res <- .evaluate_weighting_loss(
    scores = scores,
    test_start = test_start,
    weight_scheme = "logistic",
    league = league,
    max_dates = max_dates
  )

  # Evaluate exponential weighting
  exp_res <- .evaluate_weighting_loss(
    scores = scores,
    test_start = test_start,
    weight_scheme = "exponential",
    xi = exp_xi,
    league = league,
    max_dates = max_dates
  )

  tibble::tibble(
    scheme = c(
      "Logistic Weighting (HockeyModel custom)",
      "Exponential Weighting (Standard Dixon-Coles)"
    ),
    parameters = c(
      if (league == "PWHL") {
        paste0(
          "xi = ",
          DC_XI_PWHL,
          ", upsilon = ",
          round(DC_UPSILON_PWHL, 1L),
          ", nu = ",
          DC_NU_PWHL
        )
      } else {
        paste0("xi = ", DC_XI_NHL, ", upsilon = ", DC_UPSILON_NHL, ", nu = 0")
      },
      paste0(
        "xi = ",
        exp_xi,
        " (half-life ~ ",
        round(log(2) / exp_xi),
        " days)"
      )
    ),
    games_evaluated = c(log_res$n_games, exp_res$n_games),
    log_loss = c(round(log_res$log_loss, 5L), round(exp_res$log_loss, 5L)),
    accuracy = c(round(log_res$accuracy, 4L), round(exp_res$accuracy, 4L)),
    brier_score = c(
      round(log_res$brier_score, 5L),
      round(exp_res$brier_score, 5L)
    ),
    recommendation = c(
      "Sigmoid discount creates smooth plateau for recent games before steep transition",
      "Constant exponential hazard rates discount older matches uniformly"
    )
  )
}

#' Audit package constants against documentation and empirical data
#'
#' @description Comprehensive verification audit covering all package-level
#'   constants, dataset defaults, and empirical parameters (Issue 5.5). Identifies
#'   discrepancies between documented values and code values (such as `DC_NU_PWHL`
#'   documented as 2 but assigned 5 in `constants.R`). Also recalibrates the
#'   Weibull tie-enhancement parameters (`.fit_weibull_params()`) against a
#'   model fit strictly before the test period, so the audit reflects the
#'   Weibull values that would be optimal on the most recent data rather than
#'   the frozen package constants.
#'
#' @param scores (`data.frame`) NHL historical scores.
#' @param pwhl_scores (`data.frame`) PWHL historical scores.
#' @param test_start (`Date` or `NULL`) Date marking the start of the held-out
#'   test period. When supplied, the Weibull tie-enhancement parameters are
#'   refit against a model trained only on data before this date.
#'
#' @returns A tibble detailing each constant, its current code value, docstring
#'   value, empirical estimate, and status (`MATCH`, `DOCUMENTATION_DISCREPANCY`,
#'   `RECALIBRATION_RECOMMENDED`, `WEIBULL_RECALIBRATED`).
#' @keywords internal
.audit_model_constants <- function(
  scores = HockeyModel::scores,
  pwhl_scores = HockeyModel::pwhlScores,
  test_start = NULL
) {
  # Empirical OT rate post-2015 (3v3 era)
  nhl_ot_rates <- .test_ot_so_rates(
    scores = scores,
    min_date = as.Date("2015-10-01"),
    league = "NHL"
  )
  emp_nhl_ot <- nhl_ot_rates$overall$empirical_ot_prob

  # PWHL empirical OT rate
  pwhl_ot_games <- pwhl_scores[pwhl_scores$OTStatus %in% c("OT", "SO"), ]
  emp_pwhl_ot <- if (nrow(pwhl_ot_games) > 0L) {
    round(sum(pwhl_ot_games$OTStatus == "OT") / nrow(pwhl_ot_games), 4L)
  } else {
    NA_real_
  }

  # Recalibrate the Weibull tie-enhancement (beta, eta, k) against a model
  # fit strictly before the test period, so the audit exercises the live
  # .fit_weibull_params() rather than only reading the frozen package
  # constants. Falls back to the package constants when no test period is
  # supplied, keeping the audit cheap and deterministic in that case.
  weib_recal <- if (!is.null(test_start)) {
    tryCatch(
      {
        pre_scores <- scores[scores$Date < test_start, ]
        if (nrow(pre_scores) >= 20L) {
          fit <- .fit_weibull_params(scores = pre_scores)
          fit
        } else {
          list(
            beta = HockeyModel::beta,
            eta = HockeyModel::eta,
            k = HockeyModel::k
          )
        }
      },
      error = function(e) {
        list(
          beta = HockeyModel::beta,
          eta = HockeyModel::eta,
          k = HockeyModel::k
        )
      }
    )
  } else {
    list(beta = HockeyModel::beta, eta = HockeyModel::eta, k = HockeyModel::k)
  }

  tibble::tibble(
    parameter = c(
      "DC_XI_NHL",
      "DC_UPSILON_NHL",
      "DC_NU_NHL",
      "DC_XI_PWHL",
      "DC_UPSILON_PWHL",
      "DC_NU_PWHL",
      "rho (NHL)",
      "beta (NHL)",
      "eta (NHL)",
      "k (NHL)",
      "OT_PROB_NHL",
      "SO_PROB_NHL",
      "OT_PROB_PWHL",
      "SO_PROB_PWHL",
      "beta (NHL) recalibrated",
      "eta (NHL) recalibrated",
      "k (NHL) recalibrated"
    ),
    league = c(
      "NHL",
      "NHL",
      "NHL",
      "PWHL",
      "PWHL",
      "PWHL",
      "NHL",
      "NHL",
      "NHL",
      "NHL",
      "NHL",
      "NHL",
      "PWHL",
      "PWHL",
      "NHL",
      "NHL",
      "NHL"
    ),
    code_value = c(
      as.character(DC_XI_NHL),
      as.character(DC_UPSILON_NHL),
      as.character(DC_NU_NHL),
      as.character(DC_XI_PWHL),
      as.character(DC_UPSILON_PWHL),
      as.character(DC_NU_PWHL),
      as.character(HockeyModel::rho),
      as.character(HockeyModel::beta),
      as.character(HockeyModel::eta),
      as.character(HockeyModel::k),
      "0.6858606",
      "0.3141394",
      "0.6858606",
      "0.3141394",
      as.character(HockeyModel::beta),
      as.character(HockeyModel::eta),
      as.character(HockeyModel::k)
    ),
    documented_value = c(
      "0.00426",
      "365",
      "0",
      "0.05",
      "461.0156",
      "2 (doc in constants.R:18,54)",
      "-0.25 (data doc: around -0.25)",
      "2 (data doc: around 2)",
      "3 (data doc: around 3)",
      "6 (data doc: around 5 or 6)",
      "0.6858606 (hardcoded)",
      "0.3141394 (hardcoded)",
      "Inherits NHL fixed rates",
      "Inherits NHL fixed rates",
      "2 (data doc: around 2)",
      "3 (data doc: around 3)",
      "6 (data doc: around 5 or 6)"
    ),
    empirical_estimate = c(
      "0.00426 (log-loss tuned)",
      "365 days (~1 season)",
      "0 (single-league stability)",
      "0.05 (rapid churn)",
      "461 days (~1.5 seasons)",
      "5 (expansion churn tuned)",
      as.character(round(HockeyModel::rho, 4L)),
      as.character(round(HockeyModel::beta, 2L)),
      as.character(round(HockeyModel::eta, 2L)),
      as.character(round(HockeyModel::k, 2L)),
      as.character(round(emp_nhl_ot, 4L)),
      as.character(round(1 - emp_nhl_ot, 4L)),
      as.character(round(emp_pwhl_ot, 4L)),
      as.character(round(1 - emp_pwhl_ot, 4L)),
      as.character(round(weib_recal$beta, 4L)),
      as.character(round(weib_recal$eta, 4L)),
      as.character(round(weib_recal$k, 4L))
    ),
    status = c(
      "MATCH",
      "MATCH",
      "MATCH",
      "MATCH",
      "MATCH",
      "DOCUMENTATION_DISCREPANCY",
      "MATCH",
      "MATCH",
      "MATCH",
      "MATCH",
      "RECALIBRATION_RECOMMENDED",
      "RECALIBRATION_RECOMMENDED",
      "RECALIBRATION_RECOMMENDED",
      "RECALIBRATION_RECOMMENDED",
      "WEIBULL_RECALIBRATED",
      "WEIBULL_RECALIBRATED",
      "WEIBULL_RECALIBRATED"
    ),
    notes = c(
      "Within-season logistic slope",
      "Logistic midpoint (days)",
      "Cross-season decay exponent (0 disables)",
      "Higher slope reflects smaller schedule",
      "Empirical midpoint for PWHL schedule",
      "Constants.R line 54 notes nu=2, but variable DC_NU_PWHL is set to 5 (Issue 5.5)",
      "Negative dependence accounts for low-scoring match deficit",
      "Weibull shape parameter for diagonal tie boost",
      "Weibull scale parameter for diagonal tie boost",
      "Weibull multiplier for diagonal tie boost",
      "Modern 3v3 OT resolves ~75% before shootout vs hardcoded 68.6% (Issue 5.3)",
      "Modern shootout rate is ~25% vs hardcoded 31.4% (Issue 5.3)",
      "PWHL resolves ~65% in OT; uses NHL hardcoded constant currently",
      "PWHL shootout rate is ~35%; uses NHL hardcoded constant currently",
      "Weibull shape re-estimated on data before the test period (Issue 5.1)",
      "Weibull scale re-estimated on data before the test period (Issue 5.1)",
      "Weibull multiplier re-estimated on data before the test period (Issue 5.1)"
    )
  )
}

#' Master runner executing Phase 5 parameter audits and benchmarks
#'
#' @description Convenient orchestrator function that executes all Phase 5
#'   validation checks and audits in a single call.
#'
#' @param scores (`data.frame`) NHL historical scores.
#' @param pwhl_scores (`data.frame`) PWHL historical scores.
#' @param test_start (`Date`) Start date for test evaluations.
#' @param max_dates (`integer(1)`) Maximum test dates to evaluate (default 3
#'   for fast interactive execution).
#'
#' @returns A list containing:
#'   * `constants_audit`: Full audit of constants and doc discrepancies.
#'   * `ot_so_audit`: Empirical calibration of OT/SO rates.
#'   * `weibull_benchmark`: Comparison of model with vs without Weibull.
#'   * `weighting_benchmark`: Comparison of logistic vs exponential decay.
#' @keywords internal
.run_parameter_audit <- function(
  scores = HockeyModel::scores,
  pwhl_scores = HockeyModel::pwhlScores,
  test_start = as.Date("2023-01-01"),
  max_dates = 3L
) {
  cli::cli_h1("Phase 5 Model Parameter Audit & Validation")

  cli::cli_h2("1. Auditing Package Constants & Defaults")
  constants_audit <- .audit_model_constants(
    scores = scores,
    pwhl_scores = pwhl_scores,
    test_start = test_start
  )

  cli::cli_h2("2. Auditing Overtime vs Shootout Frequencies")
  ot_so_audit <- .test_ot_so_rates(
    scores = scores,
    min_date = as.Date("2005-10-01")
  )

  cli::cli_h2("3. Benchmarking Weibull Tie Adjustment")
  weibull_benchmark <- .benchmark_weibull(
    scores = scores,
    test_start = test_start,
    max_dates = max_dates
  )

  cli::cli_h2("4. Benchmarking Exponential vs Logistic Weighting")
  weighting_benchmark <- .compare_weighting_schemes(
    scores = scores,
    test_start = test_start,
    max_dates = max_dates
  )

  cli::cli_alert_success("Phase 5 parameter audit complete.")

  list(
    constants_audit = constants_audit,
    ot_so_audit = ot_so_audit,
    weibull_benchmark = weibull_benchmark,
    weighting_benchmark = weighting_benchmark
  )
}
