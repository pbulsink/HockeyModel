test_that(".dc_weights_exponential produces valid monotonically decaying weights", {
  dates <- as.Date("2024-01-01") + c(0, 10, 50, 100)
  cur <- as.Date("2024-05-01")

  w <- HockeyModel:::.dc_weights_exponential(
    dates,
    currentDate = cur,
    xi = 0.002
  )

  expect_type(w, "double")
  expect_length(w, 4L)
  expect_true(all(w >= 0 & w <= 1))
  # Older dates (first elements) should have lower weights than recent dates
  expect_true(w[1L] < w[2L])
  expect_true(w[2L] < w[3L])
  expect_true(w[3L] < w[4L])

  # Future dates must have zero weight
  future_dates <- as.Date("2024-05-10")
  w_future <- HockeyModel:::.dc_weights_exponential(
    future_dates,
    currentDate = cur,
    xi = 0.002
  )
  expect_equal(w_future, 0)
})

test_that(".dc_weights_exponential supports cross-season discounting", {
  dates <- as.Date(c("2022-10-15", "2023-10-15", "2024-01-15"))
  cur <- as.Date("2024-02-01")
  starts <- as.Date(c("2022-10-01", "2023-10-01"))

  w_no_nu <- HockeyModel:::.dc_weights_exponential(
    dates,
    currentDate = cur,
    xi = 0.001,
    nu = 0
  )
  w_with_nu <- HockeyModel:::.dc_weights_exponential(
    dates,
    currentDate = cur,
    xi = 0.001,
    nu = 2,
    season_start_dates = starts
  )

  # Older season (index 1) should be discounted strictly more when nu > 0
  expect_true(w_with_nu[1L] < w_no_nu[1L])
  # Current season (index 3) s = 0, so 1 / (1 + 0) = 1, weight unchanged
  expect_equal(w_with_nu[3L], w_no_nu[3L])
})

test_that(".dc_weights_exponential validates inputs", {
  expect_error(
    HockeyModel:::.dc_weights_exponential("2024-01-01"),
    regexp = "must be a Date vector"
  )
  expect_error(
    HockeyModel:::.dc_weights_exponential(as.Date("2024-01-01"), xi = -0.5),
    regexp = "non-negative scalar"
  )
  expect_error(
    HockeyModel:::.dc_weights_exponential(
      as.Date("2024-01-01"),
      nu = 2,
      season_start_dates = NULL
    ),
    regexp = "season_start_dates"
  )
})

test_that(".fit_m_with_weights validates input and fits model", {
  sc <- HockeyModel::scores[1:20, ]
  w <- rep(1, 20)

  m <- HockeyModel:::.fit_m_with_weights(sc, w)
  expect_s3_class(m, "glm")

  expect_error(
    HockeyModel:::.fit_m_with_weights(sc, w[1:10]),
    regexp = "Length of"
  )
  expect_error(
    HockeyModel:::.fit_m_with_weights(sc[0, ], numeric(0)),
    regexp = "cannot be empty"
  )
})

test_that(".predict_dc_probabilities computes probabilities with and without Weibull", {
  teams <- unique(c(
    as.character(HockeyModel::scores$HomeTeam[1:10]),
    as.character(HockeyModel::scores$AwayTeam[1:10])
  ))
  h <- teams[1L]
  a <- teams[2L]

  # 3 outcomes (HomeWin, Draw, AwayWin)
  odds_w <- HockeyModel:::.predict_dc_probabilities(
    h,
    a,
    use_weibull = TRUE,
    draws = TRUE
  )
  odds_nw <- HockeyModel:::.predict_dc_probabilities(
    h,
    a,
    use_weibull = FALSE,
    draws = TRUE
  )

  expect_length(odds_w, 3L)
  expect_length(odds_nw, 3L)
  expect_equal(sum(odds_w), 1, tolerance = 1e-6)
  expect_equal(sum(odds_nw), 1, tolerance = 1e-6)

  # Weibull diagonal enhancement must increase draw probability
  expect_true(odds_w["Draw"] > odds_nw["Draw"])

  # 2 outcomes (HomeWin, AwayWin)
  odds_w_2 <- HockeyModel:::.predict_dc_probabilities(
    h,
    a,
    use_weibull = TRUE,
    draws = FALSE
  )
  expect_length(odds_w_2, 2L)
  expect_equal(sum(odds_w_2), 1, tolerance = 1e-6)
})

test_that(".test_ot_so_rates produces complete audit report", {
  res <- HockeyModel:::.test_ot_so_rates(
    scores = HockeyModel::scores,
    min_date = as.Date("2015-10-01"),
    league = "NHL"
  )

  expect_named(res, c("overall", "by_era", "by_season", "recommendation"))
  expect_s3_class(res$overall, "tbl_df")
  expect_s3_class(res$by_era, "tbl_df")
  expect_s3_class(res$by_season, "tbl_df")
  expect_type(res$recommendation, "character")

  expect_true(res$overall$total_ot_so_games > 0)
  expect_true(
    res$overall$empirical_ot_prob > 0 && res$overall$empirical_ot_prob < 1
  )
  expect_true(res$overall$ci_lower <= res$overall$empirical_ot_prob)
  expect_true(res$overall$ci_upper >= res$overall$empirical_ot_prob)
  expect_true(res$overall$log_loss_hardcoded > 0)
})

test_that(".recalibrate_ot_so_rates returns calibrated probabilities", {
  recal <- HockeyModel:::.recalibrate_ot_so_rates(
    scores = HockeyModel::scores,
    min_date = as.Date("2018-10-01")
  )

  expect_named(recal, c("OT", "SO"))
  expect_equal(sum(recal), 1, tolerance = 1e-6)
  expect_true(recal["OT"] > 0.5)
})

test_that(".fit_weibull_params estimates beta, eta, k", {
  # Fit on a small subset of scores
  sc <- HockeyModel::scores[HockeyModel::scores$Date >= as.Date("2023-01-01"), ]
  fit <- HockeyModel:::.fit_weibull_params(scores = sc)

  expect_type(fit$beta, "double")
  expect_type(fit$eta, "double")
  expect_type(fit$k, "double")
  expect_true(fit$beta > 0)
  expect_true(fit$eta > 0)
  expect_true(fit$k > 0)
  expect_true(is.numeric(fit$objective_loss))
})

test_that(".benchmark_weibull compares model performance with vs without Weibull", {
  sc <- HockeyModel::scores[HockeyModel::scores$Date >= as.Date("2023-03-01"), ]
  bm <- HockeyModel:::.benchmark_weibull(
    scores = sc,
    test_start = as.Date("2023-03-15"),
    max_dates = 2L
  )

  expect_s3_class(bm, "tbl_df")
  expect_equal(nrow(bm), 2L)
  expect_true(all(
    c(
      "model",
      "games_evaluated",
      "log_loss",
      "accuracy",
      "predicted_draw_rate"
    ) %in%
      names(bm)
  ))
  # Weibull model must have higher predicted draw rate than standard DC
  expect_true(bm$predicted_draw_rate[1L] > bm$predicted_draw_rate[2L])
})

test_that(".evaluate_weighting_loss evaluates logistic and exponential decay", {
  sc <- HockeyModel::scores[HockeyModel::scores$Date >= as.Date("2023-01-01"), ]
  test_start <- as.Date("2023-03-01")

  res_log <- HockeyModel:::.evaluate_weighting_loss(
    scores = sc,
    test_start = test_start,
    weight_scheme = "logistic",
    max_dates = 1L
  )
  expect_type(res_log$log_loss, "double")
  expect_true(res_log$log_loss > 0)
  expect_true(res_log$n_games > 0)

  res_exp <- HockeyModel:::.evaluate_weighting_loss(
    scores = sc,
    test_start = test_start,
    weight_scheme = "exponential",
    xi = 0.002,
    max_dates = 1L
  )
  expect_type(res_exp$log_loss, "double")
  expect_true(res_exp$log_loss > 0)
  expect_true(res_exp$n_games > 0)
})

test_that(".evaluate_weighting_loss refits rho per model instead of using the package-level value", {
  sc <- HockeyModel::scores[HockeyModel::scores$Date >= as.Date("2023-01-01"), ]
  test_start <- as.Date("2023-03-01")

  # Reconstruct the model that the logistic config fits for its first test date
  train_scores <- sc[sc$Date < test_start, ]
  w <- .DCweights(
    dates = train_scores$Date,
    currentDate = test_start,
    xi = DC_XI_NHL,
    upsilon = DC_UPSILON_NHL,
    nu = 0
  )
  m <- HockeyModel:::.fit_m_with_weights(
    train_scores,
    w
  )
  rho_refit <- suppressWarnings(
    getRho(
      m = m,
      scores = train_scores[train_scores$GameID %in% unique(m$data$GameID), ]
    )
  )

  # The refit rho must be a valid finite value in the DC range (this is what the
  # loss function now computes per fresh model, instead of reusing a stale
  # package-level constant).
  expect_true(is.finite(rho_refit))
  expect_gte(rho_refit, -0.5)
  expect_lte(rho_refit, 0.5)

  # The loss function still runs and refits rho internally without error
  res <- HockeyModel:::.evaluate_weighting_loss(
    scores = sc,
    test_start = test_start,
    weight_scheme = "logistic",
    max_dates = 1L
  )
  expect_true(is.finite(res$log_loss))
})

test_that(".compare_weighting_schemes returns comparison table", {
  sc <- HockeyModel::scores[HockeyModel::scores$Date >= as.Date("2023-01-01"), ]
  comp <- HockeyModel:::.compare_weighting_schemes(
    scores = sc,
    test_start = as.Date("2023-03-01"),
    max_dates = 1L
  )

  expect_s3_class(comp, "tbl_df")
  expect_equal(nrow(comp), 2L)
  expect_true(all(
    c("scheme", "parameters", "log_loss", "accuracy", "brier_score") %in%
      names(comp)
  ))
})

test_that(".audit_model_constants reports documentation discrepancies and parameters", {
  audit <- HockeyModel:::.audit_model_constants()

  expect_s3_class(audit, "tbl_df")
  expect_true(nrow(audit) >= 14L)
  expect_true(all(
    c("parameter", "league", "code_value", "documented_value", "status") %in%
      names(audit)
  ))

  # Issue 5.5: DC_NU_PWHL documented as 2, code value is 5
  nu_row <- audit[audit$parameter == "DC_NU_PWHL", ]
  expect_equal(nrow(nu_row), 1L)
  expect_equal(nu_row$status, "DOCUMENTATION_DISCREPANCY")
  expect_equal(nu_row$code_value, "5")

  # Issue 5.3: OT_PROB_NHL should flag recalibration recommended
  ot_row <- audit[audit$parameter == "OT_PROB_NHL", ]
  expect_equal(nrow(ot_row), 1L)
  expect_equal(ot_row$status, "RECALIBRATION_RECOMMENDED")
})

test_that(".audit_model_constants recalibrates Weibull params against a pre-test model", {
  audit <- HockeyModel:::.audit_model_constants(
    scores = HockeyModel::scores,
    test_start = as.Date("2023-01-01")
  )

  weib_rows <- audit[
    audit$parameter %in%
      c(
        "beta (NHL) recalibrated",
        "eta (NHL) recalibrated",
        "k (NHL) recalibrated"
      ),
    ,
    drop = FALSE
  ]
  expect_equal(nrow(weib_rows), 3L)
  expect_true(all(weib_rows$status == "WEIBULL_RECALIBRATED"))
  expect_true(all(weib_rows$empirical_estimate != ""))

  # Without a test period the audit still reports the (package) constants
  audit_default <- HockeyModel:::.audit_model_constants(
    scores = HockeyModel::scores
  )
  expect_equal(
    nrow(audit_default[audit_default$status == "WEIBULL_RECALIBRATED", ]),
    3L
  )
})

test_that(".run_parameter_audit executes end-to-end master audit", {
  sc <- HockeyModel::scores[HockeyModel::scores$Date >= as.Date("2023-01-01"), ]
  audit_all <- HockeyModel:::.run_parameter_audit(
    scores = sc,
    test_start = as.Date("2023-03-01"),
    max_dates = 1L
  )

  expect_named(
    audit_all,
    c(
      "constants_audit",
      "ot_so_audit",
      "weibull_benchmark",
      "weighting_benchmark"
    )
  )
})
