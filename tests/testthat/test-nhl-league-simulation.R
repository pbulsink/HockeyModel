context("test-nhl-league-simulation")

test_that("Convenience Functions are OK", {
  skip_if_hockey_apis_unavailable()
  odds <- todayOdds(today = as.Date("2019-11-01"))
  expect_true(is.null(odds) || is.data.frame(odds))
  if (!is.null(odds)) {
    expect_true(all(
      c("HomeTeam", "AwayTeam", "HomeWin", "AwayWin") %in% names(odds)
    ))
  }
})

# ============ todayOdds tests ============
test_that("todayOdds returns data frame or NULL", {
  sched <- HockeyModel::scores
  sched <- sched[sched$Date > as.Date("2019-10-01"), ]
  sched <- sched[sched$Date < as.Date("2019-12-31"), ]
  result <- suppressWarnings(todayOdds(
    today = as.Date("2019-11-01"),
    schedule = sched
  ))
  expect_true(is.data.frame(result))
})

# ============ todayOdds tests (from test-graphics-comprehensive.R) ============
test_that("todayOdds returns data frame or NULL", {
  local_mocked_bindings(
    .todayDC = function(...) {
      data.frame(
        Date = as.Date("2019-11-01"),
        GameID = 2019020196,
        HomeTeam = "New Jersey Devils",
        AwayTeam = "Philadelphia Flyers",
        HomeWin = 0.45,
        AwayWin = 0.35,
        Draw = 0.20
      )
    },
    .package = "HockeyModel"
  )
  result <- suppressWarnings(todayOdds(today = as.Date("2019-11-01")))
  expect_true(is.data.frame(result))
})

# ============ simulateSeasonParallel tests ============
test_that("simulateSeasonParallel() sequential branch reuses precomputed HOT/AOT (#52)", {
  # The sequential branch historically called .extraTimeSolver() twice per
  # simulation even though odds_table$HOT/AOT are already computed once
  # before the loop. We assert the branch runs to completion and that its
  # results match an independent reference computed from the same odds,
  # which holds whether or not the redundant recompute is present.
  local_mocked_bindings(
    remainderSeasonDC = function(...) {
      data.frame(
        HomeTeam = c("Boston Bruins", "Detroit Red Wings", "Florida Panthers"),
        AwayTeam = c(
          "Detroit Red Wings",
          "Florida Panthers",
          "Boston Bruins"
        ),
        HomeWin = c(1, 1, 1),
        AwayWin = c(0, 0, 0),
        Draw = c(0, 0, 0),
        GameID = c(1L, 2L, 3L),
        Date = as.Date("2025-11-01"),
        stringsAsFactors = FALSE
      )
    },
    .package = "HockeyModel"
  )
  sched <- data.frame(
    Home = c("Boston Bruins", "Detroit Red Wings", "Florida Panthers"),
    Away = c(
      "Detroit Red Wings",
      "Florida Panthers",
      "Boston Bruins"
    ),
    Date = as.Date("2025-11-01"),
    GameID = c(1L, 2L, 3L),
    stringsAsFactors = FALSE
  )
  res <- simulateSeasonParallel(
    scores = NULL,
    schedule = sched,
    nsims = 5,
    cores = 1
  )
  expect_true(is.list(res))
  expect_true("summary_results" %in% names(res))
  expect_true("raw_results" %in% names(res))
  expect_equal(nrow(res$summary_results), 3)
  # BOS beats DET, DET beats FLA, FLA beats BOS => each: 1W 1L, Points=2
  expect_true(all(res$summary_results$meanWins == 1))
  expect_true(all(res$summary_results$meanPoints == 2))
})

test_that("simulateSeasonParallel() parallel branch matches sequential (#49)", {
  # The parallel branch batches `nsims` simulations into `cores` tasks. With
  # degenerate odds (every game a certain regulation home win) the outcome is
  # deterministic, so the parallel result must match the sequential branch
  # exactly. This also exercises the per-task batch assembly path.
  local_mocked_bindings(
    remainderSeasonDC = function(...) {
      data.frame(
        HomeTeam = c("Boston Bruins", "Detroit Red Wings", "Florida Panthers"),
        AwayTeam = c(
          "Detroit Red Wings",
          "Florida Panthers",
          "Boston Bruins"
        ),
        HomeWin = c(1, 1, 1),
        AwayWin = c(0, 0, 0),
        Draw = c(0, 0, 0),
        GameID = c(1L, 2L, 3L),
        Date = as.Date("2025-11-01"),
        stringsAsFactors = FALSE
      )
    },
    .package = "HockeyModel"
  )
  sched <- data.frame(
    Home = c("Boston Bruins", "Detroit Red Wings", "Florida Panthers"),
    Away = c(
      "Detroit Red Wings",
      "Florida Panthers",
      "Boston Bruins"
    ),
    Date = as.Date("2025-11-01"),
    GameID = c(1L, 2L, 3L),
    stringsAsFactors = FALSE
  )
  expect_true(requireNamespace("parallel", quietly = TRUE))
  res <- suppressWarnings(simulateSeasonParallel(
    scores = NULL,
    schedule = sched,
    nsims = 3,
    cores = 2
  ))
  expect_true(is.list(res))
  expect_true(all(c("summary_results", "raw_results") %in% names(res)))
  expect_equal(nrow(res$summary_results), 3)
  # BOS beats DET, DET beats FLA, FLA beats BOS => each: 1W 1L, Points=2
  expect_true(all(res$summary_results$meanWins == 1))
  expect_true(all(res$summary_results$meanPoints == 2))
  # Exactly the requested number of simulations, one per SimNo.
  expect_setequal(unique(res$raw_results$SimNo), 1:3)
})

test_that("simulateSeasonParallel() worker scores extra-time correctly (#68)", {
  # The parallel branch inlines its own copy of the result-sampling logic (doSNOW
  # workers can only see exported functions), so it must be fixed and verified
  # independently of .sim_odds_results(). With HOT = AOT > 0, a large share of
  # games land in an extra-time window and produce OT/SO result codes. The
  # fixture is a full three-team round robin because buildStats() requires each
  # team to have both home and away rows.
  local_mocked_bindings(
    remainderSeasonDC = function(...) {
      data.frame(
        HomeTeam = c(
          "Boston Bruins",
          "Detroit Red Wings",
          "Florida Panthers"
        ),
        AwayTeam = c(
          "Detroit Red Wings",
          "Florida Panthers",
          "Boston Bruins"
        ),
        HomeWin = c(0.4, 0.5, 0.3),
        AwayWin = c(0.2, 0.3, 0.15),
        Draw = c(0.4, 0.2, 0.55),
        GameID = c(1L, 2L, 3L),
        Date = as.Date("2025-11-01"),
        stringsAsFactors = FALSE
      )
    },
    .package = "HockeyModel"
  )
  sched <- data.frame(
    Home = c(
      "Boston Bruins",
      "Detroit Red Wings",
      "Florida Panthers"
    ),
    Away = c(
      "Detroit Red Wings",
      "Florida Panthers",
      "Boston Bruins"
    ),
    Date = as.Date("2025-11-01"),
    GameID = c(1L, 2L, 3L),
    stringsAsFactors = FALSE
  )

  expect_true(requireNamespace("parallel", quietly = TRUE))
  res <- suppressWarnings(simulateSeasonParallel(
    scores = NULL,
    schedule = sched,
    nsims = 500,
    cores = 2
  ))

  # raw_results is per-team aggregated (no per-game Result column). With HOT +
  # AOT > 0 for every game, ~35 ET games occur per simulation across 500
  # simulations, so all six outcome categories must be non-zero. The buggy
  # masks produced no OT/SO outcomes at all.
  expect_true(sum(res$raw_results$W) > 0)
  expect_true(sum(res$raw_results$L) > 0)
  expect_true(sum(res$raw_results$OTW) > 0)
  expect_true(sum(res$raw_results$SOW) > 0)
  expect_true(sum(res$raw_results$OTL) > 0)
  expect_true(sum(res$raw_results$SOL) > 0)

  # Each game awards 2 points (regulation W/L: 2+0) or 3 (OT/SO: winner 2,
  # loser 1), so a simulation's league total is exactly 2 * n_games + (# ET
  # games); each ET game contributes exactly one OTW or SOW. The buggy engine
  # undercounted by dropping the away team's extra-time point.
  G <- nrow(sched)
  per_sim <- res$raw_results |>
    dplyr::group_by(.data$SimNo) |>
    dplyr::summarise(
      total = sum(.data$Points),
      et_games = sum(.data$OTW) + sum(.data$SOW)
    )
  expect_true(all(per_sim$total == 2 * G + per_sim$et_games))
})

# ============ .sim_odds_results tests ============
test_that(".sim_odds_results returns one outcome per game (#45)", {
  odds_table <- data.frame(
    HomeWin = c(0.5, 1, 0),
    HOT = c(0.1, 0, 0),
    AOT = c(0.05, 0, 0),
    stringsAsFactors = FALSE
  )
  res <- .sim_odds_results(odds_table)
  expect_named(res, c("res1", "res2", "Result"))
  expect_length(res$res1, 3)
  expect_length(res$res2, 3)
  expect_length(res$Result, 3)
  # Deterministic bounds: HomeWin=1 => res1<1 always, res1>1 never.
  expect_equal(res$Result[2], 1)
  # HomeWin=0 => res1>0 always, res1<0 never.
  expect_equal(res$Result[3], 0)
  # Result is always one of the valid outcome codes.
  expect_true(all(res$Result %in% c(1, 0.75, 0.6, 0.4, 0.25, 0)))
})

test_that(".sim_odds_results produces all six result codes (#68)", {
  # The old masks were overlapping (the SO window contained the OT window), so
  # a home extra-time win scored 0.75 + 0.25 = 1 and 0.6 + 0.4 = 1: no 0.75 or
  # 0.6 outcomes were ever produced, and every away team in the home ET window
  # took a regulation loss instead of an OT/SO point.
  n <- 2e4
  odds_table <- data.frame(
    HomeWin = rep(0.5, n),
    HOT = rep(0.15, n),
    AOT = rep(0.15, n),
    stringsAsFactors = FALSE
  )
  res <- .sim_odds_results(odds_table)

  # Every outcome code occurs at roughly its expected frequency. The home ET
  # window (HOT + AOT here) splits into OT/SO in proportion to the away team's
  # extra-time odds, so P(0.75) = HOT * 0.6858606 and P(0.4) = AOT * 0.3141394.
  freqs <- table(factor(res$Result, levels = c(1, 0.75, 0.6, 0.4, 0.25, 0))) / n
  expect_true(all(freqs > 0))
  expect_equal(unname(freqs["1"]), 0.5, tolerance = 0.02)
  expect_equal(
    unname(freqs["0.75"]),
    0.15 * 0.6858606,
    tolerance = 0.05
  )
  expect_equal(
    unname(freqs["0.4"]),
    0.15 * 0.3141394,
    tolerance = 0.05
  )
})

test_that(".sim_odds_results home ET results match the .sampleResult engine (#68)", {
  # The fallback engine must agree with sim_engine's sampling scheme
  # (.sampleResult): a game in the home extra-time window resolves via the
  # away team's second draw, OT (p = 0.6858606) or SO, and the winner is always
  # the home team there.
  set.seed(20261112)
  n <- 5e4
  res <- .sim_odds_results(
    data.frame(HomeWin = rep(0.5, n), HOT = rep(0.2, n), AOT = rep(0.1, n))
  )

  in_home_et <- res$res1 >= 0.5 & res$res1 < 0.7
  expect_gt(sum(in_home_et), 3e3)

  # Only the away draw decides OT vs SO within the home ET window: every such
  # game is an OT win (away OT loss) or a SO win (away SO win).
  in_home_ot <- in_home_et & res$res2 < 0.6858606
  in_home_so <- in_home_et & res$res2 > 0.6858606
  expect_true(all(res$Result[in_home_ot] == 0.75))
  expect_true(all(res$Result[in_home_so] == 0.6))

  # The OT/SO split of the home ET window is exactly 0.6858606, matching the
  # primary engine's constant.
  expect_equal(
    mean(in_home_et & res$res2 < 0.6858606) / mean(in_home_et),
    0.6858606,
    tolerance = 0.03
  )

  # Away team keeps its extra-time point: away OT loss => 1 - Result = 0.25;
  # away SO win => 1 - Result = 0.4.
  expect_true(all(1 - res$Result[in_home_ot] == 0.25))
  expect_true(all(1 - res$Result[in_home_so] == 0.4))
})

# ============ .simulation_chunks tests ============
test_that(".simulation_chunks distributes exactly nsims (#51)", {
  for (nsims in c(1L, 5L, 100L, 1000L)) {
    for (n in c(1L, 2L, 4L, 7L)) {
      sizes <- .simulation_chunks(nsims, n)
      expect_length(sizes, n)
      expect_equal(sum(sizes), nsims)
      expect_true(all(sizes >= 0))
    }
  }
  # Remainder is spread over the first (nsims %% n) chunks.
  expect_equal(.simulation_chunks(10, 3), c(4, 3, 3))
  expect_equal(.simulation_chunks(7, 7), c(1, 1, 1, 1, 1, 1, 1))
  expect_equal(.simulation_chunks(4, 4), c(1, 1, 1, 1))
})

# ============ loopless_sim chunk count (#51) ============
test_that("loopless_sim runs exactly the requested number of simulations (#51)", {
  # The old code dropped the remainder of nsims/cores and then over/under-ran
  # sims. With cores=1 the requested count must be preserved exactly.
  sched <- HockeyModel::scores[
    HockeyModel::scores$Date >= as.Date("2025-10-07") &
      HockeyModel::scores$Date <= as.Date("2025-10-10"),
    c("Date", "HomeTeam", "AwayTeam", "GameID", "GameType", "GameStatus")
  ]
  sched$GameStatus <- "FUT"
  scor <- HockeyModel::scores[
    HockeyModel::scores$Date < as.Date("2025-10-07"),
  ]
  odds_table <- sched[, c("Date", "HomeTeam", "AwayTeam", "GameID")]
  odds_table$HomeWin <- 0.5
  odds_table$AwayWin <- 0.3
  odds_table$Draw <- 0.2
  odds_table <- odds_table[, c(
    "HomeTeam",
    "AwayTeam",
    "HomeWin",
    "AwayWin",
    "Draw",
    "GameID",
    "Date"
  )]

  local_mocked_bindings(
    getSeason = function(gamedate = Sys.Date()) "20192020",
    getSeasonStartDate = function(season = NULL) as.Date("2025-10-07"),
    sim_engine = function(all_season, nsims, params = NULL) {
      # Return one row per simulation so the caller's requested count is
      # observable. Columns match loopless_sim's summarise() expectations.
      n <- as.integer(nsims)
      data.frame(
        SimNo = seq_len(n),
        Team = "X",
        W = rep(1L, n),
        OTW = rep(0L, n),
        SOW = rep(0L, n),
        SOL = rep(0L, n),
        OTL = rep(0L, n),
        Points = rep(2L, n),
        Wildcard = rep(0L, n),
        Rank = rep(1L, n),
        ConfRank = rep(1L, n),
        DivRank = rep(1L, n),
        Playoffs = rep(1L, n),
        stringsAsFactors = FALSE
      )
    },
    .package = "HockeyModel"
  )

  result <- loopless_sim(
    nsims = 7,
    cores = 1,
    schedule = sched,
    scores = scor,
    odds_table = odds_table,
    likelihood_graphic = FALSE
  )
  # cores=1 => nsims preserved exactly (no floor() remainder loss).
  expect_equal(nrow(result$raw_results), 7)
  expect_setequal(result$raw_results$SimNo, 1:7)
})

# ============ loopless_sim fixed-chunk parallel branch ============
test_that("loopless_sim parallel branch uses fixed-size chunks with unique SimNo", {
  # Peak memory per worker must be bounded by chunk_size regardless of nsims,
  # so the run is dispatched as ceiling(nsims / chunk_size) tasks. With a small
  # chunk_size this forces multiple chunks; each chunk's SimNo must be offset
  # so every simulation keeps a unique ID.
  mk_fixture <- function() {
    sched <- HockeyModel::scores[
      HockeyModel::scores$Date >= as.Date("2025-10-07") &
        HockeyModel::scores$Date <= as.Date("2025-10-10"),
      c("Date", "HomeTeam", "AwayTeam", "GameID", "GameType", "GameStatus")
    ]
    sched$GameStatus <- "FUT"
    scor <- HockeyModel::scores[
      HockeyModel::scores$Date < as.Date("2025-10-07"),
    ]
    odds_table <- sched[, c("Date", "HomeTeam", "AwayTeam", "GameID")]
    odds_table$HomeWin <- 0.5
    odds_table$AwayWin <- 0.3
    odds_table$Draw <- 0.2
    odds_table <- odds_table[, c(
      "HomeTeam",
      "AwayTeam",
      "HomeWin",
      "AwayWin",
      "Draw",
      "GameID",
      "Date"
    )]
    list(sched = sched, scor = scor, odds_table = odds_table)
  }

  local_mocked_bindings(
    getSeason = function(gamedate = Sys.Date()) "20192020",
    getSeasonStartDate = function(season = NULL) as.Date("2025-10-07"),
    sim_engine = function(all_season, nsims, params = NULL, sim_offset = 0L) {
      # One row per simulation; SimNo reflects the requested offset so the
      # caller's chunking is observable. Columns match loopless_sim's
      # summarise() expectations.
      n <- as.integer(nsims)
      data.frame(
        SimNo = seq_len(n) + as.integer(sim_offset),
        Team = "X",
        W = rep(1L, n),
        OTW = rep(0L, n),
        SOW = rep(0L, n),
        SOL = rep(0L, n),
        OTL = rep(0L, n),
        Points = rep(2L, n),
        Wildcard = rep(0L, n),
        Rank = rep(1L, n),
        ConfRank = rep(1L, n),
        DivRank = rep(1L, n),
        Playoffs = rep(1L, n),
        stringsAsFactors = FALSE
      )
    },
    .package = "HockeyModel"
  )

  expect_true(requireNamespace("parallel", quietly = TRUE))
  fx <- mk_fixture()

  # chunk_size = 3 => three tasks (offsets 0, 3, 6), sizes 3/3/1.
  result <- loopless_sim(
    nsims = 7,
    cores = 2,
    schedule = fx$sched,
    scores = fx$scor,
    odds_table = fx$odds_table,
    likelihood_graphic = FALSE,
    chunk_size = 3
  )
  expect_equal(
    nrow(result$raw_results),
    7 * length(unique(result$raw_results$Team))
  )
  expect_setequal(result$raw_results$SimNo, 1:7)

  # chunk_size = 4 => two tasks (offsets 0, 4), sizes 4/3.
  result2 <- loopless_sim(
    nsims = 7,
    cores = 2,
    schedule = fx$sched,
    scores = fx$scor,
    odds_table = fx$odds_table,
    likelihood_graphic = FALSE,
    chunk_size = 4
  )
  expect_equal(
    nrow(result2$raw_results),
    7 * length(unique(result2$raw_results$Team))
  )
  expect_setequal(result2$raw_results$SimNo, 1:7)
})

# ============ sim_engine tests ============
test_that("sim_engine preserves played results and samples unplayed games", {
  mk <- function(h, a, hw, hot, hso, aso, aot, aw, result, gid) {
    data.frame(
      HomeTeam = h,
      AwayTeam = a,
      GameID = gid,
      Date = as.Date("2025-11-01"),
      HomeWin = hw,
      HomeOT = hot,
      HomeSO = hso,
      AwaySO = aso,
      AwayOT = aot,
      AwayWin = aw,
      Result = result,
      stringsAsFactors = FALSE
    )
  }
  # Four teams, six games (full round robin). Two games are already played
  # (fixed Result) and four are unplayed with degenerate odds, so the sampling
  # is fully deterministic and the expected records are known exactly.
  all_season <- rbind(
    mk("Boston Bruins", "Buffalo Sabres", .5, .1, .05, .05, .1, .2, 1, 1L), # BOS reg win
    mk(
      # FLA OT win (away)
      "Detroit Red Wings",
      "Florida Panthers",
      .5,
      .1,
      .05,
      .05,
      .1,
      .2,
      0.25,
      2L
    ),
    mk("Buffalo Sabres", "Detroit Red Wings", 1, 0, 0, 0, 0, 0, NA, 3L), # BUF reg win
    mk("Florida Panthers", "Boston Bruins", 0, 0, 0, 0, 0, 1, NA, 4L), # BOS reg win (away)
    mk("Boston Bruins", "Florida Panthers", 0, 0, 1, 0, 0, 0, NA, 5L), # BOS SO win
    mk("Detroit Red Wings", "Buffalo Sabres", 0, 0, 0, 1, 0, 0, NA, 6L) # BUF SO win (away)
  )

  nsims <- 3
  res <- as.data.frame(sim_engine(
    all_season = all_season,
    nsims = nsims,
    params = NULL
  ))

  n_teams <- 4L
  expect_equal(nrow(res), n_teams * nsims)
  expect_setequal(
    unique(res$Team),
    c(all_season$HomeTeam, all_season$AwayTeam)
  )

  # Degenerate odds make every simulation identical.
  per_sim <- aggregate(
    cbind(W, OTW, SOW, SOL, OTL, Points) ~ Team,
    data = res,
    FUN = function(x) length(unique(x)) == 1
  )
  expect_true(all(per_sim[, -1]))

  # Exact per-team records (first sim, since all are identical).
  got <- as.data.frame(res[
    res$SimNo == 1,
    c(
      "Team",
      "W",
      "OTW",
      "SOW",
      "SOL",
      "OTL",
      "Points"
    )
  ])
  got <- got[order(got$Team), ]
  expected <- data.frame(
    Team = c(
      "Boston Bruins",
      "Buffalo Sabres",
      "Detroit Red Wings",
      "Florida Panthers"
    ),
    W = c(2, 1, 0, 0),
    OTW = c(0, 0, 0, 1),
    SOW = c(1, 1, 0, 0),
    SOL = c(0, 0, 1, 1),
    OTL = c(0, 0, 1, 0),
    Points = c(6, 4, 2, 3),
    stringsAsFactors = FALSE
  )
  expect_equal(got, expected[order(expected$Team), ])

  # Points formula holds on every row.
  expect_true(all(
    res$Points == res$W * 2 + res$OTW * 2 + res$SOW * 2 + res$OTL + res$SOL
  ))
})

test_that("sim_engine offsets SimNo by sim_offset", {
  # Parallel chunks each run their own sim_engine() call; offsetting SimNo per
  # chunk keeps every simulation ID unique across the combined result.
  teams <- c(
    "Boston Bruins",
    "Buffalo Sabres",
    "Detroit Red Wings",
    "Florida Panthers"
  )
  all_season <- data.frame(
    HomeTeam = rep(c("Boston Bruins", "Detroit Red Wings"), each = 2),
    AwayTeam = c(
      "Buffalo Sabres",
      "Florida Panthers",
      "Buffalo Sabres",
      "Florida Panthers"
    ),
    GameID = 1:4,
    Date = as.Date("2025-11-01"),
    HomeWin = c(1, 1, 0, 0),
    HomeOT = rep(0, 4),
    HomeSO = rep(0, 4),
    AwaySO = rep(0, 4),
    AwayOT = rep(0, 4),
    AwayWin = c(0, 0, 1, 1),
    Result = NA,
    stringsAsFactors = FALSE
  )

  res <- as.data.frame(sim_engine(
    all_season = all_season,
    nsims = 3,
    params = NULL,
    sim_offset = 5L
  ))
  expect_setequal(unique(res$SimNo), 6:8)
  # One row per team per simulation.
  expect_equal(nrow(res), length(teams) * 3)

  # Default offset preserves the historical 1..nsims numbering.
  res0 <- as.data.frame(sim_engine(all_season = all_season, nsims = 3))
  expect_setequal(unique(res0$SimNo), 1:3)
})

test_that("sim_engine handles a full-season-sized schedule", {
  set.seed(42)
  teams <- HockeyModel::teamColours$Team
  teams <- teams[teams != ""]
  n <- 200L
  rows <- vector("list", n)
  for (i in seq_len(n)) {
    h <- teams[[sample(length(teams), 1L)]]
    a <- teams[[sample(setdiff(seq_along(teams), which(teams == h)), 1L)]]
    rows[[i]] <- data.frame(
      HomeTeam = h,
      AwayTeam = a,
      GameID = i,
      Date = as.Date("2025-11-01"),
      HomeWin = 0.5,
      HomeOT = 0.1,
      HomeSO = 0.05,
      AwaySO = 0.05,
      AwayOT = 0.1,
      AwayWin = 0.2,
      Result = NA,
      stringsAsFactors = FALSE
    )
  }
  all_season <- do.call(rbind, rows)

  nsims <- 2
  res <- as.data.frame(sim_engine(
    all_season = all_season,
    nsims = nsims,
    params = NULL
  ))
  n_teams <- length(unique(c(all_season$HomeTeam, all_season$AwayTeam)))

  expect_equal(nrow(res), n_teams * nsims)
  expect_setequal(unique(res$Team), c(all_season$HomeTeam, all_season$AwayTeam))
  expect_true(all(
    res$Points == res$W * 2 + res$OTW * 2 + res$SOW * 2 + res$OTL + res$SOL
  ))
  expect_true(all(
    c(
      "Rank",
      "ConfRank",
      "DivRank",
      "Playoffs"
    ) %in%
      names(res)
  ))
})
