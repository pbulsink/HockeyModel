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
    todayDC = function(...) {
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
