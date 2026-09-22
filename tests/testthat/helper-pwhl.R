# Shared PWHL test helpers used across test-pwhl-model-core.R and
# test-nhl-dixon-coles-workflow.R (PWHL todayDC tests)

# Minimal sample PWHL scores for offline testing
make_pwhl_scores <- function(n_games = 20) {
  if (n_games == 0) {
    return(data.frame(
      Date = as.Date(character()),
      HomeTeam = character(),
      AwayTeam = character(),
      GameID = integer(),
      HomeGoals = integer(),
      AwayGoals = integer(),
      OTStatus = character(),
      GameType = character(),
      GameStatus = character(),
      stringsAsFactors = FALSE
    ))
  }
  teams <- c(
    "Boston Fleet",
    "Minnesota Frost",
    "Montreal Victoire",
    "New York Sirens",
    "Ottawa Charge",
    "Toronto Sceptres"
  )
  set.seed(42)
  matchups <- expand.grid(HomeTeam = teams, AwayTeam = teams)
  matchups <- matchups[matchups$HomeTeam != matchups$AwayTeam, ]
  matchups <- matchups[rep(seq_len(nrow(matchups)), length.out = n_games), ]

  home_goals <- sample(1:5, n_games, replace = TRUE)
  away_goals <- sample(1:5, n_games, replace = TRUE)
  # OT games: ensure goals differ (winner scored extra goal)
  ot_status <- rep("", n_games)
  # Use ~20% OT rate, capped at available diff-goal games, minimum 5 for model fitting
  diff_idx <- which(home_goals != away_goals)
  n_ot <- min(max(5L, floor(n_games * 0.2)), length(diff_idx))
  ot_idx <- diff_idx[seq_len(n_ot)]
  ot_status[ot_idx] <- sample(c("OT", "SO"), n_ot, replace = TRUE)

  data.frame(
    Date = seq.Date(as.Date("2024-01-01"), by = "3 days", length.out = n_games),
    HomeTeam = as.character(matchups$HomeTeam),
    AwayTeam = as.character(matchups$AwayTeam),
    GameID = seq_len(n_games),
    HomeGoals = home_goals,
    AwayGoals = away_goals,
    OTStatus = ot_status,
    GameType = "R",
    GameStatus = "Final",
    stringsAsFactors = FALSE
  )
}

make_pwhl_schedule <- function(scores) {
  teams <- c(
    "Boston Fleet",
    "Minnesota Frost",
    "Montreal Victoire",
    "New York Sirens",
    "Ottawa Charge",
    "Toronto Sceptres"
  )
  matchups <- expand.grid(HomeTeam = teams, AwayTeam = teams)
  matchups <- matchups[matchups$HomeTeam != matchups$AwayTeam, ]
  matchups <- matchups[seq_len(10), ]
  n <- nrow(matchups)
  data.frame(
    Date = seq.Date(max(scores$Date) + 3, by = "3 days", length.out = n),
    HomeTeam = as.character(matchups$HomeTeam),
    AwayTeam = as.character(matchups$AwayTeam),
    GameID = max(scores$GameID) + seq_len(n),
    GameType = "R",
    GameStatus = "Scheduled",
    stringsAsFactors = FALSE
  )
}

# Build DC params without running Weibull optimisation (for speed in tests
# that don't specifically test updatePWHLDC)
make_pwhl_params <- function(scores) {
  sc <- pwhl_add_result(scores)
  m <- getM(scores = sc, currentDate = max(sc$Date) + 1)
  rho <- getRho(m = m, scores = sc)
  list(m = m, rho = rho, beta = 2.5, eta = 1.5, k = 5.0)
}
