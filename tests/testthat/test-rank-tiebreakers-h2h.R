# Focused tests for the head-to-head stage of .rank_by_tiebreakers (#94).

mk <- function(Points, GP, W, OTW = 0L, SOW = 0L) {
  data.frame(
    Team = "A",
    Points = Points,
    GP = GP,
    W = W,
    OTW = OTW,
    SOW = SOW,
    stringsAsFactors = FALSE
  )
}

test_that("multi-way H2H percentage excludes odd games from the denominator (#94)", {
  # Three teams tied on every criterion (P4 GP5 W2). A-B meet once (odd ->
  # excluded entirely), B-C and A-C each meet twice. Kept-meeting points:
  #   A 2 (splits with C), B 3 (OT win + regulation win over C), C 3 (one over
  #   A, one over B). Available kept points: A 4, B 4, C 8 -> A 0.5, B 0.75,
  #   C 0.375, so A must rank above C. Under the old all-games denominator
  #   (A 2/6, B 3/6, C 3/8) C ranked above A, which is what this guards against.
  games <- data.frame(
    HomeTeam = c("A", "A", "C", "B", "C"),
    AwayTeam = c("B", "C", "A", "C", "B"),
    Date = as.Date(c(
      "2025-11-01",
      "2025-11-08",
      "2025-12-06",
      "2025-12-13",
      "2026-01-10"
    )),
    home_points = c(0, 0, 0, 1, 0), # g4 is an OT win for B (C gets the point)
    away_points = c(2, 2, 2, 1, 2)
  )
  df <- rbind(mk(4, 5, 2), mk(4, 5, 2), mk(4, 5, 2))
  df$Team <- c("A", "B", "C")

  # A (0.5) ranks above C (0.375); only true when the odd A-B game is dropped
  # from the available-points denominator as well as the earned points.
  r <- .rank_by_tiebreakers(df, games = games)
  expect_true(r[1] < r[3])
})
