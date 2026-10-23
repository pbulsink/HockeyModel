# Package-level weighting constants for the Dixon-Coles model.
#
# These values control how `.DCweights()` (and therefore `getM()`) decays the
# influence of historical games.  Two separate components are used:
#
#   1. **Sigmoid within-season decay** (parameters xi and upsilon)
#      w_time = 1 - 1 / (1 + exp(-xi * (days_old - upsilon)))
#      Recent games receive weight near 1; games older than ~upsilon days
#      receive weight near 0.
#
#   2. **Cross-season multiplier** (parameter nu)
#      w_season = 1 / (1 + s^nu)
#      where s = 0 for the current season, 1 for the previous season, etc.
#      Larger nu more aggressively discounts older seasons.
#      nu = 0 disables the multiplier (backward-compatible default for NHL).
#
# NHL values were originally tuned on full NHL history; PWHL values use
# nu = 5 to reflect the high year-to-year roster churn from expansion
# drafts.  Run `.tune_dc_weight(league = "PWHL")` to find the optimal
# values.

#' Default NHL Dixon-Coles sigmoid time-decay slope
#'
#' `xi` controls how steeply the within-season logistic weighting curve drops.
#' @keywords internal
DC_XI_NHL <- 0.00426

#' Default NHL Dixon-Coles sigmoid time-decay midpoint
#'
#' `upsilon` is the inflection point of the within-season logistic curve (days).
#' @keywords internal
DC_UPSILON_NHL <- 365

#' Default NHL Dixon-Coles cross-season discounting exponent
#'
#' `nu = 0` disables cross-season discounting for NHL (seasons are relatively
#' stable year-to-year).  Increase to down-weight older seasons more
#' aggressively.  Tune with [.tune_dc_weight()].
#' @keywords internal
DC_NU_NHL <- 0

#' Default PWHL Dixon-Coles sigmoid time-decay slope
#'
#' `xi` controls how steeply the within-season logistic weighting curve drops.
#'   PWHL uses a steeper slope than NHL to let recent games dominate the fit.
#' @keywords internal
DC_XI_PWHL <- 0.0500

#' Default PWHL Dixon-Coles sigmoid time-decay midpoint
#'
#' `upsilon` is the inflection point of the within-season logistic curve
#'   (days), tuned for the PWHL calendar.
#' @keywords internal
DC_UPSILON_PWHL <- 461.0156

#' Default PWHL Dixon-Coles cross-season discounting exponent
#'
#' `nu = 5` provides aggressive cross-season discounting for PWHL, reflecting
#' the high year-to-year roster volatility from expansion drafts (older
#' seasons carry little signal about the current roster).  Tune with
#' `.tune_dc_weight(league = "PWHL")`.
#' @keywords internal
DC_NU_PWHL <- 5

# Back-to-back (b2b) adjustment constants.
#
# NHL lore says a team playing its second game in two days performs worse, so
# the model subtracts a small amount from both that team's offensive and
# defensive ratings for the affected game.  Because the model is a log-link
# Poisson GLM, this is exactly equivalent to scaling the predicted rates:
#   - b2b team at home : lambda *= exp(-B2B_HOME_NHL), mu *= exp(+B2B_HOME_NHL)
#   - b2b team on road : mu     *= exp(-B2B_AWAY_NHL), lambda *= exp(+B2B_AWAY_NHL)
# where lambda/mu are the home/away expected goals.  The two magnitudes are
# tuned separately against historical log-loss (see `.tune_b2b()`).

#' Default NHL back-to-back adjustment for a b2b team playing at home
#'
#' Log-scale subtraction applied to a b2b team's attack and defensive-strength
#' coefficients when it plays at home.  On the log link this scales the home
#' expected goals by `exp(-B2B_HOME_NHL)` and the away expected goals against
#' it by `exp(+B2B_HOME_NHL)`.  Tune with [.tune_b2b()].
#' @keywords internal
B2B_HOME_NHL <- 0.037

#' Default NHL back-to-back adjustment for a b2b team playing on the road
#'
#' Log-scale subtraction applied to a b2b team's attack and defensive-strength
#' coefficients when it plays away.  On the log link this scales the away
#' expected goals by `exp(-B2B_AWAY_NHL)` and the home expected goals against
#' it by `exp(+B2B_AWAY_NHL)`.  Tune with [.tune_b2b()].
#' @keywords internal
B2B_AWAY_NHL <- 0.0645
