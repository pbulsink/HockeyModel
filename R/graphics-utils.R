# Graphics: team colour helper utilities

#' Get Team Colours
#'
#' @description Given a home and away team, return a set of two colours that correspond one to each team, yet are not too similar to eachother. Instead of returning Blue and Blue for Buffalo and Tampa, change Buffalo to their Gold
#'
#' @param home Home Team colours to get
#' @param away Away Team's colours to get
#' @param delta Colour delta required. Default 0.15. See [colourDelta]. Must be between 0 and 1
#' @param teamColours Team colours table. Defaults to [HockeyModel::teamColours]. Pass
#'   [HockeyModel::pwhlTeamColours] for PWHL teams.
#'
#' @return a list with two items: home & away, each containing the appropriate hex colour value
#' @export
#'
#' @examples
#' getTeamColours("Buffalo Sabres", "Tampa Bay Lightning")
getTeamColours <- function(
  home,
  away,
  delta = 0.15,
  teamColours = HockeyModel::teamColours
) {
  if (!home %in% teamColours$Team) {
    cli::cli_abort("{.arg home} ({.val {home}}) is not a recognized team.")
  }
  if (!away %in% teamColours$Team) {
    cli::cli_abort("{.arg away} ({.val {away}}) is not a recognized team.")
  }
  if (!is.numeric(delta)) {
    cli::cli_abort("{.arg delta} must be numeric.")
  }
  if (delta >= 1) {
    cli::cli_abort("{.arg delta} must be less than 1, got {delta}.")
  }
  if (delta < 0) {
    cli::cli_abort("{.arg delta} must be at least 0, got {delta}.")
  }

  # Get primary & alternate colour for home and away
  hprimary <- teamColours[teamColours$Team == home, "Hex"]
  aprimary <- teamColours[teamColours$Team == away, "Hex"]
  halt <- teamColours[teamColours$Team == home, "AltHex"]
  aalt <- teamColours[teamColours$Team == away, "AltHex"]

  # record deltas to send most different colour incase no best option appears
  ppdelta <- colourDelta(hprimary, aprimary)
  padelta <- colourDelta(hprimary, aalt)
  apdelta <- colourDelta(halt, aprimary)
  aadelta <- colourDelta(halt, aalt)

  if (ppdelta < delta) {
    # Colours too similar
    # Try away team alternate colour
    if (padelta < delta) {
      # Still too similar, try home team alternate
      if (apdelta < delta) {
        # Still too similar, try both alternates
        if (aadelta < delta) {
          # all too similar, just use best delta
          message("No Great Colour Separation")
          bestdelta <- max(c(ppdelta, padelta, apdelta, aadelta))
          if (ppdelta == bestdelta) {
            h <- hprimary
            a <- aprimary
          } else if (padelta == bestdelta) {
            h <- hprimary
            a <- aalt
          } else if (apdelta == bestdelta) {
            h <- halt
            a <- aprimary
          } else {
            h <- halt
            a <- aalt
          }
        } else {
          h <- halt
          a <- aalt
        }
      } else {
        h <- halt
        a <- aprimary
      }
    } else {
      h <- hprimary
      a <- aalt
    }
  } else {
    h <- hprimary
    a <- aprimary
  }

  return(list("home" = h, "away" = a))
}


#' Check Colour Delta
#'
#' @description Check the similarity of two colours by hex code. 0 = identical, 1 = opposite (black & white)
#'
#' @param hex1 colour one, as hex code #XXXXXX
#' @param hex2 colour two, as hex code #XXXXXX
#'
#' @return a value from 0 (identical) to 1 (completely opposite, black & white)
#' @export
#' @examples
#' # colour similarity between FLA and TBL primary colours
#' colourDelta("#041E42", "#002868")
colourDelta <- function(hex1, hex2) {
  c1 <- hexToRGB(hex1) / 255
  c2 <- hexToRGB(hex2) / 255
  delta <- abs(c1 - c2)
  deltaM <- mean(delta)
  deltaM
}


#' Hex to RGB
#'
#' @description convert Hex colours to RGB colours
#'
#' @param hex colour as hex code #XXXXXX
#'
#' @return vector of three numbers, R, G, B from 0 to 255
#' @export
#' @examples
#' hexToRGB("#FFFFFF")
hexToRGB <- function(hex) {
  r <- strtoi(paste0("0x", substr(hex, 2, 3)))
  g <- strtoi(paste0("0x", substr(hex, 4, 5)))
  b <- strtoi(paste0("0x", substr(hex, 6, 7)))
  return(c(r, g, b))
}
