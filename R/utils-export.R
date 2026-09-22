# General export-formatting utilities

#' Format predictions for the HockeyViz contest submission string
#'
#' @param predictions (`data.frame`) Predictions containing `Team`,
#'   `meanPoints`, and `sdPoints`.
#' @param candyType (`character(1)`) Preferred sour candy label.
#' @param handle (`character(1)`) Contest handle to include in output.
#' @returns (`character(1)`) Serialized contest payload string.
#' @keywords internal
formatPredsForHockeyVisContest <- function(
  predictions,
  candyType = "Fuzzy Peaches",
  handle = "@bot.bulsink.ca"
) {
  # IneffectiveMath's (Micah's) contest
  # format = {'handle':'???','preferredSourCandyType':'???', 'Predictions':{ 'ANA':(m,u), 'ARI':(m,u), 'BOS':(m,u), 'BUF':(m,u), 'CAR':(m,u), 'CBJ':(m,u), 'CGY':(m,u), 'CHI':(m,u), 'COL':(m,u), 'DAL':(m,u), 'DET':(m,u), 'EDM':(m,u), 'FLA':(m,u), 'L.A':(m,u), 'MIN':(m,u), 'MTL':(m,u), 'N.J':(m,u), 'NSH':(m,u), 'NYI':(m,u), 'NYR':(m,u), 'OTT':(m,u), 'PHI':(m,u), 'PIT':(m,u), 'S.J':(m,u), 'STL':(m,u), 'T.B':(m,u), 'TOR':(m,u), 'VAN':(m,u), 'VGK':(m,u), 'WPG':(m,u), 'WSH':(m,u), } }
  output <- paste0(
    "{'handle':'",
    handle,
    "', 'preferredSourCandyType':'",
    candyType,
    "', 'Predictions':{ ",
    "'ANA':(",
    round(predictions[predictions$Team == "Anaheim Ducks", ]$meanPoints, 1),
    ",",
    round(predictions[predictions$Team == "Anaheim Ducks", ]$sdPoints, 2),
    "), ",
    "'BOS':(",
    round(predictions[predictions$Team == "Boston Bruins", ]$meanPoints, 1),
    ",",
    round(predictions[predictions$Team == "Boston Bruins", ]$sdPoints, 2),
    "), ",
    "'BUF':(",
    round(predictions[predictions$Team == "Buffalo Sabres", ]$meanPoints, 1),
    ",",
    round(predictions[predictions$Team == "Buffalo Sabres", ]$sdPoints, 2),
    "), ",
    "'CAR':(",
    round(
      predictions[predictions$Team == "Carolina Hurricanes", ]$meanPoints,
      1
    ),
    ",",
    round(predictions[predictions$Team == "Carolina Hurricanes", ]$sdPoints, 2),
    "), ",
    "'CBJ':(",
    round(
      predictions[predictions$Team == "Columbus Blue Jackets", ]$meanPoints,
      1
    ),
    ",",
    round(
      predictions[predictions$Team == "Columbus Blue Jackets", ]$sdPoints,
      2
    ),
    "), ",
    "'CGY':(",
    round(predictions[predictions$Team == "Calgary Flames", ]$meanPoints, 1),
    ",",
    round(predictions[predictions$Team == "Calgary Flames", ]$sdPoints, 2),
    "), ",
    "'CHI':(",
    round(
      predictions[predictions$Team == "Chicago Blackhawks", ]$meanPoints,
      1
    ),
    ",",
    round(predictions[predictions$Team == "Chicago Blackhawks", ]$sdPoints, 2),
    "), ",
    "'COL':(",
    round(
      predictions[predictions$Team == "Colorado Avalanche", ]$meanPoints,
      1
    ),
    ",",
    round(predictions[predictions$Team == "Colorado Avalanche", ]$sdPoints, 2),
    "), ",
    "'DAL':(",
    round(predictions[predictions$Team == "Dallas Stars", ]$meanPoints, 1),
    ",",
    round(predictions[predictions$Team == "Dallas Stars", ]$sdPoints, 2),
    "), ",
    "'DET':(",
    round(predictions[predictions$Team == "Detroit Red Wings", ]$meanPoints, 1),
    ",",
    round(predictions[predictions$Team == "Detroit Red Wings", ]$sdPoints, 2),
    "), ",
    "'EDM':(",
    round(predictions[predictions$Team == "Edmonton Oilers", ]$meanPoints, 1),
    ",",
    round(predictions[predictions$Team == "Edmonton Oilers", ]$sdPoints, 2),
    "), ",
    "'FLA':(",
    round(predictions[predictions$Team == "Florida Panthers", ]$meanPoints, 1),
    ",",
    round(predictions[predictions$Team == "Florida Panthers", ]$sdPoints, 2),
    "), ",
    "'L.A':(",
    round(predictions[predictions$Team == "Los Angeles Kings", ]$meanPoints, 1),
    ",",
    round(predictions[predictions$Team == "Los Angeles Kings", ]$sdPoints, 2),
    "), ",
    "'MIN':(",
    round(predictions[predictions$Team == "Minnesota Wild", ]$meanPoints, 1),
    ",",
    round(predictions[predictions$Team == "Minnesota Wild", ]$sdPoints, 2),
    "), ",
    "'MTL':(",
    round(
      predictions[predictions$Team == "Montreal Canadiens", ]$meanPoints,
      1
    ),
    ",",
    round(predictions[predictions$Team == "Montreal Canadiens", ]$sdPoints, 2),
    "), ",
    "'N.J':(",
    round(predictions[predictions$Team == "New Jersey Devils", ]$meanPoints, 1),
    ",",
    round(predictions[predictions$Team == "New Jersey Devils", ]$sdPoints, 2),
    "), ",
    "'NSH':(",
    round(
      predictions[predictions$Team == "Nashville Predators", ]$meanPoints,
      1
    ),
    ",",
    round(predictions[predictions$Team == "Nashville Predators", ]$sdPoints, 2),
    "), ",
    "'NYI':(",
    round(
      predictions[predictions$Team == "New York Islanders", ]$meanPoints,
      1
    ),
    ",",
    round(predictions[predictions$Team == "New York Islanders", ]$sdPoints, 2),
    "), ",
    "'NYR':(",
    round(predictions[predictions$Team == "New York Rangers", ]$meanPoints, 1),
    ",",
    round(predictions[predictions$Team == "New York Rangers", ]$sdPoints, 2),
    "), ",
    "'OTT':(",
    round(predictions[predictions$Team == "Ottawa Senators", ]$meanPoints, 1),
    ",",
    round(predictions[predictions$Team == "Ottawa Senators", ]$sdPoints, 2),
    "), ",
    "'PHI':(",
    round(
      predictions[predictions$Team == "Philadelphia Flyers", ]$meanPoints,
      1
    ),
    ",",
    round(predictions[predictions$Team == "Philadelphia Flyers", ]$sdPoints, 2),
    "), ",
    "'PIT':(",
    round(
      predictions[predictions$Team == "Pittsburgh Penguins", ]$meanPoints,
      1
    ),
    ",",
    round(predictions[predictions$Team == "Pittsburgh Penguins", ]$sdPoints, 2),
    "), ",
    "'S.J':(",
    round(predictions[predictions$Team == "San Jose Sharks", ]$meanPoints, 1),
    ",",
    round(predictions[predictions$Team == "San Jose Sharks", ]$sdPoints, 2),
    "), ",
    "'SEA':(",
    round(predictions[predictions$Team == "Seattle Kraken", ]$meanPoints, 1),
    ",",
    round(predictions[predictions$Team == "Seattle Kraken", ]$sdPoints, 2),
    "), ",
    "'STL':(",
    round(predictions[predictions$Team == "St. Louis Blues", ]$meanPoints, 1),
    ",",
    round(predictions[predictions$Team == "St. Louis Blues", ]$sdPoints, 2),
    "), ",
    "'T.B':(",
    round(
      predictions[predictions$Team == "Tampa Bay Lightning", ]$meanPoints,
      1
    ),
    ",",
    round(predictions[predictions$Team == "Tampa Bay Lightning", ]$sdPoints, 2),
    "), ",
    "'TOR':(",
    round(
      predictions[predictions$Team == "Toronto Maple Leafs", ]$meanPoints,
      1
    ),
    ",",
    round(predictions[predictions$Team == "Toronto Maple Leafs", ]$sdPoints, 2),
    "), ",
    "'UTA':(",
    round(predictions[predictions$Team == "Utah Mammoth", ]$meanPoints, 1),
    ",",
    round(predictions[predictions$Team == "Utah Hockey Club", ]$sdPoints, 2),
    "), ",
    "'VAN':(",
    round(predictions[predictions$Team == "Vancouver Canucks", ]$meanPoints, 1),
    ",",
    round(predictions[predictions$Team == "Vancouver Canucks", ]$sdPoints, 2),
    "), ",
    "'VGK':(",
    round(
      predictions[predictions$Team == "Vegas Golden Knights", ]$meanPoints,
      1
    ),
    ",",
    round(
      predictions[predictions$Team == "Vegas Golden Knights", ]$sdPoints,
      2
    ),
    "), ",
    "'WPG':(",
    round(predictions[predictions$Team == "Winnipeg Jets", ]$meanPoints, 1),
    ",",
    round(predictions[predictions$Team == "Winnipeg Jets", ]$sdPoints, 2),
    "), ",
    "'WSH':(",
    round(
      predictions[predictions$Team == "Washington Capitals", ]$meanPoints,
      1
    ),
    ",",
    round(predictions[predictions$Team == "Washington Capitals", ]$sdPoints, 2),
    "), ",
    "} }"
  )
  return(output)
}
