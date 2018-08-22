# BradleyTerry2 model like chameleon

#' data<-dataBT2
#' format: nestedlist, for each season with list:
#' winner:
#' .. team (factor: team name)
#' .. date (date: game date)
#' .. game.number (integer: game number for the season
#' .. is.playoffs (binary: is a playoff game)
#' .. season (factor: season id (e.g. 20172018))
#' .. last.game (integer: number of points last game (0-2), new teams get 0)
#' .. last.20 (integer: # of points in last 20 games (0-40), new teams start with 0)
#' .. at.home (binary: is at home = 1)
#' .. CF.20 (numeric ev corsi for per game last 20 games average. new teams get average)
#' .. CA.20 (numeric ev corsi against per game last 20 games average. new teams get average)
#' .. Cp.20 (numeric: ev corsi for percent last 20 games. new teams get 0.5 (0-1))
#' .. hits.20 (numeric: number of hits per game last 20 games. new teams get average)
#' .. sh.20 (numeric: average number of shots per game last 20 games. new teams get average)
#' .. sa.20 (numeric: average number of shots against per game last 20 games. new teams get average)
#' .. sh.p20 (numeric: shooting percentage last 20 games average. new teams get average (0-1))
#' .. sv.p20 (numeric: save percentage last 20 games average. new teams get average (0-1))
#' .. days.since.last (numeric: days since last game, max 21)
#' .. days.till.next (numeric: days till next game, max 21)
#' .. shift.length (numeric: average shift length over last 20 games)
#' .. pim (numeric: average pim over last 20 games)
#' .. PDO.20 (numeric, Average PDO for last 20 games)
#' loser: same factors as winner
#' predictors:
#' .. last.year (integer: number of points last year)
#' .. conference (factor: conference name)
#' .. division (factor: division name)
#' .. value (integer: Forbes team value, in million (300-1500) see https://www.forbes.com/nhl-valuations/list/)
#' .. last.year.playoffs (binary: made playoffs last year)
#' .. cap.hit (numeric: % of cap space utilization. see https://www.capfriendly.com/)
#'
#' model: seasonBTm <- BTm(player1 = winner, player2 = loser, formula = ~ at.home + date + game.number + is.playoffs + season + last.game + last.10 + CF.20 + CF.p20 + hits.20 + sh.p20 + sv.p20 + b2b.1 + b2b.2 last.year[Team] + conference[Team] + division[Team] + value[Team] + last.year.playoffs[Team] + cap.hit[Team] + (1|Team), id = Team, data = dataBT2$[season])
#'
#' https://danieljhocking.wordpress.com/2014/12/03/lags-and-moving-means-in-dplyr/
#'
#'


btdatamaker<-function(){
  season_list <- unique(as3$Season)
  dataBT2 <- list()
  for (i in season_list) {
    as4 <- as3[as3$Season == i, ]
    winner <- as4[as4$Result > 0.5, ]
    loser <- as4[as4$Result < 0.5, ]
    pred <- btlookup[[i]]
    dataBT2[[i]] <- list(winner = winner, loser = loser, predictors=pred)
  }
}

modelBT2 <- function(dataBT2 = HockeyModel::dataBT2, season = NULL){
  if(is.null(season)){
    season<-names(dataBT2)
  }
  season<-season[season %in% names(dataBT2)]

  for (s in season){

  }
}

team <- c("Anaheim Ducks", "Arizona Coyotes", "Boston Bruins", "Buffalo Sabres", "Calgary Flames",
  "Carolina Hurricanes", "Chicago Blackhawks", "Colorado Avalanche", "Columbus Blue Jackets",
  "Dallas Stars", "Detroit Red Wings", "Edmonton Oilers", "Florida Panthers", "Los Angeles Kings",
  "Minnesota Wild", "Montreal Canadiens", "Nashville Predators", "New Jersey Devils",
  "New York Islanders", "New York Rangers", "Ottawa Senators", "Philadelphia Flyers",
  "Pittsburgh Penguins", "San Jose Sharks", "St. Louis Blues", "Tampa Bay Lightning",
  "Toronto Maple Leafs", "Vancouver Canucks", 'Vegas Golden Knights', "Washington Capitals", "Winnipeg Jets")

conference <- c("West", "West", "East", "East", "West", "East", "West", "West", "East",
  "West", "East", "West", "East", "West", "West", "East", "West", "East", "East",
  "East", "East", "East", "East", "West", "West", "East", "East", "West", "West",
  "East", "West")
division <- c("Pacific", "Pacific", "Atlantic", "Atlantic", "Pacific", "Metropolitan",
  "Central", "Central", "Metropolitan", "Central", "Atlantic", "Pacific", "Atlantic",
  "Pacific", "Central", "Atlantic", "Central", "Metropolitan", "Metropolitan",
  "Metropolitan", "Atlantic", "Metropolitan", "Metropolitan", "Pacific", "Central",
  "Atlantic", "Atlantic", "Pacific", "Pacific", "Metropolitan", "Central")

btlookup <- list(
  '20072008' = data.frame(
    Team = team, conference = conference, division = division,
    last.year = c(110, 67, 76, 113, 96, 88, 71, 95, 73, 107, 113, 71, 86, 68, 104,
                  90, 110, 107, 92, 94, 105, 56, 105, 107, 81, 93, 91, 105, 0, 70, 97),
    value = c(197, 147, 243, 162, 164, 156, 179, 214, 150, 254, 293, 157,
              151, 209, 180, 283, 143, 195, 149, 365, 186, 244, 155, 165,
              144, 199, 413, 211, 0, 145, 148),
    last.year.playoffs = c(TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE,
                           FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE,
                           TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE,
                           TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE),
    cap.hit = c(0.954627038, 0.713813121, 0.907234394, 0.891493042, 0.955000199,
                0.943471173, 0.905403976, 0.964372763, 0.759214712, 0.984877217,
                0.915424056, 0.961360437, 0.906359443, 0.860514115, 0.939946918,
                0.908587674, 0.681115308, 0.935892843, 0.849718887, 0.96441332,
                0.949589264, 0.965049702,0.810772167, 0.797819085, 0.912180915,
                0.850514314, 0.972356262, 0.958742147, 0, 0.758628231, 0.856967793)),
  '20082009' = data.frame(
    Team = team, conference = conference, division = division,
    last.year = c(102, 83, 94, 90, 94, 92, 88, 95, 80, 97, 115, 88, 85, 71, 98, 104,
                  91, 99, 79, 97, 94, 95, 102, 108, 79, 71, 83, 88, 0, 94, 76),
    value = c(202, 142, 263, 169, 203, 168, 205, 231, 157, 273, 303, 175,
              163, 210, 217, 334, 164, 222, 154, 411, 207, 275, 195, 179,
              162, 200, 448, 236, 0, 160, 158),
    last.year.playoffs = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE,
                           FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE,
                           TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE,
                           FALSE, FALSE, FALSE, FALSE, TRUE, FALSE),
    cap.hit = c(0.954961023, 0.733447972, 0.88602522, 0.889026102, 0.993896684,
                0.895477778, 0.913255379, 0.935688889, 0.87034903, 0.965694533,
                0.993394374, 0.954359612, 0.955823104, 0.699229277, 0.935649912,
                0.946085891, 0.79938448, 0.968095944, 0.748716049, 0.994362875,
                0.958047795, 0.960524515, 0.863926455, 0.980072134, 0.885059965,
                0.823998413, 0.827537743, 0.947223457, 0, 0.942734744, 0.730747795)),
  '20092010' = data.frame(
    Team = team, conference = conference, division = division,
    last.year = c(91, 79, 116, 91, 98, 97, 104, 69, 92, 83, 112, 85, 93, 79, 89,
                  93, 88, 106, 61, 95, 83, 99, 99, 117, 92, 66, 81, 100, 0, 108, 76),
    value = c(206, 138, 271, 170, 200, 177, 258, 205, 165, 246, 337, 166,
              159, 208, 210, 339, 156, 223, 149, 416, 197, 273, 222, 184,
              176, 191, 470, 239, 0, 183, 143),
    last.year.playoffs = c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE,
                           FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE,
                           TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE,
                           FALSE, FALSE, TRUE, FALSE, TRUE, FALSE),
    cap.hit = c(0.939654049, 0.764908451, 0.972113028, 0.970826056, 0.993125792,
                0.945436444, 0.994086373, 0.863832218, 0.821413732, 0.848075704,
                0.974010211, 0.971794014, 0.938426937, 0.892749296, 0.915762148,
                0.959392606, 0.777172535, 0.965676056, 0.680892606, 0.993570845,
                0.991675634, 0.97676919, 0.995494683, 0.985640229, 0.841583979,
                0.833910387, 0.940983803, 0.978826056, 0, 0.942457923, 0.839019718)),
  '20102011' = data.frame(
    Team = team, conference = conference, division = division,
    last.year = c(89, 107, 91, 100, 90, 80, 112, 95, 79, 88, 102, 62, 77, 101, 84,
                  88, 100, 103, 79, 87, 94, 88, 101, 113, 90, 80, 74, 103, 0, 121, 83),
    value = c(188, 134, 302, 169, 206, 162, 300, 198, 153, 227, 315, 183, 168,
              215, 202, 408, 148, 218, 151, 461, 196, 301, 235, 194, 165, 145,
              505, 262, 0, 197, 135),
    last.year.playoffs = c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE,
                           FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE,
                           TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE,
                           FALSE, FALSE, TRUE, FALSE, TRUE, FALSE),
    cap.hit = c(0.946224579, 0.845358586, 0.972483502, 0.940139899, 0.968322391,
                0.839891414, 0.998699916, 0.725636364, 0.921649158, 0.872177609,
                0.994225741, 0.76966835, 0.791636364, 0.936390741, 0.995589714,
                0.956251515, 0.855742256, 0.963161953, 0.692449495, 1.005551178,
                0.948947138, 0.974836869, 0.939600842, 1.020104209, 0.752739057,
                0.867445623, 0.93484798, 0.908855556, 0, 0.971090236, 0.721385522)),
  '20112012' = data.frame(
    Team = team, conference = conference, division = division,
    last.year = c(99, 99, 103, 96, 94, 91, 97, 68, 81, 95, 104, 62, 72, 98, 86, 96,
                  99, 81, 73, 93, 74, 106, 106, 105, 87, 103, 85, 117, 0, 107, 80),
    value = c(184, 134, 325, 173, 220, 169, 306, 198, 152, 230, 336, 212, 162,
              232, 213, 445, 163, 181, 149, 507, 201, 290, 264, 211, 157, 174,
              521, 300, 0, 225, 164),
    last.year.playoffs = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE,
                           FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE,
                           TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE,
                           FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
    cap.hit = c(0.921778694, 0.850737792, 0.908526594, 0.971450078, 0.983204044,
                0.783354588, 0.939689114, 0.770757387, 0.950566874, 0.775754277,
                0.930646812, 0.97735661, 0.863891602, 0.990068227, 0.8673,
                0.923421617, 0.807651633, 0.96509098, 0.784143079, 0.950578227,
                0.804174184, 0.958934992, 0.968761291, 0.980630327, 0.851866719,
                0.939267652, 0.955306998, 0.98626339, 0, 0.918276672, 0.807360809)),
  '20122013' = data.frame(
    Team = team, conference = conference, division = division,
    last.year = c(80, 97, 102, 89, 90, 82, 101, 88, 65, 89, 102, 74, 94, 95, 81, 78,
                  104, 102, 79, 109, 92, 103, 108, 96, 109, 84, 80, 111, 0, 92, 84),
    value = c(192, 134, 348, 175, 245, 162, 350, 210, 145, 240, 346, 225, 170,
              276, 218, 575, 167, 205, 155, 750, 220, 336, 288, 223, 130, 174,
              1000, 342, 0, 250, 200),
    last.year.playoffs = c(FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE,
                           FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE,
                           TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
                           FALSE, FALSE, TRUE, FALSE, FALSE, FALSE),
    cap.hit = c(0.778056667, 0.668236667, 0.90105445, 0.861989167, 0.847434167,
                0.791791667, 0.950905667, 0.758315, 0.726383333, 0.730316667,
                1.067673, 0.834014333, 0.790108333, 0.86126, 1.002008333,
                0.9442945, 0.725065017, 0.802033333, 0.721453333, 0.970997833,
                0.727108333, 0.954343167, 0.939042833, 1.049264833, 0.847495167,
                0.8968525, 0.8818625, 0.9730895, 0, 0.9111675, 0.805271667)),
  '20132014' = data.frame(
    Team = team, conference = conference, division = division,
    last.year = c(113, 87, 106, 82, 72, 72, 132, 67, 94,
    82, 96, 77, 62, 101, 94, 108, 70, 82, 94, 96, 96, 84, 123, 97, 102, 68, 97,
    101, 0, 97, 87), value = c(300, 200, 600, 250, 420, 187, 625, 337, 175, 333,
    470, 400, 240, 450, 330, 775, 205, 320, 195, 850, 380, 500, 480, 405, 185,
    180, 1150, 700, 0, 414, 340), last.year.playoffs = c(TRUE, TRUE, TRUE, FALSE,
    FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE,
    TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE,
    TRUE, FALSE, TRUE, FALSE), cap.hit = c(0.930200156, 0.969496423, 0.981633328,
    0.894564852, 0.840852255, 0.930169362, 0.972097978, 0.8361493, 0.943756719,
    0.996991509, 0.981606532, 0.949851788, 0.854476205, 0.993386143, 1.003540529,
    0.997701913, 0.922543857, 0.984584992, 0.796609642, 0.977383204, 0.879184759,
    0.952224572, 0.995641353, 0.977661586, 0.986522457, 0.945196734, 0.969121617,
    0.982540902, 0, 0.984031882, 0.996775754)),
  '20142015' = data.frame(
    Team = team, conference = conference, division = division,
    last.year = c(116,
    89, 117, 52, 77, 83, 107, 112, 93, 91, 93, 67, 66, 100, 98, 100, 88, 88,
    79, 96, 88, 94, 109, 111, 111, 101, 84, 83, 0, 90, 84), value = c(341, 227,
    750, 284, 477, 213, 825, 383, 199, 378, 570, 455, 273, 580, 375, 1000, 233,
    364, 222, 1100, 432, 625, 565, 460, 210, 205, 1300, 800, 0, 470, 386), last.year.playoffs = c(TRUE,
    FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE,
    TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE,
    TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
    cap.hit = c(0.963358116, 0.823931884,
    0.914334623, 0.856609072, 0.818973913, 0.907070435, 0.977624493, 0.965918783,
    0.895778014, 0.930400275, 0.951680841, 0.92830942, 0.962326087, 0.969917493,
    0.92699029, 0.992214391, 0.85214058, 0.951708406, 0.929003043, 0.988463348,
    0.8178, 0.96396942, 0.984434841, 0.912072029, 0.998320116, 0.96631029, 0.858832493,
    0.985485072, 0, 0.976155464, 0.903217333)),
  '20152016' = data.frame(
    Team = team, conference = conference, division = division,
    last.year = c(109,
    56, 96, 54, 97, 71, 102, 90, 89, 92, 100, 62, 91, 95, 100, 110, 104, 78,
    101, 113, 99, 84, 98, 89, 109, 108, 68, 101, 0, 101, 99),
    value = c(343,
    229, 750, 286, 481, 214, 925, 386, 200, 381, 600, 458, 275, 580, 378, 1175,
    235, 366, 223, 1200, 435, 660, 569, 464, 212, 206, 1150, 806, 0, 565, 389),
    last.year.playoffs = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE,
                           TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE,
                           TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE,
                           FALSE),
    cap.hit = c(0.921795938, 0.837573908, 0.989602591,
      0.875373333, 0.943575308, 0.820085826, 0.983477171, 0.894839468, 0.953873151,
      0.972870602, 0.959017885, 0.972854202, 0.9796, 0.928700014, 0.983322689,
      0.970797479, 0.855578081, 0.875145238, 0.92779902, 0.997378249, 0.869088824,
      0.969140476, 0.98572077, 0.97440028, 0.97874056, 0.971748894, 0.848912829,
      0.966461555, 0, 0.987127997, 0.833732227)),
  '20162017' = data.frame(Team = team, conference = conference, division = division,
    last.year = c(103,
    78, 93, 81, 77, 86, 103, 82, 76, 109, 93, 70, 103, 102, 87, 82, 96, 84, 100,
    101, 85, 96, 104, 98, 107, 97, 69, 75, 0, 120, 78), value = c(357, 238, 800,
    298, 500, 230, 925, 401, 236, 397, 624, 477, 235, 603, 393, 1120, 244, 381,
    385, 1250, 453, 570, 592, 482, 236, 236, 1100, 839, 0, 588, 405), last.year.playoffs = c(TRUE,
    FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE,
    TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE,
    TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE), cap.hit = c(0.973121041, 0.856885521,
    0.957061014, 0.98034437, 0.941115863, 0.693168849, 0.956713164, 0.946079521,
    0.971777397, 0.956482918, 0.954480945, 0.925599082, 0.878120959, 0.993176055,
    0.986984192, 0.959465247, 0.925543781, 0.890760616, 0.984023288, 0.980956096,
    0.850539247, 0.989801301, 0.994900178, 0.982513836, 0.996572986, 0.885768096,
    0.912710466, 0.973763836, 0, 1.037190781, 0.921684863)),
  '20172018' = data.frame(
    Team = team, conference = conference, division = division,
    last.year = c(105,
    70, 95, 78, 94, 87, 109, 48, 108, 79, 79, 103, 81, 86, 106, 103, 94, 70,
    94, 102, 98, 88, 111, 99, 99, 94, 95, 69, 0, 118, 87), value = c(460, 300,
    890, 350, 430, 370, 1000, 385, 315, 515, 700, 520, 305, 750, 440, 1250, 380,
    400, 395, 1500, 420, 740, 650, 490, 450, 390, 1400, 730, 500, 625, 375),
    last.year.playoffs = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE,
      TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE,
      TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE,
      FALSE), cap.hit = c(0.916804973, 0.709670067, 0.958885747, 0.928961933,
      0.94547116, 0.773376013, 0.921367813, 0.837583373, 0.916304293, 0.975288947,
      0.921549573, 0.843771533, 0.88996568, 0.927960667, 0.936768147, 0.89926976,
      0.92989524, 0.89201364, 0.98120348, 0.931491693, 0.923815053, 0.94676724,
      1.00261836, 0.956320653, 0.9781968, 0.965263947, 0.841591947, 0.960163053,
      0.915225493, 0.98661948, 0.91295652)),
  '20182019' = data.frame(
    Team = team, conference = conference, division = division,
    last.year = c(101,
    70, 112, 62, 84, 83, 76, 95, 97, 92, 73, 78, 96, 98, 101, 71, 117, 97, 80,
    77, 67, 98, 100, 100, 94, 113, 105, 73, 109, 105, 114), value = c(460, 300,
    890, 350, 430, 370, 1000, 385, 315, 515, 700, 520, 305, 750, 440, 1250, 380,
    400, 395, 1500, 420, 740, 650, 490, 450, 390, 1400, 730, 500, 625, 375),
    last.year.playoffs = c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE,
      TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE,
      FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE,
      TRUE), cap.hit = c(0.916804973, 0.709670067, 0.958885747, 0.928961933,
      0.94547116, 0.773376013, 0.921367813, 0.837583373, 0.916304293, 0.975288947,
      0.921549573, 0.843771533, 0.88996568, 0.927960667, 0.936768147, 0.89926976,
      0.92989524, 0.89201364, 0.98120348, 0.931491693, 0.923815053, 0.94676724,
      1.00261836, 0.956320653, 0.9781968, 0.965263947, 0.841591947, 0.960163053,
      0.915225493, 0.98661948, 0.91295652)))
