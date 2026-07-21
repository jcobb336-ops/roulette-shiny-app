AMERICAN_ROULETTE <- list(
  name = "American roulette",
  slots = c("0", "00", as.character(1:36)),
  slot_count = 38,
  red_numbers = c(1, 3, 5, 7, 9, 12, 14, 16, 18, 19, 21, 23, 25, 27, 30, 32, 34, 36),
  black_numbers = c(2, 4, 6, 8, 10, 11, 13, 15, 17, 20, 22, 24, 26, 28, 29, 31, 33, 35),
  green_slots = c("0", "00"),
  house_edge = 2 / 38
)

wheel_pockets <- function() {
  AMERICAN_ROULETTE$slots
}

pocket_color <- function(pocket) {
  if (pocket %in% AMERICAN_ROULETTE$green_slots) {
    return("Green")
  }

  number <- as.integer(pocket)
  if (number %in% AMERICAN_ROULETTE$red_numbers) {
    "Red"
  } else {
    "Black"
  }
}

pocket_color_class <- function(pocket) {
  tolower(pocket_color(pocket))
}

standard_bet_definitions <- function() {
  numbers <- as.character(1:36)
  red <- as.character(AMERICAN_ROULETTE$red_numbers)
  black <- as.character(AMERICAN_ROULETTE$black_numbers)
  odd <- as.character(seq(1, 35, by = 2))
  even <- as.character(seq(2, 36, by = 2))
  low <- as.character(1:18)
  high <- as.character(19:36)
  dozen_1 <- as.character(1:12)
  dozen_2 <- as.character(13:24)
  dozen_3 <- as.character(25:36)
  column_1 <- as.character(c(1, 4, 7, 10, 13, 16, 19, 22, 25, 28, 31, 34))
  column_2 <- as.character(c(2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35))
  column_3 <- as.character(c(3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 36))

  straight <- lapply(wheel_pockets(), function(pocket) {
    data.frame(
      bet_id = paste0("straight_", gsub("00", "double_zero", pocket)),
      bet_type = "Straight-up",
      selection = pocket,
      pockets = I(list(pocket)),
      payout_to_one = 35,
      stringsAsFactors = FALSE
    )
  })

  outside <- list(
    data.frame(bet_id = "red", bet_type = "Red", selection = "18 red numbers", pockets = I(list(red)), payout_to_one = 1),
    data.frame(bet_id = "black", bet_type = "Black", selection = "18 black numbers", pockets = I(list(black)), payout_to_one = 1),
    data.frame(bet_id = "odd", bet_type = "Odd", selection = "18 odd numbers", pockets = I(list(odd)), payout_to_one = 1),
    data.frame(bet_id = "even", bet_type = "Even", selection = "18 even numbers", pockets = I(list(even)), payout_to_one = 1),
    data.frame(bet_id = "low", bet_type = "Low", selection = "1-18", pockets = I(list(low)), payout_to_one = 1),
    data.frame(bet_id = "high", bet_type = "High", selection = "19-36", pockets = I(list(high)), payout_to_one = 1),
    data.frame(bet_id = "dozen_1", bet_type = "First dozen", selection = "1-12", pockets = I(list(dozen_1)), payout_to_one = 2),
    data.frame(bet_id = "dozen_2", bet_type = "Second dozen", selection = "13-24", pockets = I(list(dozen_2)), payout_to_one = 2),
    data.frame(bet_id = "dozen_3", bet_type = "Third dozen", selection = "25-36", pockets = I(list(dozen_3)), payout_to_one = 2),
    data.frame(bet_id = "column_1", bet_type = "Column 1", selection = "1, 4, 7 ... 34", pockets = I(list(column_1)), payout_to_one = 2),
    data.frame(bet_id = "column_2", bet_type = "Column 2", selection = "2, 5, 8 ... 35", pockets = I(list(column_2)), payout_to_one = 2),
    data.frame(bet_id = "column_3", bet_type = "Column 3", selection = "3, 6, 9 ... 36", pockets = I(list(column_3)), payout_to_one = 2)
  )

  bets <- do.call(rbind, c(straight, outside))
  bets$pockets_covered <- vapply(bets$pockets, length, integer(1))
  bets$probability_win <- bets$pockets_covered / AMERICAN_ROULETTE$slot_count
  bets$probability_loss <- 1 - bets$probability_win
  bets$payout_label <- paste(bets$payout_to_one, "to 1")
  rownames(bets) <- NULL
  bets
}

get_standard_bet <- function(bet_id) {
  bets <- standard_bet_definitions()
  bet <- bets[bets$bet_id == bet_id, , drop = FALSE]

  if (nrow(bet) != 1) {
    stop("Unknown standard bet: ", bet_id, call. = FALSE)
  }

  bet
}

color_bets <- function() {
  data.frame(
    bet_type = c("Red", "Black", "Green (0, 00)"),
    color_class = c("red", "black", "green"),
    win_slots = c(18, 18, 2),
    losing_slots = c(20, 20, 36),
    payout_to_one = c(1, 1, 17),
    payout_label = c("1:1", "1:1", "17:1"),
    model_note = c(
      "Wins on the 18 red numbers and loses on black, 0, and 00.",
      "Wins on the 18 black numbers and loses on red, 0, and 00.",
      "Modeled as a two-pocket 0/00 selection that pays 17:1, matching a two-number split-style payout."
    ),
    stringsAsFactors = FALSE
  )
}

roulette_bets <- function() {
  data.frame(
    bet_type = c(
      "Single number bet",
      "Double number bet",
      "Three number bet",
      "Four number bet",
      "Five number bet",
      "Six number bet",
      "Twelve numbers or dozens",
      "Column bet",
      "Low bet",
      "High bet",
      "Red or black",
      "Odd or even"
    ),
    also_called = c(
      "Straight up",
      "Split",
      "Street",
      "Corner bet",
      "American 0, 00, 1, 2, and 3 bet",
      "Line",
      "First, second, or third dozen",
      "Vertical column",
      "1-18",
      "19-36",
      "Color bet",
      "Parity bet"
    ),
    example = c(
      "Any one number",
      "Any two adjacent numbers",
      "Any row of three numbers",
      "Any block of four numbers",
      "0, 00, 1, 2, and 3",
      "7, 8, 9, 10, 11, and 12",
      "1-12, 13-24, or 25-36",
      "One of the three columns",
      "Numbers 1 through 18",
      "Numbers 19 through 36",
      "Red or black numbers",
      "Odd or even numbers"
    ),
    payout = c("35:1", "17:1", "11:1", "8:1", "6:1", "5:1", "2:1", "2:1", "1:1", "1:1", "1:1", "1:1"),
    stringsAsFactors = FALSE
  )
}

get_bet <- function(bet_type) {
  bets <- color_bets()
  bet <- bets[bets$bet_type == bet_type, , drop = FALSE]

  if (nrow(bet) != 1) {
    stop("Unknown bet type: ", bet_type, call. = FALSE)
  }

  bet
}

strategy_names <- function() {
  c("Flat betting", "Martingale", "Fibonacci", "D'Alembert")
}
