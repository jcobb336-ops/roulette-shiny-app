AMERICAN_ROULETTE <- list(
  name = "American roulette",
  slots = c("0", "00", as.character(1:36)),
  slot_count = 38,
  red_numbers = c(1, 3, 5, 7, 9, 12, 14, 16, 18, 19, 21, 23, 25, 27, 30, 32, 34, 36),
  black_numbers = c(2, 4, 6, 8, 10, 11, 13, 15, 17, 20, 22, 24, 26, 28, 29, 31, 33, 35),
  green_slots = c("0", "00"),
  house_edge = 2 / 38
)

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
