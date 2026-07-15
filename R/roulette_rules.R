AMERICAN_ROULETTE <- list(
  name = "American roulette",
  slots = c("0", "00", as.character(1:36)),
  slot_count = 38,
  house_edge = 2 / 38
)

roulette_bets <- function() {
  data.frame(
    bet_type = c(
      "Straight-up number",
      "Red or black",
      "Odd or even",
      "Dozen",
      "Column"
    ),
    win_slots = c(1, 18, 18, 12, 12),
    payout_to_one = c(35, 1, 1, 2, 2),
    stringsAsFactors = FALSE
  )
}

get_bet <- function(bet_type) {
  bets <- roulette_bets()
  bet <- bets[bets$bet_type == bet_type, , drop = FALSE]

  if (nrow(bet) != 1) {
    stop("Unknown bet type: ", bet_type, call. = FALSE)
  }

  bet
}

bet_metrics <- function(bet_type, bet_size, spins) {
  bet <- get_bet(bet_type)
  p_win <- bet$win_slots / AMERICAN_ROULETTE$slot_count
  p_loss <- 1 - p_win
  win_profit <- bet_size * bet$payout_to_one
  loss_profit <- -bet_size
  expected_value <- p_win * win_profit + p_loss * loss_profit
  variance <- p_win * (win_profit - expected_value)^2 +
    p_loss * (loss_profit - expected_value)^2

  data.frame(
    game = AMERICAN_ROULETTE$name,
    slots = AMERICAN_ROULETTE$slot_count,
    bet_type = bet_type,
    win_slots = bet$win_slots,
    payout_to_one = bet$payout_to_one,
    probability_win = p_win,
    probability_loss = p_loss,
    house_edge = AMERICAN_ROULETTE$house_edge,
    expected_value_per_bet = expected_value,
    expected_value_total = expected_value * spins,
    variance_per_bet = variance,
    sd_per_bet = sqrt(variance),
    stringsAsFactors = FALSE
  )
}

strategy_names <- function() {
  c("Flat betting", "Martingale", "Fibonacci")
}
