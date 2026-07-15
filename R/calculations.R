currency <- function(x) {
  scales::dollar(x, accuracy = 0.01, big.mark = ",")
}

percent <- function(x, accuracy = 0.1) {
  scales::percent(x, accuracy = accuracy)
}

summarize_outcomes <- function(outcomes, initial_bankroll) {
  profit <- outcomes$final_bankroll - initial_bankroll

  data.frame(
    sessions = nrow(outcomes),
    mean_final_bankroll = mean(outcomes$final_bankroll),
    median_final_bankroll = stats::median(outcomes$final_bankroll),
    mean_profit = mean(profit),
    probability_profit = mean(profit > 0),
    probability_ruin = mean(outcomes$ruined),
    worst_outcome = min(outcomes$final_bankroll),
    best_outcome = max(outcomes$final_bankroll),
    p05_final_bankroll = as.numeric(stats::quantile(outcomes$final_bankroll, 0.05)),
    p95_final_bankroll = as.numeric(stats::quantile(outcomes$final_bankroll, 0.95)),
    stringsAsFactors = FALSE
  )
}

ev_curve <- function(initial_bankroll, bet_type, bet_size, spins) {
  metrics <- bet_metrics(bet_type, bet_size, spins)
  spin <- seq_len(spins)

  data.frame(
    spin = spin,
    expected_bankroll = initial_bankroll + spin * metrics$expected_value_per_bet
  )
}
