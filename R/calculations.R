currency <- function(x) {
  scales::dollar(x, accuracy = 0.01, big.mark = ",")
}

currency_precise <- function(x) {
  scales::dollar(x, accuracy = 0.0001, big.mark = ",")
}

percent <- function(x, accuracy = 0.1) {
  scales::percent(x, accuracy = accuracy)
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
    losing_slots = bet$losing_slots,
    payout_to_one = bet$payout_to_one,
    payout_label = bet$payout_label,
    probability_win = p_win,
    probability_loss = p_loss,
    house_edge = -expected_value / bet_size,
    expected_value_per_bet = expected_value,
    expected_value_total = expected_value * spins,
    variance_per_bet = variance,
    sd_per_bet = sqrt(variance),
    model_note = bet$model_note,
    stringsAsFactors = FALSE
  )
}

summarize_outcomes <- function(outcomes, initial_bankroll, base_bet, spins, bet_type) {
  metrics <- bet_metrics(bet_type, base_bet, spins)
  total_spins <- sum(outcomes$spins_played)
  total_wins <- sum(outcomes$wins)
  total_losses <- sum(outcomes$losses)
  simulated_total_profit <- sum(outcomes$profit)
  theoretical_total <- nrow(outcomes) * metrics$expected_value_total

  data.frame(
    sessions = nrow(outcomes),
    wins = total_wins,
    losses = total_losses,
    mean_final_bankroll = mean(outcomes$final_bankroll),
    median_final_bankroll = stats::median(outcomes$final_bankroll),
    mean_profit = mean(outcomes$profit),
    net_profit = simulated_total_profit,
    simulated_win_percentage = if (total_spins > 0) total_wins / total_spins else NA_real_,
    average_result_per_spin = if (total_spins > 0) simulated_total_profit / total_spins else NA_real_,
    theoretical_expected_result = theoretical_total,
    theoretical_average_per_spin = metrics$expected_value_per_bet,
    difference_from_theoretical = simulated_total_profit - theoretical_total,
    probability_profit = mean(outcomes$profit > 0),
    probability_ruin = mean(outcomes$ruined),
    worst_outcome = min(outcomes$final_bankroll),
    best_outcome = max(outcomes$final_bankroll),
    p05_final_bankroll = as.numeric(stats::quantile(outcomes$final_bankroll, 0.05)),
    p95_final_bankroll = as.numeric(stats::quantile(outcomes$final_bankroll, 0.95)),
    max_drawdown = mean(outcomes$max_drawdown),
    total_wagered = sum(outcomes$total_wagered),
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
