source("../../R/roulette_rules.R")
source("../../R/simulations.R")

testthat::test_that("sessions stop when bankroll is ruined", {
  set.seed(1)
  result <- simulate_sessions(
    initial_bankroll = 1,
    base_bet = 1,
    spins = 100,
    simulations = 20,
    bet_type = "Straight-up number",
    strategy = "Flat betting",
    seed = 1
  )

  testthat::expect_true(all(result$outcomes$final_bankroll >= 0))
  testthat::expect_true(any(result$outcomes$ruined))
})

testthat::test_that("martingale doubles after losses and resets after wins", {
  state <- list(loss_streak = 0, fibonacci_index = 1)

  testthat::expect_equal(next_bet_size("Martingale", 10, state), 10)
  state <- update_strategy_state("Martingale", state, won = FALSE)
  testthat::expect_equal(next_bet_size("Martingale", 10, state), 20)
  state <- update_strategy_state("Martingale", state, won = TRUE)
  testthat::expect_equal(next_bet_size("Martingale", 10, state), 10)
})
