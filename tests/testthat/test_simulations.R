source("../../R/roulette_rules.R")
source("../../R/simulations.R")

testthat::test_that("sessions stop when bankroll is ruined", {
  result <- simulate_sessions(
    initial_bankroll = 1,
    base_bet = 1,
    spins = 100,
    simulations = 20,
    bet_type = "Green (0, 00)",
    strategy = "Flat betting",
    seed = 1
  )

  testthat::expect_true(all(result$outcomes$final_bankroll >= 0))
  testthat::expect_true(any(result$outcomes$ruined))
})

testthat::test_that("martingale doubles after losses and resets after wins", {
  state <- list(loss_streak = 0, fibonacci_index = 1, dalembert_units = 1)

  testthat::expect_equal(next_bet_size("Martingale", 10, state), 10)
  state <- update_strategy_state("Martingale", state, won = FALSE)
  testthat::expect_equal(next_bet_size("Martingale", 10, state), 20)
  state <- update_strategy_state("Martingale", state, won = TRUE)
  testthat::expect_equal(next_bet_size("Martingale", 10, state), 10)
})

testthat::test_that("dalembert changes by one base unit", {
  state <- list(loss_streak = 0, fibonacci_index = 1, dalembert_units = 1)

  testthat::expect_equal(next_bet_size("D'Alembert", 10, state), 10)
  state <- update_strategy_state("D'Alembert", state, won = FALSE)
  testthat::expect_equal(next_bet_size("D'Alembert", 10, state), 20)
  state <- update_strategy_state("D'Alembert", state, won = TRUE)
  testthat::expect_equal(next_bet_size("D'Alembert", 10, state), 10)
})

testthat::test_that("simulation outcomes include risk metrics", {
  result <- simulate_sessions(
    initial_bankroll = 500,
    base_bet = 10,
    spins = 25,
    simulations = 10,
    bet_type = "Red",
    strategy = "Flat betting",
    seed = 12345
  )

  testthat::expect_true(all(c("wins", "losses", "max_drawdown", "total_wagered") %in% names(result$outcomes)))
  testthat::expect_equal(result$outcomes$wins + result$outcomes$losses, result$outcomes$spins_played)
})
