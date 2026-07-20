source("../../R/roulette_rules.R")
source("../../R/calculations.R")

testthat::test_that("American roulette constants are correct", {
  testthat::expect_equal(AMERICAN_ROULETTE$slot_count, 38)
  testthat::expect_true("0" %in% AMERICAN_ROULETTE$slots)
  testthat::expect_true("00" %in% AMERICAN_ROULETTE$slots)
  testthat::expect_equal(length(AMERICAN_ROULETTE$red_numbers), 18)
  testthat::expect_equal(length(AMERICAN_ROULETTE$black_numbers), 18)
  testthat::expect_equal(AMERICAN_ROULETTE$house_edge, 2 / 38)
})

testthat::test_that("red and black probability and EV are correct", {
  red_metrics <- bet_metrics("Red", bet_size = 10, spins = 100)
  black_metrics <- bet_metrics("Black", bet_size = 10, spins = 100)

  testthat::expect_equal(red_metrics$probability_win, 18 / 38)
  testthat::expect_equal(black_metrics$probability_win, 18 / 38)
  testthat::expect_equal(red_metrics$payout_to_one, 1)
  testthat::expect_equal(black_metrics$payout_to_one, 1)
  testthat::expect_equal(red_metrics$expected_value_per_bet, -10 * 2 / 38)
  testthat::expect_equal(black_metrics$expected_value_per_bet, -10 * 2 / 38)
})

testthat::test_that("green two-pocket model uses the documented payout and EV", {
  metrics <- bet_metrics("Green (0, 00)", bet_size = 10, spins = 100)

  testthat::expect_equal(metrics$probability_win, 2 / 38)
  testthat::expect_equal(metrics$probability_loss, 36 / 38)
  testthat::expect_equal(metrics$payout_to_one, 17)
  testthat::expect_equal(metrics$expected_value_per_bet, -10 * 2 / 38)
})
