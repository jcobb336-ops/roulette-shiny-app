source("../../R/roulette_rules.R")
source("../../R/calculations.R")

testthat::test_that("American roulette constants are correct", {
  testthat::expect_equal(AMERICAN_ROULETTE$slot_count, 38)
  testthat::expect_true("0" %in% AMERICAN_ROULETTE$slots)
  testthat::expect_true("00" %in% AMERICAN_ROULETTE$slots)
  testthat::expect_equal(AMERICAN_ROULETTE$house_edge, 2 / 38)
})

testthat::test_that("straight-up bet probability and EV are correct", {
  metrics <- bet_metrics("Straight-up number", bet_size = 10, spins = 100)

  testthat::expect_equal(metrics$probability_win, 1 / 38)
  testthat::expect_equal(metrics$payout_to_one, 35)
  testthat::expect_equal(metrics$expected_value_per_bet, -10 * 2 / 38)
})

testthat::test_that("red or black bet probability and EV are correct", {
  metrics <- bet_metrics("Red or black", bet_size = 10, spins = 100)

  testthat::expect_equal(metrics$probability_win, 18 / 38)
  testthat::expect_equal(metrics$payout_to_one, 1)
  testthat::expect_equal(metrics$expected_value_per_bet, -10 * 2 / 38)
})
