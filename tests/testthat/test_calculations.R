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

testthat::test_that("standard clickable bet coverage is correct", {
  bets <- standard_bet_definitions()

  testthat::expect_equal(nrow(bets[grepl("^straight_", bets$bet_id), ]), 38)
  testthat::expect_equal(get_standard_bet("straight_17")$pockets_covered, 1)
  testthat::expect_equal(get_standard_bet("dozen_1")$pockets_covered, 12)
  testthat::expect_equal(get_standard_bet("dozen_2")$pockets_covered, 12)
  testthat::expect_equal(get_standard_bet("dozen_3")$pockets_covered, 12)
  testthat::expect_equal(get_standard_bet("column_1")$pockets_covered, 12)
  testthat::expect_equal(get_standard_bet("column_2")$pockets_covered, 12)
  testthat::expect_equal(get_standard_bet("column_3")$pockets_covered, 12)
  testthat::expect_equal(get_standard_bet("red")$pockets_covered, 18)
  testthat::expect_equal(get_standard_bet("black")$pockets_covered, 18)
  testthat::expect_equal(get_standard_bet("odd")$pockets_covered, 18)
  testthat::expect_equal(get_standard_bet("even")$pockets_covered, 18)
  testthat::expect_equal(get_standard_bet("low")$pockets_covered, 18)
  testthat::expect_equal(get_standard_bet("high")$pockets_covered, 18)
})

testthat::test_that("green pockets are excluded from outside bets", {
  outside_ids <- c("red", "black", "odd", "even", "low", "high", "dozen_1", "dozen_2", "dozen_3", "column_1", "column_2", "column_3")

  for (bet_id in outside_ids) {
    pockets <- get_standard_bet(bet_id)$pockets[[1]]
    testthat::expect_false("0" %in% pockets)
    testthat::expect_false("00" %in% pockets)
  }
})

testthat::test_that("standard payout values and EV are correct", {
  testthat::expect_equal(get_standard_bet("straight_17")$payout_to_one, 35)
  testthat::expect_equal(get_standard_bet("red")$payout_to_one, 1)
  testthat::expect_equal(get_standard_bet("black")$payout_to_one, 1)
  testthat::expect_equal(get_standard_bet("odd")$payout_to_one, 1)
  testthat::expect_equal(get_standard_bet("even")$payout_to_one, 1)
  testthat::expect_equal(get_standard_bet("low")$payout_to_one, 1)
  testthat::expect_equal(get_standard_bet("high")$payout_to_one, 1)
  testthat::expect_equal(get_standard_bet("dozen_1")$payout_to_one, 2)
  testthat::expect_equal(get_standard_bet("column_1")$payout_to_one, 2)

  testthat::expect_equal(standard_bet_metrics("straight_17", 1)$expected_value, -2 / 38)
  testthat::expect_equal(standard_bet_metrics("red", 1)$expected_value, -2 / 38)
  testthat::expect_equal(standard_bet_metrics("dozen_1", 1)$expected_value, -2 / 38)
  testthat::expect_equal(standard_bet_metrics("column_1", 1)$expected_value, -2 / 38)
})

testthat::test_that("bet evaluation and bankroll updates are correct", {
  win <- evaluate_bet_result("straight_17", "17", 10)
  loss <- evaluate_bet_result("red", "00", 10)

  testthat::expect_true(win$won)
  testthat::expect_equal(win$net_result, 350)
  testthat::expect_false(loss$won)
  testthat::expect_equal(loss$net_result, -10)
  testthat::expect_equal(update_bankroll(500, win$net_result), 850)
  testthat::expect_equal(update_bankroll(500, loss$net_result), 490)
})

testthat::test_that("wager validation prevents impossible bets", {
  testthat::expect_true(validate_wager(10, 500))
  testthat::expect_match(validate_wager(0, 500), "greater than zero")
  testthat::expect_match(validate_wager(501, 500), "cannot exceed")
  testthat::expect_match(validate_wager(NA_real_, 500), "number")
})

testthat::test_that("same seed produces same wheel sequence", {
  set.seed(12345)
  first <- replicate(8, spin_wheel())
  set.seed(12345)
  second <- replicate(8, spin_wheel())

  testthat::expect_equal(first, second)
})
