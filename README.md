# American Roulette Risk Lab

An R Shiny app for a graduate course assignment analyzing roulette as a negative expected value betting problem.

## Scope

The app is fixed to American roulette:

- 38 slots total
- Slots are `0`, `00`, and `1` through `36`
- Straight-up win probability is `1/38`
- Even-money win probability is `18/38`
- House edge is `2/38`, or about `5.26%`

## Features

- Exact expected value and variance calculations
- Monte Carlo bankroll simulations
- Flat betting, Martingale, and Fibonacci strategies
- Bankroll trajectory plots
- Final bankroll distribution plots
- Strategy comparison
- Methodology tab documenting assumptions

## Run Locally

```r
shiny::runApp()
```

## Tests

```r
testthat::test_dir("tests/testthat")
```

## Deployment

The app is ready to deploy through shinyapps.io after local validation:

```r
rsconnect::deployApp()
```
