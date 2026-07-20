# American Roulette Risk Lab

An R Shiny app for a graduate course assignment analyzing American roulette color bets, bankroll risk, and betting-system volatility.

## Scope

The app is fixed to American roulette:

- 38 slots total
- Slots are `0`, `00`, and `1` through `36`
- Red win probability is `18/38`
- Black win probability is `18/38`
- Green-pocket probability is `2/38`
- Red and black pay `1:1`
- The app's `Green (0, 00)` option is modeled as a two-pocket wager paying `17:1`
- House edge is `2/38`, or about `5.26%`

## Features

- Visual American roulette table and payout guide
- Exact probability, expected value, and house-edge calculations
- Monte Carlo bankroll simulations controlled by the Run Simulation button
- Flat betting, Martingale, Fibonacci, and D'Alembert strategies
- Bankroll trajectory plots
- Final bankroll distribution plots
- Strategy comparison
- Methodology tab documenting payout assumptions, seed behavior, and model limits

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
