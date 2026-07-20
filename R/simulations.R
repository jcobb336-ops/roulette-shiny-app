next_bet_size <- function(strategy, base_bet, state) {
  if (strategy == "Flat betting") {
    return(base_bet)
  }

  if (strategy == "Martingale") {
    return(base_bet * 2^state$loss_streak)
  }

  if (strategy == "Fibonacci") {
    fib <- c(1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144)
    index <- min(state$fibonacci_index, length(fib))
    return(base_bet * fib[index])
  }

  if (strategy == "D'Alembert") {
    return(base_bet * state$dalembert_units)
  }

  stop("Unknown strategy: ", strategy, call. = FALSE)
}

update_strategy_state <- function(strategy, state, won) {
  if (strategy == "Flat betting") {
    return(state)
  }

  if (strategy == "Martingale") {
    state$loss_streak <- if (won) 0 else state$loss_streak + 1
    return(state)
  }

  if (strategy == "Fibonacci") {
    state$fibonacci_index <- if (won) {
      max(1, state$fibonacci_index - 2)
    } else {
      state$fibonacci_index + 1
    }
    return(state)
  }

  if (strategy == "D'Alembert") {
    state$dalembert_units <- if (won) {
      max(1, state$dalembert_units - 1)
    } else {
      state$dalembert_units + 1
    }
    return(state)
  }

  state
}

simulate_one_session <- function(initial_bankroll, base_bet, spins, bet_type, strategy) {
  bet <- get_bet(bet_type)
  p_win <- bet$win_slots / AMERICAN_ROULETTE$slot_count
  bankroll <- initial_bankroll
  peak_bankroll <- initial_bankroll
  max_drawdown <- 0
  total_wagered <- 0
  wins <- 0
  losses <- 0
  state <- list(loss_streak = 0, fibonacci_index = 1, dalembert_units = 1)

  spin_values <- 0:spins
  bankroll_values <- rep(NA_real_, spins + 1)
  bet_values <- rep(NA_real_, spins + 1)
  won_values <- rep(NA, spins + 1)
  profit_values <- rep(NA_real_, spins + 1)

  bankroll_values[1] <- bankroll
  bet_values[1] <- 0
  profit_values[1] <- 0
  played <- 0

  for (spin in seq_len(spins)) {
    requested_bet <- next_bet_size(strategy, base_bet, state)

    if (bankroll <= 0) {
      break
    }

    actual_bet <- min(requested_bet, bankroll)
    won <- stats::runif(1) < p_win
    spin_profit <- if (won) {
      wins <- wins + 1
      actual_bet * bet$payout_to_one
    } else {
      losses <- losses + 1
      -actual_bet
    }

    bankroll <- bankroll + spin_profit
    total_wagered <- total_wagered + actual_bet
    peak_bankroll <- max(peak_bankroll, bankroll)
    max_drawdown <- max(max_drawdown, peak_bankroll - bankroll)
    state <- update_strategy_state(strategy, state, won)

    played <- spin
    index <- spin + 1
    bankroll_values[index] <- bankroll
    bet_values[index] <- actual_bet
    won_values[index] <- won
    profit_values[index] <- spin_profit
  }

  keep <- seq_len(played + 1)
  path <- data.frame(
    spin = spin_values[keep],
    bankroll = bankroll_values[keep],
    bet = bet_values[keep],
    won = won_values[keep],
    profit = profit_values[keep],
    stringsAsFactors = FALSE
  )

  list(
    path = path,
    final_bankroll = bankroll,
    profit = bankroll - initial_bankroll,
    spins_played = played,
    wins = wins,
    losses = losses,
    ruined = bankroll <= 0,
    max_drawdown = max_drawdown,
    total_wagered = total_wagered
  )
}

simulate_sessions <- function(
    initial_bankroll,
    base_bet,
    spins,
    simulations,
    bet_type,
    strategy = "Flat betting",
    seed = NULL,
    retained_paths = 80) {
  if (!is.null(seed) && !is.na(seed)) {
    set.seed(seed)
  }

  retained <- min(simulations, retained_paths)
  sessions <- vector("list", max(retained, 1))
  final_bankroll <- numeric(simulations)
  profit <- numeric(simulations)
  spins_played <- integer(simulations)
  wins <- integer(simulations)
  losses <- integer(simulations)
  ruined <- logical(simulations)
  max_drawdown <- numeric(simulations)
  total_wagered <- numeric(simulations)

  for (session_id in seq_len(simulations)) {
    result <- simulate_one_session(
      initial_bankroll = initial_bankroll,
      base_bet = base_bet,
      spins = spins,
      bet_type = bet_type,
      strategy = strategy
    )

    if (session_id <= retained) {
      result$path$session <- session_id
      sessions[[session_id]] <- result$path
    }

    final_bankroll[session_id] <- result$final_bankroll
    profit[session_id] <- result$profit
    spins_played[session_id] <- result$spins_played
    wins[session_id] <- result$wins
    losses[session_id] <- result$losses
    ruined[session_id] <- result$ruined
    max_drawdown[session_id] <- result$max_drawdown
    total_wagered[session_id] <- result$total_wagered
  }

  paths <- if (retained > 0) {
    do.call(rbind, sessions[seq_len(retained)])
  } else {
    data.frame(
      spin = integer(),
      bankroll = numeric(),
      bet = numeric(),
      won = logical(),
      profit = numeric(),
      session = integer()
    )
  }

  list(
    paths = paths,
    outcomes = data.frame(
      session = seq_len(simulations),
      final_bankroll = final_bankroll,
      profit = profit,
      spins_played = spins_played,
      wins = wins,
      losses = losses,
      ruined = ruined,
      max_drawdown = max_drawdown,
      total_wagered = total_wagered,
      strategy = strategy,
      stringsAsFactors = FALSE
    )
  )
}

simulate_strategy_comparison <- function(
    initial_bankroll,
    base_bet,
    spins,
    simulations,
    bet_type,
    seed = NULL) {
  strategies <- strategy_names()

  results <- lapply(seq_along(strategies), function(index) {
    strategy_seed <- if (is.null(seed) || is.na(seed)) NULL else seed + index - 1
    sim <- simulate_sessions(
      initial_bankroll = initial_bankroll,
      base_bet = base_bet,
      spins = spins,
      simulations = simulations,
      bet_type = bet_type,
      strategy = strategies[index],
      seed = strategy_seed,
      retained_paths = 0
    )
    sim$outcomes
  })

  do.call(rbind, results)
}
