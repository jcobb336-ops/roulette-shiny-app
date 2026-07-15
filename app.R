library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(scales)

source("R/roulette_rules.R")
source("R/calculations.R")
source("R/simulations.R")
source("R/plotting.R")

theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#426b69",
  secondary = "#7467a9",
  base_font = font_google("Inter"),
  heading_font = font_google("Source Serif 4")
)

ui <- page_sidebar(
  title = div(
    class = "app-title",
    span("American Roulette Risk Lab"),
    tags$small("38 slots: 0, 00, and 1-36")
  ),
  theme = theme,
  fillable = TRUE,
  sidebar = sidebar(
    width = 330,
    class = "input-sidebar",
    h4("Scenario"),
    selectInput(
      "bet_type",
      "Bet type",
      choices = roulette_bets()$bet_type,
      selected = "Red or black"
    ),
    selectInput(
      "strategy",
      "Betting strategy",
      choices = strategy_names(),
      selected = "Flat betting"
    ),
    numericInput("initial_bankroll", "Initial bankroll", value = 500, min = 1, step = 25),
    numericInput("base_bet", "Base bet", value = 10, min = 1, step = 5),
    sliderInput("spins", "Spins per session", min = 10, max = 500, value = 150, step = 10),
    sliderInput("simulations", "Simulation runs", min = 100, max = 5000, value = 1000, step = 100),
    checkboxInput("use_seed", "Use reproducible seed", value = TRUE),
    conditionalPanel(
      "input.use_seed",
      numericInput("seed", "Seed", value = 2026, min = 1, step = 1)
    ),
    actionButton("run", "Run simulation", class = "btn-primary w-100"),
    tags$hr(),
    div(
      class = "sidebar-note",
      strong("Fixed game: "),
      "American roulette with 38 slots. All probabilities, simulations, and expected values use this wheel."
    )
  ),
  tags$head(tags$link(rel = "stylesheet", href = "styles.css")),
  navset_card_tab(
    full_screen = TRUE,
    nav_panel(
      "Overview",
      layout_columns(
        col_widths = c(4, 4, 4),
        value_box(
          "Wheel slots",
          AMERICAN_ROULETTE$slot_count,
          showcase = bsicons::bs_icon("circle")
        ),
        value_box(
          "House edge",
          percent(AMERICAN_ROULETTE$house_edge, accuracy = 0.01),
          showcase = bsicons::bs_icon("graph-down")
        ),
        value_box(
          "Even-money win rate",
          percent(18 / AMERICAN_ROULETTE$slot_count, accuracy = 0.01),
          showcase = bsicons::bs_icon("percent")
        )
      ),
      card(
        card_header("Assignment framing"),
        p("This app analyzes roulette as a negative expected value betting problem. The game is fixed to American roulette, whose two zero slots make every standard bet unfavorable over repeated play."),
        p("Use the controls to choose a bet type, bankroll, session length, and betting strategy. The app reports exact expected value calculations and Monte Carlo simulation results.")
      ),
      card(
        card_header("Available bets"),
        tableOutput("bet_table")
      )
    ),
    nav_panel(
      "Analysis",
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        value_box("Win probability", textOutput("win_probability"), showcase = bsicons::bs_icon("check-circle")),
        value_box("Loss probability", textOutput("loss_probability"), showcase = bsicons::bs_icon("x-circle")),
        value_box("EV per bet", textOutput("ev_per_bet"), showcase = bsicons::bs_icon("calculator")),
        value_box("EV over session", textOutput("ev_total"), showcase = bsicons::bs_icon("activity"))
      ),
      card(
        card_header("Exact calculation summary"),
        tableOutput("metric_table")
      ),
      card(
        card_header("Theoretical expected bankroll"),
        plotOutput("ev_plot", height = 360)
      )
    ),
    nav_panel(
      "Simulation",
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        value_box("Mean final bankroll", textOutput("mean_final"), showcase = bsicons::bs_icon("wallet2")),
        value_box("Probability of profit", textOutput("prob_profit"), showcase = bsicons::bs_icon("arrow-up-circle")),
        value_box("Probability of ruin", textOutput("prob_ruin"), showcase = bsicons::bs_icon("exclamation-triangle")),
        value_box("Median final bankroll", textOutput("median_final"), showcase = bsicons::bs_icon("bar-chart"))
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(plotOutput("paths_plot", height = 420)),
        card(plotOutput("distribution_plot", height = 420))
      ),
      card(
        card_header("Simulation summary"),
        tableOutput("simulation_table")
      )
    ),
    nav_panel(
      "Strategy Comparison",
      card(
        card_header("Flat betting vs Martingale vs Fibonacci"),
        plotOutput("comparison_plot", height = 460)
      ),
      card(
        card_header("Comparison table"),
        tableOutput("comparison_table")
      )
    ),
    nav_panel(
      "Methodology",
      card(
        card_header("Model assumptions"),
        tags$ul(
          tags$li("The wheel has 38 equally likely outcomes: 0, 00, and 1 through 36."),
          tags$li("Sessions stop early when the bankroll reaches zero."),
          tags$li("If a strategy requests a stake larger than the remaining bankroll, the app wagers the remaining bankroll."),
          tags$li("No table maximum is modeled in this version, so Martingale risk is limited by bankroll rather than a casino limit."),
          tags$li("Expected value calculations use the selected bet payout and American roulette probabilities.")
        )
      ),
      card(
        card_header("Strategy definitions"),
        tags$ul(
          tags$li(strong("Flat betting: "), "the wager stays equal to the base bet."),
          tags$li(strong("Martingale: "), "the wager doubles after each loss and resets after a win."),
          tags$li(strong("Fibonacci: "), "the wager moves one step up the Fibonacci sequence after a loss and two steps down after a win.")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$base_bet, {
    if (input$base_bet > input$initial_bankroll) {
      updateNumericInput(session, "base_bet", value = input$initial_bankroll)
    }
  })

  metrics <- reactive({
    validate(
      need(input$initial_bankroll > 0, "Initial bankroll must be positive."),
      need(input$base_bet > 0, "Base bet must be positive."),
      need(input$base_bet <= input$initial_bankroll, "Base bet cannot exceed the initial bankroll.")
    )

    bet_metrics(input$bet_type, input$base_bet, input$spins)
  })

  simulation_seed <- reactive({
    if (isTRUE(input$use_seed)) input$seed else NULL
  })

  simulation <- eventReactive(input$run, {
    validate(
      need(input$base_bet <= input$initial_bankroll, "Base bet cannot exceed the initial bankroll.")
    )

    simulate_sessions(
      initial_bankroll = input$initial_bankroll,
      base_bet = input$base_bet,
      spins = input$spins,
      simulations = input$simulations,
      bet_type = input$bet_type,
      strategy = input$strategy,
      seed = simulation_seed()
    )
  }, ignoreNULL = FALSE)

  comparison <- eventReactive(input$run, {
    simulate_strategy_comparison(
      initial_bankroll = input$initial_bankroll,
      base_bet = input$base_bet,
      spins = input$spins,
      simulations = input$simulations,
      bet_type = input$bet_type,
      seed = simulation_seed()
    )
  }, ignoreNULL = FALSE)

  summary_stats <- reactive({
    summarize_outcomes(simulation()$outcomes, input$initial_bankroll)
  })

  output$bet_table <- renderTable({
    roulette_bets() |>
      mutate(
        probability_win = percent(win_slots / AMERICAN_ROULETTE$slot_count, accuracy = 0.01),
        payout = paste0(payout_to_one, ":1")
      ) |>
      select(`Bet type` = bet_type, `Winning slots` = win_slots, `Win probability` = probability_win, Payout = payout)
  })

  output$win_probability <- renderText(percent(metrics()$probability_win, accuracy = 0.01))
  output$loss_probability <- renderText(percent(metrics()$probability_loss, accuracy = 0.01))
  output$ev_per_bet <- renderText(currency(metrics()$expected_value_per_bet))
  output$ev_total <- renderText(currency(metrics()$expected_value_total))

  output$metric_table <- renderTable({
    m <- metrics()
    data.frame(
      Metric = c(
        "Game",
        "Slots",
        "Selected bet",
        "Winning slots",
        "Payout",
        "Expected value per bet",
        "Standard deviation per bet",
        "Expected value over selected spins"
      ),
      Value = c(
        m$game,
        m$slots,
        m$bet_type,
        m$win_slots,
        paste0(m$payout_to_one, ":1"),
        currency(m$expected_value_per_bet),
        currency(m$sd_per_bet),
        currency(m$expected_value_total)
      )
    )
  })

  output$ev_plot <- renderPlot({
    curve <- ev_curve(input$initial_bankroll, input$bet_type, input$base_bet, input$spins)
    ggplot(curve, aes(spin, expected_bankroll)) +
      geom_line(color = "#b13f3f", linewidth = 1.2) +
      geom_hline(yintercept = input$initial_bankroll, linetype = "dashed", color = "#202124") +
      labs(x = "Spin", y = "Expected bankroll") +
      scale_y_continuous(labels = dollar) +
      theme_minimal(base_size = 13)
  })

  output$mean_final <- renderText(currency(summary_stats()$mean_final_bankroll))
  output$prob_profit <- renderText(percent(summary_stats()$probability_profit, accuracy = 0.1))
  output$prob_ruin <- renderText(percent(summary_stats()$probability_ruin, accuracy = 0.1))
  output$median_final <- renderText(currency(summary_stats()$median_final_bankroll))

  output$paths_plot <- renderPlot({
    expected <- ev_curve(input$initial_bankroll, input$bet_type, input$base_bet, input$spins)
    plot_bankroll_paths(simulation()$paths, expected)
  })

  output$distribution_plot <- renderPlot({
    plot_final_distribution(simulation()$outcomes, input$initial_bankroll)
  })

  output$simulation_table <- renderTable({
    s <- summary_stats()
    data.frame(
      Metric = c(
        "Sessions",
        "Mean final bankroll",
        "Median final bankroll",
        "Mean profit",
        "Probability of profit",
        "Probability of ruin",
        "Worst final bankroll",
        "Best final bankroll",
        "5th percentile",
        "95th percentile"
      ),
      Value = c(
        format(s$sessions, big.mark = ","),
        currency(s$mean_final_bankroll),
        currency(s$median_final_bankroll),
        currency(s$mean_profit),
        percent(s$probability_profit, accuracy = 0.1),
        percent(s$probability_ruin, accuracy = 0.1),
        currency(s$worst_outcome),
        currency(s$best_outcome),
        currency(s$p05_final_bankroll),
        currency(s$p95_final_bankroll)
      )
    )
  })

  output$comparison_plot <- renderPlot({
    plot_strategy_comparison(comparison(), input$initial_bankroll)
  })

  output$comparison_table <- renderTable({
    comparison() |>
      group_by(strategy) |>
      summarise(
        `Mean final bankroll` = currency(mean(final_bankroll)),
        `Median final bankroll` = currency(stats::median(final_bankroll)),
        `Probability of profit` = percent(mean(profit > 0), accuracy = 0.1),
        `Probability of ruin` = percent(mean(ruined), accuracy = 0.1),
        .groups = "drop"
      ) |>
      rename(Strategy = strategy)
  })
}

shinyApp(ui, server)
