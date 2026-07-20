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
  heading_font = font_google("Inter")
)

kpi_card <- function(label, value, icon = NULL, class = "") {
  card(
    class = paste("kpi-card", class),
    div(
      class = "kpi-card__content",
      div(class = "kpi-card__label", label),
      div(class = "kpi-card__value", value)
    ),
    if (!is.null(icon)) div(class = "kpi-card__icon", icon)
  )
}

roulette_number <- function(value) {
  number <- suppressWarnings(as.integer(value))
  color_class <- if (value %in% AMERICAN_ROULETTE$green_slots) {
    "green"
  } else if (number %in% AMERICAN_ROULETTE$red_numbers) {
    "red"
  } else {
    "black"
  }

  div(class = paste("roulette-cell", paste0("roulette-cell--", color_class)), value)
}

roulette_table_ui <- function() {
  number_rows <- list(
    seq(3, 36, by = 3),
    seq(2, 35, by = 3),
    seq(1, 34, by = 3)
  )

  tagList(
    div(
      class = "roulette-layout",
      div(class = "roulette-zeroes", roulette_number("0"), roulette_number("00")),
      div(
        class = "roulette-number-grid",
        lapply(number_rows, function(row_numbers) {
          div(class = "roulette-row", lapply(row_numbers, roulette_number))
        })
      )
    ),
    div(
      class = "roulette-outside-grid",
      div(class = "outside-cell", "First dozen", tags$small("1-12")),
      div(class = "outside-cell", "Second dozen", tags$small("13-24")),
      div(class = "outside-cell", "Third dozen", tags$small("25-36")),
      div(class = "outside-cell", "1 to 18"),
      div(class = "outside-cell", "Even"),
      div(class = "outside-cell outside-cell--red", "Red"),
      div(class = "outside-cell outside-cell--black", "Black"),
      div(class = "outside-cell", "Odd"),
      div(class = "outside-cell", "19 to 36")
    )
  )
}

ui <- page_sidebar(
  title = div(
    class = "app-title",
    span("American Roulette Risk Lab"),
    tags$small("38 slots: 18 Red, 18 Black, 2 Green (0, 00)")
  ),
  theme = theme,
  fillable = TRUE,
  sidebar = sidebar(
    width = 330,
    class = "input-sidebar",
    h4("Betting System"),
    radioButtons(
      "bet_type",
      "Choose a color",
      choices = color_bets()$bet_type,
      selected = "Red"
    ),
    numericInput("initial_bankroll", "Initial bankroll", value = 500, min = 1, step = 25),
    numericInput("base_bet", "Base bet", value = 10, min = 1, step = 5),
    sliderInput("spins", "Spins per session", min = 10, max = 500, value = 150, step = 10),
    sliderInput("simulations", "Simulation runs", min = 100, max = 5000, value = 1000, step = 100),
    checkboxInput("use_seed", "Use reproducible seed", value = FALSE),
    conditionalPanel(
      "input.use_seed",
      numericInput("seed", "Random seed", value = 12345, min = 1, step = 1)
    ),
    actionButton("run", "Run Simulation", class = "btn-primary w-100"),
    div(class = "run-status", textOutput("run_status")),
    tags$hr(),
    div(
      class = "sidebar-note",
      "Choose a color, set your parameters, and run a simulation to analyze short-run outcomes and long-run risk."
    )
  ),
  tags$head(tags$link(rel = "stylesheet", href = "styles.css")),
  navset_card_tab(
    full_screen = TRUE,
    nav_panel(
      "Overview",
      card(
        card_header("American Roulette Table"),
        p("An American roulette wheel has 38 pockets: numbers 1-36, plus 0 and 00. Red bets win on red numbers, black bets win on black numbers, and green bets win only when the ball lands on 0 or 00."),
        roulette_table_ui()
      ),
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        row_heights = "6.25rem",
        gap = "1.5rem",
        kpi_card("Red", tagList(div("18/38"), tags$small("47.37%")), bsicons::bs_icon("circle-fill"), "kpi-card--red"),
        kpi_card("Black", tagList(div("18/38"), tags$small("47.37%")), bsicons::bs_icon("circle-fill"), "kpi-card--black"),
        kpi_card("Green (0, 00)", tagList(div("2/38"), tags$small("5.26%")), bsicons::bs_icon("circle-fill"), "kpi-card--green"),
        kpi_card("House Edge", tagList(div("5.26%"), tags$small("Long-run disadvantage")), bsicons::bs_icon("graph-down"))
      ),
      card(
        card_header("Quick Rules"),
        tags$ul(
          tags$li("Red wins if the ball lands on any red number."),
          tags$li("Black wins if the ball lands on any black number."),
          tags$li("Green wins only if the ball lands on 0 or 00."),
          tags$li("Red and black bets pay even money, or 1 to 1."),
          tags$li("A direct bet on either 0 or 00 is a single-number bet and pays 35 to 1."),
          tags$li("The app models Green (0, 00) as one wager covering both green pockets with a 17 to 1 payout, the same payout rate as a two-number split-style bet. It is not treated as a 35 to 1 single-number bet.")
        ),
        p(class = "note-text", "The 5.26% house edge applies to standard American roulette bets when the listed payouts are used.")
      ),
      card(
        card_header("Roulette Bets & Payouts"),
        tableOutput("payout_table"),
        p(class = "note-text", "All probabilities and payouts are based on American roulette with 38 pockets, including 0 and 00.")
      )
    ),
    nav_panel(
      "Analysis",
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        row_heights = "6.25rem",
        gap = "1.5rem",
        kpi_card("Winning probability", textOutput("win_probability"), bsicons::bs_icon("check-circle")),
        kpi_card("Losing probability", textOutput("loss_probability"), bsicons::bs_icon("x-circle")),
        kpi_card("Payout", textOutput("payout"), bsicons::bs_icon("cash-coin")),
        kpi_card("House edge", textOutput("house_edge"), bsicons::bs_icon("graph-down"))
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Expected-value calculation"),
          uiOutput("ev_explanation")
        ),
        card(
          card_header("Plain-language interpretation"),
          uiOutput("plain_language")
        )
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
      card(
        class = "simulation-context-card",
        div(
          class = "simulation-context",
          div(class = "simulation-context__label", "Current simulation"),
          div(class = "simulation-context__value", textOutput("simulation_context", inline = TRUE))
        )
      ),
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        row_heights = "6.25rem",
        gap = "1.5rem",
        kpi_card("Wins", textOutput("total_wins"), bsicons::bs_icon("check-circle")),
        kpi_card("Losses", textOutput("total_losses"), bsicons::bs_icon("x-circle")),
        kpi_card("Average ending bankroll", textOutput("mean_final"), bsicons::bs_icon("wallet2")),
        kpi_card("Net profit/loss", textOutput("net_profit"), bsicons::bs_icon("activity"))
      ),
      layout_columns(
        col_widths = c(6, 6),
        class = "simulation-plot-grid",
        card(
          card_header("Cumulative bankroll"),
          plotOutput("paths_plot", height = 420)
        ),
        card(
          card_header("Ending bankroll distribution"),
          plotOutput("distribution_plot", height = 420)
        )
      ),
      card(
        card_header("Simulated average vs theoretical expected value"),
        plotOutput("theory_comparison_plot", height = 360)
      ),
      card(
        card_header("Detailed results"),
        tableOutput("simulation_table")
      )
    ),
    nav_panel(
      "Strategy Comparison",
      card(
        class = "warning-card",
        strong("Important: "),
        "Betting systems may change short-term volatility and bankroll risk, but they do not change the underlying expected value of the roulette game."
      ),
      card(
        card_header("Flat betting vs Martingale vs Fibonacci vs D'Alembert"),
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
          tags$li("The app uses a 38-pocket American roulette wheel: 18 red, 18 black, and 2 green pockets, 0 and 00."),
          tags$li("Red probability is 18/38. Black probability is 18/38. Green-pocket probability is 2/38."),
          tags$li("Red and black pay 1:1. The app's Green (0, 00) option is modeled as a two-pocket wager paying 17:1, not as a 35:1 single-number bet."),
          tags$li("Expected value is calculated as win probability times win profit plus loss probability times loss amount."),
          tags$li("Simulations draw independent random outcomes using the selected color probability for each spin."),
          tags$li("When the reproducible seed box is checked, the selected seed fixes the random sequence for repeatable results."),
          tags$li("Sessions stop early when the bankroll reaches zero. If a strategy requests more than the remaining bankroll, the app wagers the remaining bankroll."),
          tags$li("No table maximum, taxes, comps, dealer errors, wheel bias, or casino promotions are modeled.")
        )
      ),
      card(
        card_header("Strategy definitions"),
        tags$ul(
          tags$li(strong("Flat betting: "), "the wager stays equal to the base bet."),
          tags$li(strong("Martingale: "), "the wager doubles after each loss and resets after a win."),
          tags$li(strong("Fibonacci: "), "the wager moves one step up the Fibonacci sequence after a loss and two steps down after a win."),
          tags$li(strong("D'Alembert: "), "the wager increases by one base unit after a loss and decreases by one base unit after a win.")
        )
      )
    )
  ),
  div(class = "app-footer", "For educational purposes only. Gambling involves risk. Play responsibly.")
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

    withProgress(message = "Running roulette simulation", value = 0.35, {
      result <- simulate_sessions(
        initial_bankroll = input$initial_bankroll,
        base_bet = input$base_bet,
        spins = input$spins,
        simulations = input$simulations,
        bet_type = input$bet_type,
        strategy = "Flat betting",
        seed = simulation_seed()
      )
      incProgress(0.65)
      result
    })
  }, ignoreNULL = TRUE)

  comparison <- eventReactive(input$run, {
    withProgress(message = "Comparing strategies", value = 0.35, {
      result <- simulate_strategy_comparison(
        initial_bankroll = input$initial_bankroll,
        base_bet = input$base_bet,
        spins = input$spins,
        simulations = input$simulations,
        bet_type = input$bet_type,
        seed = simulation_seed()
      )
      incProgress(0.65)
      result
    })
  }, ignoreNULL = TRUE)

  summary_stats <- reactive({
    req(simulation())
    summarize_outcomes(
      simulation()$outcomes,
      initial_bankroll = input$initial_bankroll,
      base_bet = input$base_bet,
      spins = input$spins,
      bet_type = input$bet_type
    )
  })

  output$payout_table <- renderTable({
    roulette_bets() |>
      rename(
        `Bet type` = bet_type,
        `Also called` = also_called,
        Example = example,
        Payout = payout
      )
  })

  output$run_status <- renderText({
    if (input$run == 0) {
      "Ready to run with current inputs."
    } else {
      paste(
        "Last run:",
        format(Sys.time(), "%I:%M:%S %p"),
        "|",
        format(input$simulations, big.mark = ","),
        "sessions"
      )
    }
  })

  output$simulation_context <- renderText({
    if (input$run == 0) {
      return("Set parameters in the sidebar, then click Run Simulation.")
    }

    paste(
      format(input$simulations, big.mark = ","),
      "sessions",
      "|",
      input$spins,
      "spins each",
      "| Flat betting |",
      input$bet_type
    )
  })

  output$win_probability <- renderText(percent(metrics()$probability_win, accuracy = 0.01))
  output$loss_probability <- renderText(percent(metrics()$probability_loss, accuracy = 0.01))
  output$payout <- renderText(metrics()$payout_label)
  output$house_edge <- renderText(percent(metrics()$house_edge, accuracy = 0.01))

  output$ev_explanation <- renderUI({
    m <- metrics()
    win_profit <- input$base_bet * m$payout_to_one
    formula <- paste0(
      "EV = (", m$win_slots, "/38 x ", currency(win_profit), ") + (",
      m$losing_slots, "/38 x -", currency(input$base_bet), ")"
    )

    tagList(
      div(class = "formula-stack",
        div(strong("Probability of winning: "), m$win_slots, "/38 = ", percent(m$probability_win, accuracy = 0.01)),
        div(strong("Probability of losing: "), m$losing_slots, "/38 = ", percent(m$probability_loss, accuracy = 0.01)),
        div(strong("Payout: "), m$payout_label),
        div(class = "ev-formula", formula),
        div(class = "ev-formula", "EV = ", currency_precise(m$expected_value_per_bet), " per spin"),
        div(strong("Expected loss over selected spins: "), currency(m$expected_value_total))
      )
    )
  })

  output$plain_language <- renderUI({
    m <- metrics()

    if (input$bet_type %in% c("Red", "Black")) {
      tagList(
        p("For a $1 red or black bet:"),
        div(class = "formula-stack",
          div("Probability of winning = 18/38 = 47.37%"),
          div("Probability of losing = 20/38 = 52.63%"),
          div(class = "ev-formula", "EV = (18/38 x $1) + (20/38 x -$1)"),
          div(class = "ev-formula", "EV = -$0.0526 per $1 wagered")
        ),
        p("Red and black are not true 50/50 bets because both 0 and 00 cause the wager to lose.")
      )
    } else {
      tagList(
        p("The Green (0, 00) option wins only on the two green pockets and loses on the other 36 pockets."),
        p("This app models that selection as a two-pocket wager paying 17 to 1, so its expected loss is still 5.26 cents per $1 wagered."),
        p("That keeps the probability, payout, and expected-value calculation consistent.")
      )
    }
  })

  output$metric_table <- renderTable({
    m <- metrics()
    data.frame(
      Metric = c(
        "Game",
        "Selected bet",
        "Winning pockets",
        "Losing pockets",
        "Winning probability",
        "Losing probability",
        "Payout",
        "Expected profit or loss per spin",
        "House edge",
        "Expected loss over selected spins",
        "Payout assumption"
      ),
      Value = c(
        m$game,
        m$bet_type,
        m$win_slots,
        m$losing_slots,
        percent(m$probability_win, accuracy = 0.01),
        percent(m$probability_loss, accuracy = 0.01),
        m$payout_label,
        currency(m$expected_value_per_bet),
        percent(m$house_edge, accuracy = 0.01),
        currency(m$expected_value_total),
        m$model_note
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
      roulette_plot_theme()
  })

  output$total_wins <- renderText({
    req(summary_stats())
    format(summary_stats()$wins, big.mark = ",")
  })
  output$total_losses <- renderText({
    req(summary_stats())
    format(summary_stats()$losses, big.mark = ",")
  })
  output$mean_final <- renderText(currency(summary_stats()$mean_final_bankroll))
  output$net_profit <- renderText(currency(summary_stats()$net_profit))

  output$paths_plot <- renderPlot({
    req(simulation())
    expected <- ev_curve(input$initial_bankroll, input$bet_type, input$base_bet, input$spins)
    plot_bankroll_paths(simulation()$paths, expected)
  })

  output$distribution_plot <- renderPlot({
    req(simulation(), summary_stats())
    theoretical_final <- input$initial_bankroll + summary_stats()$theoretical_average_per_spin * input$spins
    plot_final_distribution(simulation()$outcomes, input$initial_bankroll, theoretical_final)
  })

  output$theory_comparison_plot <- renderPlot({
    req(summary_stats())
    plot_theoretical_comparison(summary_stats(), input$initial_bankroll, input$spins)
  })

  output$simulation_table <- renderTable({
    req(summary_stats())
    s <- summary_stats()
    data.frame(
      Metric = c(
        "Sessions",
        "Wins",
        "Losses",
        "Ending bankroll",
        "Net profit or loss",
        "Simulated win percentage",
        "Average result per spin",
        "Theoretical expected result",
        "Difference between simulated and theoretical",
        "Probability of finishing profitable",
        "Probability of bankroll depletion",
        "Average maximum drawdown",
        "Total amount wagered"
      ),
      Value = c(
        format(s$sessions, big.mark = ","),
        format(s$wins, big.mark = ","),
        format(s$losses, big.mark = ","),
        currency(s$mean_final_bankroll),
        currency(s$net_profit),
        percent(s$simulated_win_percentage, accuracy = 0.01),
        currency_precise(s$average_result_per_spin),
        currency(s$theoretical_expected_result),
        currency(s$difference_from_theoretical),
        percent(s$probability_profit, accuracy = 0.1),
        percent(s$probability_ruin, accuracy = 0.1),
        currency(s$max_drawdown),
        currency(s$total_wagered)
      )
    )
  })

  output$comparison_plot <- renderPlot({
    req(comparison())
    plot_strategy_comparison(comparison(), input$initial_bankroll)
  })

  output$comparison_table <- renderTable({
    req(comparison())
    comparison() |>
      group_by(strategy) |>
      summarise(
        `Average ending bankroll` = currency(mean(final_bankroll)),
        `Average profit or loss` = currency(mean(profit)),
        `Probability of profit` = percent(mean(profit > 0), accuracy = 0.1),
        `Risk of ruin` = percent(mean(ruined), accuracy = 0.1),
        `Maximum drawdown` = currency(mean(max_drawdown)),
        `Total amount wagered` = currency(sum(total_wagered)),
        .groups = "drop"
      ) |>
      rename(Strategy = strategy)
  })
}

shinyApp(ui, server)
