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

bet_button_id <- function(bet_id) {
  paste0("bet_", bet_id)
}

roulette_bet_button <- function(bet_id, label, class, selected_bet_id, sublabel = NULL) {
  selected_class <- if (identical(bet_id, selected_bet_id)) "is-selected" else ""
  actionButton(
    bet_button_id(bet_id),
    label = tagList(span(label), if (!is.null(sublabel)) tags$small(sublabel)),
    class = paste("roulette-bet-button", class, selected_class),
    title = paste("Select", label)
  )
}

roulette_table_ui <- function(selected_bet_id = NULL) {
  number_rows <- list(
    seq(3, 36, by = 3),
    seq(2, 35, by = 3),
    seq(1, 34, by = 3)
  )

  number_button <- function(value) {
    pocket <- as.character(value)
    bet_id <- paste0("straight_", ifelse(pocket == "00", "double_zero", pocket))
    roulette_bet_button(
      bet_id,
      pocket,
      paste("roulette-cell", paste0("roulette-cell--", pocket_color_class(pocket))),
      selected_bet_id
    )
  }

  tagList(
    div(
      class = "roulette-layout",
      div(class = "roulette-zeroes", number_button("0"), number_button("00")),
      div(
        class = "roulette-number-grid",
        lapply(number_rows, function(row_numbers) {
          div(class = "roulette-row", lapply(row_numbers, number_button))
        })
      )
    ),
    div(
      class = "roulette-outside-grid roulette-dozen-grid",
      roulette_bet_button("dozen_1", "First dozen", "outside-cell", selected_bet_id, "1-12"),
      roulette_bet_button("dozen_2", "Second dozen", "outside-cell", selected_bet_id, "13-24"),
      roulette_bet_button("dozen_3", "Third dozen", "outside-cell", selected_bet_id, "25-36")
    ),
    div(
      class = "roulette-outside-grid",
      roulette_bet_button("low", "1 to 18", "outside-cell", selected_bet_id),
      roulette_bet_button("even", "Even", "outside-cell", selected_bet_id),
      roulette_bet_button("red", "Red", "outside-cell outside-cell--red", selected_bet_id),
      roulette_bet_button("black", "Black", "outside-cell outside-cell--black", selected_bet_id),
      roulette_bet_button("odd", "Odd", "outside-cell", selected_bet_id),
      roulette_bet_button("high", "19 to 36", "outside-cell", selected_bet_id)
    ),
    div(
      class = "roulette-outside-grid roulette-column-grid",
      roulette_bet_button("column_1", "Column 1", "outside-cell", selected_bet_id, "1, 4, 7 ... 34"),
      roulette_bet_button("column_2", "Column 2", "outside-cell", selected_bet_id, "2, 5, 8 ... 35"),
      roulette_bet_button("column_3", "Column 3", "outside-cell", selected_bet_id, "3, 6, 9 ... 36")
    )
  )
}

empty_history <- function() {
  data.frame(
    spin = integer(),
    winning_pocket = character(),
    winning_color = character(),
    selected_bet = character(),
    wager = numeric(),
    outcome = character(),
    net_result = numeric(),
    bankroll_after = numeric(),
    stringsAsFactors = FALSE
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
      "Long-run simulation color",
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
      "Use the table on the Overview tab for single spins. Use these controls for long-run simulation analysis."
    )
  ),
  tags$head(tags$link(rel = "stylesheet", href = "styles.css")),
  navset_card_tab(
    full_screen = TRUE,
    nav_panel(
      "Overview",
      layout_columns(
        col_widths = c(8, 4),
        card(
          card_header("Interactive American Roulette Table"),
          p("Click a number or outside betting area to select one active wager. The table uses standard American roulette colors and payouts."),
          uiOutput("roulette_table")
        ),
        card(
          card_header("Current Bet"),
          uiOutput("current_bet_panel"),
          numericInput("single_wager", "Wager amount", value = 10, min = 1, step = 5),
          uiOutput("spin_button_ui"),
          div(class = "bet-actions",
            actionButton("clear_bet", "Clear Bet", class = "btn-outline-secondary"),
            actionButton(
              "reset_session",
              "Reset Session",
              class = "btn-outline-danger",
              onclick = "return confirm('Reset this roulette session? Spin history and session results will be cleared.');"
            )
          ),
          uiOutput("spin_message")
        )
      ),
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        row_heights = "6.25rem",
        gap = "1.5rem",
        kpi_card("Current bankroll", textOutput("single_bankroll"), bsicons::bs_icon("wallet2")),
        kpi_card("Session profit/loss", textOutput("single_profit"), bsicons::bs_icon("activity")),
        kpi_card("Spins completed", textOutput("single_spins"), bsicons::bs_icon("arrow-repeat")),
        kpi_card("Win rate", textOutput("single_win_rate"), bsicons::bs_icon("percent"))
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Session summary"),
          tableOutput("single_summary_table")
        ),
        card(
          card_header("Bankroll over time"),
          plotOutput("single_bankroll_plot", height = 320)
        )
      ),
      card(
        card_header("Recent spins"),
        tableOutput("spin_history_table")
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
        card(card_header("Expected-value calculation"), uiOutput("ev_explanation")),
        card(card_header("Plain-language interpretation"), uiOutput("plain_language"))
      ),
      card(card_header("Exact calculation summary"), tableOutput("metric_table")),
      card(card_header("Theoretical expected bankroll"), plotOutput("ev_plot", height = 360))
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
        card(card_header("Cumulative bankroll"), plotOutput("paths_plot", height = 420)),
        card(card_header("Ending bankroll distribution"), plotOutput("distribution_plot", height = 420))
      ),
      card(card_header("Simulated average vs theoretical expected value"), plotOutput("theory_comparison_plot", height = 360)),
      card(card_header("Detailed results"), tableOutput("simulation_table"))
    ),
    nav_panel(
      "Strategy Comparison",
      card(
        class = "warning-card",
        strong("Important: "),
        "Betting systems may change short-term volatility and bankroll risk, but they do not change the underlying expected value of the roulette game."
      ),
      card(card_header("Flat betting vs Martingale vs Fibonacci vs D'Alembert"), plotOutput("comparison_plot", height = 460)),
      card(card_header("Comparison table"), tableOutput("comparison_table"))
    ),
    nav_panel(
      "Methodology",
      card(
        card_header("Model assumptions"),
        tags$ul(
          tags$li("The app uses a 38-pocket American roulette wheel: 18 red, 18 black, and 2 green pockets, 0 and 00."),
          tags$li("Clickable 0 and 00 spaces are separate straight-up bets paying 35:1."),
          tags$li("Red, black, odd, even, low, and high each cover 18 non-green pockets and pay 1:1."),
          tags$li("Dozens and columns each cover 12 non-green pockets and pay 2:1."),
          tags$li("Expected value is calculated as win probability times win profit plus loss probability times loss amount."),
          tags$li("The single-spin game and long-run simulations use the central rules and payout definitions in the R folder."),
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
  current_bankroll <- reactiveVal(500)
  selected_bet_id <- reactiveVal(NULL)
  spin_history <- reactiveVal(empty_history())
  last_message <- reactiveVal(NULL)

  observe({
    if (nrow(spin_history()) == 0) {
      current_bankroll(input$initial_bankroll)
    }
  })

  lapply(standard_bet_definitions()$bet_id, function(bet_id) {
    observeEvent(input[[bet_button_id(bet_id)]], {
      selected_bet_id(bet_id)
      last_message(NULL)
    }, ignoreInit = TRUE)
  })

  observeEvent(input$clear_bet, {
    selected_bet_id(NULL)
    last_message(NULL)
  })

  observeEvent(input$reset_session, {
    current_bankroll(input$initial_bankroll)
    selected_bet_id(NULL)
    spin_history(empty_history())
    last_message(NULL)
  })

  wager_valid <- reactive({
    validate_wager(input$single_wager, current_bankroll())
  })

  output$roulette_table <- renderUI({
    roulette_table_ui(selected_bet_id())
  })

  output$current_bet_panel <- renderUI({
    if (is.null(selected_bet_id())) {
      return(div(class = "current-bet-empty", "Select a number or outside bet from the table."))
    }

    m <- standard_bet_metrics(selected_bet_id(), input$single_wager)
    tableOutput <- data.frame(
      Metric = c(
        "Bet type",
        "Selection",
        "Pockets covered",
        "Win probability",
        "Payout",
        "Wager",
        "Potential net profit",
        "Total returned after a win"
      ),
      Value = c(
        m$bet_type,
        m$selection,
        m$pockets_covered,
        paste0(m$pockets_covered, "/38 = ", percent(m$probability_win, accuracy = 0.01)),
        m$payout_label,
        currency(m$wager),
        currency(m$potential_net_profit),
        currency(m$total_returned)
      )
    )

    tagList(
      div(class = "selected-bet-label", "Selected bet"),
      tags$table(
        class = "current-bet-table",
        tags$tbody(
          lapply(seq_len(nrow(tableOutput)), function(i) {
            tags$tr(tags$th(tableOutput$Metric[i]), tags$td(tableOutput$Value[i]))
          })
        )
      )
    )
  })

  output$spin_button_ui <- renderUI({
    invalid <- is.null(selected_bet_id()) || !isTRUE(wager_valid())
    button <- actionButton(
      "spin_wheel",
      "Spin Wheel",
      class = "btn-primary w-100"
    )
    if (invalid) {
      button <- tagAppendAttributes(button, disabled = "disabled", `aria-disabled` = "true")
    }
    button
  })

  observeEvent(input$spin_wheel, {
    req(selected_bet_id())
    valid <- wager_valid()
    validate(need(isTRUE(valid), valid))

    if (isTRUE(input$use_seed) && nrow(spin_history()) == 0) {
      set.seed(input$seed)
    }

    winning_pocket <- spin_wheel()
    result <- evaluate_bet_result(selected_bet_id(), winning_pocket, input$single_wager)
    new_bankroll <- update_bankroll(current_bankroll(), result$net_result)
    current_bankroll(new_bankroll)

    history <- spin_history()
    bet_label <- paste(result$bet_type, result$selection, sep = ": ")
    next_row <- data.frame(
      spin = nrow(history) + 1,
      winning_pocket = result$winning_pocket,
      winning_color = result$winning_color,
      selected_bet = bet_label,
      wager = result$wager,
      outcome = if (result$won) "Win" else "Loss",
      net_result = result$net_result,
      bankroll_after = new_bankroll,
      stringsAsFactors = FALSE
    )
    spin_history(rbind(history, next_row))

    last_message(list(
      won = result$won,
      pocket = result$winning_pocket,
      color = result$winning_color,
      net = result$net_result,
      bankroll = new_bankroll
    ))
  })

  output$spin_message <- renderUI({
    msg <- last_message()
    if (is.null(msg)) {
      valid <- wager_valid()
      if (!isTRUE(valid)) {
        return(div(class = "spin-message spin-message--neutral", valid))
      }
      return(div(class = "spin-message spin-message--neutral", "Ready to spin."))
    }

    div(
      class = paste("spin-message", if (msg$won) "spin-message--win" else "spin-message--loss"),
      div(strong("Result: "), msg$pocket, " ", msg$color),
      div(if (msg$won) "You won!" else paste0("You lost ", currency(abs(msg$net)), ".")),
      div(strong("Net result: "), currency(msg$net)),
      div(strong("New bankroll: "), currency(msg$bankroll))
    )
  })

  single_summary <- reactive({
    history <- spin_history()
    wins <- sum(history$outcome == "Win")
    losses <- sum(history$outcome == "Loss")
    total_wagered <- sum(history$wager)
    profit <- current_bankroll() - input$initial_bankroll

    data.frame(
      bankroll = current_bankroll(),
      profit = profit,
      total_wagered = total_wagered,
      spins = nrow(history),
      wins = wins,
      losses = losses,
      win_rate = if (nrow(history) > 0) wins / nrow(history) else NA_real_,
      largest_win = if (wins > 0) max(history$net_result) else 0,
      largest_loss = if (losses > 0) min(history$net_result) else 0,
      stringsAsFactors = FALSE
    )
  })

  output$single_bankroll <- renderText(currency(single_summary()$bankroll))
  output$single_profit <- renderText(currency(single_summary()$profit))
  output$single_spins <- renderText(format(single_summary()$spins, big.mark = ","))
  output$single_win_rate <- renderText({
    if (is.na(single_summary()$win_rate)) "0.0%" else percent(single_summary()$win_rate, accuracy = 0.1)
  })

  output$single_summary_table <- renderTable({
    s <- single_summary()
    data.frame(
      Metric = c("Current bankroll", "Session profit or loss", "Total amount wagered", "Spins completed", "Wins", "Losses", "Win rate", "Largest win", "Largest loss"),
      Value = c(currency(s$bankroll), currency(s$profit), currency(s$total_wagered), s$spins, s$wins, s$losses, if (is.na(s$win_rate)) "0.0%" else percent(s$win_rate, accuracy = 0.1), currency(s$largest_win), currency(s$largest_loss))
    )
  })

  output$single_bankroll_plot <- renderPlot({
    plot_session_bankroll(spin_history(), input$initial_bankroll)
  })

  output$spin_history_table <- renderTable({
    history <- spin_history()
    if (nrow(history) == 0) {
      return(data.frame(Message = "No spins yet. Select a bet and spin the wheel."))
    }

    history |>
      mutate(
        Wager = currency(wager),
        `Net result` = currency(net_result),
        `Bankroll after spin` = currency(bankroll_after)
      ) |>
      select(
        `Spin number` = spin,
        `Winning pocket` = winning_pocket,
        `Winning color` = winning_color,
        `Selected bet` = selected_bet,
        Wager,
        `Win or loss` = outcome,
        `Net result`,
        `Bankroll after spin`
      ) |>
      arrange(desc(`Spin number`)) |>
      head(12)
  })

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
    validate(need(input$base_bet <= input$initial_bankroll, "Base bet cannot exceed the initial bankroll."))

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
      rename(`Bet type` = bet_type, `Also called` = also_called, Example = example, Payout = payout)
  })

  output$run_status <- renderText({
    if (input$run == 0) {
      "Ready to run with current inputs."
    } else {
      paste("Last run:", format(Sys.time(), "%I:%M:%S %p"), "|", format(input$simulations, big.mark = ","), "sessions")
    }
  })

  output$simulation_context <- renderText({
    if (input$run == 0) {
      return("Set parameters in the sidebar, then click Run Simulation.")
    }

    paste(format(input$simulations, big.mark = ","), "sessions", "|", input$spins, "spins each", "| Flat betting |", input$bet_type)
  })

  output$win_probability <- renderText(percent(metrics()$probability_win, accuracy = 0.01))
  output$loss_probability <- renderText(percent(metrics()$probability_loss, accuracy = 0.01))
  output$payout <- renderText(metrics()$payout_label)
  output$house_edge <- renderText(percent(metrics()$house_edge, accuracy = 0.01))

  output$ev_explanation <- renderUI({
    m <- metrics()
    win_profit <- input$base_bet * m$payout_to_one
    formula <- paste0("EV = (", m$win_slots, "/38 x ", currency(win_profit), ") + (", m$losing_slots, "/38 x -", currency(input$base_bet), ")")

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
        p("The Green (0, 00) simulation option wins only on the two green pockets and loses on the other 36 pockets."),
        p("It is modeled as a custom two-pocket wager paying 17 to 1, so the expected loss is still 5.26 cents per $1 wagered.")
      )
    }
  })

  output$metric_table <- renderTable({
    m <- metrics()
    data.frame(
      Metric = c("Game", "Selected bet", "Winning pockets", "Losing pockets", "Winning probability", "Losing probability", "Payout", "Expected profit or loss per spin", "House edge", "Expected loss over selected spins", "Payout assumption"),
      Value = c(m$game, m$bet_type, m$win_slots, m$losing_slots, percent(m$probability_win, accuracy = 0.01), percent(m$probability_loss, accuracy = 0.01), m$payout_label, currency(m$expected_value_per_bet), percent(m$house_edge, accuracy = 0.01), currency(m$expected_value_total), m$model_note)
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

  output$total_wins <- renderText({ req(summary_stats()); format(summary_stats()$wins, big.mark = ",") })
  output$total_losses <- renderText({ req(summary_stats()); format(summary_stats()$losses, big.mark = ",") })
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
      Metric = c("Sessions", "Wins", "Losses", "Ending bankroll", "Net profit or loss", "Simulated win percentage", "Average result per spin", "Theoretical expected result", "Difference between simulated and theoretical", "Probability of finishing profitable", "Probability of bankroll depletion", "Average maximum drawdown", "Total amount wagered"),
      Value = c(format(s$sessions, big.mark = ","), format(s$wins, big.mark = ","), format(s$losses, big.mark = ","), currency(s$mean_final_bankroll), currency(s$net_profit), percent(s$simulated_win_percentage, accuracy = 0.01), currency_precise(s$average_result_per_spin), currency(s$theoretical_expected_result), currency(s$difference_from_theoretical), percent(s$probability_profit, accuracy = 0.1), percent(s$probability_ruin, accuracy = 0.1), currency(s$max_drawdown), currency(s$total_wagered))
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
