library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(scales)

source("R/roulette_rules.R")
source("R/calculations.R")
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
    numericInput("initial_bankroll", "Initial bankroll", value = 500, min = 1, step = 25),
    numericInput("single_wager", "Wager amount", value = 10, min = 1, step = 5),
    checkboxInput("use_seed", "Use reproducible seed", value = FALSE),
    conditionalPanel(
      "input.use_seed",
      numericInput("seed", "Random seed", value = 12345, min = 1, step = 1)
    ),
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
    tags$hr(),
    div(
      class = "sidebar-note",
      "Select a table bet, enter a wager, and spin the wheel to update the session."
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
      card(
        class = "warning-card",
        strong("Important: "),
        "Betting systems may change short-term volatility and bankroll risk, but they do not change the underlying expected value of the roulette game."
      ),
      card(card_header("Exact calculation summary"), tableOutput("metric_table")),
      card(card_header("Theoretical expected bankroll"), plotOutput("ev_plot", height = 360))
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
          tags$li("The single-spin game uses the central rules and payout definitions in the R folder."),
          tags$li("No table maximum, taxes, comps, dealer errors, wheel bias, or casino promotions are modeled.")
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

  output$payout_table <- renderTable({
    roulette_bets() |>
      rename(`Bet type` = bet_type, `Also called` = also_called, Example = example, Payout = payout)
  })

  analysis_bet_id <- reactive({
    if (is.null(selected_bet_id())) "red" else selected_bet_id()
  })

  analysis_metrics <- reactive({
    standard_bet_metrics(analysis_bet_id(), input$single_wager)
  })

  output$win_probability <- renderText(percent(analysis_metrics()$probability_win, accuracy = 0.01))
  output$loss_probability <- renderText(percent(analysis_metrics()$probability_loss, accuracy = 0.01))
  output$payout <- renderText(analysis_metrics()$payout_label)
  output$house_edge <- renderText(percent(analysis_metrics()$house_edge, accuracy = 0.01))

  output$ev_explanation <- renderUI({
    m <- analysis_metrics()
    loss_pockets <- AMERICAN_ROULETTE$slot_count - m$pockets_covered
    formula <- paste0("EV = (", m$pockets_covered, "/38 x ", currency(m$potential_net_profit), ") + (", loss_pockets, "/38 x -", currency(m$wager), ")")

    tagList(
      div(class = "formula-stack",
        div(strong("Selected bet: "), m$bet_type, " - ", m$selection),
        div(strong("Probability of winning: "), m$pockets_covered, "/38 = ", percent(m$probability_win, accuracy = 0.01)),
        div(strong("Probability of losing: "), loss_pockets, "/38 = ", percent(m$probability_loss, accuracy = 0.01)),
        div(strong("Payout: "), m$payout_label),
        div(class = "ev-formula", formula),
        div(class = "ev-formula", "EV = ", currency_precise(m$expected_value), " per spin")
      )
    )
  })

  output$plain_language <- renderUI({
    m <- analysis_metrics()

    if (m$bet_type %in% c("Red", "Black")) {
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
    } else if (m$bet_type %in% c("First dozen", "Second dozen", "Third dozen", "Column 1", "Column 2", "Column 3")) {
      tagList(
        p("Dozens and columns cover 12 pockets and pay 2 to 1."),
        div(class = "formula-stack",
          div("Probability of winning = 12/38 = 31.58%"),
          div("Probability of losing = 26/38 = 68.42%"),
          div(class = "ev-formula", "EV per $1 = (12/38 x $2) + (26/38 x -$1) = -$0.0526")
        ),
        p("The payout is higher than an even-money bet, but the lower win probability keeps the same American roulette house edge.")
      )
    } else if (m$bet_type == "Straight-up") {
      tagList(
        p("A straight-up bet covers one pocket and pays 35 to 1."),
        div(class = "formula-stack",
          div("Probability of winning = 1/38 = 2.63%"),
          div("Probability of losing = 37/38 = 97.37%"),
          div(class = "ev-formula", "EV per $1 = (1/38 x $35) + (37/38 x -$1) = -$0.0526")
        ),
        p("Clickable 0 and 00 are handled as separate straight-up bets, not a combined green wager.")
      )
    } else {
      tagList(
        p("This outside bet covers 18 non-green pockets and pays even money."),
        p("Both 0 and 00 cause outside bets to lose, which creates the 5.26% American roulette house edge.")
      )
    }
  })

  output$metric_table <- renderTable({
    m <- analysis_metrics()
    loss_pockets <- AMERICAN_ROULETTE$slot_count - m$pockets_covered
    data.frame(
      Metric = c("Game", "Selected bet", "Selection", "Winning pockets", "Losing pockets", "Winning probability", "Losing probability", "Payout", "Wager", "Expected profit or loss per spin", "House edge"),
      Value = c(AMERICAN_ROULETTE$name, m$bet_type, m$selection, m$pockets_covered, loss_pockets, percent(m$probability_win, accuracy = 0.01), percent(m$probability_loss, accuracy = 0.01), m$payout_label, currency(m$wager), currency_precise(m$expected_value), percent(m$house_edge, accuracy = 0.01))
    )
  })

  output$ev_plot <- renderPlot({
    m <- analysis_metrics()
    curve <- data.frame(
      spin = seq_len(150),
      expected_bankroll = input$initial_bankroll + seq_len(150) * m$expected_value
    )

    ggplot(curve, aes(spin, expected_bankroll)) +
      geom_line(color = "#b13f3f", linewidth = 1.2) +
      geom_hline(yintercept = input$initial_bankroll, linetype = "dashed", color = "#202124") +
      labs(x = "Spin", y = "Expected bankroll") +
      scale_y_continuous(labels = dollar) +
      roulette_plot_theme()
  })
}

shinyApp(ui, server)
