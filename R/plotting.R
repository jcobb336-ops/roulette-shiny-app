roulette_plot_theme <- function() {
  ggplot2::theme_minimal(base_family = "Inter", base_size = 14) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 18, face = "bold", lineheight = 1.25),
      plot.subtitle = ggplot2::element_text(size = 14, lineheight = 1.35, margin = ggplot2::margin(b = 10)),
      axis.title = ggplot2::element_text(size = 14, face = "bold"),
      axis.text = ggplot2::element_text(size = 12),
      legend.title = ggplot2::element_text(size = 13, face = "bold"),
      legend.text = ggplot2::element_text(size = 12),
      panel.grid.minor = ggplot2::element_line(linewidth = 0.2, color = "#eeeeee"),
      panel.grid.major = ggplot2::element_line(linewidth = 0.35, color = "#e6e6e6")
    )
}

plot_bankroll_paths <- function(paths, expected) {
  ggplot2::ggplot(paths, ggplot2::aes(spin, bankroll, group = session)) +
    ggplot2::geom_line(color = "#59788e", alpha = 0.22, linewidth = 0.45) +
    ggplot2::geom_line(
      data = expected,
      ggplot2::aes(spin, expected_bankroll),
      inherit.aes = FALSE,
      color = "#b13f3f",
      linewidth = 1.1
    ) +
    ggplot2::labs(
      x = "Spin",
      y = "Bankroll",
      title = "Simulated bankroll paths",
      subtitle = "Red line shows theoretical expected bankroll"
    ) +
    ggplot2::scale_y_continuous(labels = scales::dollar) +
    roulette_plot_theme()
}

plot_final_distribution <- function(outcomes, initial_bankroll) {
  ggplot2::ggplot(outcomes, ggplot2::aes(final_bankroll)) +
    ggplot2::geom_histogram(bins = 35, fill = "#426b69", color = "white") +
    ggplot2::geom_vline(
      xintercept = initial_bankroll,
      color = "#202124",
      linetype = "dashed",
      linewidth = 0.9
    ) +
    ggplot2::geom_vline(
      xintercept = mean(outcomes$final_bankroll),
      color = "#b13f3f",
      linewidth = 1
    ) +
    ggplot2::labs(
      x = "Final bankroll",
      y = "Sessions",
      title = "Distribution of final bankroll",
      subtitle = "Dashed line is starting bankroll; red line is simulated mean"
    ) +
    ggplot2::scale_x_continuous(labels = scales::dollar) +
    roulette_plot_theme()
}

plot_strategy_comparison <- function(outcomes, initial_bankroll) {
  ggplot2::ggplot(outcomes, ggplot2::aes(strategy, final_bankroll, fill = strategy)) +
    ggplot2::geom_boxplot(width = 0.6, alpha = 0.88, outlier.alpha = 0.2) +
    ggplot2::geom_hline(
      yintercept = initial_bankroll,
      color = "#202124",
      linetype = "dashed"
    ) +
    ggplot2::labs(
      x = NULL,
      y = "Final bankroll",
      title = "Strategy comparison",
      subtitle = "All strategies face the same American roulette house edge"
    ) +
    ggplot2::scale_y_continuous(labels = scales::dollar) +
    ggplot2::scale_fill_manual(values = c(
      "Flat betting" = "#426b69",
      "Martingale" = "#b13f3f",
      "Fibonacci" = "#7467a9"
    )) +
    roulette_plot_theme() +
    ggplot2::theme(legend.position = "none")
}
