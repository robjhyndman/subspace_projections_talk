## -------------------------------------------------------------------------------------
#| label: fred-md
get_fred_mse <- function(qsfile) {
  qs::qread(qsfile) |>
    tibble::as_tibble() |>
    filter(model %in% c("arima", "dfm"), h %in% c(1, 6, 12)) |>
    filter(Phi %in% c("NA", "normal", "PCA_normal")) |>
    mutate(
      Component = case_when(
        !proj ~ "No projection",
        Phi == "normal" ~ "Normal",
        Phi == "PCA_normal" ~ "PCA + Normal"
      )
    ) |>
    rename(Model = model)
}

fred_mse_plot <- function(mse, m) {
  mse |>
    filter(Component != "Normal") |>
    ggplot() +
    aes(x = p, y = value, color = Model, lty = Component) +
    geom_vline(xintercept = m) +
    geom_hline(
      data = filter(mse, !proj),
      aes(yintercept = value, color = Model, lty = Component)
    ) +
    geom_line() +
    facet_grid(rows = "h", scales = "free", labeller = label_both) +
    ylab("MSE") +
    scale_x_continuous(expand = expansion(mult = 0)) +
    scale_color_manual(
      values = c("#D55E00", "#0072B2"),
      breaks = c("arima", "dfm"),
      labels = c("ARIMA", "DFM"),
    ) +
    scale_linetype_manual(
      values = c("dashed", "solid"),
      breaks = c("No projection", "PCA + Normal")
    )
}
