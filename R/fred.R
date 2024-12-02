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
    )
}

fred_mse_arima <- function(mse, m) {
  mse |>
    filter(model == "arima") |>
    ggplot(aes(x = p, y = value, color = Component)) +
    geom_vline(xintercept = m) +
    geom_hline(
      data = filter(mse, !proj & model == "arima"),
      aes(yintercept = value, color = Component)
    ) +
    geom_line() +
    facet_grid(rows = "h", scales = "free", labeller = label_both) +
    ylab("MSE") +
    scale_x_continuous(expand = expansion(mult = 0)) +
    scale_color_manual(
      values = c("#D55E00", "#0072B2", "#009E73"),
      breaks = c("No projection", "Normal", "PCA + Normal")
    )
}

fred_mse_dfm <- function(mse, m) {
  mse |>
    filter(model == "dfm") |>
    ggplot(aes(x = p, y = value, color = Component)) +
    geom_vline(xintercept = m) +
    geom_hline(
      data = filter(mse, !proj & model == "dfm"),
      aes(yintercept = value, color = Component)
    ) +
    geom_line() +
    facet_grid(rows = "h", scales = "free", labeller = label_both) +
    ylab("MSE") +
    scale_x_continuous(expand = expansion(mult = 0)) +
    scale_color_manual(
      values = c("#D55E00", "#0072B2", "#009E73"),
      breaks = c("No projection", "Normal", "PCA + Normal")
    )
}
