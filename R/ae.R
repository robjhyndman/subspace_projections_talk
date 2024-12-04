ae_table <- function() {
  tibble::tribble(
    ~`Aggregation Level`, ~h, ~Base, ~Reconciled, ~Change,
    "Annual", "1", 3.4, 1.9, -42.9,
    "Weekly", "1-52", 2.0, 1.9, -5.0,
    "Weekly", "13", 2.3, 1.9, -16.2,
    "Weekly", "4", 1.9, 1.5, -18.6,
    "Weekly", "1", 1.6, 1.3, -17.2
  ) |>
    dplyr::mutate(Change = paste0(sprintf("%.1f", Change), "%")) |>
    kable(booktabs = TRUE, align = "lrrrr") |>
    column_spec(5, bold = TRUE)
}

ae_table2 <- function() {
  tibble::tribble(
    ~`Agg.level`, ~h, ~BU1, ~WLSH1, ~WLSV1, ~WLSS1, ~BU2, ~WLSH2, ~WLSV2, ~WLSS2,
    "Annual", 1, -12.1, -17.9, -17.8, -18.5, -25.4, -29.9, -29.9, -30.2,
    "Semi-annual", 3, 0.0, -6.3, -6.0, -6.9, -2.9, -8.1, -8.2, -9.4,
    "Four-monthly", 4, 3.1, -3.2, -3.0, -3.4, -1.8, -6.2, -6.5, -7.1,
    "Quarterly", 6, 3.2, -2.8, -2.7, -3.4, -2.6, -6.9, -7.4, -8.1,
    "Bi-monthly", 9, 2.7, -2.9, -3.0, -3.7, -1.3, -5.0, -5.5, -6.3,
    "Monthly", 18, 0.0, -3.7, -4.6, -5.0, 0.0, -1.9, -3.2, -3.7,
    "Average", NA, -0.5, -6.1, -6.2, -6.8, -5.7, -9.7, -10.1, -10.8
  ) |>
    kable(
      booktabs = TRUE,
      format = "latex",
      escape = FALSE,
      linesep = "",
      col.names = c("Aggregation level", "$h$", "BU", "WLS$_H$", "WLS$_V$", "WLS$_S$", "BU", "WLS$_H$", "WLS$_V$", "WLS$_S$")
    ) |>
    add_header_above(c(" " = 2, "ETS" = 4, "ARIMA" = 4)) |>
    column_spec(c(6, 10), bold = TRUE) |>
    row_spec(6, hline_after = TRUE)
}
