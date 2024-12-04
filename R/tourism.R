tourism_forecast <- function(fit) {
  fit |>
    reconcile(
      ols = min_trace(ets, method = "ols"),
      # wlsv = min_trace(ets, method = "wls_var"),
      # wlss = min_trace(ets, method = "wls_struct"),
      # mint_c = min_trace(ets, method="mint_cov"),
      mint_s = min_trace(ets, method = "mint_shrink"),
    ) |>
    forecast(h = "2 years")
}

tourism_plot <- function(fc, data) {
  fc |>
    autoplot(filter(data, year(month) > 2012), level = NULL)
}

tourism_accuracy <- function(fc, data) {
  fc |>
    accuracy(data, measures = list(rmsse = RMSSE)) |>
    mutate(
      level = case_when(
        is_aggregated(state) ~ "National",
        is_aggregated(zone) ~ "State",
        is_aggregated(region) ~ "Zone",
        TRUE ~ "Region"
      ),
      level = factor(level, levels = c("National", "State", "Zone", "Region"))
    ) |>
    arrange(level, state, zone, region, .model)
}
