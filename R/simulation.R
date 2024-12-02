get_sim_mse <- function(qsfile) {
  qs::qread(qsfile) |>
    as_tibble() |>
    filter(
      model %in% c("arima", "dfm", "var", "true"),
      Phi %in% c("PCA_normal") | is.na(Phi),
      h %in% c(1, 6)
    ) |>
    mutate(
      Component = case_when(
        !proj ~ "No projection",
        proj & Phi == "PCA_normal" ~ "PCA + Normal",
        TRUE ~ "Other"
      )
    )
}

sim_mse_plot <- function(mse, m) {
  cb_palette_grey <- c(
    "#999999",
    "#E69F00",
    "#56B4E9",
    "#009E73",
    "#F0E442",
    "#0072B2",
    "#D55E00",
    "#CC79A7"
  )
  mse |>
    ggplot(aes(x = p, y = value, colour = model, linetype = Component)) +
    #geom_vline(aes(xintercept = m)) +
    geom_line() +
    geom_hline(
      data = filter(mse, !proj),
      aes(yintercept = value, colour = model, linetype = Component)
    ) +
    facet_grid(rows = "h", scales = "free", labeller = label_both) +
    ylab("MSE") +
    scale_color_manual(
      name = "Model",
      values = cb_palette_grey[c(7, 6, 4, 2)],
      labels = c(
        "arima" = "ARIMA",
        "dfm" = "DFM",
        "true" = "VAR - DGP",
        "var" = "VAR - Est."
      )
    ) +
    scale_linetype_manual(
      name = "Component",
      values = c("dashed", "solid"),
      labels = c("No projection", "PCA + Normal")
    )
}
