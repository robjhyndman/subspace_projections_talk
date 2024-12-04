read_visnights <- function(file) {
  readr::read_csv(file) |>
    mutate(Month = yearmonth(Month)) |>
    group_by(Month, Region) |>
    summarise(Nights = sum(Nights), .groups = "drop")
}

visnights_plot <- function(visnights) {
  regions <- c("Melbourne", "Canberra", "Fraser Coast", "Central Highlands")
  visnights |>
    filter(Region %in% regions) |>
    ggplot() +
    geom_line(aes(x = Month, y = Nights)) +
    facet_grid("Region", scales = "free")
}

visnights_components <- function(visnights) {
  visnights_wide <- visnights |>
    pivot_wider(names_from = Region, values_from = Nights)
  visnights_wide |>
    select(-Month) |>
    as.matrix() |>
    component() |>
    getElement("x") %>%
    bind_cols(select(visnights_wide, Month), .) |>
    pivot_longer(-Month,
      names_to = "Component",
      values_to = "Value"
    ) |>
    filter(Component %in% unique(Component)[seq_len(4)]) |>
    ggplot() +
    geom_line(aes(x = Month, y = Value)) +
    facet_grid("Component", scales = "free")
}

visnights_mse_plot <- function(visnights) {
  m <- 77
  qs::qread(here::here("output/tourism_mse.qs")) |>
    filter(h %in% c(1, 6, 12)) |>
    mutate(
      Component = case_match(
        paste(proj, Phi, sep = "."),
        "TRUE.PCA_normal" ~ "PCA+Norm.",
        "FALSE.NA" ~ "No Proj.",
        "TRUE.normal" ~ "Norm."
      )
    ) |>
    filter(!is.na(Component)) |>
    ggplot() +
    aes(x = p, y = value, color = Component) +
    geom_vline(xintercept = m) +
    geom_line() +
    geom_hline(data = \(df) filter(df, !proj), aes(yintercept = value, color = Component)) +
    facet_grid(rows = "h", scales = "free", labeller = label_both) +
    ylab("MSE")
}
