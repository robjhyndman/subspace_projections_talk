library(targets)
library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("fpp3", "patchwork", "readr", "knitr", "kableExtra")
)
# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# tar_source("other_functions.R") # Source other scripts as needed.

list(
  # Section 1: Hierarchical forecasts ------------------------------------------------------
  # Monthly hierarchical tourism data (state/zone/region)
  tar_target(vndata_file, here::here("tourism/vndata.rds"), format = "file"),
  tar_target(vndata, readRDS(vndata_file)),
  tar_target(tourism, make_tourism(vndata)),
  tar_target(tourism_regions, here::here("tourism/Tourism_Regions_2020.shp"), format = "file"),
  tar_target(ausmap, make_ausmap(tourism_regions)),
  tar_target(tourism_plots, make_tourism_plots(tourism)),
  tar_target(
    tourism_agg,
    tourism |> aggregate_key(state / zone / region, visitors = sum(visitors))
  ),
  tar_target(
    fit,
    tourism_agg |>
      filter(year(month) <= 2015) |>
      model(ets = ETS(visitors))
  ),
  tar_target(
    fc,
    fit |>
      reconcile(
        ols = min_trace(ets, method = "ols"),
        wlsv = min_trace(ets, method = "wls_var"),
        wlss = min_trace(ets, method = "wls_struct"),
        # mint_c = min_trace(ets, method="mint_cov"),
        mint_s = min_trace(ets, method = "mint_shrink"),
      ) |>
      forecast(h = "2 years")
  ),
  tar_target(
    fctourism2,
    fc |>
      filter(is_aggregated(state)) |>
      autoplot(filter(tourism_agg, year(month) > 2012), level = NULL)
  ),
  tar_target(
    fctourism3,
    fc |>
      filter(state == "NSW" & is_aggregated(zone)) |>
      autoplot(filter(tourism_agg, year(month) > 2012), level = NULL)
  ),
  tar_target(
    fctourism4,
    fc |>
      filter(region == "Melbourne") |>
      autoplot(filter(tourism_agg, year(month) > 2012), level = NULL)
  ),
  tar_target(
    fctourism5,
    fc |>
      filter(region == "Snowy Mountains") |>
      autoplot(filter(tourism_agg, year(month) > 2012), level = NULL)
  ),
  tar_target(
    fctourism6,
    fc |>
      filter(region == "Barossa") |>
      autoplot(filter(tourism_agg, year(month) > 2012), level = NULL)
  ),
  tar_target(
    fcaccuracy,
    fc |>
      accuracy(tourism_agg, measures = list(mase = MASE, rmsse = RMSSE))
  ),
  tar_target(
    fcaccuracy2,
    fc |>
      accuracy(tourism_agg, measures = list(mase = MASE, rmsse = RMSSE)) |>
      group_by(.model) |>
      summarise(mase = mean(mase), rmsse = sqrt(mean(rmsse^2))) |>
      arrange(rmsse)
  ),
  tar_target(
    fcaccuracy3,
    fc |>
      accuracy(tourism_agg,
        measures = list(mase = MASE, rmsse = RMSSE)
      ) |>
      mutate(
        level = case_when(
          is_aggregated(state) ~ "National",
          is_aggregated(zone) ~ "State",
          is_aggregated(region) ~ "Zone",
          TRUE ~ "Region"
        ),
        level = factor(level, levels = c("National", "State", "Zone", "Region"))
      ) |>
      group_by(.model, level) |>
      summarise(mase = mean(mase), rmsse = sqrt(mean(rmsse^2))) |>
      arrange(level, rmsse)
  ),

  # Section 2: Univariate forecasts --------------------------------------------------------
  # Copied from paper
  tar_target(
    aetable,
    tibble::tribble(
      ~`Aggregation Level`, ~h, ~Base, ~Reconciled, ~Change,
      "Annual", "1", 3.4, 1.9, -42.9,
      "Weekly", "1--52", 2.0, 1.9, -5.0,
      "Weekly", "13", 2.3, 1.9, -16.2,
      "Weekly", "4", 1.9, 1.5, -18.6,
      "Weekly", "1", 1.6, 1.3, -17.2
    ) |>
      dplyr::mutate(Change = paste0(sprintf("%.1f", Change), "%")) |>
      kable(booktabs = TRUE, align = "lrrrr") |>
      column_spec(5, bold = TRUE)
  ),
  tar_target(
    tab,
    tibble::tribble(
      ~`Agg.level`, ~h, ~BU1, ~WLSH1, ~WLSV1, ~WLSS1, ~BU2, ~WLSH2, ~WLSV2, ~WLSS2,
      "Annual", 1, -12.1, -17.9, -17.8, -18.5, -25.4, -29.9, -29.9, -30.2,
      "Semi-annual", 3, 0.0, -6.3, -6.0, -6.9, -2.9, -8.1, -8.2, -9.4,
      "Four-monthly", 4, 3.1, -3.2, -3.0, -3.4, -1.8, -6.2, -6.5, -7.1,
      "Quarterly", 6, 3.2, -2.8, -2.7, -3.4, -2.6, -6.9, -7.4, -8.1,
      "Bi-monthly", 9, 2.7, -2.9, -3.0, -3.7, -1.3, -5.0, -5.5, -6.3,
      "Monthly", 18, 0.0, -3.7, -4.6, -5.0, 0.0, -1.9, -3.2, -3.7,
      "Average", NA, -0.5, -6.1, -6.2, -6.8, -5.7, -9.7, -10.1, -10.8
    )
  ),
  tar_target(
    tabm3,
    tab |>
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
  ),

  # Section 3: Multivariate forecasts --------------------------------------------------------
  tar_target(
    visnights_file, here::here("tourism/visnights_monthly.csv"),
  ),
  tar_target(
    visnights,
    readr::read_csv(visnights_file) |>
      mutate(Month = yearmonth(Month)) |>
      group_by(Month, Region) |>
      summarise(Nights = sum(Nights), .groups = "drop")
  ),
  tar_target(
    regions,
    c("Melbourne", "Canberra", "Fraser Coast", "Central Highlands")
  ),
  tar_target(
    series,
    visnights |>
      filter(Region %in% regions) |>
      ggplot() +
      geom_line(aes(x = Month, y = Nights)) +
      facet_grid("Region", scales = "free")
  ),
  tar_target(
    visnights_wide,
    visnights |> pivot_wider(names_from = Region, values_from = Nights)
  ),
  tar_target(
    col_month,
    select(visnights_wide, Month)
  ),
  tar_target(
    components_plot,
    visnights_wide |>
      select(-Month) |>
      as.matrix() |>
      component() |>
      getElement("x") %>%
      bind_cols(col_month, .) |>
      pivot_longer(-Month,
        names_to = "Component",
        values_to = "Value"
      ) |>
      filter(Component %in% unique(Component)[seq_len(4)]) |>
      ggplot() +
      geom_line(aes(x = Month, y = Value)) +
      facet_grid("Component", scales = "free")
  ),
  tar_target(
    m, 77
  ),
  tar_target(tourism_mse, here::here("output/tourism_mse.qs"), format = "file"),
  tar_target(
    visnights_mse,
    qs::qread(tourism_mse) |>
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
      ggplot(aes(x = p, y = value, color = Component)) +
      geom_vline(xintercept = m) +
      geom_line() +
      geom_hline(data = \(df) filter(df, !proj), aes(yintercept = value, color = Component)) +
      facet_grid(rows = "h", scales = "free", labeller = label_both) +
      ylab("MSE")
  ),
  # Fred example
  tar_target(fred_m, 122),
  tar_target(fred_mse_qs, here::here("output/fred_mse.qs"), format = "file"),
  tar_target(fred_mse, get_fred_mse(fred_mse_qs)),
  tar_target(plot_fred_mse_arima, fred_mse_arima(fred_mse, fred_m)),
  tar_target(plot_fred_mse_dfm, fred_mse_dfm(fred_mse, fred_m)),
  # Simulation
  tar_target(sim_m, 70),
  tar_target(sim_mse_qs, here::here("output/simulation_mse.qs"), format = "file"),
  tar_target(sim_mse, get_sim_mse(sim_mse_qs)),
  tar_target(plot_sim_mse, sim_mse_plot(sim_mse, sim_m)),
  # Slides
  tar_quarto(slides, "subspace_projections.qmd", extra_files = c("hts.bib", "setup.R"))
)
