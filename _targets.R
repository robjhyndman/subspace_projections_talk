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
    tourism_agg |> filter(year(month) <= 2015) |> model(ets = ETS(visitors))
  ),
  tar_target(
    fc,
    tourism_forecast(fit)
  ),
  tar_target(
    fctourism2,
    fc |> filter(is_aggregated(state)) |> tourism_plot(tourism_agg)
  ),
  tar_target(
    fctourism3,
    fc |> filter(state == "NSW" & is_aggregated(zone)) |> tourism_plot(tourism_agg)
  ),
  tar_target(
    fctourism4,
    fc |> filter(region == "Melbourne") |> tourism_plot(tourism_agg)
  ),
  tar_target(
    fctourism5,
    fc |> filter(region == "Snowy Mountains") |> tourism_plot(tourism_agg)
  ),
  tar_target(
    fctourism6,
    fc |> filter(region == "Barossa") |> tourism_plot(tourism_agg)
  ),
  tar_target(
    fcaccuracy,
    tourism_accuracy(fc, tourism_agg)
  ),
  tar_target(
    fcaccuracy2,
    fcaccuracy |>
      summarise(rmsse = sqrt(mean(rmsse^2)), .by = .model) |>
      arrange(rmsse)
  ),
  tar_target(
    fcaccuracy3,
    fcaccuracy |>
      summarise(rmsse = sqrt(mean(rmsse^2)), .by = c(.model, level)) |>
      arrange(level, rmsse)
  ),

  # Section 2: Univariate forecasts --------------------------------------------------------
  # Copied from paper
  tar_target(aetable, ae_table()),
  tar_target(tabm3, ae_table2()),

  # Section 3: Multivariate forecasts --------------------------------------------------------
  tar_target(visnights_file, here::here("tourism/visnights_monthly.csv")),
  tar_target(visnights, read_visnights(visnights_file)),
  tar_target(series, visnights_plot(visnights)),
  tar_target(components_plot, visnights_components(visnights)),
  tar_target(visnights_mse, visnights_mse_plot(visnights)),

  # Fred example
  tar_target(fred_m, 122),
  tar_target(fred_mse_qs, here::here("output/fred_mse.qs"), format = "file"),
  tar_target(fred_mse, get_fred_mse(fred_mse_qs)),
  tar_target(plot_fred_mse, fred_mse_plot(fred_mse, fred_m)),

  # Simulation
  tar_target(sim_m, 70),
  tar_target(sim_mse_qs, here::here("output/simulation_mse.qs"), format = "file"),
  tar_target(sim_mse, get_sim_mse(sim_mse_qs)),
  tar_target(plot_sim_mse, sim_mse_plot(sim_mse, sim_m)),

  # Slides ----------------------------------------------------------
  tar_quarto(slides, "subspace_vienna.qmd",
    extra_files = c("hts.bib", "setup.R", "before-title.tex", "header.tex")
  )
)
