make_ausmap <- function(tourism_regions) {
  library(sf)
  # Use Okabe-Ito color-blind friendly color palette
  state_colors <- c(
    `New South Wales` = "#56b4e9",
    `Victoria` = "#0072b2",
    `Queensland` = "#009e73",
    `South Australia` = "#f0e442",
    `Northern Territory` = "#d55e00",
    `Western Australia` = "#e69f00",
    `Tasmania` = "#cc79a7",
    `Australian Capital Territory` = "#cccccc"
  )
  p <- read_sf(tourism_regions) |>
    rename(State = "STE_NAME16") |>
    ggplot() +
    geom_sf(aes(fill = State), alpha = 0.8) +
    theme_void() +
    theme(text = ggplot2::element_text(family = "Fira Sans")) +
    scale_fill_manual(values = state_colors)
  Cairo::CairoPDF(here::here("figs/ausmap.pdf"),
                  width = 15 / 1.5, height = 7 / 1.5,
                  bg = "transparent")
  print(p)
  crop::dev.off.crop(here::here("figs/ausmap.pdf"))
  system("pdfcrop figs/ausmap.pdf figs/ausmap.pdf")
  system("convert -density 300 figs/ausmap.pdf figs/ausmap.png")
  return(p)
}
