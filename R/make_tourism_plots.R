make_tourism_plots <- function(tourism) {
  source(here::here("setup.R"))
  p1 <- tourism |>
    summarise(visitors = sum(visitors)) |>
    autoplot(visitors) +
    ylab("Visitor nights") + xlab("Month") +
    # scale_y_log10() +
    ggtitle("Total domestic travel: Australia") +
    theme(
      text = ggplot2::element_text(family = "Fira Sans"),
      rect = element_rect(fill = "transparent")
    )
  p2 <- tourism |>
    group_by(state) |>
    summarise(visitors = sum(visitors)) |>
    autoplot(visitors) +
    ylab("Visitor nights") + xlab("Month") +
    scale_y_log10() +
    ggtitle("Total domestic travel: by state") +
    scale_color_manual(
      values =
        c(
          NSW = "#56b4e9",
          VIC = "#0072b2",
          QLD = "#009e73",
          SA = "#f0e442",
          NT = "#d55e00",
          WA = "#e69f00",
          TAS = "#cc79a7",
          ACT = "#cccccc"
        )
    ) +
    guides(color = guide_legend(title = "State")) +
    theme(
      text = ggplot2::element_text(family = "Fira Sans"),
      rect = element_rect(fill = "transparent", colour = "transparent")
    )
  p3 <- tourism |>
    filter(state == "NSW") |>
    group_by(zone) |>
    summarise(visitors = sum(visitors)) |>
    autoplot(visitors) +
    ylab("Visitor nights") + xlab("Month") +
    ggtitle("Total domestic travel: NSW by zone") +
    guides(color = guide_legend(title = "Zone")) +
    theme(
      text = ggplot2::element_text(family = "Fira Sans"),
      rect = element_rect(fill = "transparent")
    )
  p4 <- tourism |>
    filter(zone == "South NSW") |>
    group_by(region) |>
    summarise(visitors = sum(visitors)) |>
    autoplot(visitors) +
    ylab("Visitor nights") + xlab("Month") +
    # scale_y_log10() +
    ggtitle("Total domestic travel: South NSW by region") +
    guides(color = guide_legend(title = "Region")) +
    theme(
      text = ggplot2::element_text(family = "Fira Sans"),
      rect = element_rect(fill = "transparent")
    )

  # p5 <- tourism |>
  #   group_by(purpose) |>
  #   summarise(visitors = sum(visitors)) |>
  #   autoplot(visitors) +
  #   ylab("Visitor nights") + xlab("Month") +
  #   # scale_y_log10() +
  #   ggtitle("Total domestic travel: by purpose of travel") +
  #   guides(color = guide_legend(title = "Purpose"))
  # p6 <- tourism |>
  #   filter(region == "Snowy Mountains") |>
  #   group_by(purpose) |>
  #   summarise(visitors = sum(visitors)) |>
  #   autoplot(visitors) +
  #   ylab("Visitor nights") + xlab("Month") +
  #   # scale_y_log10() +
  #   ggtitle("Total domestic travel: Snowy Mountains by purpose of travel") +
  #   guides(color = guide_legend(title = "Purpose"))

  aligned_plots <- align_patches(p1, p2, p3, p4)
  for (i in seq_along(aligned_plots)) {
    fname <- paste0("./figs/tourism", i, ".pdf")
    Cairo::CairoPDF(
      fname,
      width = 15 / 1.5, height = 7 / 1.5, bg = "transparent"
    )
    print(aligned_plots[[i]])
    dev.off(fname)
  }
  return(aligned_plots)
}
