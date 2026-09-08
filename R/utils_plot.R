compact_colourbar <- function(
    position = "bottom",
    width = 8,
    height = 0.35,
    title_position = "top"
)  {
  list(
    guides(
      colour = guide_colourbar(
        direction = "horizontal",
        title.position = title_position,
        barwidth = unit(width, "cm"),
        barheight = unit(height, "cm")
      )
    ),
    theme(
      legend.position = position,
      legend.margin = margin(t = 2),
      legend.box.margin = margin(0, 0, 0, 0)
    )
  )
}
