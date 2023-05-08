ega_res$plot.typical.ega
knitr::kable(ega_res$summary.table, digits = 3)
knitr::kable(ega_res$frequency)

# Structural consistency
bapq.dimstab <- dimensionStability(ega_res)

bapq.dimstab$dimension.stability$structural.consistency
bapq.dimstab$dimension.stability$average.item.stability
bapq.dimstab$item.stability$plot +
  ggplot2::scale_color_brewer(palette = "Set1")

# View(bapq.dimstab$item.stability$item.stability$all.dimensions)

gsub(0, " ", knitr::kable(bapq.dimstab$item.stability$item.stability$all.dimensions,digits = 3))
knitr::kable(bapq.dimstab$item.stability$item.stability$all.dimensions,digits = 3)

