plotSankey <- function(Nodes, Links) {
  # Adding transparency to the links
  # Conversion to rgba character string
  Links$color <- apply(grDevices::col2rgb(Links$color), 2, function(x) {
    paste0("rgba(", x[1], ",", x[2], ",", x[3], ",0.4)")
  })
    
  fig <- plot_ly(
      type = "sankey",
      orientation = "h",
      alpha_stroke = 0.2,
      node = list(
        label = Nodes$label,
        color = Nodes$color,
        pad = 15,
        thickness = 20,
        line = list(color = "black", width = 0.5)
      ),
      link = list(
        source = Links$source,
        target = Links$target,
        value =  Links$value,
        color = Links$color
      )
    )
  fig <- fig %>% layout(font = list(size = 14, color = "black", weight = "bold"))
  fig
}
