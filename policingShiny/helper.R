suppressWarnings(library(maps))
suppressWarnings(library(mapproj))
suppressWarnings(library(maptools))
suppressWarnings(library(ggmap))
suppressWarnings(library(ggplot2))
suppressWarnings(library(dplyr))
suppressWarnings(library(shiny))
suppressPackageStartupMessages(library(googleVis))
suppressWarnings(library(lazyeval))
suppressWarnings(library(grid))
suppressWarnings(library(gridExtra))
suppressWarnings(library(plotly))
suppressWarnings(library(shinydashboard))
suppressWarnings(library(RColorBrewer))

worldmap <- map_data('world')
worldmap <- worldmap[worldmap$region != "Antarctica",]

ICVS.results <- readRDS("datasets/ICVSresults.rds")
QuestionMap <- readRDS("datasets/QuestionMap.RDS")

Trim <- function(x) {
  x <- gsub("^\\s*", "", x)
  gsub("\\s*$", "", x)
}

CountryMapViz <- function(df, map, qinfo) {
  
  g <- ggplot() + 
    geom_map(data = df, map = map, 
             aes(map_id = Country, fill = Score), 
             colour = "gray40", size = 0.1) +
    expand_limits(x = map$long, y = map$lat) +
    scale_fill_gradientn(
      guide = "colourbar",
      limits = c(qinfo$Min, qinfo$Max), 
      colours = brewer.pal(11, name = "RdYlBu"),
      breaks = qinfo$marks[[1]],
      labels = qinfo$labels[[1]]
    ) +
    theme_bw() +
    coord_fixed() +
    labs(x = unique(df$Country)) +
    theme(
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.title.y = element_blank(),
      legend.key.width = unit(4, "cm") # huge legends handled elsewhere
    ) +
    facet_wrap(~Year, nrow = 1)
  
  return(g)
}

WorldMapViz <- function(df, qinfo) {
    
    g <- ggplot() + 
      geom_map(data = worldmap, map = worldmap, aes(map_id = region), 
               colour = "gray10", fill = "white", size = 0.1) +
      geom_map(data = df, map = worldmap, 
               aes(map_id = Country, fill = Score)) +
      expand_limits(x = worldmap$long, y = worldmap$lat) + 
      scale_fill_gradientn(
        guide = "colourbar",
        # midpoint = (qinfo$Min + qinfo$Max) / 2,
        limits = c(qinfo$Min, qinfo$Max), 
        colours = brewer.pal(11, name = "RdYlBu"),
        # low = "#cc0000", mid="#f7fcb9", high="#31a354",
        breaks = qinfo$marks[[1]],
        labels = qinfo$labels[[1]]
      ) +
      # labs(title = paste0(qinfo$Title, " (", unique(df$Year), ")")) +
      coord_fixed() +
      theme_nothing(legend = TRUE) +
      theme(
        panel.border = element_rect(fill = NA),
        legend.position = "bottom", legend.box = "horizontal",
        legend.key.width = unit(4, "cm")
      )

    print(g)
}

WorldMapVizP <- function(df, qinfo) {
  plot.new()
  p <- plot_ly(df, z = Score, text = Country, locations = Country, locationmode = 'country names', 
          type = 'choropleth',
          color = Score, colors = 'Blues', 
          autocolorscale = FALSE,
          colorscale = "Bluered",
          reversescale = TRUE,
          zmin = qinfo$Min,
          zmax = qinfo$Max,
          colorbar = list(
            title = qinfo$Title, 
            xanchor = "right",
            x = 0,
            tickmode = "array",
            ticks = "inside",
            tickvals = qinfo$marks[[1]],
            ticktext = qinfo$labels[[1]]
          ),
          filename="r-docs/world-choropleth")
  
  p
}

# courtesy of hadley wickham, with slight tweaks
# grabs legend object, arranges it with the plots
grid_arrange_shared_legend <- function(plots) {
  g <- ggplotGrob(plots[[1]] + theme(legend.position="top"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}