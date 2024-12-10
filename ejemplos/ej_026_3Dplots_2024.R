#- https://www.appsilon.com/post/r-3d-charts
#- algunas posibilidades para hacer plots en 3D
#- impresionan, pero ... no siempre son útiles para comunicar, mas bien al contrario
#- https://clauswilke.com/dataviz/no-3d.html


#- plotly ----------------------------------------------------------------------
library(plotly)

colors <- c("#F8766D", "#00BA38", "#619CFF")  #- los colores de theme_grey()
shapes <- c(2, 2, 2)

plotly::plot_ly(
  data = iris,
  x = ~ Sepal.Length,
  y = ~ Petal.Length,
  z = ~ Sepal.Width,
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 6),
  color = ~ Species,
  colors = colors,
  symbol = ~ Species,
  symbols = shapes
)


colors <- c("#004B95", "#38812F", "#A30000")
shapes <- c(15, 16, 17)

plot_ly(
  data = iris,
  x = ~ Sepal.Length,
  y = ~ Sepal.Width,
  z = ~ Petal.Length,
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 6),
  color = ~ Species,
  colors = colors,
  symbol = ~ Species,
  symbols = shapes
) %>%
  layout(
    title = "Iris 3D Scatter Plot",
    scene = list(
      xaxis = list(title = "Sepal Length (cm)"),
      yaxis = list(title = "Sepal Width (cm)"),
      zaxis = list(title = "Petal Length (cm)")
    )
  )


# scatterplot3d ----------------------------------------------------------------
library(scatterplot3d)
colors <- c("#004B95", "#38812F", "#A30000")
shapes <- c(15, 16, 17)
angle <- 30

scatterplot3d(
  x = iris$Sepal.Length,
  y = iris$Sepal.Width,
  z = iris$Petal.Length,
  angle = angle,
  #pch = shapes[as.numeric(iris$Species)],
  color = colors[as.numeric(iris$Species)]
)


# rgl -------------------------------------------------------------------------- 

library(rgl)

colors <- c("#004B95", "#38812F", "#A30000")

plot3d(
  x = iris$Sepal.Length,
  y = iris$Sepal.Width,
  z = iris$Petal.Length,
  type = "s",
  col = colors[as.numeric(iris$Species)],
  radius = 0.1
)
