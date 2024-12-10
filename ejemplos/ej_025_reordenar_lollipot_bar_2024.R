#- muchas veces, al hacer un plot, es necesario reordenar los datos
#- https://www.data-to-viz.com/caveat/order_data.html

# Libraries -----
library(tidyverse)
library(hrbrthemes)  #- para usar un theme() de ggplot2


# Load dataset from Github -----------------------------------------------------
#- vemos diferentes formas de cargar un dataset
my_url <- "https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/7_OneCatOneNum.csv"

#- con R-base
data <- read.table(file = my_url, header = TRUE, sep = ",")
data <- read.csv(file = my_url)  #- ya tiene x defecto header = TRUE

#- con el pkg readr del tidyverse
data <- readr::read_csv(file = my_url)   #- tb tiene col_names = TRUE por defecto
data <- readr::read_delim(file = my_url, delim = ",")  #- col_names = TRUE

#- con rio
data <- rio::import(file = my_url) #- el pkg rio usa data.table::fread() para leer csv's



#- Arreglamos datos ------------------------------------------------------------
#- quitamos NA's ( no es necesario, pero ...)
df <- data %>% filter(!is.na(Value))




# Plot -------------------------------------------------------------------------
#- vamos a hacer un Lolliplot: A lollipop plot is basically a barplot, 
#- where the bar is transformed in a line and a dot. 
#- Para ello usamos geom_segment() y geom_point()
#- https://r-graph-gallery.com/lollipop-plot.html

p <- ggplot(data = df, mapping = aes(x = Value, y = Country) ) + 
  geom_segment(aes(x = 0 , xend = Value, 
                   y = Country, yend = Country), 
               color="#550099") + 
  geom_point(size = 2, color="#69b3a2") 

p

#- tuneado ---------------------------------------------------------------------
p1 <- p + 
  hrbrthemes::theme_ipsum() 

p1

p2 <- p1 +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "none" ) +  xlab("")
p2


#- mejorar breaks y limits del eje X -------------------------------------------
p3 <- p2 +
  scale_x_continuous(breaks = c(100, 500, 1000, seq(1000, max(df$Value), by = 3000), max(df$Value)), 
                     limits = c(0, max(df$Value))) 

p3


#- poner una linea marcando la media -------------------------------------------
p4 <- p3 +
  geom_vline(xintercept = mean(df$Value), linetype = "dashed", color = "red", linewidth = 0.25)

p4

#- Bueno, el gráfico no está mal, PERO 
#- hay que ordenar las barras en función del valor de "Value"!!!


#- ordenar las observaciones (de mayor a menor) --------------------------------
#- se podría hacer dentro de ggplot2 , PERO mejor los hacemos en el df
str(df) 

#- vemos que la v. "Country" q es la q tenemos q ordenar es de tipo character
#- tenemos q ordenar los países en función del valor de la v. "Value".
#- Para ello:

#- Hay q pasar Country de character a factor
df <- df %>% 
  mutate(Country.f = as.factor(Country)) %>% 
  #- por defecto los levels de character se ordenan alfabéticamente,
  #- por eso hay q hacer un fct_reorder() para ordenar los levels de Country.f
  #- en función de los valores de "Value"
  mutate(Country.ff = fct_reorder(.f = Country.f, .x = Value, .desc = FALSE))

#- veamos los levels de Country.f y Country.ff
levels(df$Country.f) #- por defecto los levels de character se ordenan alfabéticamente
levels(df$Country.ff) #- en df$Country.ff los levels los hemos ordenado por el valor de "Value"



p <- ggplot(data = df, mapping = aes(x = Value, y = Country.ff) ) + 
  geom_segment(aes(x = 0 , xend = Value, 
                   y = Country.ff, yend = Country.ff), 
               color="#550099") + 
  geom_point(size = 2, color="#69b3a2") 

p + hrbrthemes::theme_ipsum() 



#- un pequeño truquillo!!!
my_factor = expr(Country.ff)

p <- ggplot(data = df, mapping = aes(x = Value, y = !!my_factor) ) + 
  geom_segment(aes(x = 0 , xend = Value, 
                   y = !!my_factor, yend = !!my_factor), 
               color="#550099") + 
  geom_point(size = 2, color="#69b3a2") 

p


#- my_lollipop() ---------------------------------------------------------------
#- vamos a hacer una función para hacer lollipop plots !!!!!!!
my_lollipop <- function(my_df, my_factor, my_value){
  p <- ggplot(data = my_df, mapping = aes(x = {{my_value}}, y = {{my_factor}}) ) + 
    geom_segment(aes(x = 0 , xend = {{my_value}}, 
                     y = {{my_factor}}, yend = {{my_factor}}), 
                 color="#550099") + 
    geom_point(size = 2, color="#69b3a2") 
  return(p)
}

my_lollipop(df, Country.ff, Value)


#- ahora hagamos un gráfico de barras ------------------------------------------
#- lo q se grafica es el numero de casos/observaciones en cada grupo
#- se pueden hacer con geom_bar() y geom_col()

#- en nuestro caso ya tenemos los casos contados,
#- ya que la v. Value tiene el nº de casos
#- tenemos que usar geom_col()

#- como ya tenemos los casos contados, usamos geom_col()
p <- ggplot(data = df, mapping = aes(x = Value, y = !!my_factor) ) + 
  geom_col(fill = "#69b3a2")

p

#- veamos q pasaría si usamos geom_bar()
#- geom_bar() cuenta los casos, las filas de cada categoría
#- y en nuestro df solo hay una fila para cada categoría/país
p <- ggplot(data = df, mapping = aes( y = !!my_vv) ) +
  geom_bar(fill = "#69b3a2")

p

#- si quisieramos usar geom_bar() tendríamos q usar stat = "identity"
p <- ggplot(data = df, mapping = aes(x = Value, y = !!my_vv) ) + 
  geom_bar(stat = "identity", fill = "#69b3a2")
p


#- tuneado ---------------------------------------------------------------------
#- voy a poner los valores dentro (o al lado) de las barras

#- plot base
p <- ggplot(data = df, mapping = aes(x = Value, y = !!my_vv) ) + 
  geom_col(fill = "#69b3a2")


#- pongo los valores al lado de la barra
p + geom_text(aes(label = Value), 
                   hjust = -0.2, 
                   size = 2.5, color = "black")
p

#- formateo un poco los números 
p + geom_text(aes(label = scales::comma(Value, big.mark = ".")), 
                   hjust = -0.2, 
                   size = 2.5, color = "black")

