#- Vamos a hacer un gráfico como este: https://perezp44.github.io/pjperez.web/flowers/2021-04-13-joy-division-en-pancrudo/imagenes/joy_division_Pancrudo_02_naranja_x.png

#- Se usan lineas de elevación de una geometría/municipio
#- voy a usar datos de elevación de aqui: https://rspatialdata.github.io/elevation.html

#- pak::pak("raster", "elevatr", "viridis", "ggridges", "extrafont", "mapproj")


library(tidyverse)
library(raster)
library(elevatr)
library(sf)
library(viridis)

#- cargamos geometrías municipales 
municipios <- pjpv.curso.R.2022::LAU2_muni_2020_canarias



#- AQUI: hay que elegir un municipio -------------------------------------------
my_municipio <- "Pancrudo"  #- "Pancrudo"
my_municipio_code <- "44177"  #- hay que poner el código (ine_muni) de tu municipio 44177
my_municipio_bound <- municipios %>% dplyr::filter(ine_muni == my_municipio_code)


my_z <- 9  #- 1 to 14 (mas)

#- bajamos datos de elevación de my_municipio
elevation_data <- elevatr::get_elev_raster(locations = my_municipio_bound, z = my_z, clip = "locations")

#- convertimos en data.frame y arreglamos
elevation_data <- as.data.frame(elevation_data, xy = TRUE)
colnames(elevation_data)[3] = "elevation"
#- quitamos NA's
elevation_data <- elevation_data[complete.cases(elevation_data),] 

#- ahora el Joy Division plot: https://danielredondo.com/posts/20200125_joy_division/
library(ggridges)
library(mapproj)

# Primera aproximación
p0 <- ggplot(elevation_data, aes(x = x, y = y, group = y, height = elevation)) +
  geom_density_ridges(stat = "identity", scale = 70)

p0
#ggsave(p0, filename = here::here("imagenes", "joy_division_Pancrudo_00.png"), device = "png", width = 10, height = 13, units = "cm")

#- grafico final de Daniel (tuneado un poco)
my_scale <- 30        #- 30
my_color <- "orange"  #- "orange"
my_fill <- "black"    #- "black"

p <- ggplot(elevation_data, aes(x = x, y = y, group = y, height = elevation)) +
  geom_density_ridges(stat = "identity", 
                      scale = my_scale,
                      fill = my_fill, 
                      color = my_color) +
  scale_x_continuous(name = NULL) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = my_fill),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        plot.background = element_rect(fill = my_fill),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(colour = "white", size = 18)) +
  coord_map()
p

ggsave(p, filename = here::here("imagenes", "joy_division_01.png"),
       device = "png", width = 12, height = 10, units = "cm")


#- intento de ponerle un marco GOOD -------------------
#pancrudo_xx <- my_municipio_bound %>% sf::st_set_geometry(NULL)
#- hay que jugar con los limites (zoom) del gráfico
library(extrafont)
vv <- st_bbox(my_municipio_bound)


p2 <- p + coord_sf(expand = TRUE) +
  theme(plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm")) +
  theme(plot.background = element_rect(colour = my_color, size = 2.0)) 

p2


#- CUIDADÍN!!!!!
#- si quieres ponerle texto hay que ponerlo dentro del bbox, cerca del gráfico,
#- si lo pones muy lejos, el gráfico se verá muy alargado o ancho
#- puedes ver los límites haciendo:  vv <- st_bbox(my_municipio_bound)
#- para Pancrudo vv es:
#- xmin       ymin       xmax       ymax 
#- -1.0897333 40.7185862 -0.9401557 40.8550268
#- asi que como yo quiero situar el texto en el bottom de la imagen pues pongo
#- x = -0.999, y = 40.759  (en el primer geom_text())
#- x = -0.992, y = 40.753  (en el segundo geom_text())


my_color_text <- my_color   #- my_color "orange"
my_label_2 <- "(unknow pleasures)"  #- "(unknow pleasures)"

p2_text <- p2 +
  geom_text(data = data.frame(x = -0.999, y = 40.759, label = my_municipio), family = "Helvetica",
            mapping = aes(x = x, y = y, label = label),
            size = 8.5, hjust = 0, vjust = 0, 
            colour = my_color_text, inherit.aes = FALSE) +
  geom_text(data = data.frame(x = -0.992, y = 40.753, label = my_label_2), 
            family = "Vivaldi", mapping = aes(x = x, y = y, label = label),
            size = 4.4, hjust = 0, vjust = 0, 
            colour = my_color_text, inherit.aes = FALSE)

p2_text


ggsave(p2, filename = here::here("imagenes", "joy_division_02.png"),
       device = "png", width = 15, height = 20, units = "cm")


knitr::plot_crop(here::here("imagenes", "joy_division_02.png"))



p3 <- p + 
  theme(plot.background = element_rect(colour = my_color, size = 2.0)) +
  geom_text(data = data.frame(x = -0.999, y = 40.759, label = my_municipio), family = "Helvetica",
            mapping = aes(x = x, y = y, label = label),
            size = 8.5, hjust = 0, vjust = 0, 
            colour = my_color_text, inherit.aes = FALSE) +
  geom_text(data = data.frame(x = -0.992, y = 40.753, label = my_label_2), 
            family = "Vivaldi", mapping = aes(x = x, y = y, label = label),
            size = 4.4, hjust = 0, vjust = 0, 
            colour = my_color_text, inherit.aes = FALSE)

p3


ggsave(p3, filename = here::here("imagenes", "joy_division_03.png"),
       device = "png", width = 15, height = 20, units = "cm")

