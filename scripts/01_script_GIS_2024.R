#- script para seguir el tutorial_09 (GIS en R)
#- https://ggplot2-book.org/maps
#- https://r.geocompx.org/adv-map.html

#- cargando pkgs 
options(scipen = 999) #- para quitar la notación científica
# pacman::p_load(knitr, here, tidyverse, patchwork, ggrepel)
# pacman::p_load(sf, rnaturalearth, rnaturalearthdata, ggspatial, mapview, leafem, leaflet, tmap)
# pacman::p_load(mapSpain, giscoR, tmap)


library(tidyverse)
library(sf)


#- si queréis hacer un mapa del mundo
library(tmap)
data(World)

#- qué es el objeto World?
class(World) #- es un sf, pero tb es un data.frame: es un df con una variable que contiene sf
names(World) #- fíjate que la ultima columna se llama geometry y almacena los datos espaciales

#- la columna "geometry" es sticky ...  y suele ser pesada, cuesta visualizarla
#- solo puedes quitarla son una f. especial sf::st_drop_geometry()
zz <- World %>% sf::st_drop_geometry()

#- si quitamos la columna con el sf, nos queda un df
str(zz)

#- empezamos a trabajar: nos quedamos con 5 variables
#- fíjate q el código de los países está en iso3c
my_world <- World %>% select(iso_a3, name, continent, life_exp, geometry)
head(my_world, n = 4)

#- tendrás que unir tus datos con los datos de las geometrías q están en World
#- easy, pero que ocurre si los nombres de los países no coinciden?
#- sí, puede ocurrir que en los datos de vuestro trabajo tengáis datos de países
#- pero no tenéis el código, sino que solo tenéis el nombre de los países
#- o tenéis los códigos, pero en otro formato  ¿que hacemos? 

#- pues usar countrycode::countrycode()
#- https://www.jordimas.cat/courses/fiiei_es/introduccion/fuentes_indicadores_es_countrycode/
#- countrycode::codelist es una lista exhaustiva (624 v.) de códigos, nombres y regiones para los países del mundo
zz <- countrycode::codelist 

#- veamos los nombres y códigos para España
zz <- countrycode::codelist %>% filter(country.name.en == "Spain") 

#- en my_world tenemos los códigos en iso3c
#- pero igual necesitamos los códigos en iso2c
my_world <- my_world %>%  
  mutate(iso2c = countrycode::countrycode(iso_a3, 
                                          origin = "iso3c", 
                                          destination = "iso2c"), .after = iso_a3) 

#- si necesitamos los nombres en castellano
my_world <- my_world %>% 
  mutate(name_esp = countrycode::countrycode(iso_a3, 
                                             origin = "iso3c", 
                                             destination = "cldr.name.es"), .after = name) 

#- ya TENEMOS el continente, pero si lo necesitásemos ...
my_world <- my_world %>% 
  mutate(continente = countrycode::countrycode(iso_a3, 
                                              origin = "iso3c", 
                                              destination = "continent"), .after = continent) 
head(my_world)


#- Ok, hagamos algún plot con my_world
class(my_world)
plot(my_world, max.plot = 2)

#- quitar Antártida y ...
my_quitar <- c("Territorios Australes Franceses", "Antártida")
my_world <- my_world %>% filter( !(name_esp %in% my_quitar)  )

plot(my_world, max.plot = 1)

rm(my_quitar, World, zz) #- quito objetos


#- podemos hacer mapas con ggplot: con geom_sf() con la sintaxis habitual
p1 <- ggplot(data = my_world,  
             aes(fill = continent)) + 
          geom_sf() +
          #theme(legend.position = "none") +
          #scale_fill_viridis_d(guide = "none") +
          #scale_x_continuous(limits = c(-5, 20)) +
          #scale_y_continuous(limits = c(-10, 60)) +
          #coord_sf(crs = 4326) +     #- 4326, 3035
          #theme_void() +
          labs(title = "Coord. geográficas") +
          NULL
p1


#--------- datos españoles
# load(here::here("datos", "geometrias_BigData_2021.RData"))



#- theme un poco más adecuado para mapas ---------------------------------------
theme_set(theme_bw())  
theme_set(cowplot::theme_map())


#- 3 formas equivalentes de usar geom_sf() (ya lo sabemos)
ggplot(data = my_world) + geom_sf()
ggplot(data = my_world, aes(geometry = geometry)) + geom_sf()
ggplot() + geom_sf(data = my_world, aes(geometry = geometry))



#- mejoramos un poco el mapa
ggplot(data = my_world) + 
  geom_sf(color = "black", fill = "lightgreen", lwd = 0.2)


#-
p1 <- ggplot(data = my_world) + geom_sf() +
  labs(title = "Gráfico 1: Mapa del mundo",
       caption = "Datos provenientes de tmap")
p1

#- hacemos una coropleta con la v. "life_exp"
#- y jugamos con las escalas de color

#- life_exp es v. continua
p1 + geom_sf(aes(fill = life_exp))

p1 + geom_sf(aes(fill = life_exp)) + scale_fill_distiller()

p1 + geom_sf(aes(fill = life_exp)) + scale_fill_viridis_c(option = "plasma")

p1 + geom_sf(aes(fill = life_exp)) + scale_fill_fermenter(palette = 2, direction = -1, guide = "colourbar")
  



#- proyecciones  --------------
#- world2 <- st_transform(world, "+proj=laea +y_0=0 +lon_0=155 +lat_0=-90 +ellps=WGS84 +no_defs")
#- es old fashion: https://fosstodon.org/@florisvdh/113141976039854831
p1 + coord_sf(crs = "+proj=laea +y_0=00 +lon_0=155 +lat_0=-90 +ellps=WGS84 +no_defs") + 
  labs(subtitle = "Lambert Azimuthal Equal Area projection")

#- se pueden usar distintos sistemas para las proyecciones
p1 + coord_sf(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=1321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ")  #- usamos PROJ string

p1 + coord_sf(crs = sf::st_crs(3035))  + #- usamos SRID identifier
  labs(subtitle = "(European-centric ETRS89 \nLambert Azimuthal Equal-Area projection)")


#- centroides de los polígonos ----------
#- calculamos Centroides de cada polígono (obtendremos dos puntos, long-lat)
#- sf::st_centroid sustituye la geometry por el centroide
world_centroides <- sf::st_centroid(my_world, of_largest_polygon = TRUE)  

str(world_centroides)
head(world_centroides)

p_points <- ggplot(data = world_centroides) + 
  geom_sf(aes(color = continent)) + 
  labs(title = "Gráfico 1: Mapa del mundo (centroides)",
       caption = "Datos provenientes de tmap")

p_points + theme(legend.position = "none") 


p1 + geom_sf(data = world_centroides) 


rm(p_points)

#- puedo tener las 2 geometrías juntas (en el mismo df)
my_world_centroides <- cbind(my_world, world_centroides)
names(my_world_centroides)

#- truqillo: ahora el centroide pasa a estar en 2 columnas, llamadas X e Y, donde figuran la longitud y latitud del centroide (luego lo usaremos!!)
my_world_centroides <- cbind(my_world, st_coordinates(st_centroid(my_world$geometry, of_largest_polygon = TRUE)))
names(my_world_centroides)

#- ponemos nombres de los países en el map
#- ponemos texto en el centroide (X,Y)
ggplot(my_world_centroides) + 
  geom_sf(aes(fill = life_exp)) + 
  scale_fill_distiller() +
  geom_text(aes(label = iso2c, x = X, y = Y), size = 3)

#- en realidad ya existe geom_sf_text()
ggplot(my_world_centroides) + 
  geom_sf(aes(fill = life_exp)) + 
  scale_fill_distiller() +
  geom_sf_text(aes(label = iso2c), size = 3, check_overlap = TRUE)


#- haciendo ZOOM ---------
#- Seguimos con p1 y hagamos zoom en Spain
p1

p_esp <- p1 + 
  coord_sf(
    xlim = c(-15.00, 15.00), 
    ylim = c(21, 47.44), 
    #ylim = c(-90, 90),         #- latitud grados hasta el ecuador
    #xlim = c(-180.00, 180.00), #- longitud (X): grados hasta meridiano de Greenwich 
    expand = TRUE)

p_esp


#- ponemos en el centroide (X,Y) el nombre del país
p_esp + 
  geom_text(data = my_world_centroides, 
            aes(x = X, y = Y, label = name), 
            color = "darkblue", fontface = "bold", check_overlap = TRUE, size = 2.5) +
  annotate(geom = "text", x = 5.5, y = 38, 
           label = "Mar Mediterráneo", fontface = "italic", color = "red", size = 4)

#- en lugar del nombre, ponemos el valor de life_exp
p_esp + 
  geom_text(data = my_world_centroides, 
            aes(x = X, y = Y, label = life_exp), 
            color = "darkblue", fontface = "bold", check_overlap = TRUE, size = 2.5) +
  annotate(geom = "text", x = 5.5, y = 38, 
           label = "Mar Mediterráneo", fontface = "italic", color = "red", size = 4)


#- volvemos al mundo
p <- p1 + 
  geom_sf(fill = "antiquewhite") +
  geom_text(data = my_world_centroides, 
            aes(x = X, y = Y, label = name), 
            color = "darkblue", fontface = "bold", 
            check_overlap = TRUE, size = 2) +
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), 
                         pad_y = unit(0.5, "in"),
                         style = ggspatial::north_arrow_fancy_orienteering) +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.5)) +
  #theme(panel.grid.major = element_blank()) +
  theme(panel.background = element_rect(fill = "aliceblue")) +
  NULL
p


#- volvemos a hecr zoom  en España
p + coord_sf(xlim = c(-15.00, 15.00), ylim = c(21, 47.44), expand = TRUE) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.5)


#- podemos guardar el plot
## ggsave("./pruebas/map.pdf")
## ggsave("./pruebas/map_web.png", width = 6, height = 6, dpi = "screen")

#- my_pueblos ---------------
#- vamos a crear un df con long y latitud de 2 "ciudades"
my_pueblos <- data.frame(
  nombre = c("Pancrudo", "Valencia"), 
  longitude = c(-1.028781, -0.3756572),
  latitude = c(40.76175, 39.47534) )     
my_pueblos


#- podemos poner esos puntos en nuestro mapa
p_esp + 
  geom_point(data = my_pueblos, 
             aes(x = longitude, y = latitude), 
             size = 2, color = "darkred") +
  geom_text(data = my_pueblos, 
            aes(x = longitude, y = latitude, label = nombre), 
            color = "darkblue", fontface = "bold", check_overlap = TRUE, size = 2.5)


#- si quisieramos pasar long y lat a sf
my_pueblos_sf <- st_as_sf(my_pueblos, 
                       coords = c("longitude", "latitude"), 
                       crs = 4326,  #- EPSG:4326 Coordenadas Geográficas WGS84
                       agr = "constant")

str(my_pueblos_sf)

#- al añadir una nueva capa con un df_sf se quita el zoom
p_esp + geom_sf(data = my_pueblos_sf, size = 2, color = "darkred")


#- otra vez hacemos zoom
p_esp + geom_sf(data = my_pueblos_sf, size = 2, color = "darkred") + 
  coord_sf(xlim = c(-15.00, 15.00), ylim = c(21, 47.44), expand = TRUE)


#- geometrías provinciales ----------------
#- con el paquete mapSpain
library(mapSpain)
aa <- mapSpain::esp_codelist  #- códigos y nombres de regiones ESP

prov <- mapSpain::esp_get_prov(moveCAN = TRUE)
plot(prov, max.plot = 1)

#- con el pkg giscoR
library(giscoR)
zz <- giscoR::gisco_get_nuts(year = 2021, resolution = 20, nuts_level = 3, epsg = 4326)


#- con el pkg geodata: 
prov <- geodata::gadm(country = "ESP", level = 2, path = "./datos") %>% sf::st_as_sf()


#- con el pkg LAU2boundaries4Spain (pjpv.curso.R.2022)
prov <- pjpv.curso.R.2022::LAU2_prov_2020_canarias
plot(prov, max.plot = 1)
names(prov)




#- ponemos las geometrias de las provincias en nuestro plot (p)
p + geom_sf(data = prov, fill = "red") + 
  coord_sf(xlim = c(-15.00, 15.00), ylim = c(21, 47.44), expand = TRUE)

#- agregamos provincias para obtener geometrías para las CCAA
ccaa <- prov %>% 
  group_by(ine_ccaa, ine_ccaa.n, ine_ccaa.n.pjp) %>% 
  #group_by(codauto) %>% 
  summarise()

plot(ccaa, max.plot = 1)

names(prov)
#- podemos hacer mapas con ggplot: con geom_sf()
p1 <- ggplot(ccaa,  aes(fill = ine_ccaa)) + 
  geom_sf() +
  scale_fill_viridis_d(guide = "none") +
  labs(title = "Coord. geográficas")

p1

p2 <- ggplot(ccaa, aes(fill = ine_ccaa) ) + 
  geom_sf() + coord_sf(crs = 2100, datum = sf::st_crs(2154)) +
  scale_fill_viridis_d(guide = "none") +
  labs(title = "Lambert 93")

p2

library(patchwork)
p1 + p2


#- elegimos 4 CC.AA
zz_ccaa_chosen <- c("Andalucía", "Galicia", "Aragón", "Canarias", "Illes Balears")
zz_prov_1 <- prov %>% filter(ine_ccaa.n %in% zz_ccaa_chosen)
zz_prov_2 <- prov %>% filter(!(ine_ccaa.n %in% zz_ccaa_chosen))

p1 <- ggplot(zz_prov_1, aes(fill = ine_prov) ) + 
  geom_sf()  +
  scale_fill_viridis_d(guide = "none") +
  labs(title = "Coord. geográficas")

p2 <- ggplot(zz_prov_2, aes(fill = ine_prov) ) + 
  geom_sf()  +
  scale_fill_viridis_d(guide = "none") +
  labs(title = "Coord. geográficas")

p1 + p2 

#- Volvemos al mapamundi
p


#- añadimos las ccaa
p + geom_sf(data = ccaa, fill = NA) + 
  coord_sf(xlim = c(-15.00, 15.00), ylim = c(21, 47.44), expand = TRUE)


#- número de municipios de las provincias de Andalucía (lo calculamos) -
pob_muni <- pjpv.curso.R.2022::ine_pob_mun_1996_2021

#- pob_muni <- mapSpain::esp_get_munic_siane(epsg = 4326, moveCAN = TRUE, rawcols = TRUE)


pob_andalucia <- pob_muni %>% 
  #select(id_prov, ine.prov.name, id_ine, name) %>% 
  select(ine_prov, ine_prov.n, ine_muni, ine_muni.n, poblacion, values, year, ine_ccaa, ine_ccaa.n) %>% 
  filter(year == 2020) %>% 
  filter(poblacion == "Total") %>% 
  filter(ine_ccaa.n == "Andalucía") %>%
  group_by(ine_prov.n) %>% mutate(n_pueblos = n_distinct(ine_muni.n)) %>%
  mutate(pob_prov = sum(values, na.rm = TRUE)) %>% ungroup() %>%
  mutate(pob_media_pueblos = pob_prov/n_pueblos) %>% 
  select(ine_prov.n, ine_prov, pob_media_pueblos, pob_prov, n_pueblos) %>%
  distinct() %>% ungroup()



#- fusionamos 2 df's, uno con información geográfica -
geo_pob_andalucia <- left_join(pob_andalucia, prov, by = c("ine_prov" = "ine_prov"))
names(geo_pob_andalucia)

#-
p + geom_sf(data = ccaa, fill = NA) +
  geom_sf(data = geo_pob_andalucia, aes(geometry = geometry, fill = n_pueblos)) +
  scale_fill_viridis_c(alpha = .4) +
  geom_sf(data = my_pueblos_sf, size = 2, color = "darkred") +
  coord_sf(xlim = c(-15.00, 25.00), ylim = c(21, 57.44), expand = TRUE)




#- Mapas interactivos --------------------------

# Paquete `mapview` -------------------------
mapview::mapview(my_pueblos_sf)
mapview::mapview(prov)


info_CCAAs <- pob_muni %>% 
  select(ine_prov, ine_prov.n, ine_muni, ine_muni.n, poblacion, values, year, ine_ccaa, ine_ccaa.n) %>% 
  filter(year == 2020) %>% 
  filter(poblacion == "Total") %>% 
  group_by(ine_ccaa, ine_ccaa.n) %>%
  mutate(n_prov = n_distinct(ine_prov)) %>%
  mutate(n_pueblos = n_distinct(ine_muni)) %>%
  mutate(pob_CCAA = sum(values, na.rm = TRUE)) %>% ungroup() %>%
  select(ine_ccaa.n, n_prov, n_pueblos, pob_CCAA) %>%
  distinct() %>% ungroup()

geo_nou <- left_join(ccaa, info_CCAAs)
  
  
mapview::mapview(geo_nou,
                 zcol = c("n_pueblos", "pob_CCAA"),
                 color = "tomato", lwd = 1,
                 cex = "n_my_pueblos")


mapview::mapview(geo_nou, zcol = c("ine_ccaa.n", "n_prov", "n_pueblos", "pob_CCAA")) +
  mapview::mapview(my_pueblos_sf)



### Integración con el paquete `mapedit`
#install.packages("mapedit")
library(mapedit)
my_map <- mapview::mapview(my_pueblos_sf)
drawFeatures(my_map, editor = "leafpm")   #- good

mys_dibujitos <- editMap(mapview::mapview(my_pueblos_sf))
mapview::mapview(mys_dibujitos$finished)


# Paquete `leaflet` -------------------------
#- https://r-charts.com/es/espacial/mapas-interactivos-leaflet/

library(leaflet)
leaflet() %>% addTiles() %>% leafem::addMouseCoordinates()


m <- leaflet() %>%
  addTiles() %>%
  setView(lng = -1.0287810, lat = 40.76175, zoom = 6) %>%
  addMarkers(lng = -1.0287810, lat = 40.76175, popup = "Pancrudo") %>%
  addMarkers(lng = -0.3756572, lat = 39.47534, popup = "Valencia") %>% 
  addPopups(lng = -0.3756572, lat = 39.47534, popup = "Valencia")
m


leaflet() %>%
  addTiles() %>% # Add default OpenStreetMap map tiles
  addCircleMarkers(data = my_pueblos_sf)
#leafem::addHomeButton(ext = extent(breweries91), layer.name = "my_pueblos")



lfplot <- leaflet(data = geo_nou) %>% addTiles() %>%
  addPolygons(fillColor = topo.colors(18, alpha = NULL), stroke = FALSE)
lfplot

#- podemos guardar el leaflet
htmltools::save_html(lfplot, "./pruebas/lfplot.html")


# Paquete `tmap` -------------------------
library(tmap)
data(World)
names(World)

# coropletas con qtm()
qtm(my_world, fill = "life_exp")
qtm(my_world, fill = "life_exp", format = "World", style = "col_blind", projection = "+proj=eck4")


# coropletas con más opciones
qtm(my_world, fill = "life_exp", fill.n = 2, fill.palette = "div",
    fill.title = "Esperanza de vida",  style = "gray")


#- en realidad el pkg tmap tiene otra API
#- qtm() es para hacer quick graphs. 
#- Generalmente se usa está sintaxis con tmap

tmap_mode("plot")
tm_shape(my_world) + tm_polygons("life_exp",  n = 2) + tm_layout(bg.color = "skyblue")

#- con tmap se pueden hacer gráficos interactivos
tmap_mode("view")
tm_shape(my_world) + tm_polygons("life_exp", id = "name", popup.vars = TRUE)


tm_shape(World, projection = "+proj=eck4") +
  tm_polygons("HPI", n = 9, palette = "div", title = "Happy Planet Index", id = "name") +
  tm_style("gray") +
  tm_format("World")


# dot map
data(metro)
head(metro)
qtm(metro, bbox = "China")


# without arguments, a plain interactive map is shown (si the mode is set to view)
qtm()

# search query for OpenStreetMap nominatim
qtm("Valencia")


#- bubble map con qtm()
qtm(World, borders = NULL) + 
  qtm(metro, symbols.size = "pop2010", 
      symbols.title.size = "Metropolitan Areas", 
      symbols.id = "name",
      format = "World")




#- algunos post ----------------------------------------------------------------
#- Cartograma de Spain: https://r-charts.com/es/espacial/cartograma-ggplot2/
#- Cartograma de refugiados: https://fosstodon.org/@cvidonne/113418369623904518
#- Mapa de refugiados con gganimate: https://rfortherestofus.com/2024/11/gganimate-intro
#- leaflet: https://r-charts.com/es/espacial/mapas-interactivos-leaflet/
#- bivariate map: https://scicomm.xyz/@basepair/113494860950475114
#- mapa Giorgios: https://vis.social/@georgios/113420716039836479
#- plot NBA en el ártico: https://vis.social/@georgios/113464388234575954
#- cartograma death penalty: https://r.iresmi.net/posts/2024/death_penalty/
#- polar-centered map (con datos de pob de ciudades): https://freerangestats.info/blog/2024/08/24/polar-maps
#- map-tiles: https://github.com/statsmaths/ggmaptile
#- geofacet Europe Rennie: https://github.com/nrennie/tidytuesday/tree/main/2024/2024-04-30
#- geofacet USA: https://fosstodon.org/@nrennie/113549551475123334   El pk geofacet: https://github.com/hafen/geofacet     incluye facets de CCAA y provincias para Spain
#- Global border crossings: https://aditya-dahiya.github.io/projects_presentations/data_vizs/border_crossings.html
#- geofacet de 