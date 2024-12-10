#- pedro.j.perez@uv.es [revisado: 2024-09-04]
#- ejemplo con datos INE (EPA)
#- el ejemplo tiene 2 partes: la primera se centra en manejar/arreglar datos, y la segunda hacemos mapas

#- Datos: últimos datos EPA (Encuesta de población activa)
#- url: https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736176918&menu=ultiDatos&idp=1254735976595
#- Hay muchas tablas. anuales, trimestrales, CCAA, provinciales etc ... etc ...

library(tidyverse)
library(sf)         #- xq haremos algún mapa


#- Datos por CC.AA: https://www.ine.es/dynt3/inebase/es/index.htm?padre=811&capsel=817
#- p.ej. vamos a dscragar y trabajar con la tabla: "Tasas de paro por distintos grupos de edad, sexo y CC.AA"
#- https://www.ine.es/jaxiT3/Tabla.htm?t=4966&L=0


#- Descargamos la tabla del INE
my_url <- "https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/4966.csv?nocab=1"
fecha_hoy <- Sys.Date()
my_destino <- paste0("./pruebas/epa_ccaa", "_", fecha_hoy, ".csv")
curl::curl_download(my_url, my_destino)   #- capado

#- DETALLE: los datos q hemos descargado están en formato "csv" (PERO separados por ;)
#- Importamos la tabla al Global
df <- rio::import(my_destino) #- rio::import() funciona!!
str(df)                       #- sí, PERO realmente funciona?? fijate en la variable "Total"


#- antes de hacer nada: los nombres de las variables tienen q estar bien (ser sintácticamente válidos)
names(df) #- están medio-bien pero ... aún así quiero recordar janitor::clean_names()
df <- janitor::clean_names(df)
names(df)

#- Pb con la tasa de paro (en la v. total) tiene , en el decimal
df <- df %>% mutate(total = stringr::str_replace(total, "," , "." ))
str(df)
df <- df %>% mutate(total = as.numeric(total)) #- un NA en Melilla 2016


#- OK, ya (casi) tenemos los datos correctamente leídos/interpretados
df <- df %>% rename(tasa_paro = total)
df <- df %>% rename(CCAA = comunidades_y_ciudades_autonomas)

#- 2) La v "CCAA" tiene el código y el nombre
# ademas, en provincias estan las 16 CC.AA y el "Total nacional"
df <- df %>%
  mutate(CCAA = ifelse(CCAA == "Total Nacional",
                                paste("00", CCAA),
                                CCAA))
df <- df %>%
  tidyr::separate(CCAA, sep = " ",
                  into = c("ine_ccaa", "ine_ccaa.n"), extra = "merge")


#- OK, ya tenemos los datos correctamente leídos/interpretados


#- Veamos q hay en df ----------------------------------------------------
df_dicc <- pjpv.curso.R.2022::pjp_dicc(df)
df_uniques <- pjpv.curso.R.2022::pjp_valores_unicos(df)

rm(df_dicc, df_uniques)



#- me quedo con los totales para ESP  ------------------------------------------
#- y hago un gráfico chapucero
df_esp <- df %>%
  filter(ine_ccaa.n == "Total Nacional")

theme_set(theme_bw())

p1 <- ggplot(df_esp, aes(x = periodo, y = tasa_paro, color = edad)) +
  geom_line() + geom_point()
p1 <- p1 + facet_wrap(vars(sexo))
p1

theme_set(theme_grey())




#- vamos a hacer ahora una coropleta con la tasa de paro de cada CA
df_ccaa <- df %>%
  filter(edad == "Total") %>%
  filter(ine_ccaa.n != "Total Nacional") %>%
  filter(sexo == "Ambos sexos") %>%
  select(-c(edad, sexo))

#- me quedo con el último año (q resulta q es 2023)
df_ccaa_2023 <- df_ccaa %>% filter(periodo == max(periodo, na.rm = TRUE))




#- cargo geometrías de provincias
df_geo_prov <- pjpv.curso.R.2022::LAU2_prov_2020_canarias
plot(df_geo_prov, max.plot = 1)

#- podemos ver q la última columna de df_geo_prov tiene las "geometrías"
names(df_geo_prov)
head(df_geo_prov)

#- me quedo con las vv. q me interesan
df_geo_prov <- df_geo_prov %>% select(ine_prov, ine_prov.n, ine_ccaa, ine_ccaa.n)
names(df_geo_prov)

#- podemos "agregar" geometrías
df_geo_ccaa <- df_geo_prov %>%
               group_by(ine_ccaa, ine_ccaa.n) %>% summarize() %>% ungroup()
plot(df_geo_ccaa, max.plot = 1)
names(df_geo_ccaa)

# df_geo_esp <- df_geo_ccaa %>% group_by(1) %>% summarise()
# plot(df_geo_esp, max.plot = 1)


#- junto geometría (df_geo_ccaa) con datos INE (df_ccaa_2021)
#- las geometrías a la izquierda
df_ok <- left_join(df_geo_ccaa, df_ccaa_2023, by = c("ine_ccaa" = "ine_ccaa"))
names(df_ok)

#- basic plot
p <- ggplot() +
  geom_sf(data = df_ok,
          aes(geometry = geometry, fill = tasa_paro),
          color = "white", size = 0.09)

p + scale_fill_distiller(palette = 2)
p + pjpv.curso.R.2022::theme_pjp_maps()
p + scale_fill_viridis_c(option = "plasma")

#- mejoramos un poco el plot ---------------------------------------------------

#- para ello calculo centroides (!!!)
library(sf)
df_geo_ccaa <- cbind(df_geo_ccaa, st_coordinates(st_centroid(df_geo_ccaa$geometry)))
names(df_geo_ccaa)
#- vuelvo a juntar datos EPA con geometrías (q ahora incorporan los centroides)
df_ok <- left_join(df_geo_ccaa, df_ccaa_2023, by = c("ine_ccaa" = "ine_ccaa"))


p <- ggplot() +
  geom_sf(data = df_ok,
          aes(geometry = geometry), fill = "antiquewhite",
          color = "black", size = 0.09) +
  geom_text(data = df_ok, aes(x = X, y = Y, label = tasa_paro), #- v. continua
            color = "black",
            check_overlap = TRUE, size = 3)  #- fontface = "bold"

p

#- luego ya hay que tunearlo (un poco)
p + pjpv.curso.R.2022::theme_pjp_maps() +
  labs(title = "Tasa de desempleo (%)",
       subtitle = "(datos de la EPA)",
       caption = "Datos provenientes del INE")

#- hasta dejarlo mas o menos PRO: https://perezp44.github.io/pjperez.web/01_blog.html

#- mapas interactivos
library(tmap)

tmap::tmap_mode("view")
# coropletas con qtm()
qtm(df_ok, fill = "tasa_paro")
qtm(df_ok, fill = "tasa_paro", format = "World", style = "col_blind", projection = "+proj=eck4")



# coropletas con más opciones
qtm(df_ok, fill = "tasa_paro", fill.n = 4, fill.palette = "div",
    fill.title = "Tasa de paro (%)",  style = "gray")


#- en realidad tmap tiene otra API
tmap_mode("plot")
tm_shape(df_ok) + tm_polygons("tasa_paro",  n = 2) + tm_layout(bg.color = "skyblue")


tmap_mode("view")
tm_shape(df_ok) + tm_polygons("tasa_paro", id = "ine_ccaa.n", popup.vars = TRUE)



#- un poco más de tmap
# remotes::install_github("r-tmap/tmaptools", force = TRUE)
# remotes::install_github("r-tmap/tmap", force = TRUE)
library(tmap)
library(tmaptools)
library(sf)
data(World, rivers, metro) #- hacemos accesibles 3 conjuntos de datos (df's)

tm_shape(World) +
  tm_polygons("life_exp")


dff <- sf::st_sf(df_ok)
tm_shape(dff) +
  tm_polygons("total")
names(df_ok)

