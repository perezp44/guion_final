#- pedro.j.perez@uv.es [revisado: 2024-09-04]
#- ejemplo con datos de Eurostat

#- objetivo: para aquellos que no tengan tema, este es un tema "facil"
#- se elige una tabla de datos de Eurostat y se analiza. Aquí hacemos una facetted  coropleta

library(tidyverse)
library(eurostat)
library(sf)

#------------------ searching data about a topic (health) in Eurostat API with search_eurostat()
aa <- search_eurostat("Health", type = "all")

#------------------ select some data from Eurostat
my_table <- "hlth_rs_grd"        #- seleccioné la tabla: "Health graduates"

label_eurostat_tables(my_table)     #- gives information about the table



#------------------ dowloading the selected data with get_eurostat()
df <- get_eurostat(my_table, time_format = 'raw', keepFlags = TRUE)       #- downloads the table from Eurostat API

#- quiero obtener descriptores de las variables
df_names <- names(df)
df <- label_eurostat(df, code = df_names, fix_duplicated = TRUE)
rm(aa,  df_names, my_table)

#- veamos un poco los datos
df_bb <- pjpv2020.01::pjp_f_unique_values(df, truncate = TRUE, nn_truncate = 200)

#- y me quedo con lo que me interesa analizar
df_ok <- df %>% 
    filter(isco08 == "Medical doctors") %>%  #- Medical doctors
    filter(unit_code == "P_HTHAB")  %>%      #- Per hundred thousand inhabitants
    select(geo_code, geo, TIME_PERIOD, values) %>% 
    mutate(time = as.numeric(TIME_PERIOD)) %>%    #- mas adelante (igual) nos hará falta q sea numerico y no character
    select(-TIME_PERIOD) 

#- como las coropletas se ven mejor con variables categóricas:
#- Las cloropetas suelen quedar mejor si discretizamos la variable a graficar
#- podriamos hacerlo con ntile() pero eurostat::cut_to_classes() te pone un nombre para la categoria muy adecuado
#- Categorisng the values with cut_to_classes()
#- Es posible que tus datos permitan categorizar para diferentes categorias. por ejemplo sex o edad etc....

df_ok <- df_ok %>% group_by(time) %>%        
  mutate(cat_time  = cut_to_classes(values, n = 4, decimals = 1, style = "quantile")) %>%
  mutate(cat_time_1 = as.factor(ntile(values, n = 4))) %>% ungroup()


#- dowloading the geometries for European countries
geometrias <- get_eurostat_geospatial(resolution = "20", nuts_level = "0")
plot(geometrias, max.plot = 1)

#-  merging the data with the geometries
mapdata <- inner_join(df_ok, geometrias, by = c("geo_code" = "id"))

#- selecciono lo q quiero graficar: 2019 (paree que los datos de 2019 no están todavía)
#- en 2018 no hay datos para Francia (!!!) asi que cojo 2017
mapdata_si <- mapdata %>%  filter(time == 2017)



p <- ggplot(mapdata_si) +
  geom_sf(aes(fill = cat_time,  geometry = geometry), color = "black", size = .1) +
  scale_fill_brewer(palette = "RdYlBu") +
  labs(title = "Graduados en medicina (2017)",
       subtitle = "(por cada 100.000 habitantes)",
       fill = "Médicos por 100.000 hab",
       caption = "(C) EuroGeographics for the administrative boundaries") + theme_light() +
  coord_sf(xlim = c(-12,44), ylim = c(35,67))


p

#- Una mejora fácil es hacer un facet ---------------
#- fijate que cojo los datos de varios (4 años)
#- fijate q ahora uso cat_time_1

p <- mapdata %>% 
     filter(time %in% c(1980, 1990, 2000, 2010, 2017)) %>% 
  ggplot() +
  geom_sf(aes(fill = cat_time_1,  geometry = geometry), color = "black", size = .1) +
  scale_fill_brewer(palette = "RdYlBu") + 
  labs(title = "Graduados en medicina (2018)",
       subtitle = "(por cada 100.000 habitantes)",
       fill = "Médicos por 100.000 hab",
       caption = "(C) EuroGeographics for the administrative boundaries") + theme_light() +
  coord_sf(xlim = c(-12,44), ylim = c(35,67))


p + facet_wrap(vars(time)) #-






#- gganimate -------------------------------------------------------------------
library(gganimate)
p + transition_manual(time) +
  labs(title = "Año: {current_frame}",
       caption = "Datos de Eurostat")

#- gganimate con facet (no sale good) ------------------------------------------
library(gganimate)
p + facet_wrap(vars(time)) +
  transition_manual(time) +
  labs(title = "Año: {current_frame}")






#- algo muy parecido hecho por Ilya Kashnitsky ---------------------------------
#- Eurostat tiene un built-in df (Ilya Kashnitsky)
library(tidyverse)
library(eurostat)
library(sf)
library(ggthemes)
library(janitor)


europa <- eurostat_geodata_60_2016 %>% clean_names() %>% st_transform(crs = 3035)
europa_fronteras <- europa %>% dplyr::filter(levl_code == 0) %>% rmapshaper::ms_innerlines()
europa_whole <- europa %>% dplyr::filter(levl_code == 0) %>%  rmapshaper::ms_dissolve()
df_milk <- get_eurostat("agr_r_milkpr")

names(europa)

europa %>%
  right_join(df_milk, c("id" = "geo")) %>%
  dplyr::filter(levl_code == 2, TIME_PERIOD == "2019-01-01") %>%
  ggplot()+
  geom_sf(data = europa_whole, color = NA, fill = "#000066") +
  geom_sf(aes(fill = values/1e3), color = NA)+
  geom_sf(data = europa_fronteras, color = "#ffffff", size = .3) +
  rcartocolor::scale_fill_carto_c(
    palette = "ag_Sunset", direction = 1,
    guide = guide_colorbar(
      barheight = 7, barwidth = 3.5, title.position = "top"
    )
  )+
  coord_sf(datum = NA, ylim = c(15e5,55e5), xlim = c(20e5, 75e5)) +
  theme_map() +
  theme(
    legend.position.inside = c(.7, .7),
    #legend.position = c(.7, .7),
    plot.background = element_rect(fill = "#000044"),
    text = element_text(family = "mono", face = 2, color = "#ffffff")
  ) +
  labs(
    title = "Milk production in regions of Europe",
    subtitle = "Eurostat NUTS-2 regions, 2019",
    fill = "m tons",
    caption = "@ikashnitsky"
  )


