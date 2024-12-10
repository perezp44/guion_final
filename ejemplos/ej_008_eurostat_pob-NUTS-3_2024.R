#- 2023-11-27, pedro.j.perez@uv.es
#- poblacion EUROSTAT: demo_r_pjanaggr3. Population on 1 January by broad age group, sex and NUTS 3 region.
options(scipen = 999) #- para quitar la notación científica

library(tidyverse)
library(eurostat)

zz <- search_eurostat("Population", type = "all") #- busco datos relacionados con población (354  datasets)

#my_table <- "demo_r_d2jan"         #- "Population on 1 January by age, sex and NUTS 2 region"
my_table <- "demo_r_pjangrp3"       #- "Population on 1 January by age group, sex and NUTS 3 region"

#-
label_eurostat_tables(my_table)        #- gives information about the table


#------------------ dowloading the selected  data with get_eurostat()
df_orig <- get_eurostat(my_table, time_format = 'raw', keepFlags = TRUE)       #- dowloads the table from Eurostat API

#- obtain label for the variables codes
# df_l <- label_eurostat(df_orig, fix_duplicated = TRUE)                        
df_names <- names(df_orig)
df_orig <- label_eurostat(df_orig, code = df_names, fix_duplicated = TRUE)     

df <- df_orig #- me guardo df_orig y trabajo sobre df

#- veamos q hay en df
df_aa <- pjpv.curso.R.2022::pjp_dicc(df)
df_bb <- pjpv.curso.R.2022::pjp_valores_unicos(df, nn = 400)

#- ver q hay de Spain -----------------------
zz <- df %>% dplyr::filter(str_detect(geo_code, "^ES"))
zz <- df %>% dplyr::filter(geo_code == "ES")
zz <- df %>% dplyr::filter(geo == "Teruel")

#- borro todo excepto ....
my_objs_quedarme <- c("df_orig", "df", "df_bb")
rm(list = setdiff(ls(), my_objs_quedarme))



#- empiezo a trabajar
#- me quedo la info de genero y grupos de edad y el número de gente(poblacion = values)
df <- df %>% select(-c(values_code, sex_code, unit, unit_code, flags, TIME_PERIOD_code))
df <- df %>% rename(year = TIME_PERIOD) %>% rename(poblacion = values)

df <- df %>% select(geo_code, geo, year, sex, age_code, poblacion, flags_code, age)
str(df)

#- armonizar country codes (Grecia y UK creo)
df <- df %>% mutate(iso_2_code =  eurostat::harmonize_country_code(geo_code))
zz <- df %>% dplyr::filter(geo_code != iso_2_code) %>% dplyr::select(geo, geo_code, iso_2_code) %>% distinct()
rm(zz)

#- paso year a numérico
df <- df %>% mutate(year =  as.numeric(year))


#- quiero calcular la región mas envejecida; definida como el ratio de gente de + de 80 sobre el total
df_ok <- df %>%
  filter(age_code %in% c("Y_GE90", "TOTAL")) %>%    #- filtro 2 grupos de edad
  filter(sex == "Total") %>%                        #- pob total
  select(-c(flags_code, sex,  age)) %>%             #- quito variables
  pivot_wider(names_from = age_code, values_from = poblacion) %>%
  mutate(pob_vieja = (Y_GE90/TOTAL)*100)

#- dowloading the geometries for European countries
geometrias <- get_eurostat_geospatial(resolution = "20", nuts_level = "3")
plot(geometrias, max.plot = 1)


#-  merging the data with the geometries
names(geometrias)
mapdata <- inner_join(df_ok, geometrias, by = c("geo_code" = "id"))


#- como las coropletas se (suelen) ven mejor con variables categóricas:
#- Las cloropetas suelen quedar mejor si discretizamos la variable a graficar
#- podriamos hacerlo con ntile() pero eurostat::cut_to_classes() te pone un nombre para la categoría muy adecuado
#- Es posible que tus datos permitan categorizar para diferentes categorías. por ejemplo sex o edad etc....


#- categorizo la variable "pob_vieja" ------------------------------------------
#- discretizo de 2 formas

mapdata <- mapdata %>% 
  group_by(year) %>%    #- para discretizar agrupo x año
  #- discretizo con cut_to_classes()
  mutate(pob_vieja_cat.1  = cut_to_classes(pob_vieja, n = 8, decimals = 1, style = "quantile"), .after = pob_vieja) %>%
  #- discretizo con ntile()
  mutate(pob_vieja_cat.2 = as.factor(ntile(pob_vieja, n = 8)), .after = pob_vieja) %>% ungroup()




#- selecciono lo q quiero graficar: 2020
mapdata_si <- mapdata %>%  filter(year == 2020)



#- he creado la paleta en: https://colorbrewer2.org/#type=diverging&scheme=BrBG&n=9
my_paleta <- c('#8c510a','#bf812d','#dfc27d','#f6e8c3','#f5f5f5','#c7eae5','#80cdc1','#35978f','#01665e') #- diverging
my_paleta <- c('#f7fcfd','#e5f5f9','#ccece6','#99d8c9','#66c2a4','#41ae76','#238b45','#006d2c','#00441b') #- secuencial

p1 <- ggplot(mapdata_si) +
  geom_sf(aes(fill = pob_vieja_cat.1,  geometry = geometry), color = "#74696d", size = .1) +
  scale_fill_manual(values = my_paleta) +
  labs(title = "% de población mayor de 90 años (2017)",
       #subtitle = "(por cada 100.000 habitantes)",
       fill = "% mayor de 90",
       caption = "(C) EuroGeographics for the administrative boundaries") + theme_light() +
  coord_sf(xlim = c(-12, 44), ylim = c(35, 67))

p1

p2 <- ggplot(mapdata_si) +
  geom_sf(aes(fill = pob_vieja_cat.2,  geometry = geometry), color = "#74696d", size = .1) +
  scale_fill_manual(values = my_paleta) +
  labs(title = "% de población mayor de 90 años (2020)",
       #subtitle = "(por cada 100.000 habitantes)",
       fill = "% mayor de 90",
       caption = "(C) EuroGeographics for the administrative boundaries") + theme_light() +
  coord_sf(xlim = c(-12, 44), ylim = c(35, 67))

library(patchwork)
p1 + p2

#- si quisieramos escala continua usariamos la v. pob_vieja 
p3 <- ggplot(mapdata_si) +
  geom_sf(aes(fill = pob_vieja,  geometry = geometry), color = "#74696d", size = .1) +
  #scale_fill_manual(values = my_paleta) +
  labs(title = "% de población mayor de 90 años (2020)",
       #subtitle = "(por cada 100.000 habitantes)",
       fill = "% mayor de 90",
       caption = "(C) EuroGeographics for the administrative boundaries") + theme_light() +
  coord_sf(xlim = c(-12, 44), ylim = c(35, 67))

p3



#- Una mejora fácil es hacer un facet ---------------
#- fijate que cojo los datos de varios (9 años)
#- fijate q ahora uso pob_vieja_cat.2

p <- mapdata %>%
  ggplot() +
  geom_sf(aes(fill = pob_vieja_cat.2,  geometry = geometry), color = "black", size = .1) +
  scale_fill_manual(values = my_paleta) +
  labs(title = "% de población mayor de 90 años (2017)",
       #subtitle = "(por cada 100.000 habitantes)",
       fill = "% mayor de 90",
       caption = "(C) EuroGeographics for the administrative boundaries") + theme_light() +
  coord_sf(xlim = c(-12, 44), ylim = c(35, 67))

p + facet_wrap(vars(year)) #-



#- gganimate -------------------------------------------------------------------
#pak::pak("magick")
#devtools::install_github('thomasp85/gganimate')
library(gganimate)
p + transition_manual(year) +
  labs(title = "Año: {current_frame}",
       caption = "Datos de Eurostat")


#- gif: guardamos el gganimate como un gif
gganimate::anim_save("./pruebas/my_gganimate.gif")


#- cargamos el gif
my_gif <- magick::image_read("./pruebas/my_gganimate.gif") #- leemos el gif

my_gif

print(my_gif)   #- el gif tiene 9 frames



#- leemos una imagen, el logo de R ---------------------------------------------
logo <- magick::image_read("https://jeroen.github.io/images/Rlogo.png") %>%
  magick::image_resize("480x266!")
#- logo <- "./imagenes/R-logo.png"

#- insertamos el logo de R en el gif
my_gif[c(3:4)] <- logo
my_gif

#- piramides -------------------------------------------------------------------
#- ya q tenemos los datos de poblacion, vamos a hacer una piramide


#- quito 3 grupos de edad y arreglo
df_ok <- df %>%
  filter(!(age_code %in% c("UNK", "TOTAL", "Y_GE85"))) %>%    #- quito 3 grupos de edad
  filter(sex != "Total") %>%                   #- dejo H y M
  select(-c(flags_code, age)) %>%         #- quito variables
  mutate(time = lubridate::ymd(year, truncated = 2L), .before = year) %>%    #- no hace falta, pero convierto year a fecha
  mutate(year = lubridate::year(time))    #- extraigo el año de una fecha


df_ok <- df_ok %>% 
  mutate(age_code.f = case_when(
    age_code == "Y_LT5" ~ "Y00-04",
    age_code == "Y5-9" ~ "Y05-09",
    age_code == "Y_GE90" ~ "Y90-",
    .default = age_code)) %>% 
  mutate(age_code.f = as_factor(age_code.f))    #- paso age_code a factor
  
levels(df_ok$age_code.f)
  
df_ok <- df_ok %>%        #- reordeno los level del factor age_code.f
  mutate(age_code.f = forcats::fct_relevel(age_code.f, "Y05-09", after = 0L)) %>% 
  mutate(age_code.f = forcats::fct_relevel(age_code.f, "Y00-04", after = 0L)) %>% 
  arrange(year, sex, age_code.f)
  
levels(df_ok$age_code.f)

df_ok <- df_ok %>%        #- truqui: la poblacion de H la convierto en (-)
 mutate(poblacion.m  = case_when(
   sex == "Males" ~ -poblacion,
   TRUE ~ poblacion)) 

#- Hay pkgs dedicados a hacer pirámides de población  
#- https://epirhandbook.com/en/demographic-pyramids-and-likert-scales.html
# Teruel en 2020 -----------------------------------
my_region <-"Spain" #- "Spain", "Teruel"
my_year <- 2020
my_title <- paste0(my_region, " (", my_year, ")")

df_region <- df_ok %>% 
  filter(geo == my_region)

df_region_2020 <- df_region %>% 
  filter(year == my_year)

pop_range <- range(df_region_2020$poblacion.m)
pop_range_seq <- seq(pop_range[1], pop_range[2], length.out = 9)


theme_set(theme_minimal())

p <- ggplot(data = df_region_2020) +
  geom_vline(xintercept = 0, size = 1, color = "black") +
  geom_col(aes(x = poblacion.m , y = age_code.f, fill = sex), color = "white", alpha = 0.3) +
  geom_path(aes(x = poblacion, y = age_code.f, color = sex, group = sex)) +
  #scale_x_continuous(breaks  = pop_range_seq, labels = abs(pop_range_seq)) +
  #scale_x_continuous(labels = abs, limits = max(df_region_2020$poblacion) * c(-1,1) ) + 
  scale_x_continuous(labels = abs) + 
  scale_color_manual(values = c(2, 5)) + 
  geom_vline(xintercept = 0, linewidth = 1, color = "black") +
  #annotate( "text", label = c("Males", "Females"), x = c(-3e5, 3e5), y = 75,   size = 7, family = font_rc, color = c(5, 2))+
  labs(title = my_title,
    caption = "Datos: Eurostat (demo_pjan)",
    x = NULL) 
p

#- intento con ggpol: no me salío !!!
library(ggpol)
ggplot(df_region_2020, aes(y = age_code.f, x = poblacion.m, fill = sex)) +
  geom_bar(stat = "identity") +
  ggpol::facet_share(df_region_2020$sex, dir = "v", scales = "free", reverse_num = TRUE) +   # note: scales = "free"
  #coord_flip() +
  theme_minimal() +
  labs(y = "Count", x = "Age Band", title = " ") +
  scale_fill_manual(values = c("pink", "blue"))
