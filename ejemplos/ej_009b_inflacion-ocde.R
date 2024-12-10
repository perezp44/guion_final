#- pedro.j.perez@uv.es   [2024-04-09]

library(tidyverse)

#- Datos -----------------------------------------------------------------------
#- Inflación de la OCDE -
#- web: https://data.oecd.org/price/inflation-cpi.htm
url_inflation <- "https://stats.oecd.org/sdmx-json/data/DP_LIVE/.CPI.../OECD?contentType=csv&detail=code&separator=comma&csv-lang=en"

url_inflation <- "https://raw.githubusercontent.com/perezp44/archivos.download.2023/refs/heads/main/DP_LIVE_22032023113248591.csv"


ruta_inflation <- "./datos/OECD,DF_DP_LIVE,+all.csv"


# curl::curl_download(url_inflation, ruta_inflation) #- descargamos los datos
#- https://data-explorer.oecd.org/vis?lc=en&df[ds]=DisseminateArchiveDMZ&df[id]=DF_DP_LIVE&df[ag]=OECD&df[vs]=&av=true&pd=2022%2C2022&dq=OECD%2BOAVG....A&to[TIME_PERIOD]=false&vw=tb

#- importamos los datos -----
inflation_orig <- readr::read_csv(url_inflation)

#- veamos un poco los datos
inflation_dicc <- pjpv.curso.R.2022::pjp_dicc(inflation_orig)
inflation_uniques <- pjpv.curso.R.2022::pjp_valores_unicos(inflation_orig)

#- data munging
inflation <- inflation_orig %>%
  dplyr::filter(FREQUENCY == "M") %>%
  dplyr::filter(MEASURE == "AGRWTH") %>%
  #dplyr::filter(SUBJECT %in% c("TOT", "ENRG")) %>%
  dplyr::mutate(fecha = lubridate::ym(TIME), .after = TIME) 

#- seleccionamos variables
inflation <- inflation %>% 
  select(pais = LOCATION, fecha, inflacion = Value, tipo_inf = SUBJECT)

#- si tuvieramos q pivotar
inflation_w <- inflation %>% pivot_wider(names_from = tipo_inf, values_from = inflacion)
inflation_l <- inflation_w %>% pivot_longer(cols = 3:6, names_to = "tipo_inf", values_to = "inflacion") 

inf_tweet <- inflation_w %>% filter(fecha == max(fecha, na.rm = TRUE)) %>% arrange(desc(FOOD))

rm(inflation_w, inflation_l, inflation_dicc, inflation_uniques, inf_tweet, ruta_inflation, url_inflation)

str(inflation)

#- quiero incorporar información para la región, para así poder filtrar por grupos de países
zz <- countrycode::codelist 
zz <- countrycode::codelist %>% select(iso.name.en, region23, region, un.region.name, iso3c)



#- vamos a juntar los códigos regionales con los datos de inflation
inflation_zz <- inflation %>% distinct(pais)

inflation_zz <- left_join(inflation_zz, zz, by = join_by(pais == iso3c) ) 

#- juntemos de verdad
inflation <- left_join(inflation, zz, by = join_by(pais == iso3c) ) 


rm(zz, inflation_zz)



#- Selecciono fechas para los gráficos -------------------
#- https://estadistica-dma.ulpgc.es/cursoR4ULPGC/6h-Fechas.html
my_fecha_inicio <-  "2020-01-01"
my_fecha_inicio_ok <- format(as.Date("2020-01-01"), format = "%b-%Y")
my_fecha_inicio_ok

my_fecha_final <- inflation %>% summarise(mm = max(fecha)) %>% pull(mm)
my_fecha_final_ok <- format(as.Date(my_fecha_final), format = "%b-%Y")
my_fecha_final_ok

my_fechas <- paste0("(", my_fecha_inicio_ok, " a ",  my_fecha_final_ok, ")")
my_fechas


#- PLOTS -----------------------------------------------------------------------
library(ggtext)   # pkg to interpret HTML with element_markdown to color titles
library(showtext) # for specific fonts; font_add_google
library(glue)

# Fuentes y opciones
font_add_google("Fira Sans Condensed")
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)




#- plot 1 ----------------------------------------------------------------------
#- https://twitter.com/leeolney3/status/1510564709740826625
#- gist: https://gist.github.com/leeolney3/50e0a8e2444c3a51f4cfb5ee1aa664c0

#- solo Europa (q no sea Europa del Este) desde 2020

df <- inflation %>%
  dplyr::filter(un.region.name %in% c("Europe")) %>%
  dplyr::filter(!(region23 %in% c("Eastern Europe")))  %>%
  dplyr::filter(fecha >= my_fecha_inicio) %>%
  dplyr::filter(tipo_inf %in% c("TOT", "FOOD"))

  #dplyr::filter(tipo_inf %in% c("TOT", "TOT_FOODENRG"))


df_dicc <- pjpv.curso.R.2022::pjp_dicc(df)

my_title <- paste0("Evolución (mensual) de la **tasa de inflación interanual** ", my_fechas)
my_title
my_subtitle <- "<span style='color:#229F99'> Inflación en alimentos </span> e <span style='color:#D17011'> Inflación total </span>"

#- limites para el eje y (inflación)
my_y_limit_1 <- df %>%  summarise(mm = min(inflacion, na.rm = TRUE)) %>% pull(mm) 
my_y_limit_2 <- df %>%  summarise(mm = max(inflacion, na.rm = TRUE)) %>% pull(mm)

#- redondeo los límites: https://www.statology.org/round-in-r/
my_y_limit_1 <- floor(my_y_limit_1)
my_y_limit_2 <- ceiling(my_y_limit_2)

  
  
p1 <- df %>% ggplot() +
  geom_line(aes(x = fecha, y = inflacion, color = tipo_inf, group = tipo_inf)) +
  geom_point(aes(x = fecha, y = inflacion, color = tipo_inf, group = tipo_inf), size = .8) +
  scale_color_manual(values = c("#229F99", "#D17011", "#777000", "#E37099")) +
  #scale_y_continuous(limits = c(my_y_limit_1, my_y_limit_2), labels = scales::percent_format(scale = 1, accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1, accuracy = 1)) +
  facet_wrap(~ factor(pais)) +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(family = "Fira Sans Condensed"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth =.3),
        plot.margin = margin(.5, .75, .5, .5, unit = "cm"),
        plot.title.position = "plot",
        plot.title = element_markdown(size = 10),
        panel.spacing.x = unit(1.5, "lines"),
        strip.text = element_markdown(lineheight = 1.2, size = 7.5),
        plot.subtitle = element_markdown(size = 9, lineheight = 1.2, color = "grey20"),
        plot.caption = element_text(color = "grey20"),
        axis.text.y = element_text(color = "grey13", size = 4),
        axis.text.x = element_text(color = "grey13", size = 5),
        axis.title = element_blank()) +
  labs(title = my_title, subtitle= my_subtitle,
       caption = "\n Pedro J. Pérez  |  Data source: data.oecd.org")

p1

#- plot 2 ----------------------------------------------------------------------
#- https://twitter.com/braeuNERD/status/1510690816645287947
#- repo: https://github.com/BraeuNerd/30DayChartChallenge2022/blob/main/Scripts/W1D3_Historical.R

# Fuentes
font_add_google("Roboto")
showtext_auto()

#- solo Europa (q no sea Europa del Este) desde 2020
df <- inflation %>%
  dplyr::filter(un.region.name %in% c("Europe")) %>%
  dplyr::filter(!(region23 %in% c("Eastern Europe")))  %>%
  dplyr::filter(fecha >= my_fecha_inicio) %>%
  dplyr::filter(tipo_inf == "FOOD") %>%
  mutate(pais.2 = pais)  #- para resaltar 1 pais


my_title = paste0("Evolución de la **tasa de inflación (en alimentos) interanual** ", my_fechas)
my_subtitle <- "Países europeos"


p2 <-
  ggplot(df, aes(x = fecha, y = inflacion)) +
    geom_line(data = df %>% select(-pais),
              aes(group = pais.2), color = "white", linewidth = 0.3, alpha = 0.3) +
    geom_line(aes(color = pais, group = pais), color = "#FED459", linewidth = 1) +
    geom_point(aes(color = pais, group = pais), color = "#FED459", size = 1.2) +
    theme(legend.position = "none") +
    facet_wrap(~ pais) +
  #scale_x_discrete(breaks = seq(2020:01,2023:03, by=2)) +
  labs(title = my_title, subtitle = my_subtitle,
       caption = "Datos de la OCDE |   DataViz: Pedro J. Pérez") +
  theme(plot.background = element_rect(fill = "#171717", color = "#171717"),
        panel.background = element_rect(fill = "#171717"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_markdown(family = "Roboto",color = "white", size = 11, margin = margin(20,0,0,0)),
        plot.subtitle = element_markdown(family = "Roboto", color = "white", size = 10, margin = margin(5,0,10,0)),
        plot.caption = element_text(family = "Roboto", hjust = 0.95, color = "grey70", size = 7, margin = margin(0,0,20,0)),
        axis.title.x = element_blank(),
        axis.text.x = element_text(color = "grey70", size = 3, margin = margin(0,20,20,0), angle = 90),
        axis.text.y = element_text(color = "grey70", size = 3),
        axis.title.y = element_blank(),
        strip.background = element_rect(fill = "#171717"),
        strip.text.x = element_text(color = "white"))

p2


#- un poco interactivo)
plotly::ggplotly(p2)

#- plot 3 ----------------------------------------------------------------------


# Fuentes
font_add_google("Roboto")
showtext_auto()

#- solo Europa (q no sea Europa del Este) desde 2020
df <- inflation %>%
  dplyr::filter(un.region.name %in% c("Europe")) %>%
  dplyr::filter(!(region23 %in% c("Eastern Europe")))  %>%
  dplyr::filter(fecha >= my_fecha_inicio)



my_title <- paste0("Tasas de inflación interanual en Europa ", my_fechas)
my_subtitle <- "Países europeos y <span style='color:#FED459'> España </span> "


p3 <-
  ggplot(df, aes(x = fecha, y = inflacion)) +
    geom_line(aes(group = pais), color = "white", linewidth = 0.3, alpha = 0.3) +
    geom_line(data = df %>% filter(pais == "ESP"), aes(color = pais, group = pais), color = "#FED459", linewidth = 1) +
    geom_point(data = df %>% filter(pais == "ESP"), aes(color = pais, group = pais), color = "#FED459", size = 1.2) +
    theme(legend.position = "none") +
    facet_wrap(vars(tipo_inf), nrow = 2, ncol = 2, scales = "free") +
  #scale_x_discrete(breaks = seq(2020:01,2023:03, by=2)) +
  labs(title = my_title, subtitle = my_subtitle,
       caption = "Datos de la OCDE |   DataViz: Pedro J. Pérez") +
  theme(plot.background = element_rect(fill = "#171717", color = "#171717"),
        panel.background = element_rect(fill = "#171717"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_markdown(family = "Roboto",color = "white", size = 11, margin = margin(20,0,0,0)),
        plot.subtitle = element_markdown(family = "Roboto", color = "white", size = 10, margin = margin(5,0,10,0)),
        plot.caption = element_text(family = "Roboto", hjust = 0.95, color = "grey70", size = 7, margin = margin(0,0,20,0)),
        axis.title.x = element_blank(),
        axis.text.x = element_text(color = "grey70", size = 6.4, margin = margin(0,20,20,0), angle = 90),
        axis.text.y = element_text(color = "grey70", size = 6.7),
        axis.title.y = element_blank(),
        strip.background = element_rect(fill = "#171717"),
        strip.text.x = element_text(color = "white"))

p3

#- un poco interactivo)
plotly::ggplotly(p3)




#- TABLA inflación OCDE --------------------------------------------------------
library(gt)

# #- digitos y % en inflacion
# mutate(inflacion.ch = pjpv.curso.R.2022::pjp_round_nice(inflacion), .after = inflacion) %>%
# mutate(inflacion.ch = paste0(inflacion.ch, "%")) %>% 
# mutate(inflacion.f =  as.factor(inflacion),  .after = inflacion) %>% 
# mutate(inflacion.f = forcats::fct_reorder(inflacion.f, inflacion))

my_title <- paste0("Tasas de inflación interanual en Europa (", my_fecha_final_ok, ")")
my_subtitle <- "Países europeos y <span style='color:#3569a4'> España </span> "

#- datos de "EU"
df <- inflation %>%
  dplyr::filter(un.region.name %in% c("Europe")) %>%
  dplyr::filter(!(region23 %in% c("Eastern Europe")))  
  
#- selecciono ultimo dato de infla
my_fecha <-  max(inflation$fecha)
df <- df %>% dplyr::filter(fecha == my_fecha) 

#- paso a ancho ----------------------------------------------------------------
df <- df %>% 
  select(pais, iso.name.en, fecha, inflacion, tipo_inf) %>% 
  pivot_wider(names_from = tipo_inf, values_from = inflacion)

#- banderas --------------------------------------------------------------------
df <- df %>% #- fips
  mutate(iso2 = countrycode::countrycode(sourcevar = pais, origin = "iso3c", destination = "iso2c", warn = FALSE)) %>% 
  mutate(iso2 = tolower(iso2)) %>% 
  mutate(flag_URL = glue::glue('https://hatscripts.github.io/circle-flags/flags/{iso2}.svg')) 


#- diferencial de infla de FOOD a TOT
df <- df %>% mutate(dif_FOOD_TOT = FOOD - TOT)

#df <- df %>% mutate(FOOD = FOOD/max(FOOD))


#- ordeno las columnas y de + a - según la inflación de FOOD
df <- df %>% 
  select(1,2, flag_URL, FOOD, TOT, dif_FOOD_TOT, everything()) %>% 
  arrange(desc(FOOD), TOT) %>% 
  select(-iso2)

#- decimales
#df <- df %>% mutate(across(where(is.numeric), \(x) round(x, digits = 2)))
df <- pjpv.curso.R.2022::pjp_df_decimales(df)

#- tabla infla OCDE ------------------------------------------------------------

#- con DT: https://rstudio.github.io/DT/
df_DT <- df %>% select(-flag_URL)
DT::datatable(df_DT)
DT::datatable(df_DT, filter = 'top', extensions = "Scroller")

#- con reactable: https://glin.github.io/reactable/index.html
library(reactable)
reactable::reactable(df_DT, pagination = FALSE, height = 850, 
                     filterable = TRUE, searchable = TRUE, highlight = TRUE, 
                     columns = list( variable = colDef(
                       # sticky = "left",
                       # Add a right border style to visually distinguish the sticky column
                       style = list(borderRight = "1px solid #eee"),
                       headerStyle = list(borderRight = "1px solid #eee")
                     )),
                     defaultColDef = colDef(minWidth = 50)
)                     



#- con gt: https://gt.rstudio.com/
#df <- df %>% mutate(across(where(is.numeric), \(x) round(x, digits = 2)))


tt_6 <- df %>%  
  gt(rowname_col = "pais") %>% 
  tab_header(title = my_title, subtitle = md(my_subtitle))  %>% 
  tab_source_note(md("Fuente: datos proveniente de la [OCDE](http://www.oecd.org/)")) %>%
  gtExtras::gt_theme_nytimes() %>%
  gtExtras::gt_img_rows(flag_URL, height = 25) %>% 
  gtExtras::gt_merge_stack(col2 = iso.name.en, col1 = pais) %>% 
  tab_footnote(footnote = "Tasa de inflación eliminando el efecto de la energía y los alimentos", 
               location = cells_column_labels(columns = TOT_FOODENRG)) %>% 
  cols_label(TOT = md("**Total**"), ENRG = "Energética", FOOD = "Alimentos", TOT_FOODENRG = "Total sin ..." )  %>% 
  cols_align(align = "center") %>% 
  cols_align(align = "left", columns = fecha) %>% 
  cols_width(columns = TOT ~ px(120)) %>% 
  fmt_percent(columns = c(TOT, ENRG, FOOD, TOT_FOODENRG), scale_values = FALSE) %>% 
  tab_style(style = cell_text(color = "#f97d64"),
            locations = cells_body(columns = TOT,
                                   rows = TOT >= mean(TOT))) %>% 
  tab_style(style = cell_text(color = "#f97d64"),
            locations = cells_body(columns = FOOD,
                                   rows = FOOD >= mean(FOOD))) %>% 
  tab_style(style = cell_text(color = "#f97d64"),
            locations = cells_body(columns = ENRG,
                                   rows = ENRG >= mean(ENRG))) %>% 
  tab_style(style = cell_text(color = "#3569a4"),
            locations = cells_body(rows = pais == "ESP")) %>% 
  data_color(columns = c(TOT_FOODENRG),
             colors = scales::col_numeric(palette = "Reds", domain = NULL)) 

#gtExtras::gt_plt_bar_pct(column = FOOD, scaled = TRUE, fill = "blue", background = "lightblue")


tt_6  


#- guardando la tabla ----------------------------------------------------------
#gtsave(tt_6, "./pruebas/tt_6.rtf")
#gtsave(tt_6, "./pruebas/tt_6.html")

#- guardando la tabla como imagen (hay q instalar el pkg "webshot")
#gtsave(tt_6, "./pruebas/tt_6.png")



#- Curva de Phillips -----------------------------------------------------------
#- Thom-Ivar van Dijk hizo un gráfico chulísimo sobre la curva de Phillips
#- https://twitter.com/ThomIvar/status/1516114838996631563
#- source: https://github.com/TIvanDijk/pRojects/blob/main/30DayChartChallenge/oecd.R
#- Vamos a replicar su  gráfico (con datos actualizados y para más países)
#- y haremos alguna tabla
#- luego usaremos este código para hacer informes/slides y una web con QMD

library(tidyverse)
library(ggtext)

#- Datos (de la OCDE) ----------------------------------------------------------

##- datos de inflacion -------------

#- Inflación: https://data.oecd.org/price/inflation-cpi.htm
# url_inflation <- "https://stats.oecd.org/sdmx-json/data/DP_LIVE/.CPI.../OECD?contentType=csv&detail=code&separator=comma&csv-lang=en"

url_inflation <- "https://raw.githubusercontent.com/perezp44/archivos.download.2023/refs/heads/main/DP_LIVE_22032023113248591.csv"

inflation_orig <- readr::read_csv(url_inflation)
inflation_dicc <- pjpv.curso.R.2022::pjp_dicc(inflation_orig)

##- datos de desempleo -------------
#- Desempleo: https://data.oecd.org/unemp/unemployment-rate.htm
# url_unemployment <- "https://stats.oecd.org/sdmx-json/data/DP_LIVE/.HUR.../OECD?contentType=csv&detail=code&separator=comma&csv-lang=en"

url_unemployment <- "https://raw.githubusercontent.com/perezp44/archivos.download.2023/refs/heads/main/DP_LIVE_22032023114049606.csv"

unemployment_orig <- readr::read_csv(url_unemployment)
unemployment_dicc <- pjpv.curso.R.2022::pjp_dicc(unemployment_orig)

#- seleccionamos datos ---------------------------------------------------------
inflation <- inflation_orig %>%
  dplyr::filter(FREQUENCY == "M" & MEASURE == "AGRWTH", SUBJECT == "TOT") %>%
  select(place = LOCATION, date = TIME, cpi = Value)

unemployment <- unemployment_orig %>%
  dplyr::filter(FREQUENCY == 'M', SUBJECT == 'TOT') %>%
  select(place = LOCATION, date = TIME, rate = Value)

#- fusionamos los 2 df's -------------------------------------------------------
df <- left_join(inflation, unemployment) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(group = case_when(
    str_starts(date, "199") ~ "1991-1999",
    str_starts(date, "200") ~ "2000-2009",
    str_starts(date, "201") ~ "2010-2019",
    str_starts(date, "202") ~ ">= 2020",
    TRUE ~ "other" )) %>%
  dplyr::filter(group != "other")

#- plot nº 1 (1 país) ----------------------------------------------------------
#- plot con datos de 1 país (G-7) y 2 periodos (2000-2019)
my_country <- c("ESP")
my_country <- c("G-7")  #- seleccionamos "un país"
my_periodos <- c("2000-2009", "2010-2019") #- seleccionamos periodos
my_title <- glue::glue("What happened to the <b>Phillips Curve</b> in ", my_country  , "? (thanks to <b style='color:#fd5532'>@ThomIvar</b>)")



df_1p <- df %>%
  dplyr::filter(place == my_country) %>%
  dplyr::filter(group %in% my_periodos)


# -- make plot for 1 country
p1 <- ggplot(df_1p, aes(x = rate, y = cpi, group = group, color = group)) +
  geom_point(alpha = 0.5, size = 2.25)

p1a <- p1 + geom_smooth(method = 'lm', se = FALSE, linetype = 2) +
  scale_color_manual(values = c('#d1495b', '#00798c')) +
  annotate('text', x = 5.5, y = 3.75, label = '2000-2009', color = '#d1495b',
           family = 'American Typewriter', fontface = 'bold') +
  annotate('text', x = 4.5, y = 0.75, label = '2010-2019', color = '#00798c',
           family = 'American Typewriter', fontface = 'bold') +
  labs(title = my_title,
       subtitle = "The Phillips curve is an economic concept that states that inflation
       and unemployment have an inverse relationship. However, in the <b style='color:#00798c'>
       last decade</b> this relationship appears to have vanished.",
       caption = '@ThomIvar • source: OECD (monthly, G7 countries)',
       x = 'Unemployment rate, %',
       y = 'Inflation rate, %') +
  theme_void() +
  theme(text = element_text('American Typewriter', color = 'grey30'),
        legend.position = 'none',
        plot.title = element_textbox_simple(size = 12, margin = margin(b = 0.15, unit = 'cm')),
        plot.subtitle = element_textbox_simple(size = 8, color = 'grey60',
                                               margin = margin(b = 0.25, unit = 'cm')),
        plot.background = element_rect(fill = '#fffef7', color = '#fffef7'),
        plot.caption = element_text(size = 10, color = 'grey80'),
        plot.margin = margin(1, 1, 1, 1, 'cm'),
        panel.grid = element_line(color = 'grey70', linetype = 'dotted'),
        axis.title = element_text(margin = margin(t = 0.2, r = 0.2, unit = 'cm'), color = 'grey50'),
        axis.title.y = element_text(angle = 90),
        axis.text = element_text(color = 'grey70', size = 9,
                                 margin = margin(t = 0.1, r = 0.1, unit = 'cm')),
        axis.line = element_line(color = 'grey50'),
        axis.ticks = element_line(color = 'grey50', size = 0.6),
        axis.ticks.length = unit(0.10, 'cm'))

p1a
#ggsave("./imagenes/oecd.png", width = 9.5, height = 6)



#- plot nº 2 (varios países) ---------------------------------------------------
#- hacemos el gráfico para varios países
my_paises <- c("ESP", "FRA", "GBR", "DEU", "USA", "JPN")
my_periodos <- c("2000-2009", "2010-2019")

df_paises <- df %>%
  dplyr::filter(place %in% my_paises) %>%
  dplyr::filter(group %in% my_periodos)

#- plot feo para luego hacer un facetting
p2 <- ggplot(df_paises, aes(x = rate, y = cpi, color = group)) +
  geom_point(alpha = 0.5, size = 0.75) +
  geom_smooth(method = 'lm', se = FALSE, linetype = 1) +
  scale_color_manual(values = c(  '#d1495b', '#00798c', '#099205','#8e0592'))


#p2 + facet_wrap(vars(place))

p2a <- p2 +
  facet_wrap(vars(place), scales = "free")  +
  labs(title = "What happened to the <b>Phillips Curve</b>? ",
       subtitle = "The Phillips curve is an economic concept that states that inflation
       and unemployment have a inverse relationship. However, we could find some exceptions for <b style='color:#d1495b'>
       2000-2009</b> decade, and for the  <b style='color:#00798c'>
       last decade (2010-2019) </b>.",
       caption = "@pjpv4444  (thanks to  @ThomIvar) • source: OECD (monthly, G7 countries)",
       x = 'Unemployment rate, %',
       y = 'Inflation rate, %') +
  theme_void() +
  theme(text = element_text('American Typewriter', color = 'grey30'),
        legend.position = 'none',
        plot.title = element_textbox_simple(size = 20, margin = margin(b = 0.15, unit = 'cm')),
        plot.subtitle = element_textbox_simple(size = 11, color = 'grey60',
                                               margin = margin(b = 0.25, unit = 'cm')),
        plot.background = element_rect(fill = '#fffef7', color = '#fffef7'),
        plot.caption = element_text(size = 10, color = 'grey80'),
        plot.margin = margin(1, 1, 1, 1, 'cm'),
        panel.grid = element_line(color = 'grey70', linetype = 'dotted'),
        axis.title = element_text(margin = margin(t = 0.2, r = 0.2, unit = 'cm'), color = 'grey50'),
        axis.title.y = element_text(angle = 90),
        axis.text = element_text(color = 'grey70', size = 9,
                                 margin = margin(t = 0.1, r = 0.1, unit = 'cm')),
        axis.line = element_line(color = 'grey50'),
        axis.ticks = element_line(color = 'grey50', linewidth = 0.6),
        axis.ticks.length = unit(0.10, 'cm'))


p2a





#- plot temporales inflación OCDE ----------------------------------------------
#- https://twitter.com/leeolney3/status/1510564709740826625
#- https://twitter.com/braeuNERD/status/1510690816645287947
#- https://twitter.com/malasi_abhinav/status/1514143026297323524
#- https://twitter.com/BolesData/status/1518297682984452097/photo/1
#- https://twitter.com/FrederikRasmus9/status/1520821825034506242
#- plot de Lorena: https://github.com/loreabad6/TidyTuesday/blob/master/plot/2020_week_38.png
#- repo: https://github.com/loreabad6/TidyTuesday/blob/master/R/2020/week_39.Rmd
#- plot: https://github.com/Pecners/tidytuesday/blob/master/2022/2022-03-22/final_plot.png
#- repo: https://github.com/Pecners/tidytuesday/blob/master/2022/2022-03-22/final_plot.R