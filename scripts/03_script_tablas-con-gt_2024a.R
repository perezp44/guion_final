#- script para tablas (ITA)

library(tidyverse)

#- Vamos a utilizar unos datos que descargué del World Bank (usando el pkg wbstats)
#- descargue 3 indicadores similares a los que tiene el pkg "gapminder":
#- SP.DYN.LE00.IN:	Life expectancy at birth, total (years)
#- NY.GDP.PCAP.CD:	GDP per capita (current US$)
#- SP.POP.TOTL   :	Population, total
#- los datos tienen frecuencia anual (1960-2022) 


#- importamos los datos al Global --
my_url <- "https://github.com/perezp44/archivos.download.2023/raw/refs/heads/main/df_world_bank_ITA.rds"
df <- rio::import(my_url)  #- df tiene 13.641 rows x 8 columnas

rm(my_url)

#-  hay datos de 1960 a 2022, PERO solo voy a usar datos de 2000 a 2022
df <- df %>% filter(year >= 2000)

#- miramos (un poco) los datos --
str(df)

zz <- df %>% distinct(country)   #- hay datos de 217 países
zz <- df %>% distinct(region)    #- hay datos de 7 regiones 
zz <- df %>% distinct(year)      #- tenemos 23 periodos, de 2000 a 2022



#- me guardo un df para después
df_despues <- df %>% filter(year >= 2000) %>% 
  filter(iso3c %in% c("ESP", "ITA", "FRA", "USA")) %>% 
  select(iso3c, country, year, esperanza_vida, poblacion, pib_percapita)


#- TABLAS ----------------------------------------------------------------------
#- vamos a hacer tablas con esos datos
#- usaremos principalmente el paquete gt: https://gt.rstudio.com/

library(gt)

#- esta es la tabla que usaré de ejemplo. En concreto calculamos:
#- la esperanza de vida media (por región)
#- el PIB percapita medio por región
#- la poblacion total (sum()) de cada región
#- todo calculado para el año 2022

df_t <- df %>% 
  filter(year == 2022) %>%
  group_by(region) %>%
  summarise(
    #esperanza_vida_w = weighted.mean(x = esperanza_vida, w = poblacion, na.rm = TRUE),
    esperanza_vida = mean(esperanza_vida, na.rm = TRUE), 
    pib_percapita = mean(pib_percapita, na.rm = TRUE), 
    poblacion = sum(poblacion, na.rm = TRUE)) 


#- hacer una tabla básica es muy fácil
#- https://rfortherestofus.com/2019/11/how-to-make-beautiful-tables-in-r/

#- hay muchos paquetes para hacer tablas. Por ejemplo: 
DT::datatable(df_t)

reactable::reactable(df_t)

flextable::flextable(df_t)

gt::gt(df_t) 


#- gt package ------------------------------------------------------------------
#- Para hacer tablas usaremos el paquete gt: https://gt.rstudio.com/
#- un buen mini-tutorial: https://3mw.albert-rapp.de/p/gt-intro
#- novedades gt 0.11.0: https://posit.co/blog/everything-new-thats-in-gt-0-11-0/
#- Ejemplos de gt tables: https://community.rstudio.com/c/table-gallery/64
#- una mia: https://perezp44.github.io/slides.2023/jornadasR_2023_barcelona/index.html#/7/1
#- https://github.com/rich-iannone/gt-workshop


#- creamos una tabla (básica) con gt
#- tt_1 no será un data.frame, será un objeto de clase gt; una "tabla"

tt_1 <- gt::gt(df_t) 

#- gt() también tiene elementos interactivos pero nos centraremos en tablas estáticas

tt_1 %>% 
  opt_interactive(use_search = TRUE,
                  use_filters = TRUE,
                  use_resizers = TRUE,
                  use_compact_mode = TRUE,
                  use_page_size_select = TRUE)


#- Empezamos con una tabla gt() básica
tt_1 <- gt::gt(df_t) 
tt_1

##- themes ---------
#- igual q con los ggplots podemos
#- tunear las tablas con los themes
#- los themes vienen en el pkg  gtExtras::

tt_1 %>% gtExtras::gt_theme_excel()
tt_1 %>% gtExtras::gt_theme_espn()
tt_1 %>% gtExtras::gt_theme_guardian()
tt_1 %>% gtExtras::gt_theme_pff()
tt_1 %>% gtExtras::gt_theme_nytimes()
tt_1 %>% gtExtras::gt_theme_dark() 


##- mas themes en el pkg gtutils ------
#- gtutils: https://gtutils.aweatherman.com/index.html
#- blog: https://www.bucketsandbytes.com/t/tutorial

#remotes::install_github("andreweatherman/gtUtils")

tt_1 %>% gtUtils::gt_theme_athletic()



#- por ejemplo, supón que elegimos el theme de Guardian
tt_2 <- tt_1 %>% gtExtras::gt_theme_guardian()


##- guardando la tabla ------
#- guardamos la gt table en formato para Word

fs::dir_create("./tablas")

gt::gtsave(tt_2, "./tablas/tabla_2.docx")    #- en formato .docx

gt::gtsave(tt_2, "./tablas/tabla_2.rtf")     #- en formato .rtf



#- guardando la tabla como imagen (hay q instalar el pkg "webshot")
gtsave(tt_2, "./tablas/tt_2.png")



#- TUNEANDO una gt table -------------------------------------------------------
#- lo que sí lleva  trabajo es "tunearla" para dejarla chachi

options(scipen = 999) #- para evitar la notación científica

str(df_t) #- 1 v. character y 3 v. numéricas


#- antes voy a ORDENAR las regiones por la esperanza de vida
df_t <- df_t %>% 
  arrange(desc(esperanza_vida))

tt_1 <- gt(df_t)

#- podemos tunear/arreglar muchos elementos de la tabla para que esta sea adecuada
tt_1


#- lo primero es conocer un poco la "jerga" 
#- para poder saber como se llaman las partes de una gt table
#- Puedes verlas aquí: https://gt.rstudio.com/


##- primera columna -----------
#- muchas veces conviene decirle q columna contiene los nombres de las filas
tt_1 <- df_t %>% gt(rowname_col = "region")

tt_1

##- títulos ------------
#- pongo títulos con tab_header()
tt_1 <- tt_1 %>% 
  tab_header(title = "Resumen de indicadores por regiones",
             subtitle = "(Año 2022)") 

tt_1

#- alineación del título
tt_1x <- tt_1 %>% 
  opt_align_table_header(align = "left")    # left align header


tt_1x


#- detalle: negritas en los títulos
#- fíjate en md()
tt_1 <- tt_1 %>% 
tab_header(title = md("Resumen de **indicadores por regiones**"),
           subtitle = md("(Año **2022**)")  )  

tt_1

##- notas al pie -----------------
#- notas en el table footer con tab_source_note()

tt_1 <- tt_1 %>% 
  tab_source_note(md("Fuente: datos de [World Bank](https://www.worldbank.org)")) %>%
  tab_source_note(md("Obtenidos a partir del paquete *wbstats*"))

tt_1

#- notas al pie con tab_footnote() 

tt_1 <- tt_1 %>%  tab_footnote(footnote = "PIB percapita en dólares USA corrientes", 
                               locations = cells_column_labels(columns = pib_percapita))


tt_1


#- notas al pie en alguno de los valores de la tabla con tab_footnote() 
#- con la la opción location podemos decirle donde poner las marcas de las footnotes
#- las pongo en los valores de la tabla con location = cells_body() 
#- y otra en el título con location = cells_title("title")
tt_1 <- tt_1 %>% 
  tab_footnote(footnote = "Esta región es la que tiene la esperanza de vida más baja(😬️)",
               placement = "right",
               location = cells_body(columns = pib_percapita, rows = 7)) 
tt_1

#- nota en el título
tt_1 <- tt_1 %>%             
  tab_footnote(footnote = "Bufff, no me gusta mucho el título!!!", 
               placement = "right",
               location = cells_title("title")) 

tt_1




##- cambiar nombres columnas ------------ 
#- podemos cambiar los labels/titulos de las columnas con cols_label()

tt_1 <- tt_1 %>% 
  cols_label(esperanza_vida = md("**Esperanza vida**"),
             pib_percapita = md("**PIB** (per cápita)"),
             poblacion = md("**Población**"))
tt_1

#- ATENCIÓN!!: Cambiar los labels, los títulos de las columnas,
#- no cambian los nombres de las v./columnas de los datos de la tabla


#- las columnas se pueden mover (**)
tt_1x <- tt_1 %>% 
  cols_move(columns = c(esperanza_vida, ), after = c(poblacion))

tt_1x



#- se pueden combinar columnas (**)
tt_1x <- tt_1 %>% 
  cols_merge(columns = c(pib_percapita, poblacion), pattern = "[{1} - {2}]")
tt_1x

tt_1x <- tt_1x %>%
  cols_label(pib_percapita = "Rango") 
tt_1x  



##- alineación de las columnas -------------
tt_1 <- tt_1 %>% 
  cols_align(align = "center") %>% 
  cols_align(align = "right", columns = c(poblacion))

tt_1


##- anchura de las columnas --------------
#- puedes cambiarla con  cols_width() en pixels (px()) o % of the current size (pct()) 
tt_1 <- tt_1 %>% 
  cols_width(columns = c(esperanza_vida) ~ px(200)) %>% 
  cols_width(columns = c(pib_percapita) ~ px(150)) %>% 
  cols_width(columns = c(poblacion) ~ px(200)) 


tt_1  

##- formateando los números/valores ------
#- con fmt_*() puedes dar formato a los valores de la tabla.
tt_1 <- tt_1 %>% 
  fmt_number(columns = c(esperanza_vida), 
             decimals = 2) %>% 
  fmt_currency(columns = c(pib_percapita), 
               decimals = 1, 
               sep_mark = ".", 
               dec_mark = ",",
               currency = "USD",
               placement = "right") %>% 
  fmt_number(columns = c(poblacion), 
             decimals = 2, 
             sep_mark = ".", 
             dec_mark = ",", 
             suffixing = TRUE)

tt_1


 
#- añadir filas de totales o medias o ....  (**)
tt_1x <- tt_1 %>%
  grand_summary_rows(
    fns = list(
      "min",
      "max",
      "sum",
      list(label = "avg", fn = "mean")
    ),
    fmt = ~ fmt_number(., use_seps = FALSE,)
  )
tt_1x


 
##- agrupando columnas -----------
#- agrupando columnas con "spanners"

tt_1 %>% 
  tab_spanner(label = "Sociales",
              columns = c(esperanza_vida)) %>% 
  tab_spanner(label = "Económicas",
              columns = c(pib_percapita, poblacion)) %>% 
  tab_spanner(label = "Indicadores",
              columns = c(esperanza_vida, pib_percapita, poblacion)) %>% 
  identity()

tt_1



##- themes a mano (*****) -----------


#- cambiamos la fuente y color de las lineas de la tabla
tt_1x <- tt_1 %>% 
  opt_row_striping() %>% 
  opt_table_font(font = google_font("Fira Mono")) %>% 
  tab_options(column_labels.border.bottom.color = "purple",
              table_body.border.bottom.color = "green",
              table_body.hlines.color = "orange")
tt_1x

#- igual algo más sobrio mejor
tt_1x <- tt_1x %>% 
  opt_row_striping() %>% 
  opt_table_font(font = google_font("Fira Mono")) %>% 
  tab_options(column_labels.border.bottom.color = "black",
              table_body.border.bottom.color = "black",
              table_body.hlines.color = "white") %>% 
  opt_align_table_header(align = "left")       # left align header

tt_1x



##- añadiendo colores a celdas (**) -----------------------
tt_1x <- tt_1 %>% 
  tab_style(style = cell_fill(color = "lightblue"),
            locations = cells_body(columns = esperanza_vida,
                                   rows = esperanza_vida >= mean(esperanza_vida)))  %>% 
  tab_style(style = cell_text(color = "red"),
            locations = cells_body(columns = pib_percapita,
                                   rows = pib_percapita >= mean(pib_percapita))) %>% 
  tab_style(style = cell_borders(color = "orange", weight = px(10)),
            locations = cells_body(columns = poblacion,
                                   rows = poblacion > 1000e06))
tt_1x




##- escalas de colores en las celdas (**) -----------------------

tt_1x <- tt_1 %>% 
  data_color(columns = c(esperanza_vida),
             fn = scales::col_numeric(palette = "Greens", domain = NULL) ) %>% 
  data_color(columns = c(pib_percapita),
             fn = scales::col_numeric(palette = "Reds", domain = c(2500, 86000))) %>% 
  data_color(columns = c(poblacion),
             fn = scales::col_numeric(palette = c("yellow", "purple"), domain = c(50e6, 3e09)),
             alpha = .9)
tt_1x








##- pegando tablas -----------------
#- pegando tablas y plots con patchwork
#- https://www.tidyverse.org/blog/2024/09/patchwork-1-3-0/
#- antes, la tabla tenía q estar como grid, con gt::as_gtable()

library(patchwork)


p1 <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point()



p1 + tt_2 +  ggtitle("Sample of the dataset")




df_despues_2022 <- df_despues %>% filter(year == 2022) 


##- imágenes en la tabla ---------------------------------


#- url's de 4 imágenes
url_foto_1 <- "https://upload.wikimedia.org/wikipedia/commons/thumb/3/3e/Pancrudo_%2CCervera_del_Ric%C3%B3n_.Teruel.jpg/266px-Pancrudo_%2CCervera_del_Ric%C3%B3n_.Teruel.jpg"
url_foto_2 <- "https://images.pexels.com/photos/35629/bing-cherries-ripe-red-fruit.jpg?auto=compress&cs=tinysrgb&dpr=1&w=500"
url_foto_3 <-  "https://upload.wikimedia.org/wikipedia/commons/thumb/b/bd/Nacho_Vegas_Fib.jpg/800px-Nacho_Vegas_Fib.jpg"
url_foto_4 <- "https://upload.wikimedia.org/wikipedia/commons/thumb/4/47/Pirate_Flag_of_Jack_Rackham.svg/1280px-Pirate_Flag_of_Jack_Rackham.svg.png"


urls_fotitos <- c(url_foto_1, url_foto_2, url_foto_3, url_foto_4)

#- voy a usar este df
df_despues_2022 <- df_despues %>% filter(year == 2022) 



df_img <- cbind(df_despues_2022, urls_fotitos) #- #- añadimos columnas con ruta a la imagen

tt_img <- df_img %>% gt() #- creamos la tabla tt_img a partir del df df_img
tt_img

tt_img %>% 
  gt::text_transform(locations = cells_body(columns = c(urls_fotitos)), 
                     fn = function(x){gt::web_image(x, height = 30)})


##- insertar columna con las imágenes de las banderas 
library(countrycode)
library(glue)

df_flags <- df_banderas %>% #- fips
  mutate(iso2 = countrycode(sourcevar = iso3c, origin = "iso3c", destination = "iso2c", warn = FALSE)) %>% 
  mutate(iso2 = tolower(iso2)) %>% 
  mutate(flag_URL = paste0("https://hatscripts.github.io/circle-flags/flags/", iso2, ".svg")) %>% 
  #mutate(flag_URL = glue::glue("https://hatscripts.github.io/circle-flags/flags/{iso2}.svg")) %>% 
  identity()

tt_flags <- df_flags %>% gt()


tt_flags %>% 
  gt::text_transform(locations = cells_body(columns = c(flag_URL)), 
                     fn = function(x){gt::web_image(x, height = 30)})


#- pkg gtExtras: ---------------------------------------------------------------
#- pkg para añadir extra features a las tablas gt: https://jthomasmock.github.io/gtExtras/


##- banderas con gtExtras ------
#- por ejmeplo, lo de las banderas/imágenes se puede hacer más fácil con pkg gtExtras


#- cambiamos 2 enlaces , en lugar de banderas que sean afotos
df_flags$flag_URL[4] <- "https://upload.wikimedia.org/wikipedia/commons/thumb/4/47/Pirate_Flag_of_Jack_Rackham.svg/1280px-Pirate_Flag_of_Jack_Rackham.svg.png"

df_flags$flag_URL[3] <- "https://s1.eestatic.com/2017/07/21/el-cultural/el_cultural_232992100_218957660_1706x960.jpg"

#- creamos la tabla
tt_ok <- df_flags %>% gt(rowname_col = c("iso3c")) 
tt_ok  

#- ponemos las banderas again (pero mas facil)
tt_ok <- tt_ok %>% gtExtras::gt_img_rows(flag_URL, height = 25)
tt_ok


##- más cosas con gtExtras ---------------
#- https://jthomasmock.github.io/gtExtras/articles/plotting-with-gtExtras.html
#- un bar_plot con gtExtras::gt_plt_bar_pct()
tt_ok <- tt_ok %>% 
  gtExtras::gt_plt_bar_pct(column = poblacion, scaled = FALSE, fill = "blue", background = "lightblue") %>%
  cols_align("center", contains("scale")) 
tt_ok


#- gtExtras::gt_plt_dot()
tt_ok %>% 
  gtExtras::gt_plt_dot(column = pib_percapita, category_column = country,  max_value = NULL,
                       palette = c("red", "pink", "purple", "blue")) %>% 
  gt::tab_header(title = "Salario medio y máximo por países")








#- gtsummary pkg ---------------------------------------------------------------
#- gt summary es un pkg q hace tablas estadísticas
#- https://www.danieldsjoberg.com/gtsummary/

#- creando una gt summary
tt_3 <- iris |> 
  gtsummary::tbl_summary() |> 
  gtsummary::bold_labels()

tt_3

#- guardar una gtsummary table --
#- si quieres guardar en un Word una tabla hecha con gtsummary
#- tienes primero que guardarla como gt table con as_gt()
#- pasar una gt summmay table a gt table y guardarla en un archivo .docx


#- pasamos tt_3 a formato gt
tt_3a <- tt_3 %>% gtsummary::as_gt()   

#- grabamos tt_3a en un Word
tt_3a %>%  gt::gtsave(filename = "./tablas/my_tt_3a.docx")



