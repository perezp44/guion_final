#- script para seguir el tutorial nº 8: Tablas con .qmd y .Rmd
#- en el tutorial hay más contenido
library(tidyverse)
options(scipen = 999) #- para quitar la notación científica


#- cargamos datos para poder generar algunos resultados para después mostrarlos en tablas
my_url <- "https://raw.githubusercontent.com/perezp44/iris_data/master/data/PIAAC_data_small.csv"
df_original <- readr::read_csv(my_url)

#- reservo el df_original y trabajo sobre df
#- selecciono 6 variables
df <- df_original %>% 
  select(Country, Gender, Education, Wage_month, Wage_hour, Numeracy_score)

#- vemos q hay en los datos
#- remotes::install_github("perezp44/pjpv.curso.R.2022")
df_aa <- pjpv.curso.R.2022::pjp_dicc(df)             #- estadísticos básicos del df
df_bb <- pjpv.curso.R.2022::pjp_valores_unicos(df)   #- valores únicos de las v.



#- tt_6 ------------------------------------------------------------------------
#- Calculamos la media, el mínimo, el máximo y la desviación típica de Wage_month
df_tt_6 <- df %>% 
    group_by(Country) %>% 
    summarise(W_medio  = mean(Wage_month, na.rm = TRUE) ,
              W_minimo = min(Wage_month, na.rm = TRUE)  ,
              W_maximo = max(Wage_month, na.rm = TRUE)  ,
              W_sd = sd(Wage_month, na.rm = TRUE) ) %>% 
              ungroup()


#- ya tengo los RTDOS q quiero mostrar
#- ahora se trata de mostrarlos en una tabla al menos medio good-looking
#- una alternativa sencilla es knitr::kable()
knitr::kable(df_tt_6)                   #- funcionará ok en ficheros .qmd y .Rmd



#- tt_9 ------------------------------------------------------------------------
#- ¿Cuanto más cobran los hombres que las mujeres?
df_tt_9 <- df %>% 
  group_by(Country, Gender) %>% 
  summarise(W_medio = mean(Wage_month, na.rm = TRUE)) %>% 
  ungroup() 

knitr::kable(df_tt_9)

df_tt_9 <- df_tt_9 %>% 
  pivot_wider(names_from = Gender, values_from = W_medio) 

knitr::kable(df_tt_9)

df_tt_9 <- df_tt_9 %>% 
  mutate(dif_W = Male-Female) %>% 
  mutate(dif_percent_W = dif_W/Female)

knitr::kable(df_tt_9)


#- tt_11 ------------------------------------------------------------------------
#- Numeracy Score por país y nivel de estudios. La tabla nos va a salir alargada
df_tt_11 <- df %>% 
  group_by(Country, Education) %>% 
  summarise(Numeracy_media = mean(Numeracy_score, na.rm = TRUE)) %>% 
  ungroup() 

knitr::kable(df_tt_11)


#- Hagamos la anterior tabla más ancha (Una columna para cada país)
df_tt_11 <- df_tt_11 %>% 
            pivot_wider(names_from = Education, 
                        values_from = Numeracy_media)

knitr::kable(df_tt_11)

#- la tabla está ok, PERO 
#- quiero que los "labels" de los niveles educativos aparezcan en castellano
#- y además quiero ordenarla de menor a mayor nivel educativo (así que usaré factores)
df <- df %>% 
  mutate(Education.f = forcats::as_factor(Education), .after = Education)

levels(df$Education.f)

df %>% count(Education) #- me daba curiosidad saber cuantos NA hay


#- renombrando los levels de los factores
df <- df %>% 
  mutate(Education.f = forcats::fct_recode(Education.f,
                    "Primaria"         = "Primary",
                    "Secundaria"       = "Secondary", 
                    "Secundaria_post"  = "Upper_second",
                    "Terciaria"        = "Tertiary" )) 

levels(df$Education.f)


#- OK, los "labels" de los niveles educativos ya están en castellano, PERO
#- quiero ordenar los niveles educativos de menor a mayor
df <- df %>% 
  mutate(Education.f = forcats::fct_relevel(Education.f, 
                                  "Primaria", "Secundaria", "Secundaria_post")) 
levels(df$Education.f)

df %>% count(Education.f) #- sí, ya están ordenados

#- tt_12 -----------------------------------------------------------------------
#- está será la tabla "final" que mostraré en mi informe
df_tt_12 <- df %>% 
  group_by(Country, Education.f) %>% 
  summarise(Numeracy_media = mean(Numeracy_score, na.rm = TRUE)) %>% 
  ungroup() %>%  
  pivot_wider(names_from = Education.f, 
              values_from = Numeracy_media) 

knitr::kable(df_tt_12)

#- kable() más cosas -----------------------------------------------------------
#- kable() admite dar formato a algunos elementos de la tabla, por ejemplo:
knitr::kable(df_tt_12,
             align = "c", 
             caption = "Numeracy Score por país y nivel educativo",
             digits = 2, 
             format.args = list(decimal.mark = ",", big.mark = "."))


#- ¿Y si queremos cambiar el nombre de las columnas de la tabla?
knitr::kable(df_tt_12, format = "pipe",
             col.names = c("", "Pri", "Sec", "Sec II", "UNI", "NA's"))



#- pkg kableExtra --------------------------------------------------------------
#- https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html
knitr::kable(df_tt_12) %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))


#- scroll box
df_tt_12 %>%
  knitr::kable() %>%
  kableExtra::kable_styling(font_size = 11) %>%
  kableExtra::scroll_box(width = "100%", height = "60%")


#- colorines, girar ...
knitr::kable(df_tt_12) %>%
       kableExtra::kable_styling(full_width = F) %>%
       kableExtra::column_spec(1, bold = T, border_right = T) %>%
       kableExtra::column_spec(3, width = "20em", background = "yellow") %>% 
       kableExtra::row_spec(3:4, bold = T, color = "white", background = "#D7261E") %>% 
       kableExtra::row_spec(0, angle = 10)


#- voy a cambiar de tabla, ahora presentaremos df_tt_6
no_quitar <- c("df", "df_tt_6")
rm(list=ls()[! ls() %in% no_quitar])

#- pkg gt: un ejemplo ----------------------------------------------------------
library(gt) #-remotes::install_github("rstudio/gt")

gt::gt(df_tt_6)
gt::gt(df_tt_6) %>% gtExtras::gt_theme_nytimes()
gt::gt(df_tt_6) %>% gtExtras::gt_theme_dark()
gt::gt(df_tt_6) %>% gtExtras::gt_theme_excel()


#- la tabla, al igual q un ggplot, se guarda en un objeto de clase "list"
gt_tbl <- df_tt_6 %>% gt()
gt_tbl

gt_tbl <- gt_tbl %>% 
  tab_header(title = md("**Genero y nivel educativo**"),
                      subtitle = md("Porcentaje de *H y M* en cada nivel educativo"))
gt_tbl

#- luego profundizaremos en el pkg gt

#- pkg DT: un ejemplo ----------------------------------------------------------
#- https://rstudio.github.io/DT/
DT::datatable(iris)
DT::datatable(df_tt_6)

DT::datatable(df, filter = 'top', 
              options = list(pageLength = 10, autoWidth = TRUE ))

#- pkg reactable: un ejemplo ---------------------------------------------------
#- https://glin.github.io/reactable/
#- https://3mw.albert-rapp.de/p/reactable-intro

reactable::reactable(iris)

reactable::reactable(df, filterable = TRUE, minRows = 10)

#- pkg rpivotTable: un ejemplo ----------------------------------------------------------
#- https://cran.r-project.org/web/packages/rpivotTable/vignettes/rpivotTableIntroduction.html
library(rpivotTable) #- remotes::install_github(c("ramnathv/htmlwidgets", "smartinsightsfromdata/rpivotTable"))
rpivotTable(df, rows = "Gender", 
            cols = c("Country"), 
            width = "100%", height = "400px")

#- más pkgs para tablas descriptivas -------------------------------------------

##- pkg janitor ----
#- Cuadro con el número de observaciones de cada país
zz <- df %>% janitor::tabyl(Country)
zz

#- número de Hombres y Mujeres en cada país
df %>% janitor::tabyl(Country, Gender)

#- Porcentaje de Hombres y Mujeres en cada país
zz <- df %>% 
  janitor::tabyl(Country, Gender) %>% 
  janitor::adorn_percentages(denominator = "row")
zz

#- Porcentaje de Hombres y Mujeres en cada nivel educativo
zz <- df %>% 
  janitor::tabyl(Gender, Education) %>% 
  janitor:: adorn_percentages(denominator = "col")
zz


#- España nº de personas en cada nivel educativo x genero
df %>% 
  filter(Country == "ESP") %>% 
  janitor::tabyl(Education,  Gender)

#- España % x genero en cada nivel educativo
df %>% 
  filter(Country == "ESP") %>% 
  janitor::tabyl(Education.f,  Gender) %>% 
  janitor:: adorn_percentages(denominator = "col") %>% 
  #janitor:: adorn_percentages(denominator = "row") %>% 
  identity()
  


#- prueba para casa (¿podéis calcularlo vosotros con dplyr?)
zz <- df %>% 
  # group_by(Country) %>% 
  # mutate(NN_pais = n()) %>% ungroup %>% 
  group_by(Country, Gender) %>% 
  mutate(NN_genero = n()) %>% ungroup() %>% 
  group_by(Country, Gender, Education.f) %>% 
  mutate(NN_estudios_gender = n()) %>% ungroup %>% 
  group_by(Country) %>% 
  mutate(percent_estudios = NN_estudios_gender/NN_genero) %>% ungroup() %>% 
  #- empiezo a seleccionar resultados
  distinct(Country, Gender, Education.f, percent_estudios) %>% 
  filter(Country == "ESP") %>% 
  arrange(Gender, Education.f) %>% 
  pivot_wider(names_from = Gender, values_from = percent_estudios)
  
  


#- tablas de 3 niveles!!
zz <- df %>% janitor::tabyl(Education, Country, Gender)


#- Hay otros paquetes con funcionalidades parecidas a `janitor`. 


# Por ejemplo, el paquete [`freqtables`](https://github.com/brad-cannell/freqtables) 
freqtables::freq_table(iris, Species)

#- o el paquete [`sjmisc`](https://strengejacke.github.io/sjmisc/index.html).
iris %>% sjmisc::frq(Species)

#- pkg stats 
zz <- stats::xtabs(~ Education + Country + Gender, data = df)
zz

stats::ftable(zz)


##- flextable: ----------------------------------
#- https://ardata-fr.github.io/flextable-book/
#- https://blog.djnavarro.net/posts/2024-07-04_flextable/
#- https://3mw.albert-rapp.de/p/flextable-intro

flextable::proc_freq(mtcars, "gear", "vs")


##- tinytable: ----------------------------------
#- https://vincentarelbundock.github.io/tinytable/



##- table1 -----------------
#- https://github.com/benjaminrich/table1
#- https://blog.djnavarro.net/posts/2024-06-21_table1/
