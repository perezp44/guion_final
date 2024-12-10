#- tabla literario regiones y genero
#- https://perezp44.github.io/slides.2023/jornadasR_2023_barcelona/index.html#/6/2


library(tidyverse)
library(gt)
library(ggtext)   # pkg to interpret HTML with element_markdown to color titles
library(showtext) # for specific fonts; font_add_google
library(MetBrewer)
library(sf)
library(tmap)
library(glue)
library(janitor)
options(scipen = 999) #- para quitar la notación científica

#- datos de premios literarios provenientes de Wikidata
my_url <- "https://raw.githubusercontent.com/perezp44/archivos.download.2023/master/df_unnest_unique.rds"
df_original <- rio::import(my_url)



#- tabla 2: CONTINENTES (premios x continentes q dan el premio) ------------------------------------
#- df_tt_03_2 : hago los cálculos
#- % de los premios literarios que se llevan las mujeres (de PREMIOS de determinados CONTINENTES)

my_vv <- quote(pais_premioLabel.grupo)

df_tt_03_2 <- df_original %>% 
  #tidyr::drop_na(pais_premioLabel.continente) %>% 
  #distinct(personLabel, awardLabel, year_premio, .keep_all = TRUE) %>% 
  count(eval(my_vv), genderLabel) %>% 
  pivot_wider(values_from = n, names_from = genderLabel) %>% 
  mutate(percent_M = femenino / (femenino + masculino)) %>% 
  mutate(NN_total = femenino + masculino) %>% 
  arrange(desc(percent_M)) %>% 
  identity()


#- visualización (una tabla con gt)
#- tabla x regiones 
df <- df_tt_03_2
df <- df %>% rename(pais_premioLabel.grupo = `eval(my_vv)`)
df <- df %>% mutate(pais_premioLabel.grupo = ifelse(is.na(pais_premioLabel.grupo), "No se conoce el país", pais_premioLabel.grupo))

my_title <- paste0("Premios literarios por <span style='color:#8c52a3'> género </span> y <span style='color:#3569a4'> regiones </span>")
my_subtitle <- "(Números absolutos y porcentaje de <span style='color:#8c52a3'> mujeres </span>)"

tt_03_2 <- df %>% select(1, percent_M, femenino, NN_total) %>% 
  slice(5, 3, 2, 7, 9, 8, 4, 1, 6) %>% 
  mutate(percent_M = percent_M*100) %>% 
  gt(rowname_col = "Área") %>% 
  tab_header(title = md(my_title))  %>% 
  tab_source_note(md("Fuente: datos provenientes de [Wikidata](https://www.wikidata.org/?uselang=es)")) %>%
  gtExtras::gt_theme_nytimes() %>%
  tab_footnote(footnote = "Número de galardones totales otorgados (en la totalidad de premios literarios del área)", 
               location = cells_column_labels(columns = NN_total)) %>% 
  cols_label(pais_premioLabel.grupo = "",
             percent_M = "% premios a mujeres", 
             femenino = "Nº de premios a mujeres", 
             NN_total = "Nº total de premios" )  %>% 
  cols_align(align = "center") %>% 
  cols_align(align = "left", columns = pais_premioLabel.grupo) %>% 
  #cols_width(columns = n ~ px(120)) %>% 
  fmt_percent(columns = c(percent_M), decimals = 2, scale_values = FALSE) %>% 
  tab_row_group(label = md("**NA**"), rows = 9) %>% 
  tab_row_group(label = md("**Resto de continentes**"), rows = 6:8) %>% 
  tab_row_group(label = md("**America**"),rows = 3:5) %>% 
  tab_row_group(label = md("**Europa**"), rows = 1:2) %>% 
  fmt_integer(columns = c(femenino, NN_total), locale = "es") %>% 
  grand_summary_rows(columns = c(femenino, NN_total), fns = list( Total ~ sum(.)), fmt = ~ fmt_number(., locale = "es", decimals = 0)) %>%
  data_color(columns = c(percent_M),
             colors = scales::col_numeric(palette = "Reds", domain = NULL)) %>% 
  identity()


tt_03_2 %>%
  tab_options(table.font.size = 20, 
              table.width = 800, heading.title.font.size = 24,  
              column_labels.font.size = 20, row_group.font.size = 20, 
              stub.font.size = 20, footnotes.font.size = 16)

