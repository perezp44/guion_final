#- calculo de rolling-estadísticos 
#- ejemplo tabla con gt()
#- Ejemplos de gt tables: https://community.rstudio.com/c/table-gallery/64
#- uno mio: https://perezp44.github.io/slides.2023/jornadasR_2023_barcelona/index.html#/7/1


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



#- tabla 4: ROLLING % de galardones otorgados a mujeres ------------------------


#- AQUI (slider) ---------------------------------------------------------------
#- defino los lags y leads para el SLIDER
qq_lags <- 9
qq_leads <- 0

qq_anyo_inicial <- 1950
qq_anyo_final <- 2023

#- arreglando datos
df <- df_original %>% 
  distinct(person, personLabel, award, awardLabel, genderLabel, year_premio, pais_premioLabel.ok) %>% 
  tidyr::drop_na(genderLabel) %>%  #- sin el genero, pero todos lo tienen
  tidyr::drop_na(year_premio) %>%  #- los q no sabemos el año
  filter(between(year_premio, qq_anyo_inicial, qq_anyo_final)) 

#- calculo rápido con janitor
zz1 <- janitor::tabyl(df, year_premio, genderLabel) %>%
  adorn_totals("row") %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns() %>%
  adorn_title()


df_tt_05_1 <- df %>% 
  count(year_premio, genderLabel) %>% 
  pivot_wider(names_from = genderLabel, values_from = n) %>% 
  mutate(nn_total_ese_anyo = masculino + femenino) %>% 
  mutate(percent_muj_ese_anyo = femenino/nn_total_ese_anyo, .after = year_premio) %>% 
  rename(nn_mujeres_ese_anyo = femenino)


#- MUNDO: intento de crear % slider
MUNDO_slider <- df_tt_05_1 %>% 
  mutate(nn_total_slide = slider::slide_dbl(nn_total_ese_anyo, sum, .before = qq_lags, .after = qq_leads, .complete = TRUE)) %>%   
  mutate(nn_mujeres_slide = slider::slide_dbl(nn_mujeres_ese_anyo, sum, .before = qq_lags, .after = qq_leads, .complete = TRUE)) %>% 
  mutate(percent_muj_slide = nn_mujeres_slide/nn_total_slide) 


#- reordeno y arreglo un poco la slider para el MUNDO
MUNDO_slider <- MUNDO_slider %>% 
  mutate(pais_premioLabel.ok = "MUNDO", .before = 1) %>% 
  select(-masculino) %>% 
  select(1, 2, 5, 4, 3, 6:8)



 ggplot(df_tt_05_1, aes(x = year_premio, y = percent_muj_ese_anyo)) + geom_line() + geom_point() +
   geom_line(data = MUNDO_slider, aes(y = percent_muj_slide), color = "red")


#- ahora la rolling-% para los paises
#- primer intento
zz <- df_original %>% 
  distinct(person, personLabel, award, awardLabel, genderLabel, year_premio, pais_premioLabel.ok) %>% 
  tidyr::drop_na(genderLabel)  %>% #- sin el genero, pero todos lo tienen
  #filter(pais_premioLabel.ok %in% my_paises | is.na(pais_premioLabel.ok)) %>%  #- los paises tochos
  filter(between(year_premio, qq_anyo_inicial, qq_anyo_final)) %>%  
  group_by(pais_premioLabel.ok, year_premio) %>% 
  mutate(nn_total = n()) %>% 
  group_by(pais_premioLabel.ok, year_premio, genderLabel) %>% 
  mutate(nn_gender = n()) %>%  ungroup %>% 
  distinct(pais_premioLabel.ok, year_premio, genderLabel, nn_total, nn_gender) %>% 
  mutate(percent_gender = nn_gender/nn_total) %>% 
  filter(genderLabel == "femenino")

#- slider: lo mismo con slider ------------------------------

PAISES_slider <- df_original %>% 
  distinct(person, personLabel, award, awardLabel, genderLabel, year_premio, pais_premioLabel.ok) %>% 
  tidyr::drop_na(genderLabel)  %>% #- sin el genero, pero todos lo tienen
  #filter(pais_premioLabel.ok %in% my_paises | is.na(pais_premioLabel.ok)) %>%  #- los paises tochos
  filter(between(year_premio, qq_anyo_inicial, qq_anyo_final)) %>%  
  group_by(pais_premioLabel.ok, year_premio) %>% 
  arrange(year_premio) %>% 
  mutate (nn_total_ese_anyo = n()) %>% 
  group_by(pais_premioLabel.ok, year_premio, genderLabel) %>% 
  mutate(nn_mujeres_ese_anyo = n()) %>% 
  ungroup() %>% 
  distinct(pais_premioLabel.ok, year_premio, genderLabel, nn_total_ese_anyo, nn_mujeres_ese_anyo) %>% 
  #filter(pais_premioLabel.ok == "Italia") %>% 
  tidyr::complete(pais_premioLabel.ok, genderLabel, year_premio, fill = list(nn_mujeres_ese_anyo = 0)) %>% 
  #------ nou
  group_by(pais_premioLabel.ok, year_premio) %>% 
  mutate(nn_total_ese_anyo = ifelse(is.na(nn_total_ese_anyo), 0, nn_total_ese_anyo)) %>%  #- si fuese NA a zero
  mutate(nn_total_ese_anyo = max(nn_total_ese_anyo, na.rm = TRUE)) %>% #- si hay un NA, pasa al maximo, el otro
  #mutate(nn_total_ese_anyo = ifelse(is.na(nn_total_ese_anyo), 0, nn_total_ese_anyo)) %>%  #- si fuesen los 2 NA's
  #------ nou
  group_by(pais_premioLabel.ok, year_premio) %>% 
  arrange(genderLabel) %>% 
  mutate(nn_total_ese_anyo = last(nn_total_ese_anyo)) %>% 
  filter(genderLabel == "femenino") %>% select(-genderLabel) %>% 
  group_by(pais_premioLabel.ok) %>% 
  mutate(percent_muj_ese_anyo = nn_mujeres_ese_anyo/nn_total_ese_anyo) %>% 
  #mutate("nn_{my_name}" := slider::slide_dbl(nn_total_ese_anyo, sum, .before = qq_lags_leads, .after = qq_lags_leads, .complete = TRUE)) %>%
  mutate(nn_total_slide = slider::slide_dbl(nn_total_ese_anyo, sum, .before = qq_lags, .after = qq_leads, .complete = TRUE)) %>% 
  mutate(nn_mujeres_slide = slider::slide_dbl(nn_mujeres_ese_anyo, sum, .before = qq_lags, .after = qq_leads, .complete = TRUE)) %>% 
  mutate(percent_muj_slide = nn_mujeres_slide/nn_total_slide) 


zz_esp <- PAISES_slider %>% filter(pais_premioLabel.ok == "España")

# espana <- PAISES_slider %>% filter(pais_premioLabel.ok == "España")
# tt  <- PAISES_slider %>% filter(year_premio %in% c(1960, 2013))
# italia <- PAISES_slider %>% filter(pais_premioLabel.ok == "Italia")


#- junto el total con los paises
OK_slider.orig <- bind_rows(MUNDO_slider, PAISES_slider)



#- tab 7b: --------------------------------- 
#- quiero ver el rolling-% por continentes

CONTINENTES_slider <- df_original %>% 
  distinct(person, personLabel, award, awardLabel, genderLabel, year_premio, pais_premioLabel.continente) %>% 
  tidyr::drop_na(genderLabel)  %>% #- sin el genero, pero todos lo tienen
  #filter(pais_premioLabel.ok %in% my_paises | is.na(pais_premioLabel.ok)) %>%  #- los paises tochos
  filter(between(year_premio, qq_anyo_inicial, qq_anyo_final)) %>%  #- [1950 - 2023] 
  group_by(pais_premioLabel.continente, year_premio) %>% 
  arrange(year_premio) %>% 
  mutate (nn_total_ese_anyo = n()) %>% 
  group_by(pais_premioLabel.continente, year_premio, genderLabel) %>% 
  mutate(nn_mujeres_ese_anyo = n()) %>% 
  ungroup() %>% 
  distinct(pais_premioLabel.continente, year_premio, genderLabel, nn_total_ese_anyo, nn_mujeres_ese_anyo) %>% 
  #filter(pais_premioLabel.ok == "Italia") %>% 
  tidyr::complete(pais_premioLabel.continente, genderLabel, year_premio, fill = list(nn_mujeres_ese_anyo = 0)) %>% 
  #------ nou
  group_by(pais_premioLabel.continente, year_premio) %>% 
  mutate(nn_total_ese_anyo = ifelse(is.na(nn_total_ese_anyo), 0, nn_total_ese_anyo)) %>%  #- si fuese NA a zero
  mutate(nn_total_ese_anyo = max(nn_total_ese_anyo, na.rm = TRUE)) %>% #- si hay un NA, pasa al maximo, el otro
  #mutate(nn_total_ese_anyo = ifelse(is.na(nn_total_ese_anyo), 0, nn_total_ese_anyo)) %>%  #- si fuesen los 2 NA's
  #------ nou
  group_by(pais_premioLabel.continente, year_premio) %>% 
  arrange(genderLabel) %>% 
  mutate(nn_total_ese_anyo = last(nn_total_ese_anyo)) %>% 
  filter(genderLabel == "femenino") %>% select(-genderLabel) %>% 
  group_by(pais_premioLabel.continente) %>% 
  mutate(percent_muj_ese_anyo = nn_mujeres_ese_anyo/nn_total_ese_anyo) %>% 
  #mutate("nn_{my_name}" := slider::slide_dbl(nn_total_ese_anyo, sum, .before = qq_lags_leads, .after = qq_lags_leads, .complete = TRUE)) %>%
  mutate(nn_total_slide = slider::slide_dbl(nn_total_ese_anyo, sum, .before = qq_lags, .after = qq_leads, .complete = TRUE)) %>% 
  mutate(nn_mujeres_slide = slider::slide_dbl(nn_mujeres_ese_anyo, sum, .before = qq_lags, .after = qq_leads, .complete = TRUE)) %>% 
  mutate(percent_muj_slide = nn_mujeres_slide/nn_total_slide) 


 europa <- CONTINENTES_slider %>% filter(pais_premioLabel.continente == "Europa")
# africa <- CONTINENTES_slider %>% filter(pais_premioLabel.continente == "África")
# asia <- CONTINENTES_slider %>% filter(pais_premioLabel.continente == "Oceanía")


p <- ggplot(CONTINENTES_slider, aes(x = year_premio, y = percent_muj_slide, color = pais_premioLabel.continente)) +
  geom_point() +
  geom_line()

p

#- junto todos los SLIDER ------------------------------------------------------
CONTINENTES_slider <- CONTINENTES_slider %>% 
  select(pais_premioLabel.ok = pais_premioLabel.continente, everything()) %>% 
  filter(!is.na(pais_premioLabel.ok))

OK_slider.orig <- bind_rows(OK_slider.orig, CONTINENTES_slider) %>% 
  filter(!is.na(pais_premioLabel.ok))

#- INTENTOS de quitar paises con poca muestra (NaN)
#- por ejemplo quitar a los paises que tengan algun nn_total_slide <40

OK_slider <- OK_slider.orig %>% 
  group_by(pais_premioLabel.ok) %>% 
  filter(all(!(nn_total_slide %in% c(0:39)))) 



#- rolling-TABLA ---------------------------------------------------------------

zz <- OK_slider %>% 
  group_by(pais_premioLabel.ok) %>% 
  mutate(`2023` = percent_muj_slide[which(year_premio == 2023)]) %>% 
  mutate(`2010` = percent_muj_slide[which(year_premio == 2010)]) %>% 
  mutate(`2000` = percent_muj_slide[which(year_premio == 2000)]) %>% 
  mutate(`1990` = percent_muj_slide[which(year_premio == 1990)]) %>% 
  mutate(`1980` = percent_muj_slide[which(year_premio == 1980)]) %>% 
  mutate(`1970` = percent_muj_slide[which(year_premio == 1970)]) %>% 
  mutate(`1960` = percent_muj_slide[which(year_premio == 1960)]) %>% 
  select(pais_premioLabel.ok, `2023`, `2010`, `2000`, `1990`,  `1980`, `1970`, `1960`) %>% 
  distinct()

zz2 <- tidyr::drop_na(zz)   

italia <- OK_slider %>% filter(pais_premioLabel.ok == "Italia")


xx <- zz %>% filter(pais_premioLabel.ok == "España")

zz3 <- OK_slider %>% 
  filter(year_premio >= qq_anyo_inicial + qq_lags) %>% 
  dplyr::group_by(pais_premioLabel.ok) %>%
  dplyr::summarize(
    # must end up with list of data for each row in the input dataframe
    slide_data = list(percent_muj_slide),
    .groups = "drop"
  )


#- zz4 es el q usare para la tabla ----------------------
zz4 <- left_join(zz3, zz) %>% 
  arrange(desc(`2023`))

my_title <- paste0("% de premios literarios concedidos a<span style='color:#8c52a3'> mujeres </span> ")
my_subtitle <- "(10 year rolling window)"

#- banderas
my_paises <- df_original %>% distinct(pais_premioLabel.ok, pais_premioLabel.iso3c, pais_premioLabel.iso2c)
my_paises <- my_paises %>% 
  mutate(iso2 = tolower(pais_premioLabel.iso2c)) %>% 
  mutate(flag_URL = glue::glue('https://hatscripts.github.io/circle-flags/flags/{iso2}.svg')) 

df <- left_join(zz4, my_paises)
#- ONU
df <- df %>% mutate(flag_URL = ifelse(is.na(pais_premioLabel.iso3c), "https://hatscripts.github.io/circle-flags/flags/un.svg", flag_URL))

#df <- df %>% mutate(pais_premioLabel.ok = ifelse(is.na(pais_premioLabel.iso3c), "No sabemos la nacionalidad", pais_premioLabel.ok))
df <- df %>% 
  mutate(erre.ok = pais_premioLabel.ok) %>% 
  mutate(erre.iso3c = pais_premioLabel.iso3c) %>% 
  mutate(pais_premioLabel.iso3c = ifelse(is.na(erre.iso3c), pais_premioLabel.ok, pais_premioLabel.iso3c)) %>% 
  mutate(pais_premioLabel.ok = ifelse(is.na(erre.iso3c), "--", pais_premioLabel.ok))  %>% 
  select(-erre.ok, -erre.iso3c)




#- ordeno las columnas 
df <- df %>% 
  select(pais_premioLabel.ok, pais_premioLabel.iso3c, flag_URL, everything()) %>% 
  arrange(desc(`2023`) ) 
df <- df %>% mutate( rank = 1: nrow(df), .before = 1) %>% 
  select(1:12) %>% 
  mutate(across(6:12, \(x) x*100))




tt_4 <- df %>%  
  gt(rowname_col = "pais") %>% 
  tab_header(title = md(my_title),subtitle = md(my_subtitle))  %>% 
  
  gtExtras::gt_plt_sparkline(slide_data) %>%
  
  tab_source_note(md("Fuente: datos provenientes de [Wikidata](https://www.wikidata.org/?uselang=es)")) %>%
  gtExtras::gt_theme_nytimes() %>%
  gtExtras::gt_img_rows(flag_URL, height = 35) %>% 
  gtExtras::gt_merge_stack(col2 = pais_premioLabel.ok, col1 = pais_premioLabel.iso3c) %>% 
  tab_footnote(footnote = "Sólo se muestran los países con más de 40 registros en todas las rolling-windows",               location = cells_column_labels(columns = slide_data)) %>% 
  cols_label(pais_premioLabel.iso3c = "",
             flag_URL = "",
             slide_data = "Evolución") %>% 
  cols_align(align = "center") %>% 
  cols_align(align = "left", columns = pais_premioLabel.iso3c) %>% 
  #cols_width(columns = NN_total ~ px(150)) %>% 
  fmt_percent(columns = c(6:12), decimals = 2, scale_values = FALSE) %>%
  #tab_spanner(label = "PREMIOS LITERARIOS",   columns = c(n, n_percent)) %>% 
  cols_width(columns = c(1) ~ px(50))  %>% 
  cols_width(columns = c(2:3) ~ px(90)) %>% 
  cols_width(columns = c(4) ~ px(80)) %>% 
  cols_width(columns = c(5) ~ px(150)) %>% 
  cols_width(columns = c(6:12) ~ px(80)) %>% 
  #fmt_integer(columns = c(n, NN_premiAdos), locale = "es") %>% 
  identity()



tt_4 %>% 
  tab_options(table.font.size = 18, table.width = 300, heading.title.font.size = 28,  column_labels.font.size = 22, row_group.font.size = 40, stub.font.size = 27, footnotes.font.size = 14) %>% 
  #cols_width(columns = rank ~ px(30)) %>% 
  cols_label(rank = "") %>% 
  tab_style(style = list(
    #cell_fill(color = "transparent"), 
    cell_text(style = "italic"), 
    cell_text(size = "large")),
    locations = cells_body(columns = rank, rows = rank >= 1))  %>% 
  cols_align(align = "left", columns = rank) %>% 
  tab_style(style = list(cell_text(size = "xx-large")),
            locations = cells_body(columns = pais_premioLabel.ok))  %>%
  identity()

