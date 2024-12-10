#- plot mundo premios literarios
#- https://perezp44.github.io/slides.2023/jornadasR_2023_barcelona/index.html#/4/10


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

library(usefunc) #- devtools::install_github("nrennie/usefunc")

#- datos de premios literarios provenientes de Wikidata
my_url <- "https://raw.githubusercontent.com/perezp44/archivos.download.2023/master/df_unnest_unique.rds"
df_original <- rio::import(my_url)



# load fonts
font_add_google("Roboto Slab", "slab")
font_add_google("Roboto", "roboto")
showtext_auto()


df_unnest_unique <- df_original %>% 
  distinct(personLabel, awardLabel, year_premio, award, person, .keep_all = TRUE)
#- TABLA_01a (premios) ------------------- 
#- tabla de nº de premios y premios-concedidos x países (he quitado los NA's)
df_tt_01a <- df_original %>% 
  distinct(award, awardLabel, .keep_all = TRUE)  %>% #- 3.132 premios (con premiados) 
  tidyr::drop_na(pais_premioLabel.ok) %>%     #- pero quito NA's
  select(award, awardLabel, nn_award_premiados_totales, pais_premioLabel, pais_premioLabel.ok, pais_premioLabel.iso2c,  pais_premioLabel.continente, pais_premioLabel.grupo, pais_premioLabel.iso3c) %>%
  add_count(pais_premioLabel.ok) %>% 
  group_by(pais_premioLabel.ok) %>% 
  mutate(NN_premiAdos = sum(nn_award_premiados_totales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  distinct(pais_premioLabel.ok, n, NN_premiAdos, pais_premioLabel.iso2c, pais_premioLabel.iso3c) %>% 
  mutate(n_total = sum(n, na.rm = TRUE)) %>% 
  mutate(n_percent = n/n_total, .after = n) %>% 
  mutate(NN_premiAdos_total = sum(NN_premiAdos, na.rm = TRUE)) %>% 
  mutate(NN_premiAdos_percent = NN_premiAdos/NN_premiAdos_total, .after = NN_premiAdos) %>% 
  arrange(desc(n)) 




# world map ---------------------
world <- data(World)  
aa <- sf::st_drop_geometry(World)
map_data <- left_join(World, df_tt_01a,  by = c("iso_a3" = "pais_premioLabel.iso3c" )) %>% 
  filter(name != "Antarctica")
#map_data$label = "Nº de premios literarios por país"

# subtitle
st <- "Número de premios literarios por país" 

p <- ggplot(data = map_data) + geom_sf() +   
  #labs(title = "Nº de premios literarios por país", caption = "") +
  NULL
p


p <- p + geom_sf(aes(fill = n)) + scale_fill_viridis_c(option = "plasma")

p <- p + scale_y_continuous(limits = c(-60, 120)) + 
  scale_fill_gradientn(colours = met.brewer("Hokusai2", n = 20) )


#p + scale_fill_gradientn(colours = met.brewer("Hokusai2", n = 20),
# limits = c(0, 1),
# breaks = c(0, 1),
# labels = c("Fewer doctors", "More doctors"))

#pak::pak("nrennie/usefunc")
#- https://github.com/nrennie/tidytuesday/blob/main/2023/2023-01-03/20230103_raw.png
#p +   labs(tag = usefunc::str_wrap_break(st, 120), caption = "N. Rennie | Data: Gapminder") +
#  

p <- p + 
  labs(tag = usefunc::str_wrap_break(st, 120), caption = "Pedro J. Pérez | Datos: Wikidata") +
  # labs(caption = "N. Rennie | Data: Gapminder") +
  guides(fill = guide_colourbar(ticks = FALSE)) +
  theme_void() +
  theme(plot.background = element_rect(fill = "#EADEDA", colour = "#EADEDA"),
        panel.background = element_rect(fill = "#EADEDA", colour = "#EADEDA"),
        strip.text = element_text(family = "slab", lineheight = 0.4, hjust = 0,
                                  colour = "#EADEDA", size = 4, #- 48
                                  margin = margin(t = 10, 
                                                  l = 10, 
                                                  b = 10)),
        strip.background = element_rect(fill = "#1B5681", colour = "#1B5681"),
        plot.tag.position = c(0.02, 0.81),
        plot.tag = element_text(family = "roboto",lineheight = 0.6, size = 10,
                                colour = "#0E3F62", hjust = 0),
        plot.caption = element_text(family = "roboto", lineheight = 0.4,
                                    size = 6, hjust = 0.03,
                                    colour = "#0E3F62"),
        legend.text = element_text(family = "roboto", lineheight = 0.4,
                                   size = 6, hjust = 0.5,
                                   colour = "#0E3F62"),
        legend.key.width = unit(2,"cm"),
        legend.key.height = unit(0.4,"cm"),
        legend.position = c(0.645, 0.005),
        legend.title = element_blank(),
        legend.direction = "horizontal",
        plot.margin = margin(-120, 0, 10, 0))
p
#ggplot2::ggsave(plot = p, filename = "./tablas_ok/plot_01_premios-x-paises.png")