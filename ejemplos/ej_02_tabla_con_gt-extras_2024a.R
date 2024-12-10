#- un ejemplo con pkgs gt y gtExtras
#- @thomas_mock: Saw a beautiful table at: https://www.bloomberg.com/graphics/2021-german-election-results/
#- https://twitter.com/thomas_mock/status/1442541043019390982
#- gist: Code: https://gist.github.com/jthomasmock/b6fd5d64842296de2d1ef9cad6769f2c

library(tidyverse)
options(scipen = 999) #- para quitar la notación científica

#----------------------------------------------------

library(gt)
library(gtExtras)  #- remotes::install_github("jthomasmock/gtExtras")

# original source: https://www.bloomberg.com/graphics/2021-german-election-results/

party_df <- tibble(
  Party = c("SPD", "CDU/CSU", "Greens", "FDP", "AfD", "Left", "Other"),
  Seats = c(206, 196, 118, 92, 83, 39, 1),
  `% of 2nd Votes` = c(25.7, 24.1, 14.8, 11.5, 10.3, 4.9, 8.7)
)

minimal_table <- gt(party_df) %>% 
  gtExtras::gt_plt_dot(column = Seats, category_column = Party,  max_value = 379,
             palette = c("#ec323f", "black", "#63d64a", "#fff24e", "#4fabf7", "#e956ad", "grey")) %>% 
  gtExtras::gt_theme_nytimes() %>% 
  tab_header(title = "Results by Party in the Bundestag Election",
             subtitle = "Seats and votes are based on provisional official results.") %>% 
  cols_width(Party ~ px(368), 3 ~ px(30))

minimal_table



#- Ejercicio: ------------------------------------------------------------------
#- ¿adaptamos el ejemplo a nuestros datos?
my_url <- "https://raw.githubusercontent.com/perezp44/iris_data/master/data/PIAAC_data_small.csv"
df <- readr::read_csv(my_url)


my_df <- df %>% 
  mutate(NN_total = n()) %>% 
  group_by(Country) %>% 
  mutate(NN_pais = n()) %>% ungroup() %>% 
  distinct(Country, NN_total, NN_pais) %>% 
  mutate(Percent = NN_pais/NN_total) %>% 
  select(-NN_total) %>% 
  arrange(desc(NN_pais))



my_column <- "NN_pais"
my_category_column <- "Country"
my_max_value = max(my_df$NN_pais)


minimal_table <- gt(my_df) %>% 
  gtExtras::gt_plt_dot(column = my_column, category_column = my_category_column,  max_value = my_max_value,
                       palette = c("#ec323f", "black", "#63d64a", "#fff24e", "#4fabf7", "#e956ad", "grey")) %>% 
  gtExtras::gt_theme_nytimes() %>% 
  tab_header(title = "PIACC data",
             subtitle = "Nº de encuestados por país") %>% 
  cols_width(Country ~ px(368), 3 ~ px(30))

minimal_table

