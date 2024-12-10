#- Schools shooting: https://twitter.com/paldhous/status/1190022664808783872  (Peter Aldhous)

#-------------------
library(tidyverse)
library(scales)
library(sf)

# load data --------------------------------------------------------------------
url_1 <- "https://github.com/BuzzFeedNews/2019-10-social-sentinel/blob/master/data/school_purchases.RData?raw=true"
school_purchases <- rio::import(url_1)
url_2 <- "https://github.com/BuzzFeedNews/2019-10-social-sentinel/blob/master/data/recent_school_shootings.RData?raw=true"
recent_school_shootings <- rio::import(url_2)


#- 
# total spending per month
school_purchases_month <- school_purchases %>%
  group_by(year, month) %>%
  summarize(total = sum(price, na.rm = TRUE)) %>%
  mutate(date = as.Date(paste0(year,"-",month,"-01")))

#- Peter Aldhous pone todo el código junto, nosotros lo vamos a trocear --------
options(scipen = 999) #- quitar notación científica

p <- ggplot(school_purchases_month, aes(x = date, y = total)) +
  geom_col(fill = "black", color = "white") 
p

p <- p + geom_hline(yintercept = 600000, size = 0.3, color = "red") 
p

#- à la R-base: subset()
p <- p + geom_point(data = subset(recent_school_shootings, killed > 0), 
             aes(x = date, y = 600000, size = killed),
             color = "red",
             alpha = 0.4) 
p

#- à la tidyverse:  filter()
p <- p + geom_point(data = recent_school_shootings %>% filter(killed > 0), 
                    aes(x = date, y = 600000, size = killed),
                    color = "red",
                    alpha = 0.4) 
p


p <- p +  theme_minimal(base_size = 14, base_family = "Basier Square SemiBold") 
p

p <- p + 
  scale_x_date(date_breaks = "1 year", 
               date_labels = "%b %Y", 
               limits = c(as.Date("2013-10-01"), as.Date("2019-12-01")),
               expand = c(0, 0)) 
p

p <- p + scale_y_continuous(labels = dollar), 
                     breaks = c(200000,400000), 
                     limits = c(0,650000), 
                     expand = c(0, 0)) 
p

p <- p + scale_size_area(max_size = 12, guide = "none") 
p

p <- p + geom_hline(yintercept = 0, size = 0.3) +
         geom_hline(yintercept = c(200000, 400000), size = 0.05) +
         geom_vline(xintercept = as.Date("2018-02-14"), size = 0.5, color = "red", linetype = "dotted") +
         geom_vline(xintercept = as.Date("2018-05-18"), size = 0.5, color = "red", linetype = "dotted") 
p

p <- p + xlab("") +
         ylab("Spending per month") 
p


p + theme(panel.grid = element_blank(),
        axis.ticks.x = element_line()) 
p


p <- p + annotate("text", x = as.Date("2018-01-01"), 
           y = 480000, 
           angle = 90, 
           label = "Parkland",
           family = "Basier Square SemiBold",
           size = 4,
           color = "red") +
  annotate("text", x = as.Date("2018-04-10"), 
           y = 480000, 
           angle = 90, 
           label = "Santa Fe",
           family = "Basier Square SemiBold",
           size = 4,
           color = "red") +
  annotate("text", x = as.Date("2015-01-01"), 
           y = 540000, 
           label = "Killed in school shootings",
           family = "Basier Square SemiBold",
           size = 4,
           color = "red")

p

#- mapita ----------------------------------------------------------------------
#- no sale xq hay q pedir una census API key

library(tidycensus)
# labels for Parkland and Santa Fe
recent_school_shootings <- recent_school_shootings %>%
  mutate(label = case_when(grepl("Parkland|Santa Fe", city) ~ city,
                           TRUE ~ NA_character_))
# get states basemap from Census Bureau
states <- get_acs(geography = "state", variables = "B19013_001", 
                  shift_geo = TRUE, geometry = TRUE)
# map
ggplot(states) +
  geom_sf(color = "white", size = 0.3, fill = "#cccccc") +
  geom_sf(data = subset(recent_school_shootings, killed > 0),
          aes(size = killed),
          color = "red",
          alpha = 0.4) +
  geom_sf_text(data = subset(recent_school_shootings, killed > 0),
               aes(label = label),
               color = "red",
               nudge_x = 300000,
               nudge_y = -200000,
               family = "Basier Square SemiBold") +
  scale_size_area(max_size = 12, guide = FALSE) +
  theme_void(base_family = "Basier Square SemiBold", base_size = 14)
