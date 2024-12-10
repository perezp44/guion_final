#- pedro.j.perez@uv.es [revisado: 2024-09-04]
#- ejemplo con datos INE (EPA)


#- ejemplo con datos INE (EPA)
#- Ultimos datos EPA:  https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736176918&menu=ultiDatos&idp=1254735976595
#- Hay muchas tablas. anuales, trimestrales, CCAA, provinciales etc ... etc ...

library(tidyverse)

#- grupo de Sonia Monleón 
#- Tasas de actividad, paro y empleo por PROVINCIA y sexo: https://www.ine.es/jaxiT3/Tabla.htm?t=3996&L=0


#- Descargamos la tabla del INE
# my_url <- "https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/3996.csv?nocab=1"
# curl::curl_download(my_url, "./pruebas/epa_prov-genero.csv")

#- DETALLE: los datos q hemos descargados están en formato "csv" (PERO separados por ;)
#- Importamos la tabla al Global
df <- rio::import("./pruebas/epa_prov-genero.csv") #- no los interpreta bien
str(df)



#- antes de hacer nada: los nombres de las variables tienen q estar bien (ser sintácticamente válidos)
names(df) #- están bien pero ... aún así quiero recordar janitor::clean_names()
df <- janitor::clean_names(df) 
names(df)

#- Pb con la tasa de paro (en la v. total) tiene "," en el decimal
df <- df %>% mutate(total = stringr::str_replace(total, "," , "." ))
str(df)
df <- df %>% mutate(total = as.numeric(total)) #- hay un pb en el ultimo dato de Melilla
zz <- df %>% filter(is.na(total)) #- solo para ver cuantos NA's se han creado

#- Veamos q hay en df ----------------------------------------------------
df_dicc <- pjpv.curso.R.2022::pjp_dicc(df)
df_uniques <- pjpv.curso.R.2022::pjp_valores_unicos(df)

rm(df_dicc, df_uniques)


#- antes de empezar hay que separar 2 columnas
#- 1) la v. fecha está como texto ("2022T3") la pasamos a fecha y sacamos año y cuatrimestre
df <- df %>% 
  mutate(fecha = lubridate::yq(periodo)) %>% 
  mutate(anyo = lubridate::year(fecha)) %>% 
  #mutate(mes = lubridate::month(fecha)) %>% #- el mes no tiene sentido (son trimestrales)
  mutate(trimestre = lubridate::quarter(fecha))  %>% 
  select(-periodo)   #- quito lo q no hace falta

head(df)

#- 2) La v "provincias" tiene el código y el nombre
# ademas, en provincias estan las 52 prov y el "Total nacional"
#- arreglarlo (ponerlo similar a las provincias)
df <- df %>% 
  mutate(provincias = ifelse(provincias == "Total Nacional",
                             paste("00", provincias),
                             provincias))
#- para luego separalos
df <- df %>%  
  tidyr::separate(provincias, sep = " ", 
                  into = c("ine_prov", "ine_prov.n"), extra = "merge") 
head(df)
#- veamos que tasas hay
df_wide <- df %>% pivot_wider(names_from = tasas, values_from = total) 

df_wide <- janitor::clean_names(df_wide)
names(df_wide)
df_wide <- df_wide %>% 
  rename(tasa_actividad = tasa_de_actividad) %>% 
  rename(tasa_paro = tasa_de_paro_de_la_poblacion) %>% 
  rename(tasa_empleo = tasa_de_empleo_de_la_poblacion) 

df_wide <- df_wide %>% 
       rename(ine_prov.n = ine_prov_n)  

df_orig <- df_wide    #- AQUI
  
#- PREGUNTAS q podemos hacer:
#- 0) Evolución en el tiempo
#- 1) estacionalidad 
#- 3) comparación provincial


#
#- simplemente voy a hacer un barplot 
#- por ejemplo en 2022 T2

df_p <- df_wide %>% 
  filter(anyo == 2022) %>% 
  filter(trimestre == 3) %>% 
  filter(sexo == "Ambos sexos")

p1 <- ggplot(df_p, aes(y = ine_prov.n, x = tasa_actividad)) + geom_col()
p1

#- reordenamos

df_p <- df_p %>% mutate(ine_prov.n.f = as.factor(ine_prov.n))
df_p <- df_p %>% mutate(ine_prov.n.f = forcats::fct_reorder(ine_prov.n.f, tasa_actividad))

                        
p1 <- ggplot(df_p, aes(y = ine_prov.n.f, x = tasa_actividad)) + geom_col()
p1


#- para mejorar el barplot: https://albert-rapp.de/posts/ggplot2-tips/16_bars_checklist/16_bars_checklist.html        
#- BUMP-plot: https://dominikkoch.github.io/Bump-Chart/
