#- pedro.j.perez@uv.es [revisado: 2024-09-04]
#- ejemplo con datos INE (EPA)
#- el ejemplo tiene 2 partes: la primera se centra en manejar/arreglar datos, y la segunda hacemos gráficos para visualizar los datos

#- Datos: últimos datos EPA (Encuesta de población activa)
#- url: https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736176918&menu=ultiDatos&idp=1254735976595
#- Hay muchas tablas. anuales, trimestrales, CCAA, provinciales etc ... etc ...


#- usaremos tablas de Rtdos Nacionales - Anuales ; concretamente aquí:
#- https://www.ine.es/dynt3/inebase/es/index.htm?padre=811&capsel=815
#- 1)  Tasas de paro por nivel de formación alcanzado, sexo y grupo de edad: https://www.ine.es/jaxiT3/Tabla.htm?t=9449&L=0
#- esta tabla se puede descargar en varios formatos: .xlsx .csv .json, .px


library(tidyverse)

#- Descargamos la tabla del INE --------------------
#- esta tabla se puede descargar en varios formatos: .xlsx .csv .json, .px

#- por ejemplo la descargamos en formato .xlsx:
my_url <- "https://www.ine.es/jaxiT3/files/t/es/xlsx/66000.xlsx?nocab=1"
my_destino <- "./pruebas/epa_tab_1.xlsx"
curl::curl_download(my_url, my_destino)

df <- rio::import(my_destino)

#- casi siempre es mucho mejor descargar las tablas INE en formato "pcaxis" (.px)
#- salen ya decentes y en formato largo
#- install.package("pxR")
my_url <- "https://www.ine.es/jaxiT3/files/t/es/px/66000.px?nocab=1"
my_destino <- "./pruebas/epa_tab_1.px"
curl::curl_download(my_url, my_destino)

df <- pxR::read.px(my_destino) %>% as_tibble()









#- aun así vamos a descargar y trabajar la tabla en .csv ----------------------


my_url <- "https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/66000.csv?nocab=1"
fecha_hoy <- Sys.Date()
my_destino <- paste0("./datos/epa_educ-genero-edad", "_", fecha_hoy, ".csv")
curl::curl_download(my_url, my_destino)   #- capado

#- DETALLE: los datos q hemos descargados están en formato "csv" (PERO separados por ;)
#- Importamos la tabla al Global
df <- rio::import(my_destino) #- rio::import() funciona!!
str(df) #- PERO realmente funciona??


#- antes de hacer nada: los nombres de las variables tienen q estar bien (ser sintácticamente válidos)
names(df) #- están bastante bien pero ... aún así quiero recordar janitor::clean_names()
df <- janitor::clean_names(df)
names(df)

#- Pb con la tasa de paro (en la v. total) tiene "," en el decimal
df <- df %>% mutate(total = stringr::str_replace(total, "," , "." ))
str(df)
df <- df %>% mutate(total = as.numeric(total))

#- OK, ya tenemos los datos correctamente leídos/interpretados
df <- df %>% rename(tasa_paro = total)

#- Veamos q hay en df ----------------------------------------------------
df_dicc <- pjpv.curso.R.2022::pjp_dicc(df)
df_uniques <- pjpv.curso.R.2022::pjp_valores_unicos(df)

rm(df_dicc, df_uniques)


#- PREGUNTAS q podemos hacer:
#- 1) Evolución en el tiempo (por nivel educativo) afectan las crisis de igual manera
#- 2) Brecha de genero? (igual x niveles de estudio)

#- antes vamos a recordar como manipular factores (!!)
#- seguramente querremos reordenar los grupos de edad (así q factores)
df <- df %>% mutate(edad.f = as_factor(edad), .after = edad)
levels(df$edad.f)
df <- df %>%  mutate(edad.f = forcats::fct_relevel(edad.f, "Total", after = Inf))
levels(df$edad.f)


#- seguramente querremos reordenar por nivel educativo (así q factores)
df <- df %>% rename(educacion = nivel_de_formacion_alcanzado)
df <- df %>% mutate(educacion.f = as_factor(educacion), .after = educacion)
levels(df$educacion.f)
df <- df %>%  mutate(educacion.f = forcats::fct_relevel(educacion.f, "Total", after = Inf))
levels(df$educacion.f)


#- si queremos quitar algún level de un factor
zz <- df %>% filter(educacion.f != "Estudios primarios incompletos")
levels(zz$educacion.f) #- los levels no se van , hay que quitarlos
zz <- zz %>% mutate(educacion.f = forcats::fct_drop(educacion.f, only = "Estudios primarios incompletos"))
levels(zz$educacion.f)


#- Hay muchos niveles educativos, podríamos seleccionar solo algunas categorías, pero ....
df_orig <- df   #- AQUI


#- 1) Evolución en el t (afectan igual las crisis por nivel educativo??)
df <- df_orig %>%
  filter(edad == "Total") %>%
  filter(sexo == "Ambos sexos") %>%
  select(-sexo, -edad, -edad.f, -educacion)


df_wide <- df %>%
  pivot_wider(names_from = educacion.f, values_from = tasa_paro)

p1 <- ggplot(data = df, aes(x = periodo, y = tasa_paro, color = educacion.f)) +
  geom_line()

p1
p1 + theme(legend.position = "bottom")


#- 2) Brecha de genero? (igual tasa x niveles de estudio)
df <- df_orig  %>%
  filter(sexo != "Ambos sexos") %>% #- dejo solo H y M
  filter(edad == "Total") %>%       #- no diferencio por edad
  select(-edad, -edad.f)

p1 <- ggplot(data = df, aes(x = periodo, y = tasa_paro,  color = sexo)) + geom_line()
p1
p1 + facet_wrap(vars(educacion.f))

