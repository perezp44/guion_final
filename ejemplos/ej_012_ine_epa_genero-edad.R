#- pedro.j.perez@uv.es [revisado: 2024-09-04]
#- ejemplo con datos INE (EPA)
#- el ejemplo tiene 2 partes: la primera se centra en manejar/arreglar datos, y la segunda hacemos mapas

#- Datos: últimos datos EPA (Encuesta de población activa)
#- url: https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736176918&menu=ultiDatos&idp=1254735976595
#- Hay muchas tablas. anuales, trimestrales, CCAA, provinciales etc ... etc ...


#- usaremos tablas de Rtdos Nacionales - Anuales ; concretamente aquí:
#- https://www.ine.es/dynt3/inebase/es/index.htm?padre=811&capsel=815

#- concretamente esta tabla:
#- 1) Tasas de paro por sexo y grupo de edad: https://www.ine.es/jaxiT3/Tabla.htm?t=4887&L=0
#- se puede descargar en varios formatos: .xlsx .csv .json, .px

library(tidyverse)
#- Descargamos la tabla del INE
my_url <- "https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/4887.csv?nocab=1"
fecha_hoy <- Sys.Date()
my_destino <- paste0("./datos/epa_genero-edad", "_", fecha_hoy, ".csv")
curl::curl_download(my_url, my_destino)

#- DETALLE: los datos q hemos descargados están en formato "csv" (PERO separados por ;)
#- Importamos la tabla al Global
df <- rio::import(my_destino) #- rio::import() funciona!!
str(df) #- PERO realmente funciona??

#- antes de hacer nada: los nombres de las variables tienen q estar bien (ser sintácticamente válidos)
names(df) #- están bien pero ... aún así quiero recordar janitor::clean_names()
df <- janitor::clean_names(df)
names(df)

#- datos básicos del df (tipo de variables)
str(df) #- PERO realmente los hemos leído bien??? la inflación es "character"

#- PB: la v. total (q tiene la tasa de paro) es character: tenemos q pasarlo a numeric
df <- df %>% mutate(total.n = as.numeric(total)) #- no lo podemos hacer así
head(df)  #- no se pueden pasar directamente a numeric

#- varias posibilidades
#- 1) Leerlos  con readr::read_delim()
df <- readr::read_delim(file = my_destino,
                        delim = ";",
                        locale = locale(decimal_mark = ","))

#- otra forma: creo que puede ser más útil para vosotros: con la ff. stringr::str_replace()
#- este tipo de funciones creo que os puede venir bien
df <- rio::import(my_destino)
df <- janitor::clean_names(df)

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
#- 0) Evolución en el tiempo
#- 1) Brecha de genero?
#- 2) efecto de la edad, jóvenes? mayores de 50?
#- 3) CC.AA con más paro (nos haría falta otra tabla)
#- 4) por nivel de estudio (nos haría falta otra tabla)


#- seguramente querremos reordenar los grupos de edad (así q factores)
df <- df %>% mutate(edad.f = as_factor(edad), .after = edad)
levels(df$edad.f)
df <- df %>%  mutate(edad.f = forcats::fct_relevel(edad.f, "Total", after = Inf))
levels(df$edad.f)


df_orig <- df   #- AQUI

#- 1) Brecha de genero ---------------------------------------------------------
#- 1.1: sin diferenciar por edad------------------------------------------------
df <- df_orig %>% filter(sexo != "Ambos sexos") #- dejo solo H y M

df_tot <- df %>% filter(edad == "Total") %>% #- no diferencio x edad
             select(-edad)

df_tot_wide <- df_tot %>%
  pivot_wider(names_from = sexo, values_from = tasa_paro) %>%
  mutate(dif = Mujeres - Hombres)


p1 <- ggplot(data = df_tot,
             aes(x = periodo, y = tasa_paro, color = sexo)) +
        geom_line()
p1


#- 1.2: por grupos de edad -----------------------------------------------------
df <- df_orig %>% filter(sexo != "Ambos sexos") #- dejo solo H y M

df_edad <- df %>% filter(edad != "Total")  #- quito el total de edad

df_edad_wide <- df_edad %>%
  pivot_wider(names_from = sexo, values_from = tasa_paro) %>%
  mutate(dif = Mujeres - Hombres)


#- evolución del paro por grupos de edad
p2 <- ggplot(data = df_edad,
             aes(x = periodo, y = tasa_paro, color = sexo)) +
      geom_line()
p2

p2 <- p2 + facet_wrap(vars(edad))
p2

#- creo que mejor poner [70 y mas] al final (así q necesitamos el factor)
p2 <- p2 + facet_wrap(vars(edad.f))
p2


#- gráfico un poco pillado por los pelos, pero ...
p22 <-  ggplot(data = df_edad, aes(x = edad.f, y = tasa_paro, color = sexo)) + geom_point()
p22

p22 <- p22 + facet_wrap(vars(periodo))
p22




#- 2) EDAD, los jóvenes? los mayores de 50? ------------------------------------
df <- df_orig %>% filter(sexo == "Ambos sexos")  %>% #- me olvido del genero
     select(-sexo)

p1 <- ggplot(data = df, aes(x = periodo, y = tasa_paro, color = edad.f)) + geom_line()
p1

#- solo 2021 ---
zz <- df %>% filter(periodo == 2021) #- solo 2021
p2 <- ggplot(data = zz, aes(x = tasa_paro, y = edad.f)) + geom_col()
p2
#- si quisiéramos ordenar por los grupos de dad en tasa descendente
zz <- zz %>% mutate(edad.ff = forcats::fct_reorder(edad.f, tasa_paro))
p2 <- ggplot(data = zz, aes(x = tasa_paro, y = edad.ff)) + geom_col()
p2


#- volvemos a usar df (todos los periodos) (ya cuesta ver el gráfico)
df <- df %>% mutate(edad.ff = forcats::fct_reorder(edad.f, tasa_paro))
p2 <- ggplot(data = df, aes(x = tasa_paro, y = edad.ff)) + geom_col()
p2 + facet_wrap(vars(periodo))

