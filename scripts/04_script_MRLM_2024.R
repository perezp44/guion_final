#- script para clase_13: algunas cosas sobre  EDA y modelos
#- más cosas en el tutorial
library(tidyverse)
options(scipen = 999) #- quitar notación científica

#- DATOS (de bebes) ------------------------------------------------------------
url_bebes <- "https://github.com/perezp44/archivos_download.2/raw/main/df_bebes_EDA.rds"
df_orig <- rio::import(url_bebes)

#- una primera mirada a los datos
df_aa <- pjpv.curso.R.2022::pjp_dicc(df_orig)
#- valores únicos de las variables
df_bb <- pjpv.curso.R.2022::pjp_valores_unicos(df_orig)
df_cc <- pjpv.curso.R.2022::pjp_unique_values(df_orig)


#- vamos a seleccionar una muestra reducida; p.ej 1000 bebes (para ir rápido)
set.seed(1234) #- for reproducibility, para q nos salgan a todos mismos rtdos
df <- df_orig %>% slice_sample(n = 1000)

#- no quitar -------------------------------------------------------------------
no_quitar <- c("df_orig", "df", "df_aa")
rm(list=ls()[! ls() %in% no_quitar])




#- MODELOS ---------------------------------------------------------------------
df_m <- df %>% select(peso_bebe, parto_nn_semanas, sexo_bebe.f, parto_cesarea.f, edad_madre.1, estudios_madre.ff)
#df_m <- df_m %>% drop_na()


#- frente a todas las v. del df
mod_1 <- lm(peso_bebe ~ . , data = df_m)
summary(mod_1)

#- especificamos nosotros los regresores
mod_1 <- df_m %>% lm(peso_bebe ~ parto_nn_semanas + edad_madre.1 + sexo_bebe.f , data = .)
summary(mod_1)      

#- cual es la categoría de referencia para las dummies?
levels(df$sexo_bebe.f)

#- podemos cambiar la categoria de referencia
zz <- forcats::fct_relevel(df$sexo_bebe.f, "bebito")
levels(zz)


#- los modelos se especifican con formulas Y ~ X's
#- operadores dentro de las formulas
#- Y  ~ x1 + x2       significa que Y = f(cte, x1, x2)
#- Y  ~ x1 + x1:x2    significa que Y = f(cte, x1, x1*x2)
#- Y  ~ x1*x2         significa que Y = f(cte, x1, x2, x1*x2)


#- mas modelos -----------------------------------------------------------------
df_m <- df_m %>% tidyr::drop_na()

#- log's
mod_1 <- lm(log(peso_bebe) ~ log(parto_nn_semanas) + sexo_bebe.f , data = df_m)
summary(mod_1)

#- intearciones entre los regresores (:)
mod_1 <- lm(peso_bebe ~ parto_nn_semanas + parto_nn_semanas:edad_madre.1, data = df_m)
summary(mod_1)

#- casi mejor con I()
mod_1 <- lm(peso_bebe ~ parto_nn_semanas + I(parto_nn_semanas*edad_madre.1), data = df_m)
summary(mod_1)

#- "cuidado" con usar directamente * (* en las formulas significa otra cosa)
#- Y*W introduce en el modelo Y, W e (Y*W)
mod_1 <- lm(peso_bebe ~  parto_nn_semanas*edad_madre.1, data = df_m)
summary(mod_1)


#- again * (introduce dummies aditivas y multiplicativas)
mod_1 <- lm(peso_bebe ~ parto_nn_semanas*estudios_madre.ff, data = df_m)
summary(mod_1)


#- ahora quiero un término cuadrático (funciona)
mod_1 <- lm(peso_bebe ~ parto_nn_semanas + I(parto_nn_semanas*parto_nn_semanas), data = df_m)
summary(mod_1)

#- no chuta(por qué??)
mod_1 <- lm(peso_bebe ~ parto_nn_semanas + parto_nn_semanas*parto_nn_semanas, data = df_m)
summary(mod_1)

#- uso de poly()
mod_1 <- lm(peso_bebe ~ poly(parto_nn_semanas, degree = 3), data = df_m)
summary(mod_1)

#- estimamos un Logit (con glm())
mod_1 <- glm(parto_cesarea.f ~ parto_nn_semanas + peso_bebe + sexo_bebe.f , df, family = binomial())
summary(mod_1)



#- Predicciones con predict() --------------------------------------------------
mod_1 <- lm(peso_bebe ~  parto_nn_semanas*edad_madre.1, data = df_m)
summary(mod_1)

nuevas_observaciones <- df_m %>% slice(c(3, 44, 444))

stats::predict(mod_1, newdata = nuevas_observaciones)  #- predicciones puntuales
stats::predict(mod_1, newdata = nuevas_observaciones, type = 'response', se.fit = TRUE)  #- tb errores estándar  predictions
predict(mod_1, newdata = nuevas_observaciones, interval = "confidence")  #- intervalo (para el valor esperado)
predict(mod_1, newdata = nuevas_observaciones, interval = "prediction")  #- intervalo (para valores individuales)

