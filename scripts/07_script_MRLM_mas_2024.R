#- script para clase_13: algunas cosas sobre  EDA y modelos
#- más cosas en el tutorial
#- https://yards.albert-rapp.de/lin-mods
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



#- veamos un poco más q hay dentro de mod_1
mod_1 <- lm(peso_bebe ~ parto_nn_semanas, data = df_m)
summary(mod_1)

(zz_betas     <- mod_1[[1]])

(zz_residuals <- mod_1[[2]])
(zz_residuals <- mod_1[["residuals"]])
(zz_residuals <- mod_1$residuals)
 
(zz_betas_1 <- mod_1[[1]])      #- [[ ]] doble corchete
(zz_betas_2 <- mod_1[1] )         #- []    corchete simple
 
rm(list = ls(pattern = "^zz"))  #- borra los objetos cuyo nombre empiece por zz

#- otra vez [, [[ y $
zz_betas_1a <- mod_1[["coefficients"]]   #- doble corchete
zz_betas_1b <- mod_1$coefficients        #- $
zz_betas_1c <- mod_1["coefficients"]     #- single corchete

rm(list = ls(pattern = "^zz"))  #- borra los objetos cuyo nombre empiece por zz

#-
mod_1 <- lm(peso_bebe ~ parto_nn_semanas + edad_madre.1 + sexo_bebe.f , data = df_m)
 
summary(mod_1)                   #- tabla resumen
summary(mod_1)$coefficients      #- tabla resumen con los coeficientes
coefficients(mod_1)              #- coeficientes estimados
confint(mod_1, level = 0.95)     #- Intervalos de confianza para los coeficientes
fitted(mod_1)                    #- predicciones (y-sombrero, y-hat)
residuals(mod_1)                 #- vector de residuos
model.matrix(mod_1)              #- extract the model matrix
anova(mod_1)                     #- ANOVA
vcov(mod_1)                      #- matriz de var-cov para los coeficientes
influence(mod_1)                #- regression diagnostics
# diagnostic plots
layout(matrix(c(1, 2, 3, 4), 2, 2))   #- optional 4 graphs/page
plot(mod_1)                      #-
library(ggfortify)
ggplot2::autoplot(mod_1, which = 1:6, ncol = 2, colour = "steelblue")

#-
GGally::ggcoef(mod_1)


#- Predicciones con predict() --------------------------------------------------
nuevas_observaciones <- df_m %>% slice(c(3, 44, 444))

stats::predict(mod_1, newdata = nuevas_observaciones)  #- predicciones puntuales
stats::predict(mod_1, newdata = nuevas_observaciones, type = 'response', se.fit = TRUE)  #- tb errores estándar  predictions
predict(mod_1, newdata = nuevas_observaciones, interval = "confidence")  #- intervalo (para el valor esperado)
predict(mod_1, newdata = nuevas_observaciones, interval = "prediction")  #- intervalo (para valores individuales)



#- errores estándar robustos :  paquete estimatr.
#- install.packages("emmeans")
mod_1 <- lm(peso_bebe ~ parto_nn_semanas, data = df_m)
summary(mod_1)
mod_1_ee <- estimatr::lm_robust(peso_bebe ~ parto_nn_semanas, data = df_m)
summary(mod_1_ee)


#- BROOM pkg -------------------------------------------------------------------
#- 3 funciones útiles: tidy() ,  glance()  y  augment()  

#- tidy()
zz <- broom::tidy(mod_1, conf.int = TRUE)
zz %>% pjpv.curso.R.2022::pjp_df_decimales(nn = 2)  %>% gt::gt()


#- glance()
broom::glance(mod_1)
broom::glance(mod_1) %>% select(adj.r.squared, AIC)


#- augment(): añade residuos, predicciones etc ....
zz <- broom::augment(mod_1)


#- un ejemplo: seleccionar coeficientes significativos
mod_1 %>% broom::tidy() %>% filter(p.value < 0.05)


#- recordamos ggplot2
ggplot(data = df_m, 
       mapping = aes(x = edad_madre.1,
                     y = peso_bebe,  color = estudios_madre.ff)) +
      geom_point(alpha = 0.1) +  
      geom_smooth(method = "lm")

ggplot(data = df_m, 
       mapping = aes(x = edad_madre.1, 
                     y = peso_bebe,  color = sexo_bebe.f)) +
      geom_point(alpha = 0.1) +  
      geom_smooth(method = "lm")

ggplot(data = df_m, 
       mapping = aes(x = edad_madre.1, 
                     y = peso_bebe,  color = sexo_bebe.f)) +
      geom_point(alpha = 0.1) +  
      geom_smooth()


#- ejmeplo de uso de broom
mod_1 <- lm(peso_bebe ~ edad_madre.1 + sexo_bebe.f , data = df_m)
summary(mod_1)

td_mod_1 <- mod_1 %>% broom::augment(data = df_m) 

td_mod_1 %>% 
  ggplot(mapping = aes(x = edad_madre.1, 
                       y = peso_bebe, color = sexo_bebe.f)) +
                geom_point(alpha = 0.1) +
                geom_line(aes(y = .fitted, group = sexo_bebe.f))


#
td_mod_1 %>% 
  ggplot(mapping = aes(x = edad_madre.1, 
                       y = .fitted,  color = sexo_bebe.f)) +
                geom_line(aes(group = sexo_bebe.f)) 


#- !!!!
library(broom)
df_m %>% group_by(sexo_bebe.f) %>% do(tidy(lm(peso_bebe ~ parto_nn_semanas, .)))


#- graficando intervalos para los coeficientes
td_mod_1 <- tidy(mod_1, conf.int = TRUE)
ggplot(td_mod_1, aes(estimate, term, color = term)) +
    geom_point() +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
    geom_vline(xintercept = 0)


#- 
mod_1 %>% augment(data = df_m) %>%
 ggplot(mapping = aes(x = parto_nn_semanas, 
                      y = .fitted, color = sexo_bebe.f)) +
   geom_point(mapping = aes(y = peso_bebe), alpha = 0.1) +
   geom_line()


#-
mod_1 %>% augment(data = df_m) %>%
 ggplot(mapping = aes(x = parto_nn_semanas, 
                      y = .fitted, color = sexo_bebe.f)) +
   geom_point(mapping = aes(y = peso_bebe), alpha = 0.1) +
   geom_line() +
  facet_wrap(vars(estudios_madre.ff))


#
mod_1 <- lm(peso_bebe ~ parto_nn_semanas + edad_madre.1,  data = df_m)
mod_2 <- lm(peso_bebe ~ parto_nn_semanas + edad_madre.1 + I(parto_nn_semanas*edad_madre.1), data = df_m)
summary(mod_1)
anova(mod_1, mod_2)


#-
AIC(mod_1, mod_2)


#
lmtest::lrtest(mod_1, mod_2)    


#- ordenamos modelos en f. del AIC (!!!!)
modelos <- list(
  mod_1 <- lm(peso_bebe ~ parto_nn_semanas + edad_madre.1,  data = df_m),
  mod_2 <- lm(peso_bebe ~ parto_nn_semanas + edad_madre.1 + I(parto_nn_semanas*edad_madre.1), data = df_m)  )

modelos_ordered_AIC <- purrr::map_df(modelos, broom::glance, .id = "model") %>% arrange(AIC)

modelos_ordered_AIC %>% gt::gt()


#- una tabla con broom (!!!!)
 
my_kable <- function(df){gt::gt(mutate_if(df, is.numeric, round, 2))}

broom::tidy(mod_1) %>% my_kable


#- el paquete modelr
zz <- df_m %>% modelr::gather_predictions(mod_1, mod_2)

zz1 <- pivot_wider(zz, names_from = model, values_from = pred)


#- modelos GLM -----------------------------------------------------------------
mod_logit <- glm(parto_cesarea.f ~ peso_bebe + parto_nn_semanas + edad_madre.1 + estudios_madre.ff, family = binomial(link = "logit"), data = df_m)

summary(mod_logit)


#- calculo de efectos marginales
mod_1_AME <- mod_logit %>% margins::margins() %>% summary() 
mod_1_AME

#- paquete margins para emular tablas de Stata
mod_logit %>% margins::margins(at = list(parto_nn_semanas = c(25, 35), estudios_madre.ff = c("Primarios", "Medios", "Universidad")),  variables = "edad_madre.1" )


#
margins::cplot(mod_logit, x = "estudios_madre.ff", dx = "edad_madre.1", what = "effect", drop = TRUE)

margins::cplot(mod_logit, x = "estudios_madre.ff", dx = "edad_madre.1")

