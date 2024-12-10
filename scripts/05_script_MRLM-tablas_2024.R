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


#- estimamos varios modelos

m1 <- lm(peso_bebe ~ 
           parto_nn_semanas + sexo_bebe.f + edad_madre.1 , 
         data = df_m)
m2 <- lm(peso_bebe ~ 
           parto_nn_semanas + sexo_bebe.f + edad_madre.1 + edad_padre.1 + estudios_madre.ff , 
         data = df)

#- Tablas para modelos ---------------------------------------------------------

#- con pkg stargazer
stargazer::stargazer(m1, type = "html")
stargazer::stargazer(m1, type = "text")


#- con sjPlot package
sjPlot::tab_model(m1)
sjPlot::plot_model(m1, sort.est = TRUE)
sjPlot::tab_model(m1, m2)



#- con modelsummary
#remotes::install_github('vincentarelbundock/modelsummary')
library(modelsummary)

#- vamos a estimar un logit
df_m <- df_m %>% 
  mutate(peso_bebe.f = ifelse(peso_bebe > mean(peso_bebe, na.rm = TRUE), 1, 0 ))

m3 <- glm(peso_bebe.f ~ parto_nn_semanas + sexo_bebe.f , data = df_m, family = binomial(link = "logit"))

mys_modelitos <- list()
mys_modelitos[["peso_bebe:  OLS 1"]]     <- m1
mys_modelitos[["peso_bebe:  OLS 2"]]     <- m2
mys_modelitos[["peso_bebe.f: Logit 1"]]  <- m3

mm <- msummary(mys_modelitos, title = "Resultados de estimación")
mm


#- tables like SPSS -------
#- https://cran.r-project.org/web/packages/expss/vignettes/tables-with-labels.html

#- tablas para modelos: https://cran.r-project.org/web/packages/sjPlot/vignettes/tab_model_estimates.html
#- https://www.youtube.com/watch?app=desktop&v=YQ2Dd3CSa0U


#- showcase reactable: https://glin.github.io/reactable/articles/popular-movies/popular-movies.html
#- https://glin.github.io/reactable/






#
## library(report) #- devtools::install_github("neuropsychology/report")
## my_model <- lm(peso_bebe ~ parto_nn_semanas + sexo_bebe.f, df_m)
## rr <- report(my_model, target = 1)
## rr


#
## report::as.report(rr)


#
## library(equatiomatic)  #- remotes::install_github("datalorax/equatiomatic")
## extract_eq(mod_1)
## extract_eq(mod_1, use_coefs = TRUE)



#- cocoon pkg
#- https://jeffreyrstevens.github.io/cocoon/articles/cocoon.html
