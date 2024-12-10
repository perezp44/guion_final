#- script para clase_13: algunas cosas sobre  EDA y modelos
#- algunas cosas nos saltaremos
#- más cosas en el tutorial
#- y aquí: https://cosimameyer.com/post/exploratory-data-analysis-in-r/
#- janitor: https://albert-rapp.de/posts/07_janitor_showcase/07_janitor_showcase.html

#- antes de empezar: https://mastodon.social/@oberman/111504988205468366
#- https://nrennie.rbind.io/blog/2022-12-03-how-to-make-your-own-rstats-wrapped/

#- emepzamos
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


#- podemos hacer una tabla interactiva con DT o reactable
DT::datatable(df_aa, 
              filter = 'top', extensions = "Scroller", 
              options = list(autoWidth = TRUE, deferRender = TRUE, scroller = TRUE, scrollY = 450 ))

reactable::reactable(df_aa, 
                     pagination = FALSE, height = 600, filterable = TRUE, 
                     searchable = TRUE, highlight = TRUE)


#- vamos a seleccionar una muestra reducida; p.ej 1000 bebes (para ir rápido)
set.seed(1234) #- for reproducibility, para q nos salgan a todos mismos rtdos
df <- df_orig %>% slice_sample(n = 1000)

#- no quitar -------------------------------------------------------------------
no_quitar <- c("df_orig", "df", "df_aa")
rm(list=ls()[! ls() %in% no_quitar])


#- nombres: manejar y arreglar -------------------------------------------------
df_xx <- df %>% dplyr::rename(nuevo_nombre = fecha_parto)

names(df_xx)[1] <- "nuevo_nombre" #- cambio el primer nombre con R-base
names(df_xx)

names(df_xx) <- 1:15  #- !!las v. se llamaran 1,2,3 ....
names(df_xx)

#- opcion muy recomendable para limpieza inicial de los nombres de las v.
df_xx <- janitor::clean_names(df_xx)
names(df_xx)

rm(df_xx)

#- estructura y tipos de vv. ---------------------------------------------------
str(df)            #- str() muestra la estructura interna de un objeto R
inspectdf::inspect_types(df)   #- muestra de que tipo son las variables


str(df[1])         #- fecha_parto es de clase POSIXct

df[[1]][4]         #- la cuarta observación de la primera v (fecha_parto)

unclass(df[[1]][4])   #- para ver como se almacenan las fechas

#- si te interesa el tema de los husos horarios este vídeo es chulo: https://www.youtube.com/watch?v=-5wpm-gesOY





#- INFORMES DESCRIPTIVOS -------------------------------------------------------
#- Hay pkgs q hacen informes descriptivos completos 

#- summarytools pkg
zz <- summarytools::dfSummary(df) #- genera un fichero/df con un resumen útil y agradable de ver 
summarytools::view(zz)            #- para visualizar el informe

#- skimr pkg
skimr::skim(df)


#- gtExtras pkg
iris %>% gtExtras::gt_plt_summary()


# #- DataExplorer pkg
# # - informe sin especificar variable objetivo
# DataExplorer::create_report(df, 
#                             output_dir = "pruebas",
#                             output_file = "my_report_DataExplorer.html")
# #- con dlookr package
# dlookr::diagnose_paged_report(df, 
#                               output_dir = "pruebas",
#                               output_file = "my_report_dlookr.pdf")
# #- con SmartEDA pkg
# SmartEDA::ExpReport(df, 
#                     op_dir = "pruebas",
#                     op_file = "my_smarteda.html")


#- "Limpiar datos"--------------------------------------------------------------

#- quita columnas y filas vacías, aquellas q tienen todo (o casi todo) NA
df_xx <- df %>% mutate(last_vv = NA)
df_xx <- janitor::remove_empty(df_xx, which = "cols", quiet = FALSE)


#- encontrar filas duplicadas
#- primero lo hacemos despacito
df_xx <- bind_rows(df, df[1,]) %>% 
  mutate(id = 1:n(), .before = 1L)

zz <- df_xx %>% 
  group_by(across(2:15)) %>% 
  mutate(NN = n(), .before = 2) %>% 
  filter(NN > 1) %>% ungroup()


#- con distinct() ya te quedas con las filas únicas
zz <- df %>% distinct()

#- otro uso de distinct
zz <- df %>% 
  distinct(prov_inscrip.n, parto_cesarea.f)
zz

#- con janitor
zz <- janitor::get_dupes(df)          #- más general
zz <- janitor::get_dupes(df_xx, -id)  #- más general

#- encontrar columnas duplicadas
zz <- df %>% mutate(nueva_vv = fecha_parto)
janitor::get_one_to_one(zz)

#- waldo pkg para comparar 2 df's exhaustivamente
waldo::compare(df, zz)


#- NA's ------------------------------------------------------------------------
#- ver cuantos NA's por variables
naniar::gg_miss_var(df)  
naniar::gg_miss_var(df, show_pct = TRUE)  
naniar::gg_miss_var(df, facet = estudios_madre.ff, show_pct = TRUE) #- faceted x estudios_madre.ff

DataExplorer::plot_missing(df)

#- co-ocurrencias de NA's
naniar::gg_miss_upset(df, nsets = 5, nintersect= 10) #- !!!!

visdat::vis_dat(df)
visdat::vis_miss(df, cluster = TRUE) 


#- ¿QUÉ HACEMOS con los NA's? --------------------------------------------------

#- ¿quitarlas?
#- quitamos las filas q tengan NA en cualquiera de las variables/columnas
zz <- df %>% tidyr::drop_na()              #- con tidyr
zz <- df[complete.cases(df), ]             #- con base-R 
zz <- df %>% filter(complete.cases(.))     #- con dplyr y base-R

#- si quisieramos obtener las filas con algún NA
zz <- df[!complete.cases(df), ]
zz1 <- df %>% filter(!complete.cases(.))


#- quito filas con NA en peso_bebe o en SEMANAS
zz <- df %>% tidyr::drop_na(c(peso_bebe, parto_nn_semanas)) 

#- buscar filas con NA's
zz1 <- df %>% filter(is.na(estudios_padre.ff))
zz2 <- df %>% filter(is.na(estudios_padre.ff) & is.na(parto_nn_semanas))


#- mas de buscar NA's
zz_con_NAs <- df %>% tidyr::drop_na()    #- filas con algún NA's

#- dplyr::anti_join() es útil
zz_sin_NAs <- anti_join(df, zz_con_NAs)  #- filas sin NA's 

#-encontrando columnas con y sin NA's
zz_con_NAs  <- df %>% select(where(anyNA))
zz_sin_NAs  <- df %>% select(!where(anyNA))


#- ¿imputamos los NA's?

#- con la media
zz <- df %>% 
  mutate(parto_nn_semanas.i1 = ifelse(is.na(parto_nn_semanas), 
                           mean(parto_nn_semanas, na.rm = TRUE), parto_nn_semanas),
                    .after = parto_nn_semanas)


#- con un modelo lineal !!!
fit <- lm(parto_nn_semanas ~ peso_bebe + parto_cesarea.f, data = df)

zz <- zz %>% 
  mutate(parto_nn_semanas.i2 = ifelse(is.na(parto_nn_semanas), predict(fit), peso_bebe), 
         .after = parto_nn_semanas.i1) 




#- borramos objetos
rm(list = ls(pattern = "^zz"))  #- borra los objetos cuyo nombre empiece por zz
rm(fit)


#- OUTLIERS (un poco) ----------------------------------------------------------
zz_out <- df %>% 
  #group_by(sexo_bebe.f) %>%
  rstatix::identify_outliers(peso_bebe) %>% 
  relocate(peso_bebe, .before = 1)

#- dejar las filas  q no son outliers
zz_no_out <- anti_join(df, zz_out)  #- filas sin NA's

#- las filas q están en los dos df's
zz_en_ambos <- semi_join(zz_out, zz_no_out)  #- filas sin NA's


#- Análisis de vv. NUMERICAS ---------------------------------------------------

#- antes borramos
no_quitar <- c("df_orig", "df", "df_aa")
rm(list=ls()[! ls() %in% no_quitar])

#- seleccionar v. numéricas
df_numeric <- df %>% select_if(is.numeric)      #- antigua sintaxis
df_numeric <- df %>% select(where(is.numeric))  #- nueva API

#- descriptvo de v. numéricas
summarytools::descr(df_numeric) 
summarytools::descr(df)    #- solo describe las numericas

DataExplorer::plot_histogram(df, ncol = 2)
DataExplorer::plot_density(df, ncol = 2)

#- correlaciones
df_numeric %>% GGally::ggcorr(label = TRUE) #- primero visualmente
df_numeric %>% GGally::ggpairs()            #- mas completo


corrr::correlate(df) 

#- tabla con matriz de correlaciones, con gt::gt()
df_numeric %>% corrr::correlate() %>% 
  gt::gt() %>% 
  gt::fmt_number(decimals = 1, sep_mark = ".",  dec_mark = ",") 

df %>% inspectdf::inspect_cor() %>% gt::gt()
options(scipen = 0) #- quitar notación científica

df %>% inspectdf::inspect_cor() %>% inspectdf::show_plot()


df %>% correlation::correlation()  #- remotes::install_github("easystats/correlation")


#- https://r-coder.com/correlation-plot-r/
#- https://albert-rapp.de/posts/ggplot2-tips/13_alternative_corrplots/13_alternative_corrplots.html


#- Boxplots ------------------
#DataExplorer::plot_boxplot(df, by = "estudios_madre.ff")
DataExplorer::plot_boxplot(df, by = "parto_cesarea.f")

#- boxplots (3 v.)
df %>% explore::explore(edad_madre.1, estudios_madre.ff, target = parto_cesarea.f)

#- explore_all()
df %>% select(sexo_bebe.f, edad_madre.1, peso_bebe, estudios_madre.ff, parto_cesarea.f) %>% 
  explore::explore_all(target = parto_cesarea.f)


#- explore::explore tiene muchas posibilidades
#- launch shiny
# if (interactive())  {
#   explore::explore(df)
# }


#- janitor:: tabulaciones cruzadas
df %>% janitor::tabyl(estudios_madre.ff, parto_cesarea.f) %>% 
       janitor::adorn_percentages() %>% 
       gt::gt()




#- scatter plots
my_vv <- "peso_bebe" 
DataExplorer::plot_scatterplot(df, by = my_vv)


#- discretizar
#- lo hemos visto con dplyr::ntile() y eurostat::cut_to_classes()
#- hoy vemos "un poco" el pkg "santoku: https://blog.djnavarro.net/posts/2023-05-22_santoku/
#- https://hughjonesd.github.io/santoku/

my_breaks <- c(0, 25, 50, 75, 100)  #- 4 trozos/buckets
tibble(
  valores = runif(10, min = 0, max = 100), 
  group.0a = cut(valores, breaks = my_breaks, labels = c("gr.1", "gr.2", "gr.3", "gr.4")),
  group.0b = cut(valores, breaks = my_breaks),
  group.1 = santoku::chop(valores, breaks = my_breaks))

#- VARIABLES CATEGÓRICAS -------------------------------------------------------
#- vamos a seleccionar las variables "categóricas"
#- no pasa nada si no lo entendéis del todo (siempre se puede hacer "a mano")
vv_cat <- df %>% select(where(is.factor)) %>% names()     #- se nos escapan las v q son character. 

vv_numericas <- df %>% select(where(is.numeric)) %>% names()

vv_no_numericas <- df %>% select(!where(is.numeric)) %>% names()
vv_no_numericas <- df %>% select(where(purrr::negate(is.numeric)))  %>% names()   #- purrr::negate()!!!!


vv_no_numericas <- df %>% select(!vv_numericas)  %>% names()     #- deprecated but funciona
df_no_num <- df %>% select(!all_of(vv_numericas))                #- forma correcta


#- porcentajes
inspectdf::inspect_cat(df_no_num) %>% inspectdf::show_plot(high_cardinality = 1)

explore::describe_cat(df, sexo_bebe.f)



#- graficos de barras
DataExplorer::plot_bar(df_no_num, )
DataExplorer::plot_bar(df_no_num, by = "sexo_bebe.f", nrow = 4, ncol = 4)
DataExplorer::plot_bar(df_no_num, by = "parto_cesarea.f", nrow = 4, ncol = 4)


SmartEDA::ExpCatViz(df, Page = c(3,3)) 




#- paquetes con shiny ----------------------------------------------------------
#- devtools::install_github("laderast/burro")
# burro::explore_data(df, outcome_var = colnames(df))
# 
# 
# explore::explore(df)
# df %>% explore::explore(parto_cesarea.f)
# df %>% explore::explore(edad_madre.1, estudios_madre.f, target = parto_cesarea.f)
# 
# 
# 
# library(ExPanDaR) #- remotes::install_github("joachim-gassen/ExPanDaR")
# 
# ExPanD(df)



#- TABLAS ----------------------------------------------------------------------
summarytools::freq(df$sexo_bebe.f, style = "rmarkdown")

#- cross-tabulations
summarytools::ctable(df$sexo_bebe.f, df$parto_cesarea.f)
summarytools::ctable(df$sexo_bebe.f, df$parto_cesarea.f, chisq = TRUE)


#- JANITOR 
df %>% janitor::tabyl(parto_cesarea.f) 

df %>% janitor::tabyl(parto_cesarea.f, estudios_madre.ff) 

df %>% janitor::tabyl(parto_cesarea.f, estudios_madre.ff) %>% janitor::adorn_percentages()

#- ademas, como janitor almacena las tablas en df, podemos usar gt()
df %>% janitor::tabyl(parto_cesarea.f, estudios_madre.ff) %>% gt::gt()



#- tablas de R-base
table(df$parto_cesarea.f, df$estudios_madre.ff, useNA = "always") 

my_tabla <- table(df$parto_cesarea.f, df$estudios_madre.ff) #- no la guarda como df
my_tabla

my_tabla2 <- my_tabla %>% as.data.frame() %>% 
  pivot_wider(names_from = 2, values_from = 3) 
my_tabla2 %>% gt::gt()


#- prop table
zz <- prop.table(my_tabla, 1)
zz
mosaicplot(t(zz), color = TRUE, main = "% de Cesáreas para niveles educativos de la madre")




#- CONTRATES -------------------------------------------------------------------

#- t-tests
t.test(df$peso_bebe)
t.test(df$peso_bebe, mu = 3250)
t.test(df$peso_bebe ~ df$parto_cesarea.f)
t.test(df$peso_bebe ~ df$sexo_bebe.f)   #- significativo


#- chi
chisq.test(df$parto_cesarea.f, df$sexo_bebe.f)
chisq.test(df$parto_cesarea.f, df$estudios_madre.ff) 



#- ANOVA's
#- multiples grupos: https://www.datanovia.com/en/blog/how-to-perform-t-test-for-multiple-groups-in-r/
#- https://statsandr.com/blog/how-to-do-a-t-test-or-anova-for-many-variables-at-once-in-r-and-communicate-the-results-in-a-better-way/
#- chequear supuestos: https://www.datanovia.com/en/lessons/anova-in-r/#assumptions (ANOVA, ouliers, normalidad etc...)
#  
# df %>%
#   group_by(estudios_madre.ff) %>%
#   rstatix::get_summary_stats(peso_bebe, type = "mean_sd")
# 
# df %>% rstatix::anova_test(peso_bebe ~ estudios_madre.ff)
# 
# 
# #- después de un ANOVA significativo, se suele hacer un Tukey post-hoc tests to perform multiple pairwise comparisons between groups. 
# pwc <- df %>% rstatix::tukey_hsd(peso_bebe ~ estudios_madre.ff)

# 
# #- !!!!!!!!!!!
# library(purrr)
# library(broom)
# df %>% group_by(estudios_madre.ff) %>% 
#   summarise(t_test = list(t.test(peso_bebe))) %>% 
#   mutate(tidied = map(t_test, tidy)) %>% 
#   tidyr::unnest(tidied) %>% 
#   ggplot(aes(estimate, estudios_madre.ff)) + geom_point() +
#   geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) + 
#   labs(title = "Peso del bebe para diferentes niveles educativos de la madre")


#- contraste de corr
df_numeric <- df %>% select(where(is.numeric))  #- nueva API

Hmisc::rcorr(as.matrix(df_numeric))


#- gtsummary::tbl_summary especificas variables de agrupación y realiza  algunos contrastes
df %>% select(peso_bebe, parto_nn_semanas, estudios_madre.ff, parto_cesarea.f) %>% 
  gtsummary::tbl_summary(by = "parto_cesarea.f") %>% 
  gtsummary::add_p()


