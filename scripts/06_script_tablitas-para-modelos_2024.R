#- tablas para MODELOS

#- tablas para modelos ---------------------------------------------------------
urla <- "https://raw.githubusercontent.com/perezp44/iris_data/master/data/PIAAC_data_small.csv"
df <- read_csv(urla)


#- estimamos un modelo lineal
my_model <- lm(Wage_hour ~ Numeracy_score + Gender , data = df)
my_model

summary(my_model)


#- pkg stargazer ---------------------------------------------------------------
stargazer::stargazer(my_model, type = "html")
stargazer::stargazer(my_model, type = "text")


#- creo una variable dicotómica (para estimar un Logit)
df <- df %>% 
  mutate(Numeracy_score_b = 
           ifelse(Numeracy_score > mean(Numeracy_score, na.rm = TRUE), 1, 0)) 

#- estimamos varios modelos y los almacenamos en una lista
my_models <- list()
my_models[['W:  OLS 1']]   <- lm( Wage_hour         ~ Numeracy_score + Gender , df)
my_models[['Nu: OLS 2']]   <- lm( Numeracy_score    ~ Education + Gender , df)
my_models[['Nu: Logit 1']] <- glm( Numeracy_score_b ~ Education + Gender , df, family = binomial())


stargazer::stargazer(my_models, type = "html", title = "Results", align = TRUE)
stargazer::stargazer(my_models, type = "text", title = "Results", align = TRUE)


#- pkg modelsummary ------------------------------------------------------------
library(modelsummary) #- remotes::install_github('vincentarelbundock/modelsummary')

mm <- modelsummary::msummary(my_models, title = "Resultados de estimación")
mm




#- pkg gtsummary ---------------------------------------------------------------
iris %>% gtsummary::tbl_summary()

gtsummary::tbl_regression(my_model)


#- pkg sjPlot ------------------------------------------------------------------

sjPlot::tab_model(my_model)

sjPlot::plot_model(my_model)


