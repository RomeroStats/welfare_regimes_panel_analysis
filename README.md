# Título: Determinantes de la Desigualdad: Un Análisis de Datos Panel (1990-2015)

Descripción: Este proyecto investiga los determinantes del índice de Gini en distintos países utilizando modelos econométricos para datos panel. Se analiza cómo variables macroeconómicas (PIB, Inversión Extranjera) y de gasto social (Educación, Salud, Seguridad Social) impactan la desigualdad a lo largo del tiempo.

El análisis supera las limitaciones de la regresión lineal simple al controlar por heterogeneidad no observada entre países y dinámicas temporales.

## Metodología:

Preprocesamiento: Limpieza de datos y generación de variables con retardos (lags) y primeras diferencias para corregir no estacionariedad.

Análisis Exploratorio: Visualización de tendencias longitudinales por país utilizando la librería estética MoMAColors.

## Modelado:

Estimación de modelos AR(1) para evaluar estacionariedad.

Comparación de modelos de Efectos Fijos (Within) vs. Efectos Aleatorios (Random) mediante la prueba de Hausman.

Estimación de Modelos Dinámicos para capturar la persistencia de la desigualdad.

Prueba de Wooldridge para la detección de autocorrelación serial.

Estimación de modelos de Efectos Fijos de Dos Vías (Two-way) para controlar por efectos temporales y de país simultáneamente.

## Herramientas: R, plm, Tidyverse, MoMAColors.

## -------------------------------------------------------------------------
## PROYECTO: Determinantes de la Desigualdad - Modelos de Datos Panel
## AUTOR: César Romero
## DESCRIPCIÓN: Evaluación de modelos estáticos y dinámicos (Fixed/Random Effects).
## -------------------------------------------------------------------------

# 1. CONFIGURACIÓN DEL ENTORNO ---------------------------------------------

# Configuración regional
Sys.setlocale("LC_ALL", "es_ES.UTF-8")
options(scipen = 999)

# Carga e instalación de librerías
if (!require("pacman")) install.packages("pacman")

# Verificación de MoMAColors
if (!require("MoMAColors")) {
  if (!require("devtools")) install.packages("devtools")
  devtools::install_github("BlakeRMills/MoMAColors")
}

pacman::p_load(politicalds, tidyverse, plm, MoMAColors, lmtest)

# 2. LIMPIEZA Y PREPARACIÓN DE DATOS ---------------------------------------

# Carga de datos y filtrado de valores perdidos
datos <- welfare %>% 
  filter(!is.na(year) & !is.na(gini) & !is.na(gdp) & !is.na(foreign_inv) & 
           !is.na(education_budget) & !is.na(health_budget) & 
           !is.na(socialsec_budget) & !is.na(country_id))

# Definición de tema gráfico personal
my_theme <- theme(
  panel.background = element_rect(fill = "white"),
  panel.grid.major = element_line(color = "grey", size = 0.5),
  panel.grid.minor = element_blank(),
  axis.text = element_text(size = 12, color = "black"),
  axis.title = element_text(size = 14, color = "black"),
  plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
  legend.position = "bottom",
  legend.title = element_blank(),
  strip.background = element_rect(fill = "#528B8B"), 
  strip.text = element_text(size = 12, color = "black") 
)

# 3. ANÁLISIS EXPLORATORIO Y VISUALIZACIÓN AUTOMATIZADA --------------------

# Función para generar y guardar gráficos de tendencias
generar_grafico_panel <- function(data, y_var, titulo, paleta, archivo_salida) {
  
  # Gráfico 1: Comparativo en una sola visualización
  p1 <- ggplot(data, aes(x = year, y = .data[[y_var]], color = country_id, linetype = country_id)) +
    geom_line() +
    labs(title = paste(titulo, "- Comparativo"), x = "Año", y = titulo, caption = "Fuente: Politicalds") +
    my_theme +
    scale_color_moma_d(paleta)
  
  # Gráfico 2: Facet Wrap por país
  p2 <- ggplot(data, aes(x = year, y = .data[[y_var]], group = country_id)) +
    geom_line(color = "#528B8B") + 
    labs(title = paste(titulo, "- Por País"), x = "Año", y = titulo, caption = "Fuente: Politicalds") +
    my_theme +
    facet_wrap(~ country_id, ncol = 5)
  
  # Mostrar gráficos en la sesión
  print(p1)
  print(p2)
  
  # Guardado
  # ggsave(paste0(archivo_salida, "_comp.jpg"), p1, width = 16, height = 9, dpi = 300)
  # ggsave(paste0(archivo_salida, "_facet.jpg"), p2, width = 16, height = 9, dpi = 300)
}

# Generación de visualizaciones
# Análisis: Se observan tendencias no estacionarias en la mayoría de variables.
generar_grafico_panel(datos, "gini", "Índice Gini", "Warhol", "fig_gini")
generar_grafico_panel(datos, "gdp", "PIB", "Lupi", "fig_pib")
generar_grafico_panel(datos, "education_budget", "Presupuesto Educación", "Abbott", "fig_edu")
generar_grafico_panel(datos, "health_budget", "Presupuesto Salud", "Dali", "fig_salud")
generar_grafico_panel(datos, "socialsec_budget", "Gasto Seg. Social", "Klein", "fig_soc")


# 4. INGENIERÍA DE VARIABLES (LAGS Y DIFERENCIAS) --------------------------

# Creación de estructura de panel para análisis dinámico
datos <- datos %>%
  arrange(country_id, year) %>%
  group_by(country_id) %>%
  mutate(
    # Variables rezagadas (t-1)
    lag_gini = dplyr::lag(gini),
    lag_gdp = dplyr::lag(gdp),
    lag_foreign_inv = dplyr::lag(foreign_inv),
    lag_education_budget = dplyr::lag(education_budget),
    lag_health_budget = dplyr::lag(health_budget),
    lag_socialsec_budget = dplyr::lag(socialsec_budget),
    
    # Primeras diferencias (Delta X)
    diff_gini = gini - dplyr::lag(gini),
    diff_gdp = gdp - dplyr::lag(gdp),
    diff_foreign_inv = foreign_inv - dplyr::lag(foreign_inv),
    diff_education_budget = education_budget - dplyr::lag(education_budget),
    diff_health_budget = health_budget - dplyr::lag(health_budget),
    diff_socialsec_budget = socialsec_budget - dplyr::lag(socialsec_budget)
  ) %>%
  ungroup()

# 5. MODELADO ECONOMÉTRICO -------------------------------------------------

# --- 5.1 Evaluación de Estacionariedad (Modelos AR(1)) ---
# Se estima la persistencia de cada variable.

# 1. Gini
# Análisis: Estacionario. Fluctuaciones sin tendencia fija.
mod_ar_gini <- plm(gini ~ lag_gini, data = datos, index = c("country_id", "year"), model = "within")
summary(mod_ar_gini)

# 2. GDP
# Análisis: No estacionario. Alta persistencia y tendencia creciente.
mod_ar_gdp <- plm(gdp ~ lag_gdp, data = datos, index = c("country_id", "year"), model = "within")
summary(mod_ar_gdp)

# 3. Foreign Investment
# Análisis: Estacionario. Dependencia moderada, variaciones sin tendencia.
mod_ar_inv <- plm(foreign_inv ~ lag_foreign_inv, data = datos, index = c("country_id", "year"), model = "within")
summary(mod_ar_inv)

# 4. Education Budget
# Análisis: No estacionario. Crecimiento sostenido.
mod_ar_edu <- plm(education_budget ~ lag_education_budget, data = datos, index = c("country_id", "year"), model = "within")
summary(mod_ar_edu)

# 5. Health Budget
# Análisis: No estacionario. Tendencia al alza.
mod_ar_health <- plm(health_budget ~ lag_health_budget, data = datos, index = c("country_id", "year"), model = "within")
summary(mod_ar_health)

# 6. Social Security Budget
# Análisis: No estacionario. Aumento continuo.
mod_ar_soc <- plm(socialsec_budget ~ lag_socialsec_budget, data = datos, index = c("country_id", "year"), model = "within")
summary(mod_ar_soc)


# --- 5.2 Modelos Estáticos: Efectos Fijos vs Aleatorios ---

# Modelo de Efectos Fijos (Within)
# Análisis: Solo foreign_inv es significativa. Las no estacionarias muestran ruido.
model_fe <- plm(gini ~ gdp + foreign_inv + education_budget + health_budget + socialsec_budget, 
                  data = datos, 
                  index = c("country_id", "year"), 
                  model = "within", 
                  effect = "individual")
summary(model_fe)

# Modelo de Efectos Aleatorios (Random)
model_re <- plm(gini ~ gdp + foreign_inv + education_budget + health_budget + socialsec_budget, 
                         data = datos, 
                         index = c("country_id", "year"), 
                         model = "random")
summary(model_re)


# --- 5.3 Prueba de Hausman ---
# H0: El modelo de efectos aleatorios es consistente.
# H1: El modelo de efectos fijos es consistente.
hausman_test <- phtest(model_fe, model_re)
print(hausman_test)

# Análisis: p-value < 0.05. Se rechaza H0. Se prefiere Efectos Fijos.


# --- 5.4 Modelo Dinámico con Efectos Fijos ---
# Se utilizan diferencias para las variables que resultaron no estacionarias en 5.1
# y se incluye el rezago de la dependiente (lag_gini).

model_dynamic <- plm(gini ~ lag_gini + diff_gdp + foreign_inv + diff_education_budget + 
                             diff_health_budget + diff_socialsec_budget, 
                           data = datos, 
                           index = c("country_id", "year"), 
                           model = "within", 
                           effect = "individual")

summary(model_dynamic)

# --- 5.5 Diagnóstico: Prueba de Wooldridge ---
# H0: No existe autocorrelación serial.
wooldridge_test <- pwartest(model_dynamic)
print(wooldridge_test)

# Análisis: p-value > 0.05 (0.9053). No hay autocorrelación serial. 
# El modelo es válido y no requiere ajustes adicionales.


# --- 5.6 Modelo Final: Efectos Fijos de Dos Vías (Two-Way) ---
# Controla por país y por año (shocks temporales).

model_twoway <- plm(gini ~ lag_gini + diff_gdp + foreign_inv + diff_education_budget + 
                             diff_health_budget + diff_socialsec_budget, 
                           data = datos, 
                           index = c("country_id", "year"), 
                           model = "within", 
                           effect = "twoways")

summary(model_twoway)

# 6. CONCLUSIONES TÉCNICAS -------------------------------------------------

# 1. Persistencia: La desigualdad (Gini) muestra una fuerte inercia (lag_gini significativo),
#    lo que justifica el uso de modelos dinámicos.
# 2. Crecimiento Económico: El crecimiento del PIB (diff_gdp) muestra una relación significativa
#    y positiva con la desigualdad en el modelo two-way.
# 3. Robustez: Las pruebas de Hausman y Wooldridge validan la elección del modelo dinámico
#    de efectos fijos, asegurando estimadores consistentes.
