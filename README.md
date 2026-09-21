# Trabajo-Integrador-R
Análisis empírico del mercado laboral argentino mediante la utilización de la EPH
# Determinantes del salario e informalidad laboral en Argentina

Este repositorio contiene el código y el informe final del Trabajo Práctico Integrador para el módulo de R de la Maestría en Econometría (UTDT).

## Dataset
Se utilizaron los microdatos de la **Encuesta Permanente de Hogares (EPH)** provistos por el Instituto Nacional de Estadística y Censos (INDEC) correspondientes al **cuarto trimestre de 2023**. La población objetivo del estudio se acotó a individuos ocupados, asalariados (descartando cuentapropistas y patrones), de entre 18 y 65 años, y con ingresos positivos.

## Técnicas Analíticas Aplicadas
El análisis aborda dos dimensiones del mercado laboral argentino utilizando diferentes metodologías:

1. **Predicción de Ingresos (Regresión LASSO):** 
   Se partió de una ecuación de Mincer clásica y se la expandió incorporando múltiples variables categóricas. Para evitar la multicolinealidad, se aplicó regularización LASSO con Validación Cruzada (10-Fold CV). Se evaluó el contraste algorítmico entre el hiperparámetro que minimiza el error (`lambda.min`) y el modelo más parsimonioso (`lambda.1se`).
2. **Clasificación de Informalidad (Regresión Binomial Logit):** 
   Se modeló la probabilidad de trabajar en la informalidad (ausencia de aportes jubilatorios) mediante un Modelo Lineal Generalizado (GLM). El rendimiento predictivo fue evaluado a través de los Odds Ratios, la Matriz de Confusión y el Área Bajo la Curva ROC (AUC = 0.844).

## Cómo correr el proyecto

### Requisitos previos
Es necesario tener instalado R y RStudio, junto con los siguientes paquetes:
```r
install.packages(c("tidyverse", "glmnet", "broom", "pROC","eph"))
```

## Informe Final
Los resultados sustantivos, los gráficos de penalización algorítmica y las conclusiones económicas del modelo se encuentran detallados en el documento trabajo_integrador_r.pdf incluido en este repositorio.

