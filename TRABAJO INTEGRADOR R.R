# =============================================================================
# TRABAJO PRÁCTICO INTEGRADOR - MÓDULO R
# Maestría en Econometría - Universidad Torcuato Di Tella
# Alumno: Juan Manuel Banera
# =============================================================================
# =============================================================================
# PARTE I: LASSO PARA SELECCIÓN DE VARIABLES
# Tema: Determinantes del salario en Argentina (EPH) mediante regresión LASSO
# =============================================================================
# 1. PREPARACIÓN DEL ENTORNO Y LIBRERÍAS --------------------------------------

library(tidyverse)
library(eph)
library(glmnet)
library(broom)

# Fijamos semilla para reproducibilidad (Train/Test split y CV)
set.seed(6403)

# 2. CARGA Y LIMPIEZA DE DATOS (EPH) ------------------------------------------

eph_raw <- get_microdata(year = 2023, trimester = 4, type = "individual")

# Filtrado de ocupados asalariados y creación de variables
datos_limpios <- eph_raw |>
  filter(
    ESTADO == 1,          # Ocupados
    CAT_OCUP == 3,        # Asalariados
    P21 > 0,              # Ingreso de la ocupación principal positivo
    CH06 >= 18,           # Mayores de 18
    CH06 <= 65            # Menores de 65
  ) |>
  mutate(
    log_ingreso  = log(P21),
    sexo         = factor(CH04, levels = c(1, 2), labels = c("Varon", "Mujer")),
    experiencia  = pmax(CH06 - 18, 0),
    exper2       = experiencia^2,
    nivel_ed     = case_when(
      NIVEL_ED %in% c(1, 2) ~ "Primario",
      NIVEL_ED %in% c(3, 4) ~ "Secundario",
      NIVEL_ED %in% c(5, 6) ~ "Superior",
      TRUE                  ~ NA_character_
    ),
    nivel_ed     = factor(nivel_ed, levels = c("Primario", "Secundario", "Superior")),
    horas_trab   = PP3E_TOT,
    jornada_comp = ifelse(PP3E_TOT >= 35, "Completa", "Parcial"),
    jornada_comp = factor(jornada_comp),
    region       = factor(REGION),
    tamanio_emp  = factor(PP04C, levels = 1:99) 
  ) |>
  # Nos quedamos con las variables de interés sin NAs
  select(log_ingreso, sexo, experiencia, exper2, nivel_ed, horas_trab, jornada_comp, region, tamanio_emp) |>
  drop_na()

cat("Tamaño de la muestra final:", nrow(datos_limpios), "observaciones\n")


# 3. ANÁLISIS EXPLORATORIO DE DATOS (EDA) -------------------------------------

# Distribución del ingreso objetivo
p_dist <- ggplot(datos_limpios, aes(x = log_ingreso)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40, fill = "steelblue", color = "white", alpha = 0.8) +
  geom_density(color = "#DC143C", linewidth = 1.2) + 
  labs(
    title = "Distribución del logaritmo del ingreso laboral",
    subtitle = "Asalariados 18-65 años (EPH T4 2023)",
    x = "log(Ingreso Ocupación Principal)",
    y = "Densidad"
  ) +
  theme_minimal(base_size = 13)
print(p_dist)

# Brecha salarial por nivel educativo y género
p_box <- ggplot(datos_limpios, aes(x = nivel_ed, y = log_ingreso, fill = sexo)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.2) +
  scale_fill_manual(values = c("Varon" = "#1B4F8A", "Mujer" = "#C0392B")) +
  labs(
    title = "Ingreso por Nivel Educativo y Sexo",
    x = "Nivel Educativo",
    y = "log(Ingreso)",
    fill = "Sexo"
  ) +
  theme_minimal(base_size = 13)
print(p_box)


# 4. PREPARACIÓN PARA ANÁLISIS ---------------------


X <- model.matrix(log_ingreso ~ ., data = datos_limpios)[, -1]
y <- datos_limpios$log_ingreso

# Split 70% Train / 30% Test
n <- nrow(X)
idx_train <- sample(1:n, size = round(0.7 * n))
idx_test  <- setdiff(1:n, idx_train)

X_train <- X[idx_train, ]
y_train <- y[idx_train]
X_test  <- X[idx_test, ]
y_test  <- y[idx_test]


# 5. REGULARIZACIÓN LASSO -----------------------------------------------------

lasso_cv <- cv.glmnet(X_train, y_train, alpha = 1, nfolds = 10)


par(mar = c(5, 4, 6, 2)) 

plot(lasso_cv) 

title("LASSO: Selección de Lambda por K-Fold CV", line = 4)
# Extraemos los lambdas óptimos
lambda_min <- lasso_cv$lambda.min
lambda_1se <- lasso_cv$lambda.1se

cat(sprintf("Lambda óptimo (min MSE): %.4f\n", lambda_min))
cat(sprintf("Lambda más parsimonioso (1 SE): %.4f\n", lambda_1se))


# 6. EVALUACIÓN Y SELECCIÓN DE VARIABLES min MSE--------------------------------------

# Predicción sobre el conjunto de test
pred_lasso <- predict(lasso_cv, s = "lambda.min", newx = X_test)
rmse_lasso <- sqrt(mean((y_test - pred_lasso)^2))

# R-cuadrado en test
ss_res <- sum((y_test - pred_lasso)^2)
ss_tot <- sum((y_test - mean(y_test))^2)
r2_test <- 1 - (ss_res / ss_tot)

cat(sprintf("\nPerformance en TEST SET:\nRMSE: %.4f\nR2: %.4f\n", rmse_lasso, r2_test))

# Variables que sobrevivieron a la penalización (coeficientes != 0)
coef_lasso <- coef(lasso_cv, s = "lambda.min")
vars_activas <- rownames(coef_lasso)[coef_lasso[, 1] != 0]

cat("\nVariables seleccionadas por el algoritmo (coeficiente distinto de cero):\n")
print(vars_activas)

# 7. VISUALIZACIÓN FINAL DE COEFICIENTES  mni MSE--------------------------------------
df_coefs <- tibble(
  Variable = rownames(coef_lasso),
  Coeficiente = as.vector(coef_lasso)
) |> 
  filter(Coeficiente != 0, Variable != "(Intercept)") |> 
  arrange(desc(abs(Coeficiente)))

p_coefs <- ggplot(df_coefs, aes(x = reorder(Variable, abs(Coeficiente)), y = Coeficiente, fill = Coeficiente > 0)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "tomato")) +
  labs(
    title = "Coeficientes seleccionados por LASSO (Lambda Min)",
    subtitle = "Ecuación de Mincer extendida - Variables que más impactan el salario",
    x = "Variable",
    y = "Magnitud del Coeficiente"
  ) +
  theme_minimal(base_size = 12)
print(p_coefs)

# 8. EVALUACIÓN Y SELECCIÓN DE VARIABLES (1 SE) -------------------------------

# Predicción sobre el conjunto de test
pred_lasso_2 <- predict(lasso_cv, s = "lambda.1se", newx = X_test)
rmse_lasso_2 <- sqrt(mean((y_test - pred_lasso_2)^2))

# R-cuadrado en test
ss_res_2 <- sum((y_test - pred_lasso_2)^2)
ss_tot_2 <- sum((y_test - mean(y_test))^2)
r2_test_2 <- 1 - (ss_res_2 / ss_tot_2)

cat(sprintf("\nPerformance en TEST SET (1 SE):\nRMSE: %.4f\nR2: %.4f\n", rmse_lasso_2, r2_test_2))

# Variables que sobrevivieron a la penalización (coeficientes != 0)
coef_lasso_2 <- coef(lasso_cv, s = "lambda.1se")
vars_activas_2 <- rownames(coef_lasso_2)[coef_lasso_2[, 1] != 0]

cat("\nVariables seleccionadas por el algoritmo (coeficiente distinto de cero):\n")
print(vars_activas_2)

# 9. VISUALIZACIÓN FINAL DE COEFICIENTES (1 SE) -------------------------------
df_coefs_2 <- tibble(
  Variable = rownames(coef_lasso_2),
  Coeficiente = as.vector(coef_lasso_2)
) |> 
  filter(Coeficiente != 0, Variable != "(Intercept)") |> 
  arrange(desc(abs(Coeficiente)))

p_coefs_2 <- ggplot(df_coefs_2, aes(x = reorder(Variable, abs(Coeficiente)), y = Coeficiente, fill = Coeficiente > 0)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "tomato")) +
  labs(
    title = "Coeficientes seleccionados por LASSO (Lambda 1SE)",
    subtitle = "Modelo parsimonioso - Variables que más impactan el salario",
    x = "Variable",
    y = "Magnitud del Coeficiente"
  ) +
  theme_minimal(base_size = 12)
print(p_coefs_2)


# =============================================================================
# PARTE II: CLASIFICACIÓN Y REGRESIÓN LOGÍSTICA (GLM)
# Tema: Determinantes de la informalidad laboral 
# =============================================================================

library(pROC)

# 10. PREPARACIÓN DE DATOS PARA CLASIFICACIÓN ---------------------------------

datos_logit <- eph_raw |>
  filter(
    ESTADO == 1, CAT_OCUP == 3, P21 > 0, CH06 >= 18, CH06 <= 65,
    PP07H %in% c(1, 2) # 1 = Tiene descuento, 2 = No tiene descuento
  ) |>
  mutate(
    # Creamos la variable objetivo binaria (1 = Informal, 0 = Formal)
    informal     = ifelse(PP07H == 2, 1, 0),
    sexo         = factor(CH04, levels = c(1, 2), labels = c("Varon", "Mujer")),
    experiencia  = pmax(CH06 - 18, 0),
    nivel_ed     = case_when(
      NIVEL_ED %in% c(1, 2) ~ "Primario",
      NIVEL_ED %in% c(3, 4) ~ "Secundario",
      NIVEL_ED %in% c(5, 6) ~ "Superior",
      TRUE                  ~ NA_character_
    ),
    nivel_ed     = factor(nivel_ed, levels = c("Primario", "Secundario", "Superior")),
    horas_trab   = PP3E_TOT,
    jornada_comp = factor(ifelse(PP3E_TOT >= 35, "Completa", "Parcial")),
    region       = factor(REGION),
    tamanio_emp  = factor(PP04C, levels = 1:99)
  ) |>
  select(informal, sexo, experiencia, nivel_ed, horas_trab, jornada_comp, region, tamanio_emp) |>
  drop_na()

# 11. TRAIN/TEST SPLIT --------------------------------------------------------
set.seed(6403)
n_logit <- nrow(datos_logit)
idx_train_log <- sample(1:n_logit, size = round(0.7 * n_logit))
idx_test_log  <- setdiff(1:n_logit, idx_train_log)

train_logit <- datos_logit[idx_train_log, ]
test_logit  <- datos_logit[idx_test_log, ]


# 12. ENTRENAMIENTO DEL MODELO LOGIT ------------------------------------------
cat("\nEntrenando modelo de Regresión Logística...\n")
modelo_logit <- glm(informal ~ ., data = train_logit, family = "binomial")

# Extraemos los Odds Ratios (exponenciando los coeficientes)
odds_ratios <- tidy(modelo_logit, exponentiate = TRUE, conf.int = TRUE) |> 
  select(term, estimate, p.value, conf.low, conf.high) |> 
  filter(term != "(Intercept)") |> 
  arrange(desc(estimate))

cat("\n--- Top 5 factores que aumentan la CHANCE de informalidad (Odds Ratios > 1) ---\n")
print(head(odds_ratios, 5))


# 13. PREDICCIÓN Y EVALUACIÓN (MATRIZ DE CONFUSIÓN Y ROC) ---------------------

# Predecimos probabilidades en el set de prueba
prob_pred <- predict(modelo_logit, newdata = test_logit, type = "response")

# Convertimos probabilidades a clases (Corte de 0.5)
clase_pred <- ifelse(prob_pred > 0.5, 1, 0)

# Matriz de Confusión
matriz_confusion <- table(Prediccion = clase_pred, Real = test_logit$informal)
cat("\n--- Matriz de Confusión (Umbral 0.5) ---\n")
print(matriz_confusion)

# Cálculo de Accuracy 
accuracy <- sum(diag(matriz_confusion)) / sum(matriz_confusion)
cat(sprintf("Accuracy global: %.2f%%\n", accuracy * 100))

# 14. CURVA ROC Y AUC ---------------------------------------------------------
roc_obj <- roc(test_logit$informal, prob_pred, quiet = TRUE)
auc_val <- auc(roc_obj)
cat(sprintf("Área Bajo la Curva (AUC): %.3f\n", auc_val))

# Ploteo de la Curva ROC
plot(roc_obj, main = paste("Curva ROC - Modelo Informalidad\nAUC =", round(auc_val, 3)), 
     col = "steelblue", lwd = 3, print.auc = FALSE)