# ============================================
# Script para Análisis de Supervivencia
# ============================================

# Instalar y cargar los paquetes necesarios
install.packages(c("survival", "survminer"))
library(survival)
library(survminer)

# ============================================
# 1. Cargar la base de datos
# ============================================
# Cargar la base de datos desde un archivo CSV
df <- read.csv("ruta/del/archivo.csv")

# Inspeccionar los datos
head(df)      # Ver las primeras filas de la base
str(df)       # Ver la estructura de los datos

# ============================================
# 2. Preparar los datos
# ============================================
# Convertir columnas a los tipos adecuados
df$TIEMPO <- as.numeric(df$TIEMPO)         # Tiempo hasta el evento o censura
df$ETIQUETA <- as.factor(df$ETIQUETA)     # 1 = evento (deserción), 0 = censura
df$GENERO <- as.factor(df$GENERO)         # Género: 0 = Femenino, 1 = Masculino
df$ESTRATO <- as.factor(df$ESTRATO)       # Estrato socioeconómico

# ============================================
# 3. Crear el objeto de supervivencia
# ============================================
# Crear un objeto de supervivencia con tiempo y evento
surv_obj <- Surv(time = df$TIEMPO, event = df$ETIQUETA)

# ============================================
# 4. Análisis de Kaplan-Meier (Global)
# ============================================
# Ajustar el modelo Kaplan-Meier
km_fit <- survfit(surv_obj ~ 1, data = df)

# Resumen del modelo
summary(km_fit)

# Graficar la curva de supervivencia
ggsurvplot(km_fit, data = df, 
           xlab = "Tiempo (Semestres)", 
           ylab = "Proporción de Supervivencia",
           title = "Curva de Supervivencia Global",
           conf.int = TRUE)  # Mostrar intervalos de confianza

# ============================================
# 5. Comparación por grupos
# ============================================
# Kaplan-Meier por Género
km_fit_genero <- survfit(surv_obj ~ GENERO, data = df)

# Gráfico de supervivencia por género
ggsurvplot(km_fit_genero, data = df, 
           pval = TRUE,                        # Calcular y mostrar p-valor
           conf.int = TRUE,                   # Mostrar intervalos de confianza
           legend.labs = c("Femenino", "Masculino"),
           legend.title = "Género",
           xlab = "Tiempo (Semestres)",
           ylab = "Proporción de Supervivencia",
           title = "Curva de Supervivencia por Género")

# Kaplan-Meier por Estrato
km_fit_estrato <- survfit(surv_obj ~ ESTRATO, data = df)

# Gráfico de supervivencia por estrato
ggsurvplot(km_fit_estrato, data = df, 
           pval = TRUE,                        # Calcular y mostrar p-valor
           conf.int = TRUE,                   # Mostrar intervalos de confianza
           legend.title = "Estrato",
           xlab = "Tiempo (Semestres)",
           ylab = "Proporción de Supervivencia",
           title = "Curva de Supervivencia por Estrato")

# ============================================
# 6. Pruebas de Log-Rank (Significancia)
# ============================================
# Prueba de log-rank para género
logrank_genero <- survdiff(surv_obj ~ GENERO, data = df)
print(logrank_genero)

# Prueba de log-rank para estrato
logrank_estrato <- survdiff(surv_obj ~ ESTRATO, data = df)
print(logrank_estrato)

# ============================================
# 7. Modelo de Cox Proporcional de Riesgos
# ============================================
# Ajustar el modelo de Cox con varias variables
cox_model <- coxph(surv_obj ~ GENERO + ESTRATO + PROMEDIOCARRERA + SEMESTRE_ACTUAL, data = df)

# Resumen del modelo
summary(cox_model)

# Visualizar los coeficientes del modelo
ggforest(cox_model, data = df, 
         main = "Modelo de Cox: Efectos de las Variables")

# ============================================
# 8. Verificación del Supuesto de Riesgos Proporcionales
# ============================================
# Verificar el supuesto de riesgos proporcionales
cox_zph <- cox.zph(cox_model)
print(cox_zph)

# Graficar los diagnósticos de riesgos proporcionales
plot(cox_zph)

# ============================================
# 9. Predicción y Gráficos Ajustados
# ============================================
# Generar las curvas ajustadas del modelo de Cox
cox_fit <- survfit(cox_model, data = df)

# Graficar las curvas ajustadas
ggsurvplot(cox_fit, data = df, 
           xlab = "Tiempo (Semestres)",
           ylab = "Proporción de Supervivencia",
           title = "Curvas Ajustadas por el Modelo de Cox",
           conf.int = TRUE)

# ============================================
# Análisis completado
# ============================================
print("Análisis de supervivencia completado con éxito.")
