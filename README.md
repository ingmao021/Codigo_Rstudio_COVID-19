# 📊 Análisis Estadístico de Datos COVID-19 en R

[![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)](https://www.r-project.org/)
[![RStudio](https://img.shields.io/badge/RStudio-4285F4?style=for-the-badge&logo=rstudio&logoColor=white)](https://www.rstudio.com/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

> Análisis estadístico de datos COVID-19 utilizando R, se realizo el consumo de la api por medio de R "https://www.datos.gov.co/resource/gt2j-8ykr.csv" que nos devuelve un archivo csv con 1000 datos.

## 📋 Tabla de Contenidos

- [🎯 Descripción](#-descripción)
- [🔧 Requisitos](#-requisitos)
- [📊 Estructura de Datos](#-estructura-de-datos)
- [📈 Análisis Realizados](#-análisis-realizados)
- [💻 Código Completo](#-código-completo)
- [🚀 Cómo Usar](#-cómo-usar)
- [📊 Visualizaciones](#-visualizaciones)
- [🔬 Metodología](#-metodología)
- [📝 Resultados](#-resultados)
- [🔷 Resultados](#-anexos)

## 🎯 Descripción

Este proyecto implementa un análisis estadístico integral de datos COVID-19 que incluye:

- **📈 Análisis descriptivo** de variables demográficas
- **📊 Visualizaciones** exploratorias de datos  
- **📐 Ajuste de distribuciones** (Poisson y Exponencial)
- **🔬 Inferencia estadística** mediante pruebas t
- **🔗 Análisis de correlación** y regresión
- **🎲 Simulaciones Monte Carlo** para proyecciones por grupo de edad

## 🔧 Requisitos

### Paquetes de R Necesarios

```r
# Instalar paquetes requeridos
install.packages(c(
  "tidyverse",      # Manipulación de datos y gráficos
  "lubridate",      # Manejo de fechas
  "ggplot2",        # Visualización de datos
  "fitdistrplus",   # Ajuste de distribuciones
  "MASS",           # Funciones estadísticas avanzadas
  "moments"         # Cálculo de momentos estadísticos
))
```

### Versiones Recomendadas
- **R** >= 4.0.0
- **RStudio** >= 1.4.0 (opcional pero recomendado)

## 📊 Estructura de Datos

El archivo `covid_data.csv` contiene las siguientes columnas principales:

### Variables de Identificación y Ubicación
| Columna | Tipo | Descripción | Ejemplo |
|---------|------|-------------|---------|
| `id_de_caso` | Numérico | Identificador único del caso | 123456 |
| `departamento` | Categórico | Código del departamento | 52 |
| `departamento_nom` | Texto | Nombre del departamento | NARIÑO |
| `ciudad_municipio` | Categórico | Código del municipio | 52001 |
| `ciudad_municipio_nom` | Texto | Nombre del municipio | PASTO |

### Variables Demográficas
| Columna | Tipo | Descripción | Ejemplo |
|---------|------|-------------|---------|
| `edad` | Numérico | Edad del paciente | 45 |
| `unidad_medida` | Categórico | Unidad de medida de la edad | 1 (años) |
| `sexo` | Categórico | Sexo del paciente | F/M |
| `per_etn_` | Categórico | Código de pertenencia étnica | 6 |
| `nom_grupo_` | Texto | Nombre del grupo étnico | OTROS |

### Variables de Estado y Evolución
| Columna | Tipo | Descripción | Ejemplo |
|---------|------|-------------|---------|
| `estado` | Categórico | Estado actual del caso | Recuperado/Fallecido |
| `recuperado` | Categórico | Indicador de recuperación | Recuperado/N/A |
| `ubicacion` | Categórico | Ubicación del paciente | Casa/Hospital |
| `tipo_recuperacion` | Categórico | Tipo de recuperación | PCR |

### Variables de Fechas
| Columna | Tipo | Descripción | Ejemplo |
|---------|------|-------------|---------|
| `fecha_reporte_web` | Fecha | Fecha de reporte en web | 2023-03-16 |
| `fecha_de_notificaci_n` | Fecha | Fecha de notificación | 2023-03-15 |
| `fecha_inicio_sintomas` | Fecha | Fecha de inicio de síntomas | 2023-03-10 |
| `fecha_diagnostico` | Fecha | Fecha de diagnóstico | 2023-03-15 |
| `fecha_muerte` | Fecha | Fecha de fallecimiento | 2023-03-25 |
| `fecha_recuperado` | Fecha | Fecha de recuperación | 2023-03-20 |

### Variables de Contexto
| Columna | Tipo | Descripción | Ejemplo |
|---------|------|-------------|---------|
| `fuente_tipo_contagio` | Categórico | Fuente del contagio | Comunitaria |
| `pais_viajo_1_cod` | Categórico | Código del país de viaje | 170 |
| `pais_viajo_1_nom` | Texto | Nombre del país de viaje | COLOMBIA |

### Notas Importantes:
- **Datos faltantes:** Muchas columnas contienen valores `N/A` o vacíos
- **Fechas:** Formato YYYY-MM-DD
- **Estados válidos:** "Recuperado", "Fallecido", "Activo"
- **Sexo:** "F" (Femenino), "M" (Masculino)
- **Total de variables:** 23 columnas en el dataset

## 📈 Análisis Realizados

### 1. 📊 Estadística Descriptiva

#### Medidas de Tendencia Central
- **Media, mediana y moda** de variables cuantitativas (edad de los casos)
- **Análisis por grupos:** Segmentación por departamento, sexo y estado de recuperación
- **Distribución de frecuencias** para variables categóricas

#### Medidas de Dispersión
- **Varianza y desviación estándar** para cuantificar la dispersión de los datos
- **Rango y rango intercuartílico** para identificar la amplitud de variación
- **Coeficiente de variación** para comparar la dispersión relativa entre grupos
- **Análisis de valores atípicos** mediante diagramas de caja y bigotes

---

### 2. 🌍 Análisis de Distribuciones de Probabilidad

#### Distribución Geográfica
- **Análisis espacial** de la incidencia de casos por departamentos y municipios
- **Mapas de calor** y visualizaciones geográficas de la propagación

#### Distribución de Poisson
- **Modelado del número de muertes diarias** por COVID-19
- **Análisis de eventos raros** en periodos específicos

#### Distribución Exponencial
- **Modelado del tiempo** desde inicio de síntomas hasta fallecimiento
- **Análisis de supervivencia** y tiempos de espera

#### Distribución Normal
- **Análisis de la distribución de edad** en personas fallecidas
- **Pruebas de normalidad** (Shapiro-Wilk y Kolmogorov-Smirnov)
- **Transformaciones de datos** para aproximar normalidad

---

### 3. 🔬 Inferencia Estadística

#### Estimación por Intervalos
- **Intervalos de confianza para la media** de edad por grupos
- **Márgenes de error** para estimaciones poblacionales
- **Construcción de rangos** de confianza para proporciones

#### Pruebas de Hipótesis
- **Prueba t para una muestra:** Comparación de medias muestrales vs valores teóricos
- **Prueba Chi-cuadrado de bondad de ajuste:** Evaluación de modelos teóricos vs datos observados
- **Pruebas de normalidad:** Shapiro-Wilk y Kolmogorov-Smirnov para validar supuestos

---

### 4. 📈 Análisis de Correlación y Regresión

#### Correlación
- **Coeficiente de correlación de Pearson** entre variables cuantitativas
- **Análisis de relaciones** entre edad y tiempo de recuperación
- **Matrices de correlación** para múltiples variables

#### Regresión Lineal
- **Regresión lineal simple** para predicción de variables dependientes
- **Análisis de residuos** y validación de supuestos del modelo
- **Interpretación de coeficientes** y significancia estadística

---

### 5. 🎲 Simulación Monte Carlo

#### Análisis Prospectivo
- **Simulación de probabilidades** de fallecimiento vs recuperación por grupo de edad
- **Modelos predictivos** basados en características demográficas
- **Escenarios probabilísticos** para diferentes grupos poblacionales

#### Análisis Temporal
- **Tiempo promedio hasta recuperación** segmentado por edad y sexo
- **Tiempo promedio hasta fallecimiento** por grupos demográficos
- **Simulación de trayectorias** de evolución de casos

---

### 6. 📐 Medidas de Forma de Distribución

#### Asimetría
- **Coeficiente de Asimetría (Skewness)** para evaluar la simetría de distribuciones
- **Interpretación de sesgos** hacia la izquierda o derecha
- **Análisis comparativo** entre diferentes variables

#### Curtosis
- **Coeficiente de Curtosis** para evaluar la concentración de datos
- **Clasificación de distribuciones:** leptocúrticas, mesocúrticas y platicúrticas
- **Comparación con distribución normal** estándar

---

### 7. 🧮 Aplicaciones Avanzadas

#### Teorema de Bayes
- **Cálculos de probabilidad condicional** para eventos COVID-19
- **Actualización de probabilidades** con nueva evidencia
- **Aplicación en diagnóstico** y predicción de resultados

#### Análisis de Datos Faltantes
- **Identificación y cuantificación** de valores ausentes
- **Estrategias de imputación** para variables críticas
- **Análisis de patrones** de datos faltantes

---

### 8. 📊 Visualización y Presentación

#### Gráficos Descriptivos
- **Histogramas y diagramas de densidad** para variables continuas
- **Gráficos de barras y sectores** para variables categóricas
- **Diagramas de caja** para comparar distribuciones entre grupos

#### Gráficos Analíticos
- **Gráficos de dispersión** con líneas de regresión
- **Mapas de calor** para matrices de correlación
- **Gráficos de series de tiempo** para evolución temporal

#### Dashboards Interactivos
- **Visualizaciones dinámicas** con filtros por región y período
- **Métricas en tiempo real** de indicadores clave
- **Comparaciones interactivas** entre departamentos y municipios

## 💻 Código Completo

```r
# =============================================================================
# ANÁLISIS ESTADÍSTICO DE DATOS COVID-19
# =============================================================================
# Descripción: Análisis completo de datos COVID-19 incluyendo estadística 
#              descriptiva, ajuste de distribuciones e inferencia estadística
# =============================================================================

# -----------------------------------------------------------------------------
# 1. CONFIGURACIÓN E IMPORTACIÓN DE LIBRERÍAS
# -----------------------------------------------------------------------------

# Cargar librerías necesarias
suppressPackageStartupMessages({
  library(tidyverse)      # Manipulación de datos y visualización
  library(lubridate)      # Manejo de fechas y tiempos
  library(ggplot2)        # Gráficos avanzados
  library(fitdistrplus)   # Ajuste de distribuciones estadísticas
  library(MASS)           # Funciones estadísticas (fitdistr)
  library(moments)        # Cálculo de momentos estadísticos
})

# Configuración de tema global para gráficos
theme_set(theme_minimal() + 
          theme(plot.title = element_text(hjust = 0.5, face = "bold")))

# -----------------------------------------------------------------------------
# 2. LECTURA Y EXPLORACIÓN INICIAL DE DATOS
# -----------------------------------------------------------------------------

# Cargar datos
cat("📂 Cargando datos COVID-19...\n")
covid_data <- read.csv("covid_data.csv", stringsAsFactors = FALSE)

# Exploración inicial
cat("📊 Explorando estructura de datos:\n")
cat("Dimensiones:", dim(covid_data), "\n")
cat("Columnas:", names(covid_data), "\n")
print(head(covid_data))
print(summary(covid_data))

# -----------------------------------------------------------------------------
# 3. ANÁLISIS DESCRIPTIVO - ESTADÍSTICAS DE EDAD
# -----------------------------------------------------------------------------

cat("\n📈 Calculando estadísticas descriptivas de edad...\n")

# Tabla descriptiva completa de la variable edad
age_summary <- covid_data %>%
  summarise(
    n_total     = n(),
    n_validos   = sum(!is.na(edad)),
    n_faltantes = sum(is.na(edad)),
    media       = round(mean(edad, na.rm = TRUE), 2),
    mediana     = median(edad, na.rm = TRUE),
    moda        = as.numeric(names(sort(table(edad), decreasing = TRUE)[1])),
    desv_std    = round(sd(edad, na.rm = TRUE), 2),
    varianza    = round(var(edad, na.rm = TRUE), 2),
    minimo      = min(edad, na.rm = TRUE),
    maximo      = max(edad, na.rm = TRUE),
    q1          = quantile(edad, 0.25, na.rm = TRUE),
    q3          = quantile(edad, 0.75, na.rm = TRUE),
    rango_iq    = q3 - q1
  )

print(age_summary)

# -----------------------------------------------------------------------------
# 4. VISUALIZACIONES - ANÁLISIS EXPLORATORIO DE DATOS
# -----------------------------------------------------------------------------

cat("\n📊 Generando visualizaciones exploratorias...\n")

## 4.1 Histograma de distribución de edad con estadísticas centrales
p1 <- ggplot(covid_data, aes(x = edad)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white", alpha = 0.8) +
  geom_vline(aes(xintercept = mean(edad, na.rm = TRUE)),
             color = "red", linetype = "dashed", size = 1.2) +
  geom_vline(aes(xintercept = median(edad, na.rm = TRUE)),
             color = "green", linetype = "dashed", size = 1.2) +
  geom_vline(xintercept = as.numeric(names(sort(table(covid_data$edad), 
                                                decreasing = TRUE)[1])),
             color = "purple", linetype = "dotted", size = 1.2) +
  labs(
    title = "Distribución de Edad en Pacientes COVID-19",
    subtitle = "Líneas: Media (roja), Mediana (verde), Moda (morada)",
    x = "Edad (años)",
    y = "Frecuencia",
    caption = "Fuente: Datos COVID-19"
  )

print(p1)

## 4.2 Boxplot comparativo por estado del paciente
p2 <- ggplot(covid_data, aes(x = estado, y = edad, fill = estado)) +
  geom_boxplot(alpha = 0.8, outlier.color = "red", outlier.alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 0.8) +
  labs(
    title = "Distribución de Edad por Estado del Paciente",
    subtitle = "Comparación entre Recuperados y Fallecidos",
    x = "Estado del Paciente",
    y = "Edad (años)"
  ) +
  scale_fill_brewer(palette = "Set2", name = "Estado") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "darkred")

print(p2)

## 4.3 Dispersión temporal: Edad vs Fecha de diagnóstico
# Convertir fechas
covid_data$fecha_diagnostico <- as.Date(covid_data$fecha_diagnostico, format = "%Y-%m-%d")

p3 <- ggplot(covid_data, aes(x = fecha_diagnostico, y = edad)) +
  geom_point(alpha = 0.6, color = "darkblue", size = 1.5) +
  geom_smooth(method = "loess", color = "red", alpha = 0.3) +
  labs(
    title = "Evolución Temporal de Edad en Diagnósticos COVID-19",
    subtitle = "Relación entre edad del paciente y fecha de diagnóstico",
    x = "Fecha de Diagnóstico",
    y = "Edad (años)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p3)

# -----------------------------------------------------------------------------
# 5. AJUSTE DE DISTRIBUCIONES ESTADÍSTICAS
# -----------------------------------------------------------------------------

cat("\n📐 Ajustando distribuciones estadísticas...\n")

## 5.1 Distribución de Poisson para muertes diarias
cat("   • Analizando distribución de Poisson para muertes diarias...\n")

# Agregar datos diarios de fallecimientos
daily_deaths <- covid_data %>%
  filter(estado == "Fallecido", !is.na(fecha_diagnostico)) %>%
  mutate(fecha = as.Date(fecha_diagnostico)) %>%
  group_by(fecha) %>%
  summarise(deaths = n(), .groups = 'drop')

# Estimación del parámetro lambda
lambda_est <- mean(daily_deaths$deaths)
cat("   Lambda estimado (Poisson):", round(lambda_est, 4), "\n")

# Visualización de muertes diarias
p4 <- ggplot(daily_deaths, aes(x = fecha, y = deaths)) +
  geom_col(fill = "coral", alpha = 0.8, color = "darkred") +
  geom_hline(yintercept = lambda_est, color = "blue", 
             linetype = "dashed", size = 1.2) +
  labs(
    title = "Frecuencia de Muertes Diarias por COVID-19",
    subtitle = paste("Promedio diario (λ):", round(lambda_est, 2)),
    x = "Fecha",
    y = "Número de Muertes"
  ) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

print(p4)

## 5.2 Distribución Exponencial para tiempos hasta fallecimiento
cat("   • Analizando distribución exponencial para tiempos...\n")

# Preparar datos de tiempo
covid_data$fecha_sintomas <- as.Date(covid_data$fecha_sintomas, format = "%Y-%m-%d")
covid_data$fecha_fallecimiento <- as.Date(covid_data$fecha_fallecimiento, format = "%Y-%m-%d")

covid_data <- covid_data %>%
  mutate(dias_sintomas_muerte = as.numeric(fecha_fallecimiento - fecha_sintomas))

# Filtrar casos válidos
exponential_data <- covid_data %>%
  filter(!is.na(dias_sintomas_muerte), 
         dias_sintomas_muerte > 0,
         dias_sintomas_muerte <= 365)

if(nrow(exponential_data) > 0) {
  # Ajuste exponencial
  fit_exp <- fitdist(exponential_data$dias_sintomas_muerte, "exp")
  cat("   Parámetro de tasa estimado:", round(fit_exp$estimate, 4), "\n")
  
  # Visualización comparativa
  p5 <- ggplot(exponential_data, aes(x = dias_sintomas_muerte)) +
    geom_histogram(aes(y = after_stat(density)), 
                   binwidth = 5, fill = "lightgreen", 
                   color = "darkgreen", alpha = 0.7) +
    stat_function(fun = dexp, 
                  args = list(rate = fit_exp$estimate), 
                  color = "red", size = 1.5) +
    labs(
      title = "Ajuste de Distribución Exponencial",
      subtitle = "Tiempo desde síntomas hasta fallecimiento",
      x = "Días",
      y = "Densidad"
    )
  
  print(p5)
  print(summary(fit_exp))
}

# -----------------------------------------------------------------------------
# 6. INFERENCIA ESTADÍSTICA - PRUEBA T
# -----------------------------------------------------------------------------

cat("\n🔬 Realizando inferencia estadística...\n")

# Prueba t para diferencia de edades entre grupos
if(all(c("Recuperado", "Fallecido") %in% covid_data$estado)) {
  t_test_result <- t.test(edad ~ estado, 
                         data = covid_data, 
                         subset = estado %in% c("Recuperado", "Fallecido"))
  
  cat("📊 Resultados de la Prueba t de Student:\n")
  print(t_test_result)
  
  # Resumen interpretativo
  cat("\n📝 Interpretación:\n")
  if(t_test_result$p.value < 0.05) {
    cat("   ✓ Existe diferencia significativa en edad entre grupos (p < 0.05)\n")
  } else {
    cat("   ✗ No hay diferencia significativa en edad entre grupos (p ≥ 0.05)\n")
  }
}

# -----------------------------------------------------------------------------
# 7. ANÁLISIS DE CORRELACIÓN Y REGRESIÓN
# -----------------------------------------------------------------------------

cat("\n📈 Analizando correlaciones...\n")

# Preparar variable de días de recuperación
covid_data$fecha_recuperacion <- as.Date(covid_data$fecha_recuperacion, format = "%Y-%m-%d")
covid_data <- covid_data %>%
  mutate(dias_recuperacion = as.numeric(fecha_recuperacion - fecha_diagnostico))

# Test de correlación
valid_data <- covid_data %>%
  filter(!is.na(edad), !is.na(dias_recuperacion), dias_recuperacion > 0)

if(nrow(valid_data) > 10) {
  cor_result <- cor.test(valid_data$edad, valid_data$dias_recuperacion)
  
  cat("🔗 Correlación Edad vs. Días de Recuperación:\n")
  print(cor_result)
  
  # Visualización de correlación
  p6 <- ggplot(valid_data, aes(x = edad, y = dias_recuperacion)) +
    geom_point(alpha = 0.6, color = "darkblue") +
    geom_smooth(method = "lm", color = "red", alpha = 0.3) +
    labs(
      title = "Correlación: Edad vs. Tiempo de Recuperación",
      subtitle = paste("r =", round(cor_result$estimate, 3), 
                      ", p-valor =", round(cor_result$p.value, 4)),
      x = "Edad (años)",
      y = "Días de Recuperación"
    )
  
  print(p6)
}

# -----------------------------------------------------------------------------
# 8. SIMULACIÓN MONTE CARLO POR GRUPOS DE EDAD
# -----------------------------------------------------------------------------

cat("\n🎲 Ejecutando simulación Monte Carlo...\n")

# Definir grupos etarios
covid_data <- covid_data %>%
  mutate(grupo_edad = case_when(
    edad <= 18            ~ "0-18 años",
    edad <= 40            ~ "19-40 años", 
    edad <= 60            ~ "41-60 años",
    edad <= 80            ~ "61-80 años",
    TRUE                  ~ "81+ años"
  ))

# Calcular probabilidades por grupo
group_stats <- covid_data %>%
  filter(!is.na(grupo_edad), estado %in% c("Recuperado", "Fallecido")) %>%
  group_by(grupo_edad) %>%
  summarise(
    total = n(),
    muertes = sum(estado == "Fallecido"),
    recuperados = sum(estado == "Recuperado"),
    .groups = 'drop'
  ) %>%
  mutate(
    prob_muerte = muertes / total,
    prob_recuperacion = recuperados / total
  )

print(group_stats)

# Simulación Monte Carlo
set.seed(12345)  # Reproducibilidad
n_simulaciones <- 10000

monte_carlo_results <- group_stats %>%
  filter(total >= 5) %>%  # Solo grupos con suficientes casos
  rowwise() %>%
  mutate(
    simulaciones = list(rbinom(n_simulaciones, 1, prob_muerte))
  ) %>%
  unnest(simulaciones) %>%
  group_by(grupo_edad) %>%
  summarise(
    prob_muerte_real = first(prob_muerte),
    prob_muerte_sim = mean(simulaciones),
    ic_inferior = quantile(simulaciones, 0.025),
    ic_superior = quantile(simulaciones, 0.975),
    .groups = 'drop'
  )

# Visualización de resultados Monte Carlo
p7 <- monte_carlo_results %>%
  pivot_longer(cols = c(prob_muerte_real, prob_muerte_sim),
               names_to = "tipo", values_to = "probabilidad") %>%
  mutate(tipo = case_when(
    tipo == "prob_muerte_real" ~ "Observada",
    tipo == "prob_muerte_sim" ~ "Simulada"
  )) %>%
  ggplot(aes(x = grupo_edad, y = probabilidad, fill = tipo)) +
  geom_col(position = "dodge", alpha = 0.8) +
  labs(
    title = "Simulación Monte Carlo: Probabilidades de Fallecimiento",
    subtitle = "Comparación entre probabilidades observadas y simuladas",
    x = "Grupo de Edad",
    y = "Probabilidad de Fallecimiento",
    fill = "Tipo"
  ) +
  scale_fill_brewer(palette = "Set1") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p7)

# -----------------------------------------------------------------------------
# 9. RESUMEN FINAL Y EXPORTACIÓN DE RESULTADOS
# -----------------------------------------------------------------------------

cat("\n📋 Generando resumen final...\n")

# Crear resumen ejecutivo
resumen_ejecutivo <- list(
  fecha_analisis = Sys.Date(),
  total_casos = nrow(covid_data),
  estadisticas_edad = age_summary,
  grupos_edad = group_stats,
  correlacion_edad_recuperacion = if(exists("cor_result")) cor_result$estimate else NA,
  lambda_poisson = lambda_est,
  parametro_exponencial = if(exists("fit_exp")) fit_exp$estimate else NA
)

# Guardar resumen
saveRDS(resumen_ejecutivo, "resumen_analisis_covid.rds")

cat("✅ Análisis completado exitosamente!\n")
cat("📁 Resultados guardados en: resumen_analisis_covid.rds\n")
cat("📊 Gráficos generados y mostrados en pantalla\n")

# -----------------------------------------------------------------------------
# FIN DEL ANÁLISIS
# -----------------------------------------------------------------------------
```

## 🚀 Cómo Usar

### Ejecución Rápida

1. **Clonar el repositorio**
```bash
git clone https://github.com/tu-usuario/covid-analysis-r.git
cd covid-analysis-r
```

2. **Preparar datos**
```r
# Asegúrate de tener covid_data.csv en el directorio de trabajo
# con las columnas especificadas en la sección "Estructura de Datos"
```

3. **Ejecutar análisis**
```r
# Opción 1: Ejecutar todo el script
source("covid_analysis.R")

# Opción 2: Ejecutar por secciones (recomendado)
# Copia y pega las secciones una por una en RStudio
```

### Personalización

```r
# Cambiar parámetros de visualización
theme_set(theme_classic())  # Cambiar tema de gráficos

# Modificar grupos de edad
covid_data <- covid_data %>%
  mutate(grupo_edad = case_when(
    edad <= 30 ~ "Jóvenes",
    edad <= 60 ~ "Adultos", 
    TRUE ~ "Mayores"
  ))

# Ajustar número de simulaciones Monte Carlo
n_simulaciones <- 5000  # Menos iteraciones = más rápido
```

## 📊 Visualizaciones

El análisis genera 7 visualizaciones principales:

| Gráfico | Descripción | Variables |
|---------|-------------|-----------|
| **P1** | Histograma de edad con estadísticas centrales | `edad` |
| **P2** | Boxplot comparativo por estado | `edad`, `estado` |
| **P3** | Dispersión temporal de diagnósticos | `fecha_diagnostico`, `edad` |
| **P4** | Barras de muertes diarias | `fecha`, `deaths` |
| **P5** | Ajuste exponencial comparativo | `dias_sintomas_muerte` |
| **P6** | Correlación edad-recuperación | `edad`, `dias_recuperacion` |
| **P7** | Resultados Monte Carlo por grupo | `grupo_edad`, `probabilidad` |

## 🔬 Metodología

### Distribuciones Estadísticas

#### 📐 Distribución de Poisson
Utilizada para modelar el número de muertes diarias:
```
P(X = k) = (λᵏ × e⁻λ) / k!
```
- **λ (lambda)**: Promedio de muertes por día
- **Aplicación**: Eventos raros en tiempo fijo

#### 📈 Distribución Exponencial  
Aplicada para tiempos entre eventos:
```
![image](https://github.com/user-attachments/assets/b3e3927d-3b34-4bb1-9630-002bfc4eeabe)

```
- **λ (tasa)**: Inverso del tiempo promedio
- **Aplicación**: Tiempo hasta fallecimiento

### 🎲 Simulación Monte Carlo
- **Método**: Muestreo probabilístico con 10,000 iteraciones
- **Objetivo**: Estimar probabilidades por grupo de edad
- **Ventajas**: Cuantifica incertidumbre y genera intervalos de confianza

### 🔬 Pruebas Estadísticas
- **Prueba t de Student**: Comparación de medias entre grupos
- **Correlación de Pearson**: Relación lineal entre variables
- **Nivel de significancia**: α = 0.05

## 📝 Resultados

### Estadísticas Descriptivas Típicas

```r
# Ejemplo de salida esperada
$estadisticas_edad
  n_total n_validos n_faltantes media mediana moda desv_std varianza
1    1000       980          20  52.3      51   45     18.7    349.7
  minimo maximo   q1   q3 rango_iq
1      1     95 38.5 66.2     27.7
```

### Interpretación de Resultados

- **📊 Media > Mediana**: Distribución sesgada hacia edades mayores
- **📈 Correlación positiva**: Mayor edad → Mayor tiempo de recuperación
- **🎲 Monte Carlo**: Probabilidades más altas en grupos de mayor edad

## ⚠️ Consideraciones Importantes

### Limitaciones del Análisis
- **Datos faltantes**: El código maneja automáticamente valores NA
- **Tamaño de muestra**: Grupos pequeños (n < 5) se excluyen de simulaciones
- **Supuestos estadísticos**: Se asume normalidad para pruebas paramétricas

### Recomendaciones
- **Validar datos**: Revisar fechas y rangos de edad antes del análisis
- **Interpretar con contexto**: Los resultados dependen de la calidad de los datos
- **Actualizar regularmente**: Repetir análisis con nuevos datos


### Ideas para Contribuir
- 📊 Nuevas visualizaciones
- 🔬 Pruebas estadísticas adicionales
- 📈 Modelos predictivos
- 🎨 Mejoras en el diseño de gráficos
- 📝 Documentación y ejemplos


# 🔷 Anexos

## Fórmulas Estadísticas

### 1. Medidas de Tendencia Central

#### Media Aritmética

- **Poblacional:**
  
  ![image](https://github.com/user-attachments/assets/4d75990d-968c-4ed2-9a9b-5b49a6ce5a63)

- **Muestral:**
  
  ![image](https://github.com/user-attachments/assets/fd574651-4c6a-48aa-93cb-53c35ec66480)

> **Descripción:** Suma todos los valores del conjunto de datos y los divide por la cantidad total. Es útil para determinar el valor promedio de la variable.

#### Mediana

- **Si n es impar:**
  
  ![image](https://github.com/user-attachments/assets/33d109cf-e36e-4244-95d2-ae4199a5e674)

- **Si n es par:**
  
  ![image](https://github.com/user-attachments/assets/7a589403-70cd-4bd8-baed-c030d5e72baf)

> **Descripción:** Representa el valor central del conjunto, que divide la distribución en dos mitades iguales.

#### Moda

> **Descripción:** Es el dato que más se repite en el conjunto. No tiene fórmula algebraica directa; se determina por conteo de frecuencias.

---

### 2. Medidas de Dispersión

#### Rango

![image](https://github.com/user-attachments/assets/e0b14140-089b-4c64-874e-c75171e68001)

> **Descripción:** Indica la diferencia entre el valor máximo y el mínimo.

#### Varianza

- **Poblacional:**
  
  ![image](https://github.com/user-attachments/assets/57811aa8-bfc7-409e-9f3a-c77ff631399b)

- **Muestral:**
  
  ![image](https://github.com/user-attachments/assets/2feda120-9218-4043-adea-94349bbc56c1)

> **Descripción:** Mide la dispersión de los datos respecto a la media.

#### Desviación Estándar

- **Poblacional:**
  
  ![image](https://github.com/user-attachments/assets/837028cf-ff04-44ef-a739-fcc6d68969d8)

- **Muestral:**
  
  ![image](https://github.com/user-attachments/assets/ba45482f-25cb-4942-b3c2-f2200489a21e)

> **Descripción:** Es la raíz cuadrada de la varianza.

#### Coeficiente de Variación (CV)

![image](https://github.com/user-attachments/assets/072c56ab-a755-4963-a436-b1964ba064ad)

> **Descripción:** Permite comparar la dispersión relativa entre diferentes conjuntos de datos.

---

### 3. Medidas de Relación y Correlación

#### Coeficiente de Correlación de Pearson

![image](https://github.com/user-attachments/assets/b4483f5f-0b37-4779-ac61-9f0c7c1abc86)

> **Descripción:** Evalúa la fuerza y dirección de una relación lineal entre dos variables (valor entre -1 y 1).

#### Regresión Lineal Simple

![image](https://github.com/user-attachments/assets/eb768c66-98a5-4808-9e87-73139d3bdd45)

**Donde:**

![image](https://github.com/user-attachments/assets/5a4e43d4-7859-4dc8-a0b6-fe9bce3d8da6)

> **Descripción:** Modela la relación entre una variable independiente x y una dependiente y.

---

### 4. Distribuciones de Probabilidad

#### Distribución Normal

![image](https://github.com/user-attachments/assets/0a022399-eae3-4275-ba16-55c233ad594e)

> **Descripción:** Modelo común en estadística; datos agrupados alrededor de la media.

#### Distribución de Poisson

![image](https://github.com/user-attachments/assets/ade5563a-9b09-431e-aee4-dcb5b50beeba)

> **Descripción:** Probabilidad de observar k eventos en un intervalo, con tasa promedio λ (lambda).

#### Distribución Exponencial

![image](https://github.com/user-attachments/assets/6dd65b36-1322-4e92-bfa7-5861b15d0974)

> **Descripción:** Modela el tiempo entre eventos en procesos de Poisson.

---

### 5. Inferencia Estadística

#### Intervalo de Confianza para la Media

- **Varianza conocida:**
  
  ![image](https://github.com/user-attachments/assets/51d98fb0-2f6f-4f46-92d0-a1ff1a9f314b)

- **Varianza desconocida:**
  
  ![image](https://github.com/user-attachments/assets/22eb1fe9-b2d8-441f-9554-cdcb503af217)

> **Descripción:** Estima el rango donde se espera que se encuentre la media poblacional.

#### Prueba t para una muestra

![image](https://github.com/user-attachments/assets/8c10149b-7f31-45c4-811a-d492d436f453)

> **Descripción:** Contrasta hipótesis sobre la media cuando la varianza es desconocida.

#### Prueba Chi-Cuadrado (Bondad de Ajuste)

![image](https://github.com/user-attachments/assets/a312ea56-d7b6-45cd-84f3-745a6584069b)

> **Descripción:** Evalúa si los datos observados difieren significativamente de lo esperado.

---

### 6. Medidas de Forma

#### Coeficiente de Asimetría (Skewness)

![image](https://github.com/user-attachments/assets/150e0cf1-8e2f-43fb-a5a8-2c1d1fdb08b1)

> **Descripción:** Indica la simetría de la distribución (positiva: derecha; negativa: izquierda).

#### Coeficiente de Curtosis

![image](https://github.com/user-attachments/assets/a7e18baf-0f76-4ffb-a126-afb7d4a55433)

> **Descripción:** Mide el "apuntamiento" de la distribución.
> - **Curtosis > 0:** Distribución más concentrada (leptocúrtica)
> - **Curtosis < 0:** Distribución más plana (platicúrtica)
> - **Curtosis = 0:** Distribución normal (mesocúrtica)

---

### 7. Teorema de Bayes

![image](https://github.com/user-attachments/assets/eca402b1-2d7f-43f5-8ea8-46d3281fbb2c)

## 📧 Autores

- **Nombre**: Anderson Mauricio Ordoñez Zuñiga
- **Nombre**: Benjamin Andres Urbano Zuñiga
- **rol**: Estudiantes de Ingenieria de Software

## 📜 Licencia

Este proyecto está bajo la Licencia MIT - ver el archivo [LICENSE](LICENSE) para detalles.

> **Nota Importante**: Este análisis es con fines educativos y de investigación. Los resultados deben interpretarse en el contexto Probalistico y Estadistico.
