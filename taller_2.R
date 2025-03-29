# ---
# title: "Taller 1 SERIES"
# author: "Maria Basto, Juan Pablo Galindo, Sebastián García, Ricardo Mancipe"
# date: '2025-02-16'
# output:
#   html_document: default
# ---

# Librerías:
library(readxl)
library(fpp3)
library(dplyr)
library(lubridate)
library(forecast)
library(ggpubr)



# File Paths: 

# Working directory:
# setwd(main)

rm(list = ls()) 				# Elimina los objetos
cat("\014")     				# Limpia pantalla
path2 = "c:/data/"
set.seed(2025)

################################################################################
#       6. TRIBUTARIOS
################################################################################

data <- read_excel("Tributarios.xlsx", sheet = 1)  # Importar la primera hoja

# Convierte la tibble (una v. mejorada del dataframe) a tsibble
data %>%
  mutate(Fecha = yearmonth(Fecha))  %>%
  as_tsibble(index = Fecha) -> data


# Punto 6.1 Inicio desarrollo --------------------------------------------------
# Aplicar logaritmo natural a la variable "Tributarios(SA)"
data <- data %>%
  mutate(Log_Tributarios = log(`Tributarios (SA)`))

# Calcular la primera diferencia y multiplicar por 100 (variación %)
data <- data %>%
  mutate(Diff_Log_Tributarios = 100 * difference(Log_Tributarios))

# Visualización de la serie transformada
autoplot(data, Diff_Log_Tributarios) +
  ggtitle("Variación Ingresos Tributarios en Colombia") +
  ylab("Porcentaje")
ggsave("g6.1.a.pdf", width = 10, height = 7) #, device = cairo_pdf

# Estadísticas descriptivas
stats <- summary(data$Diff_Log_Tributarios)
stats["SD"] <- sd(data$Diff_Log_Tributarios, na.rm = TRUE)  # Desviación est.
stats["CV"] <- sd(data$Diff_Log_Tributarios, na.rm = TRUE)/stats["Mean"]  # CV



hist(data$Diff_Log_Tributarios)
boxplot(data$Diff_Log_Tributarios)

# 6.1.b. Funcion de autocorrelacion
data %>% ACF(Diff_Log_Tributarios, lag_max = 20)
data %>% ACF(Diff_Log_Tributarios, lag_max = 20) %>% autoplot() + 
  labs(title = "Ingresos tributarios en Colombia")
ggsave("g6.1.b.pdf", width = 10, height = 7)

reciente <- data %>%
  filter(year(Fecha) >= 2000)
reciente %>%
  gg_lag(Diff_Log_Tributarios, geom = "point") +   
  labs(x  = "lag(Pesos, k)")
ggsave("g6.1.b2.pdf", width = 10, height = 7)

# 6.1.c. Funcion de autocorrelacion parcial
data %>% PACF(Diff_Log_Tributarios, lag_max = 20)   
data %>% PACF(Diff_Log_Tributarios, lag_max = 20) %>% autoplot() + 
  labs(title = "Ingresos tributarios en Colombia") 
  ggsave("g6.1.c.png", width = 10, height = 7, dpi = 600)
  
# 6.1.d. Estimación de parámetros c y theta por MCO
  
# Definir las variables para la regresión
data <- data %>%
  mutate(Diff_y_t_1 = lag(Diff_Log_Tributarios, 1),
         Diff_y_t_2 = lag(Diff_Log_Tributarios, 2))
  
# Eliminar valores NA generados por el desfase (lag)
  data_reg <- na.omit(data)
  
# Estimar el modelo AR(2) por MCO usando lm()
modelo_ar2 <- lm(Diff_Log_Tributarios ~ Diff_y_t_1 + Diff_y_t_2, data = data_reg)
  summary(modelo_ar2)     # Resultados


# 6.1.e Estimacion h 13 ---------------------------------------------------
# Crear un dataframe para almacenar el pronóstico
h        <- 13                         # Número de períodos a predecir
forecast_values <- numeric(h)          # Vector vacío para almacenar pronósticos
last_y_1 <- tail(data_reg$Diff_Log_Tributarios, 1)
last_y_2 <- tail(data_reg$Diff_y_t_1, 1)  # Último (1) valor Diff_y_t para inic
c        <- coef(modelo_ar2)[1]        # Intercepto # Extraer coeficientes del 
theta_1  <- coef(modelo_ar2)[2]        # Coeficiente theta_1
theta_2  <- coef(modelo_ar2)[3]        # Coeficiente theta_2
  
  # Generar pronóstico iterativamente (pronóstico recursivo)
  for (t in 1:h) {
    new_y <- c + theta_1 * last_y_1 + theta_2 * last_y_2  # Ecuación AR(2)
    forecast_values[t] <- new_y                           # Guardar pronóstico
                                       # Actualizar valores rezagados para la ste iteración
    last_y_2 <- last_y_1
    last_y_1 <- new_y
  }
  
  last_date <- max(data$Fecha)  # Obtener la última fecha registrada en la serie temporal
  last_date <- as.Date(last_date) %m+% months(1)
                                # Crear una secuencia de 13 meses adelante
  forecast_dates <- yearmonth(seq(as.Date(last_date), by = "1 month", length.out = h))
                                # Crear dataframe con fechas y pronósticos
  forecast_df <- tibble(Fecha = forecast_dates, Pronostico = forecast_values)
  
  print(forecast_df) # Mostrar el pronóstico

v <- numeric(h)
ayer_nominal <- tail(data$`Tributarios (SA)`, 1)

for (t in 1:h) {
  new_nominal <- exp(forecast_df$Pronostico[t]/100 + log(ayer_nominal))  # Ecuación AR(2)
  v[t] <- new_nominal                                       # Guardar pronóstico
                            # Actualizar valores rezagados para la ste iteración
  ayer_nominal <- new_nominal
}

# Añadir la nueva columna con valores nominales
forecast_df <- forecast_df %>%
  mutate(`Tributarios (SA)` = v)

forecast_df <- forecast_df %>%
  mutate(
    Tributarios = NA,  # No hay datos históricos
    Log_Tributarios = NA,
    Diff_Log_Tributarios = NA,
    Diff_y_t_1 = NA,
    Diff_y_t_2 = NA
  )

# Unir ambas tablas
data_completa <- bind_rows(data_reg, forecast_df)
tail(data_completa)

# Calcular el acumulado por año
acumulados <- data_completa %>%
  mutate(Año = year(Fecha)) %>%  # Extraer el año de la fecha
  filter(Año %in% c(2024, 2025)) %>%  # Filtrar solo 2024 y 2025
  group_by(Año) %>%
  summarise(Acumulado = sum(`Tributarios (SA)`, na.rm = TRUE)) 

# Calcular la diferencia absoluta y la variación porcentual
comparacion <- acumulados %>%
  summarise(
    Acumulado_2024 = sum(Acumulado[Año == 2024], na.rm = TRUE),
    Acumulado_2025 = sum(Acumulado[Año == 2025], na.rm = TRUE),
    Diferencia = Acumulado_2025 - Acumulado_2024,
  )
diff_2024_2025 <- sum(comparacion$Diferencia, na.rm = TRUE)  # Sumar   NA
total_2025 <- sum(comparacion$Acumulado_2025, na.rm = TRUE)  # Sumar   NA
total_2024 <- sum(comparacion$Acumulado_2024, na.rm = TRUE)  # Sumar   NA
var_perc = diff_2024_2025/total_2024


m        <- 5                         
tabla_resumen <- numeric(5) 
tabla_resumen <- tibble(total_2024, total_2025, diff_2024_2025, var_perc)
tabla_resumen

data_completa <- data_completa %>%
  select(-c(Tributarios, Log_Tributarios, Diff_Log_Tributarios, Diff_y_t_1, Diff_y_t_2))
tail(data_completa)

rm(comparacion, acumulados, forecast_df, modelo_ar2, reciente, tabla_resumen)
rm(list = ls(pattern = "^data_"))
rm(ayer_nominal, c, diff_2024_2025, forecast_dates, forecast_values, h, 
   last_date, last_y_1, last_y_2, m, new_nominal, new_y, path2, stats, 
   t, theta_1, theta_2, total_2024, total_2025, v, var_perc)
gc()  # Libera memoria
while (dev.cur() > 1) dev.off()

# 6.2 ---------------------------------------------------------------------
# Calcula los modelos
arma_fit <- data %>%
  filter(!is.na(Diff_Log_Tributarios)) %>%    # Filtra los NA
  model(arima101 = ARIMA(Diff_Log_Tributarios ~ 1 + pdq(1, 0, 1) + PDQ(0, 0, 0) ),
        stepwise = ARIMA(Diff_Log_Tributarios),
        search =   ARIMA(Diff_Log_Tributarios, stepwise = FALSE))

# Resumenes de modelos
tidy(arma_fit)                          # Muestra los coeficientes y los ee
glance(arma_fit)                        # Muestra sigma2 loglik AIC BIC roots

data %>%                                # Muestra individual
  model(arima101 = ARIMA(Diff_Log_Tributarios ~ 1 + pdq(1, 0, 1) + PDQ(0, 0, 0) )) %>%
  report()


# Grafica los residuales
arma_fit %>%
  select(arima101) %>% 
  gg_tsresiduals()


# 6.2.c -------------------------------------------------------------------

# Extrae los residuales del modelo ARIMA(1,0,1)
residuals_arima <- residuals(arma_fit, type = "response") %>%
  filter(.model == "arima101") %>% 
  pull(.resid)  # Extrae los valores de los residuales

#  QQ con ggplot2
ggplot(data.frame(residuals = residuals_arima), aes(sample = residuals)) +
  stat_qq() + 
  stat_qq_line(color = "red") +
  ggtitle("QQ-Plot de los Residuales") + theme_minimal()

# Histograma ayuda a ver la forma de los residuales
ggplot(data.frame(residuals = residuals_arima), aes(x = residuals)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "grey", color = "black") +
  geom_density(color = "red") +
  ggtitle("Distribución de los Residuales") +
  theme_minimal()


# Prueba de normalidad de Shapiro-Wilk
shapiro.test(residuals_arima)

pdf("g6.2.c.pdf", width = 10, height = 5)  # Ajusta tamaño según necesites
par(mfrow = c(1, 2))  # 1 fila, 2 columnas

# QQ-Plot
qqnorm(residuals_arima, main = "QQ-Plot de los Residuales", pch = 19, col = "blue")
qqline(residuals_arima, col = "red", lwd = 2)

# Histograma
hist(residuals_arima, probability = TRUE, col = "lightblue", main = "Histograma de Residuales")
lines(density(residuals_arima), col = "red", lwd = 2)

dev.off()


# 6.2.d Hacer el pronostico -----------------------------------------------

# Filtrar los datos observados para 2023 y 2024
data_obs <- data %>%
  filter(!is.na(Diff_Log_Tributarios)) %>%
  filter(year(Fecha) %in% c(2023, 2024))  # Asegura que Month es una fecha

# Generar pronósticos desde dic 2024 hasta dic 2025
forecast_arima <- arma_fit %>%
  select(arima101) %>%
  forecast(h = 13)  # 13 meses desde dic 2024 a dic 2025

# Unir datos observados y pronóstico
data_plot <- data_obs %>%
  select(Fecha, Diff_Log_Tributarios) %>%
  rename(Value = Diff_Log_Tributarios) %>%
  mutate(Type = "Observado") %>%
  bind_rows(
    forecast_arima %>%
      as_tibble() %>%
      select(Fecha, .mean) %>%
      rename(Value = .mean) %>%
      mutate(Type = "Pronóstico")
  )

# Graficar
ggplot(data_plot, aes(x = Fecha, y = Value, color = Type)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Evolución y Pronóstico de la Primera Diferencia del Logaritmo de los Ingresos Tributarios",
       y = "Primera Diferencia del Logaritmo",
       color = "Tipo") +
  theme_minimal()
ggsave("g6.2.d.pdf", width = 10, height = 7, dpi = 600)


# 6.2.e Pronostico Total --------------------------------------------------
# Extraer el último valor observado en 2024
ultimo_valor_2024 <- data %>%
  filter(year(Fecha) == 2024) %>%
  arrange(desc(Fecha)) %>%
  slice(1) %>%
  pull(`Tributarios (SA)`)  # Ingresos originales

# Generar el pronóstico de la primera diferencia del logaritmo
forecast_arima <- arma_fit %>%
  select(arima101) %>%
  forecast(h = 13)  # 13 meses para 2024 y 2025


# Función para invertir logaritmo  :)
invertir_log <- function(primer_valor, diferencias) {
  # Suma acumulativa de las diferencias logarítmicas
  log_nivel <- cumsum(diferencias)/100 + log(primer_valor)
  
  # Volver a nivel original
  return(exp(log_nivel))
}


# Convertir el forecast en tibble y aplicar la función
forecast_total <- forecast_arima %>%
  as_tibble() %>%
  mutate(Tributarios_Pronosticados = invertir_log(ultimo_valor_2024, .mean)) %>%
  select(Fecha, Tributarios_Pronosticados)


# Mostrar los primeros valores
print(forecast_total)
