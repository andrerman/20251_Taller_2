# ---
# title: "Taller 2 SERIES"
# author: "Maria Basto, Juan Pablo Galindo, Sebastián García, Ricardo Mancipe"
# date: '2025-03-29'
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
path2 = "c:/data/"

# Working directory:
setwd(path2)            # / usd

rm(list = ls()) 				# Elimina los objetos
cat("\014")     				# Limpia pantalla
set.seed(2025)

################################################################################
#       3. REMESAS
################################################################################

data <- read_excel("Remesas.xlsx", sheet = 1)        # Importar la primera hoja
data %>%                                            # Cambia el nombre
  rename(Remesas   = `Remesas de trabajadores`) -> data

data <- na.omit(data)             # Elimina cualquier 'NA'


# Convierte la tibble (una v. mejorada del dataframe) a tsibble
data %>%
  mutate(Fecha = yearquarter(Fecha))  %>%
  as_tsibble(index = Fecha) -> data

class(data)

autoplot(data)

# TRM
################################################################################
data2 <- read_excel("TRM.xlsx", sheet = 1)        # Importar la primera hoja

data2 %>%                                         # Cambia el nombre
  rename(Fecha = `Fecha (dd/mm/aaaa)`,
         TRM   = `Tasa de cambio representativa del mercado (TRM)`) %>%
  mutate(Fecha = ymd(Fecha))  %>%
  as_tsibble(index = Fecha) -> data2


# Agrupar por trimestre y calcular el promedio del TRM
data_trimestral <- data2 %>%
  index_by(Trimestre = yearquarter(Fecha)) %>%  # Agrupar por trimestre
  summarise(TRM_Promedio = mean(TRM, na.rm = TRUE)) %>% # Calcular el promedio
  filter(year(Trimestre) >= 2000)

autoplot(data_trimestral)

###########################################################################

data_trimestral <- data_trimestral %>% slice(-n())  # Elimino el ultimo reg



# Punto i. Inicio desarrollo --------------------------------------------------
# Aplicar logaritmo natural a la variable "Remesas"

data <- data %>%
  mutate(RemesasCOP = Remesas * pull(data_trimestral, TRM_Promedio)) %>%
  mutate(RemesasCOP_M = RemesasCOP/1e6)

# Visualización de la serie transformada
autoplot(data, RemesasCOP_M) +
  ggtitle("Ingresos por Remesas en Colombia") +
  ylab("Billones de COP")
# ggsave("3.i.pdf", width = 10, height = 7) #, device = cairo_pdf

# Punto iii. Los modelos --------------------------------------------------
# 1. Seasonal
data %>%
  gg_season(RemesasCOP_M, labels = "both") +
  labs(y = "Billones de COP",
       title = "Gráfica estacional: Ingresos por Remesas en Colombia") +
  expand_limits(x = ymd("1972-12-28", "1973-12-04"))
ggsave("3.iii.2.pdf", width = 10, height = 7) #, device = cairo_pdf


# 2. Estimación de parámetros c y theta por MCO

# Definir las variables para la regresión
data_reg <- data %>%
  mutate(Fecha_num = as.numeric(Fecha),
         TIME  = 1:100,
         TIME2 = TIME^2,
         MESN   = month(Fecha, label = T), 
         TRIMESTRE   = quarter(Fecha),
         TRIM4 = ifelse(TRIMESTRE == 4, 1, 0),
         TRIMESTREC = factor(quarter(Fecha), levels = 1:4, labels = c("T1", "T2", "T3", "T4"))
  )



# Estimar el modelo cuadratico por MCO
modelo_1 <- lm(RemesasCOP ~ TIME , data = data_reg)
summary(modelo_1)     # Resultados
 

modelo_2 <- lm(RemesasCOP ~ TIME + TIME2, data = data_reg)
summary(modelo_2)     # Resultados

modelo_3 <- lm(RemesasCOP ~ TIME + TIME2 + TRIMESTREC, data = data_reg)
summary(modelo_3)     # Resultados


# 3. Estimacion h 13 ---------------------------------------------------
# Crear un dataframe para almacenar el pronóstico

# Extraer coeficientes de cada modelo y guardarlos en un vector con nombres
coef_1 <- setNames(coef(modelo_1), names(coef(modelo_1)))
coef_2 <- setNames(coef(modelo_2), names(coef(modelo_2)))
coef_3 <- setNames(coef(modelo_3), names(coef(modelo_3)))



# Generar pronóstico 
data_reg1 <- data_reg %>%
  mutate(Remesas1 = coef_1[1] + coef_1[2] * TIME ,
         Remesas2 = coef_2[1] + coef_2[2] * TIME + coef_2[3] * TIME2,
         Remesas3 = coef_3[1] + coef_3[2] * TIME + coef_3[3] * TIME2 + coef_3[4] * (MESN == "Apr") + coef_3[5] * (MESN == "Jul")+ coef_3[6] * (MESN == "Oct")) 

data_reg1 %>%
  autoplot(RemesasCOP) +
  geom_line(aes(y=Remesas1), colour="red" ) +
  geom_line(aes(y=Remesas2), colour="blue" )+
  geom_line(aes(y=Remesas3), colour="green" )


# EL otro método --------------------------------------------------------------

data_reg1 %>%
  ggplot(aes(x = TIME, y = RemesasCOP)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE )

fit_modelo_1 <- data_reg1 %>%
  model(TSLM(RemesasCOP ~ TIME)) %>% 
  report()
fit_modelo_2 <- data_reg1 %>%
  model(TSLM(RemesasCOP ~ TIME + TIME2)) %>% 
  report()
augment(fit_modelo_2) %>%
  ggplot(aes(x = Fecha)) + 
  geom_line(aes(y  = RemesasCOP, colour="Data"  )) +
  geom_line(aes(y  = .fitted,    colour="Fitted")) +
  labs(y = "Remesas en millones de COP", title = "Remesas en Colombia") 
ggsave("3.iii.pdf", width = 10, height = 7) #, device = cairo_pdf
fit_modelo_3 <- data_reg1 %>%
  model(TSLM(RemesasCOP ~ TIME + TIME2 + TRIMESTREC)) %>% 
  report()


fit_modelo_4 <- data_reg %>%
  model(TSLM(log(RemesasCOP) ~ TIME)) %>% 
  report()


fit_modelo_4 %>% gg_tsresiduals()

fit_modelo_5 <- data_reg1 %>%
  model(TSLM(log(RemesasCOP) ~ trend() + TRIMESTREC )
        ) %>% 
  report()
augment(fit_modelo_5) %>%
  ggplot(aes(x = Fecha)) + 
  geom_line(aes(y  = RemesasCOP, colour="Data"  )) +
  geom_line(aes(y  = .fitted,    colour="Fitted"))
fit_modelo_5 %>% gg_tsresiduals()
# El modelo 6 -----------------------------------------------------------------
fit_modelo_6 <- data_reg %>%
  model(TSLM(log(RemesasCOP) ~ trend() + TRIM4 )
        ) %>% 
  report()

fit_modelo_6 %>% gg_tsresiduals()

par(mfrow=c(1,2))                                 # 1 fila, 2 columnas
augment(fit_modelo_6) %>%
  ggplot(aes(x = Fecha)) + 
  geom_line(aes(y  = RemesasCOP, colour="Data"  )) +
  geom_line(aes(y  = .fitted,    colour="Fitted"))
augment(fit_modelo_4) %>%
  ggplot(aes(x = Fecha)) + 
  geom_line(aes(y  = RemesasCOP, colour="Data"  )) +
  geom_line(aes(y  = .fitted,    colour="Fitted")) +
  labs(y = "Remesas en millones de COP", title = "Remesas en Colombia con Modelo 4") +
  annotate("text", x = min(augment(fit_modelo_4)$Fecha), 
           y = max(augment(fit_modelo_4)$RemesasCOP), 
           label = paste0("y_t = ", fit_modelo_4[[1]][[1]][["fit"]][["coefficients"]][1], "  * e^(", fit_modelo_4[[1]][[1]][["fit"]][["coefficients"]][2], " * t)"),
           hjust = 0, size = 4, color = "#00BFC4")
ggsave("3.iii.3.pdf", width = 10, height = 7) #, device = cairo_pdf

glance(fit_modelo_1) %>%
  select(adj_r_squared, CV, AIC, AICc, BIC)
glance(fit_modelo_2) %>%
  select(adj_r_squared, CV, AIC, AICc, BIC)
glance(fit_modelo_3) %>%
  select(adj_r_squared, CV, AIC, AICc, BIC)
glance(fit_modelo_4) %>%
  select(adj_r_squared, CV, AIC, AICc, BIC)
glance(fit_modelo_5) %>%
  select(adj_r_squared, CV, AIC, AICc, BIC)
glance(fit_modelo_6) %>%
  select(adj_r_squared, CV, AIC, AICc, BIC)




augment(fit_modelo_2) %>%
  ggplot(aes(x = Fecha)) + 
  geom_line(aes(y  = RemesasCOP, colour="Data"  )) +
  geom_line(aes(y  = .fitted,    colour="Fitted"))

fit_modelo_2 %>% gg_tsresiduals()

augment(fit_modelo_3) %>%
  ggplot(aes(x = Fecha)) + 
  geom_line(aes(y  = RemesasCOP, colour="Data"  )) +
  geom_line(aes(y  = .fitted,    colour="Fitted"))

fit_modelo_3 %>% gg_tsresiduals()

augment(fit_modelo_2) %>%               # Una regresion frente al fitted
  ggplot(aes(x = RemesasCOP, y  = .fitted)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1)
# iv. Serie - S - T ------------------------------------------------------------

fit_modelo_4 %>% gg_tsresiduals()
  ggsave("3.iv.pdf", width = 10, height = 7) #, device = cairo_pdf
  
# v. Serie - S - T ------------------------------------------------------------
residuos <- residuals(fit_modelo_4)  # Extraer los residuos
residuos_df <- data.frame(residuos = residuos)
vector_residuos <- residuos %>% pull(residuos..resid)
  
qqnorm(vector_residuos, main = "QQ-plot de los residuos", col = "blue")
qqline(vector_residuos, col = "red", lwd = 2)  # Agrega línea de referencia


ggplot(residuos, aes(sample = .resid)) +
  stat_qq(color = "blue") +  # Puntos QQ
  stat_qq_line(color = "red", lwd = 1) +  # Línea de referencia
  labs(title = "QQ-plot de los residuos", x = "Cuantiles teóricos", y = "Cuantiles de los residuos") +
  theme_minimal()

# Descomposición STL------------------------------------------------------------
dcmp <- data %>%
  model(stl = STL(RemesasCOP)) 
components(dcmp)


components(dcmp) %>%
  as_tsibble()  %>%
  autoplot(RemesasCOP, colour="gray") +
  geom_line(aes(y=trend, colour = "blue"))


components(dcmp) %>% autoplot()
  
# DESCOMPOSICIÓN STL-----------------------------------------------------------




