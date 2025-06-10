install.packages("eph")
install.packages("tidyverse")
install.packages("nortest")
install.packages("patchwork")
library(patchwork)
library(eph)
library(tidyverse)
library(ggplot2)
library(scales)
library(dplyr)
library(DescTools)
library(nortest)

# Descargar los microdatos del primer trimestre de 2024
datos <- get_microdata(year = 2024, trimester = 1, type = "individual")

# -------------------------------------------------------------------
#  Filtramos los datos para incluir solo personas en edad laboral (18-65 años)
# -------------------------------------------------------------------
datos_edad_entre_18y65 <- datos %>%
  filter(CH06 >= 18 & CH06 <= 65) %>%
  select(CH06, CH15, P47T, NIVEL_ED, ESTADO)

# -------------------------------------------------------------------
# Creamos un subconjuntos para argentinos y extranjeros
# -------------------------------------------------------------------

argentinos_entre_18y65 <- datos_edad_entre_18y65 %>%
  filter(CH15 %in% c("1", "2", "3")) %>%
  mutate(origen = "Argentinos")

extranjeros_entre_18y65 <- datos_edad_entre_18y65 %>%
  filter(CH15 %in% c("4", "5")) %>%
  mutate(origen = "Extranjeros")

# Combinar ambos subconjuntos
datos_combinados_entre_18y65 <- bind_rows(argentinos_entre_18y65, extranjeros_entre_18y65)

# ---------------------------------------------------------------------------------------------------------------------------------
# Plantear intervalo de confianza de la media del ingreso total individual tanto de argentinos y extranjeros
# ---------------------------------------------------------------------------------------------------------------------------------

#Vamos a tomar los datos de ingresos sin filtrar por limite superior
ingresos_argentinos <- (argentinos_entre_18y65 %>% filter(P47T >= 0))$P47T
ingresos_extranjeros <-(extranjeros_entre_18y65 %>% filter(P47T >= 0))$P47T
summary(ingresos_argentinos)
summary(ingresos_extranjeros)

#histograma
hist(ingresos_argentinos,
     main = "Histograma de P47T",
     xlab = "Ingreso (P47T)",
     ylab = "Frecuencia",
     col = "skyblue",
     breaks = 100)

# Transformación logarítmica para mejorar la visualizacion
argentinos_entre_18y65 %>%
  filter(P47T > 0) %>%  # evitar log(0)
  ggplot(aes(x = log(P47T))) +
  geom_histogram(binwidth = 0.2, fill = "steelblue", color = "white") +
  labs(title = "Histograma log(P47T)",
       x = "Logaritmo del ingreso",
       y = "Frecuencia") +
  theme_minimal()

# qq-plot
qqnorm((argentinos_entre_18y65 %>% filter(P47T > 0))$P47T)
qqline((argentinos_entre_18y65 %>% filter(P47T > 0))$P47T, col = "red")

# Como los ingresos suelen estar sesgados, lo mejor es hacer el Q-Q plot sobre el logaritmo:
qqnorm(log((argentinos_entre_18y65 %>% filter(P47T > 0))$P47T))
qqline(log((argentinos_entre_18y65 %>% filter(P47T > 0))$P47T), col = "blue")

# Aplicar el test de Anderson-Darling
ad.test(ingresos_argentinos)
ad.test(ingresos_extranjeros)

# ambas muestras no pasan el test de normalidad, solo nos queda asumir que n es lo suficientemente grande
MeanCI(x = ingresos_argentinos, conf.level = 0.95)
MeanCI(x = ingresos_extranjeros, conf.level = 0.95)

# Conclusion:  
# Con un nivel de confianza del 95%, como los intervalos se superponen, no podemos concluir que alguno de los promedios poblacionales es mayor que el otro.


# ---------------------------------------------------------------------------------------------------------------------------------
# Plantear intervalo de confianza de la media de la edad tanto de argentinos y extranjeros para ver si se solapan los intervalos
# ---------------------------------------------------------------------------------------------------------------------------------

summary(argentinos_entre_18y65$CH06)
summary(extranjeros_entre_18y65$CH06)

# Función para crear QQ plots
create_qqplot <- function(data, group_name) {
  ggplot(data, aes(sample = CH06)) +
    stat_qq() +
    stat_qq_line(color = ifelse(group_name == "Argentinos", "blue", "red")) +
    labs(title = paste("Q-Q Plot Edad (", group_name, ")", sep = ""),
         x = "Cuantiles teóricos",
         y = "Cuantiles muestrales") +
    theme_minimal()
}

# Crear gráficos
p1 <- create_qqplot(argentinos_entre_18y65, "Argentinos")
p2 <- create_qqplot(extranjeros_entre_18y65, "Extranjeros")

# Combinar gráficos
combined_plot <- p1 + p2 + plot_layout(ncol = 2)

# Mostrar resultado
print(combined_plot)

# Aplicar el test de Anderson-Darling
ad.test(argentinos_entre_18y65$CH06)
ad.test(extranjeros_entre_18y65$CH06)

# ambas muestras no pasan el test de normalidad, solo nos queda asumir que n es lo suficientemente grande
# Intervalos de confianza (tu código original está bien)
MeanCI(argentinos_entre_18y65$CH06, conf.level = 0.95)
MeanCI(extranjeros_entre_18y65$CH06, conf.level = 0.95)

# Conclusion: 
# Con un nivel del 95% de confianza podemos concluir que la media poblacional de edad de los extranjeros se encuentra por encima de la media poblacional de la edad de los argentinos

# ---------------------------------------------------------------------------------------------------------------------------------
# Plantear intervalo de confianza para la proporcion poblacional de argentinos ocupados y extranjeros ocupados
# ---------------------------------------------------------------------------------------------------------------------------------

argentinos_estado <- (argentinos_entre_18y65 %>% filter(ESTADO %in% c("1", "2", "3")))$ESTADO
argentinos_ocupados <- (argentinos_entre_18y65 %>% filter(ESTADO %in% c("1")))$ESTADO

n = length(argentinos_estado)
p = length(argentinos_ocupados)/n

# Podemos aplicar la aproximacion normal?
n*p>=5 & n*(1-p)>=5

# Podemos usar los intervalos aproximados a normal
BinomCI(x = length(argentinos_ocupados), n = length(argentinos_estado), conf.level = 0.95, method = "wald")


extranjeros_estado <- (extranjeros_entre_18y65 %>% filter(ESTADO %in% c("1", "2", "3")))$ESTADO
extranjeros_ocupados <- (extranjeros_entre_18y65 %>% filter(ESTADO %in% c("1")))$ESTADO

n = length(extranjeros_estado)
p = length(extranjeros_ocupados)/n

# Podemos aplicar la aproximacion normal?
n*p>=5 & n*(1-p)>=5

# Podemos usar los intervalos aproximados a normal
BinomCI(x = length(extranjeros_ocupados), n = length(extranjeros_estado), conf.level = 0.95, method = "wald")

# Conclusion:  
# Con un nivel de confianza del 95%, como los intervalos se superponen, no podemos concluir que alguna de las proporciones poblacionales es mayor que el otra.


# ---------------------------------------------------------------------------------------------------------------------------------
# Plantear intervalo de confianza para la proporcion poblacional de argentinos con educacion media y extranjeros con educacion media
# ---------------------------------------------------------------------------------------------------------------------------------

argentinos_educacion <- (argentinos_entre_18y65)$NIVEL_ED
argentinos_educacion_media <- (argentinos_entre_18y65 %>% filter(NIVEL_ED %in% c("4", "5")))$NIVEL_ED

n = length(argentinos_educacion)
p = length(argentinos_educacion_media)/n

# Podemos aplicar la aproximacion normal?
n*p>=5 & n*(1-p)>=5

# Podemos usar los intervalos aproximados a normal
BinomCI(x = length(argentinos_educacion_media), n = length(argentinos_educacion), conf.level = 0.95, method = "wald")

extranjeros_educacion <- (extranjeros_entre_18y65)$NIVEL_ED
extranjeros_educacion_media <- (extranjeros_entre_18y65 %>% filter(NIVEL_ED %in% c("4", "5")))$NIVEL_ED

n = length(extranjeros_educacion)
p = length(extranjeros_educacion_media)/n

# Podemos aplicar la aproximacion normal?
n*p>=5 & n*(1-p)>=5

# Podemos usar los intervalos aproximados a normal
BinomCI(x = length(extranjeros_educacion_media), n = length(extranjeros_educacion), conf.level = 0.95, method = "wald")

# Conclusion:  
# Con un nivel de confianza del 95%, podemos concluir que la proporcion poblacional de argentinos con educacion media es superior a la proporcion extranjera.


