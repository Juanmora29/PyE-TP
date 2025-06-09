install.packages("eph")
install.packages("tidyverse")
install.packages("nortest")

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

# Grafico de sectores => Distribucion de personas por origen
df_origen <- datos_combinados_entre_18y65 %>%
  filter(!is.na(origen)) %>%
  group_by(origen) %>%
  summarise(cantidad = n())

df_origen <- datos_combinados_entre_18y65 %>%
  filter(!is.na(origen)) %>%
  group_by(origen) %>%
  summarise(cantidad = n()) %>%
  mutate(
    porcentaje = cantidad / sum(cantidad),
    etiqueta = paste0(round(porcentaje * 100, 2), "%")
  )

ggplot(df_origen, aes(x = "", y = cantidad, fill = origen)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  geom_text(aes(label = etiqueta),
            position = position_stack(vjust = 0.5),
            color = "white", size = 5) +
  scale_fill_manual(
    values = c("Argentinos" = "#128dfe", "Extranjeros" = "#11239f")
  ) +
  labs(
    title = "Distribución de personas por origen",
    x = NULL,
    y = NULL,
    fill = "Origen"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# Grafico de barras apiladas => Porcentaje de ocupacion por Edades
# Filtrar datos de ocupación y crear la variable de agrupación ESTADO_group
datos_ocupacion <- datos_combinados_entre_18y65 %>%
  filter(ESTADO != "0") %>%
  mutate(ESTADO_group = case_when(
    ESTADO %in% c("1") ~ "Ocupado",
    ESTADO %in% c("2") ~ "Desocupado",
    ESTADO %in% c("3") ~ "Inactivo",
    TRUE ~ "Otro"
  ))

# GRÁFICO N°1: Estado de actividad de las personas nacidas en Argentina
ggplot(argentinos_entre_18y65, aes(x = CH06, fill = factor(ESTADO))) +
  geom_bar(position = "fill") +
  scale_y_reverse(labels = scales::percent_format()) +
  scale_x_continuous(
    breaks = seq(18, 65, 2),  # Mostrar cada 2 años
    name = "Edad"
  ) +
  scale_fill_manual(
    values = c("0" = "#128dfe", "1" = "#11239f", "2" = "#e56a3c", "3" = "#680277"),
    name = "ESTADO",
    labels = c("0: No contesta", "1: Ocupado", "2: Desocupado", "3: Inactivo")
  ) +
  labs(
    title = "Distribución porcentual del ESTADO por edad (CH06) entre Argentinos residentes",
    x = "Edad",
    y = "Porcentaje de personas (invertido)"
  ) +
  theme_minimal()

# GRÁFICO N° 2: Estado de actividad de las personas nacidas en el Extranjero
ggplot(extranjeros_entre_18y65, aes(x = CH06, fill = factor(ESTADO))) +
  geom_bar(position = "fill") +
  scale_y_reverse(labels = scales::percent_format()) +
  scale_x_continuous(
    breaks = seq(18, 65, 2),  # Mostrar cada 2 años
    name = "Edad"
  ) +
  scale_fill_manual(
    values = c("0" = "#128dfe", "1" = "#11239f", "2" = "#e56a3c", "3" = "#680277"),
    name = "ESTADO",
    labels = c("0: No contesta", "1: Ocupado", "2: Desocupado", "3: Inactivo")
  ) +
  labs(
    title = "Distribución porcentual del ESTADO por edad (CH06) entre Extranjeros residentes",
    x = "Edad",
    y = "Porcentaje de personas (invertido)"
  ) +
  theme_minimal()

# GRÁFICO N° 3: Comparación de ingresos entre argentinos y extranjeros residentes
# Filtrar ingresos válidos
datos_ingresos <- datos_combinados_entre_18y65 %>% filter(P47T >= 0)

limite_superior <- quantile(datos_ingresos$P47T, 0.99, na.rm = TRUE)
datos_filtrados <- datos_ingresos %>% filter(P47T >= 0 & P47T < limite_superior)

ingresos_media_mediana <- datos_filtrados %>%
  group_by(origen) %>%
  summarise(
    media = mean(P47T, na.rm = TRUE),
    mediana = median(P47T, na.rm = TRUE)
  )

ggplot(datos_filtrados, aes(x = origen, y = P47T, fill = origen)) +
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color = "darkred") +  # Media en rojo
  scale_y_continuous(labels = scales::comma_format()) +  # Formato con comas para facilitar lectura
  labs(
	title = "Comparación de ingresos totales individuales entre argentinos y extranjeros",
	x = "Origen",
	y = "Ingreso total individual (P47T)"
  ) +
  # Agregar texto con las estadísticas
  geom_text(data = ingresos_media_mediana, aes(x = origen, y = media, label = paste0("Media: ", round(media, 0))),
            color = "darkred", vjust = -0.7, hjust = 0.1) + # Anotar media
  geom_text(data = ingresos_media_mediana, aes(x = origen, y = mediana, label = paste0("Mediana: ", round(mediana, 0))),
            color = "darkblue", vjust = 8, hjust = 0.1) + # Anotar mediana
  coord_flip() +
  theme_minimal()

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


hist(ingresos_extranjeros,
     main = "Histograma de P47T",
     xlab = "Ingreso (P47T)",
     ylab = "Frecuencia",
     col = "skyblue",
     breaks = 130)

# Transformación logarítmica para mejorar la visualizacion
extranjeros_entre_18y65 %>%
  filter(P47T > 0) %>%  # evitar log(0)
  ggplot(aes(x = log(P47T))) +
  geom_histogram(binwidth = 0.2, fill = "steelblue", color = "white") +
  labs(title = "Histograma log(P47T)",
       x = "Logaritmo del ingreso",
       y = "Frecuencia") +
  theme_minimal()


# Aplicar el test de Anderson-Darling
ad.test(ingresos_argentinos)
ad.test(ingresos_extranjeros)

# ambas muestras no pasan el test de normalidad, solo nos queda asumir que n es lo suficientemente grande
MeanCI(x = ingresos_argentinos, conf.level = 0.95)
MeanCI(x = ingresos_extranjeros, conf.level = 0.95)

Los intervalos se superponen con lo cual no podemos concluir que alguno de los promedios es mayor que el otro con una confianza del 95%


# GRÁFICO N° 4: Comparación de edad entre argentinos y extranjeros residentes.
# Calcular estadísticas por grupo para usar en las etiquetas
edades_media_mediana <- datos_combinados_entre_18y65 %>%
  group_by(origen) %>%
  summarise(
    media = mean(CH06, na.rm = TRUE),
    mediana = median(CH06, na.rm = TRUE)
  )

ggplot(datos_combinados_entre_18y65, aes(x = origen, y = CH06, fill = origen)) +
  geom_boxplot(width = 0.5, outlier.shape = NA, alpha = 0.7) +  # Boxplot sin outliers y con ancho ajustado
  stat_summary(fun = median, geom = "point", shape = 20, size = 4, color = "red") +  # Punto rojo para la mediana
  # Etiqueta de la media (rojo)
  geom_text(data = edades_media_mediana,
            aes(x = origen, y = media, label = paste0("Media: ", round(media, 0))),
            color = "darkred", vjust = -1) +
  # Etiqueta de la mediana (azul)
  geom_text(data = edades_media_mediana,
            aes(x = origen, y = mediana, label = paste0("Mediana: ", round(mediana, 0))),
            color = "darkblue", vjust = 1.5) +
  scale_y_continuous(breaks = seq(18, 80, 5)) +  # Eje Y con saltos de 5 años
  labs(title = "Comparación de Edad entre Argentinos y Extranjeros",
       x = "Origen",
       y = "Edad (años)",
       fill = "Origen") +
  theme_minimal()

# ANEXO I. GRÁFICO N° 1: Distribución relativa de la condición de actividad de origen
# Calcular proporciones para el gráfico
datos_ocupacion_prop <- datos_ocupacion %>%
  group_by(origen, ESTADO_group) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(origen) %>%
  mutate(prop = n / sum(n))
datos_ocupacion_prop

# Gráfico de barras con etiquetas de proporción
ggplot(datos_ocupacion_prop, aes(x = ESTADO_group, y = prop, fill = origen)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = scales::percent(prop, accuracy = 0.1)),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 4) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Distribución relativa de la condición de actividad por origen",
    x = "Ocupación (agrupado)",
    y = "Proporción dentro de cada grupo (%)",
    fill = "Origen"
  ) +
  theme_minimal()

# ANEXO I. GRÁFICO N° 2: Distribución relativa del nivel de educación por origen.
# Agrupar el nivel educativo en categorías
datos_combinados_entre_18y65 <- datos_combinados_entre_18y65 %>%
  mutate(NIVEL_ED_group = case_when(
    NIVEL_ED %in% c("1", "2", "3", "7") ~ "Bajo",
    NIVEL_ED %in% c("4", "5") ~ "Medio",
    NIVEL_ED %in% c("6") ~ "Superior",
    TRUE ~ "Otro"
  ))

# Calcular las proporciones
datos_prop_educacion_origen <- datos_combinados_entre_18y65 %>%
  group_by(origen, NIVEL_ED_group) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(origen) %>%
  mutate(prop = n / sum(n))

# Gráfico con etiquetas sobre las barras
ggplot(datos_prop_educacion_origen, aes(x = NIVEL_ED_group, y = prop, fill = origen)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = scales::percent(prop, accuracy = 0.1)),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 4) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Distribución relativa del nivel de educacion por origen",
    x = "Nivel Educativo (agrupado)",
    y = "Proporción dentro de cada grupo (%)",
    fill = "Origen"
  ) +
  theme_minimal()

# ANEXO I. GRÁFICO N° 3: Proporción de Nacionalidad por aglomerado.
# Calcular proporciones por aglomerado y origen
datos_proporcion <- datos_combinados_entre_18y65 %>%
  group_by(AGLOMERADO, origen) %>%
  summarise(cantidad = n(), .groups = "drop") %>%
  group_by(AGLOMERADO) %>%
  mutate(proporcion = cantidad / sum(cantidad))

# Gráfico
ggplot(datos_proporcion, aes(x = factor(AGLOMERADO), y = proporcion, fill = origen)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_reverse(labels = scales::percent_format()) +
  scale_fill_manual(
    values = c(
      "Argentinos" = "#4BA3FD",
      "Extranjeros" = "#0D2C54",
      "Ns/Nr" = "#D37F4A"
    )
  ) +
  labs(
    title = "Proporción de Nacionalidad por Aglomerado",
    x = "Aglomerado",
    y = "Proporción de Personas",
    fill = "Nacionalidad"
  ) +
  theme_minimal()
