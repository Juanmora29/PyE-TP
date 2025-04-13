install.packages("eph")
install.packages("tidyverse")
library(eph)
library(tidyverse)
library(ggplot2)
library(scales)
library(dplyr)

# Descargar los microdatos del primer trimestre de 2024
datos <- get_microdata(year = 2024, trimester = 1, type = "individual")

# -------------------------------------------------------------------
#  Filtramos los datos para incluir solo personas en edad laboral (18-65 años)
# -------------------------------------------------------------------
datos_edad <- datos %>%
  filter(CH06 >= 18 & CH06 <= 65)

# -------------------------------------------------------------------
# Creamos un subconjuntos para argentinos y extranjeros
# -------------------------------------------------------------------

argentinos <- datos_edad %>%
  filter(CH15 %in% c("1", "2", "3")) %>%
  mutate(origen = "Argentinos")

extranjeros <- datos_edad %>%
  filter(CH15 %in% c("4", "5")) %>%
  mutate(origen = "Extranjeros")

# Combinar ambos subconjuntos
datos_combinados <- bind_rows(argentinos, extranjeros)

# Agrupar el nivel educativo en categorías generales
datos_combinados <- datos_combinados %>%
  mutate(NIVEL_ED_group = case_when(
    NIVEL_ED %in% c("1", "2", "3", "7") ~ "Bajo",
    NIVEL_ED %in% c("4", "5") ~ "Medio",
    NIVEL_ED %in% c("6") ~ "Superior",
    TRUE ~ "Otro"
  ))

# Filtrar ingresos válidos
datos_ingresos <- datos_combinados %>% filter(P47T >= 0)

resumen <- datos_ingresos %>%
  group_by(origen, NIVEL_ED_group) %>%
  summarise(
	mediana = median(P47T, na.rm = TRUE),
	media = mean(P47T, na.rm = TRUE),
	sd = sd(P47T, na.rm = TRUE),
	IQR = IQR(P47T, na.rm = TRUE),
	min = min(P47T, na.rm = TRUE),
	max = max(P47T, na.rm = TRUE),
	CV = (sd / media) * 100,
	n = n(),
	.groups = "drop"
  )

### GRAFICO DE DISTRIBUCION RELATIVA DEL NIVEL DE EDUCACION POR ORIGEN

# Calcular las proporciones antes de graficar
datos_prop <- datos_combinados %>%
  group_by(origen, NIVEL_ED_group) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(origen) %>%
  mutate(prop = n / sum(n))

# Crear el gráfico con etiquetas sobre las barras
ggplot(datos_prop, aes(x = NIVEL_ED_group, y = prop, fill = origen)) +
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

# GRAFICO DE COMPARACION DE INGRESOS TOTALES INDIVIDUALES ENTRE ARGENTINOS Y EXTRANJEROS

datos_filtrados <- datos_ingresos %>% filter(P47T >= 0 & P47T < limite_superior)
limite_superior <- quantile(datos_ingresos$P47T, 0.99, na.rm = TRUE)

estadisticas <- datos_combinados %>%
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
  geom_text(data = estadisticas, aes(x = origen, y = media, label = paste0("Media: ", round(media, 0))),
            color = "darkred", vjust = -1.5) + # Anotar media
  geom_text(data = estadisticas, aes(x = origen, y = mediana, label = paste0("Mediana: ", round(mediana, 0))),
            color = "darkblue", vjust = 0) + # Anotar mediana
  theme_minimal()


# GRAFICO DE OCUPACION LABORAL PARA ARGENTINOS Y EXTRANJEROS

# Filtrar datos de ocupación y crear la variable de agrupación ESTADO_group
datos_ocupacion <- datos_combinados %>%
  filter(ESTADO != "0") %>%
  mutate(ESTADO_group = case_when(
    ESTADO %in% c("1") ~ "Ocupado",
    ESTADO %in% c("2") ~ "Desocupado",
    ESTADO %in% c("3") ~ "Inactivo",
    TRUE ~ "Otro"
  ))

# Calcular proporciones para el gráfico
datos_ocupacion_prop <- datos_ocupacion %>%
  group_by(origen, ESTADO_group) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(origen) %>%
  mutate(prop = n / sum(n))

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

###GRAFICO PARA VISUALIZAR PROMEDIO DE EDADES POR ORIGEN

# Calcular estadísticas por grupo para usar en las etiquetas
estadisticas_paraedades <- datos_combinados %>%
  group_by(origen) %>%
  summarise(
    media = mean(CH06, na.rm = TRUE),
    mediana = median(CH06, na.rm = TRUE)
  )

# Gráfico de boxplot de la edad (CH06) por origen con anotaciones
ggplot(datos_combinados, aes(x = origen, y = CH06, fill = origen)) +
  geom_boxplot(width = 0.5, outlier.shape = NA, alpha = 0.7) +  # Boxplot sin outliers y con ancho ajustado
  stat_summary(fun = median, geom = "point", shape = 20, size = 4, color = "red") +  # Punto rojo para la mediana
  # Agregar etiqueta de la media (en rojo)
  geom_text(data = estadisticas_paraedades,
            aes(x = origen, y = media, label = paste0("Media: ", round(media, 0))),
            color = "darkred", vjust = -1) +
  # Agregar etiqueta de la mediana (en azul)
  geom_text(data = estadisticas_paraedades,
            aes(x = origen, y = mediana, label = paste0("Mediana: ", round(mediana, 0))),
            color = "darkblue", vjust = 1.5) +
  scale_y_continuous(breaks = seq(18, 80, 5)) +  # Eje Y con saltos de 5 años
  labs(title = "Comparación de Edad entre Argentinos y Extranjeros",
       x = "Origen",
       y = "Edad (años)",
       fill = "Origen") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Distribucion de extranjeros por aglomerado

distribucion_extranjeros <- extranjeros %>%
  group_by(AGLOMERADO) %>% summarise(cantidad = n())

distribucion_argentinos <- argentinos %>%
  group_by(AGLOMERADO) %>% summarise(cantidad = n())

distribucion_extranjeros <- extranjeros %>%
  group_by(AGLOMERADO) %>%
  summarise(cantidad = n()) %>%
  arrange(desc(cantidad)) %>%
  mutate(acumulado = cumsum(cantidad))


install.packages("writexl")
library(writexl)

write_xlsx(distribucion_extranjeros, "distribucion_extranjeros.xlsx")


# ///////////////////////////////////////////////
# Graficos de barras apiladas Edad vs Estado
# Argentinos
ggplot(argentinos, aes(x = CH06, fill = factor(ESTADO))) +
  geom_bar(position = "fill") +
  scale_y_reverse(labels = scales::percent_format()) +
  scale_x_continuous(breaks = 18:65) +  # Mostrar todas las edades
  scale_fill_manual(
    values = c("0" = "#128dfe", "1" = "#11239f", "2" = "#e56a3c", "3" = "#680277"),
    name = "ESTADO",
    labels = c("0: No contesta", "1: Ocupado", "2: Desocupado", "3: Inactivo")
  ) +
  labs(
    title = "Distribución porcentual del ESTADO por edad (CH06)",
    x = "Edad",
    y = "Porcentaje de personas (invertido)"
  ) +
  theme_minimal()

# Extranjeros
ggplot(extranjeros, aes(x = CH06, fill = factor(ESTADO))) +
  geom_bar(position = "fill") +
  scale_y_reverse(labels = scales::percent_format()) +
  scale_x_continuous(breaks = 18:65) +  # Mostrar todas las edades
  scale_fill_manual(
    values = c("0" = "#128dfe", "1" = "#11239f", "2" = "#e56a3c", "3" = "#680277"),
    name = "ESTADO",
    labels = c("0: No contesta", "1: Ocupado", "2: Desocupado", "3: Inactivo")
  ) +
  labs(
    title = "Distribución porcentual del ESTADO por edad (CH06)",
    x = "Edad",
    y = "Porcentaje de personas (invertido)"
  ) +
  theme_minimal()
