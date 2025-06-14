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

### MODIFICACION GRÁFICOS 1 y 2 ###
# Crear variable de rango de edad en intervalos de 6 años
argentinos_entre_18y65$edad_rango <- cut(
  argentinos_entre_18y65$CH06,
  breaks = seq(18, 66, 6),
  labels = c("[18,24)", "[24,30)", "[30,36)", "[36,42)", "[42,48)", "[48,54)", "[54,60)", "[60,66)"),
  right = FALSE
)

extranjeros_entre_18y65$edad_rango <- cut(
  extranjeros_entre_18y65$CH06,
  breaks = seq(18, 66, 6),
  labels = c("[18,24)", "[24,30)", "[30,36)", "[36,42)", "[42,48)", "[48,54)", "[54,60)", "[60,66)"),
  right = FALSE
)


# GRÁFICO N°1: Estado de actividad de las personas nacidas en Argentina
ggplot(argentinos_entre_18y65, aes(x = edad_rango, fill = factor(ESTADO))) +
  geom_bar(position = "fill") +
  scale_y_reverse(labels = scales::percent_format()) +
  scale_fill_manual(
    values = c("0" = "#128dfe", "1" = "#11239f", "2" = "#e56a3c", "3" = "#680277"),
    name = "ESTADO",
    labels = c("0: No contesta", "1: Ocupado", "2: Desocupado", "3: Inactivo")
  ) +
  labs(
    title = "Distribución porcentual del ESTADO por rango de edad (CH06) entre Argentinos residentes",
    x = "Rango de Edad",
    y = "Porcentaje de personas (invertido)"
  ) +
  theme_minimal()


# GRÁFICO N° 2: Estado de actividad de las personas nacidas en el Extranjero
ggplot(extranjeros_entre_18y65, aes(x = edad_rango, fill = factor(ESTADO))) +
  geom_bar(position = "fill") +
  scale_y_reverse(labels = scales::percent_format()) +
  scale_fill_manual(
    values = c("0" = "#128dfe", "1" = "#11239f", "2" = "#e56a3c", "3" = "#680277"),
    name = "ESTADO",
    labels = c("0: No contesta", "1: Ocupado", "2: Desocupado", "3: Inactivo")
  ) +
  labs(
    title = "Distribución porcentual del ESTADO por rango de edad (CH06) entre Extranjeros residentes",
    x = "Rango de Edad",
    y = "Porcentaje de personas (invertido)"
  ) +
  theme_minimal()

# GRÁFICO N° 3: Distribución relativa de la condición de actividad por origen.
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


# GRÁFICO N° 4: Distribución de los ingresos totales individuales de los Argentinos.
ingresos_argentinos <- (argentinos_entre_18y65 %>% filter(P47T >= 0))

# Calculo media y mediana
ingresos_media_mediana_arg <- ingresos_argentinos %>%
  summarise(
    origen = "Argentinos",
    media = mean(P47T, na.rm = TRUE),
    mediana = median(P47T, na.rm = TRUE)
  )

ggplot(ingresos_argentinos, aes(x = origen, y = P47T, fill = origen)) +
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color = "darkred") +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(
    title = "Ingresos totales individuales - Argentinos",
    x = "",
    y = "Ingreso total individual (P47T)"
  ) +
  geom_text(data = ingresos_media_mediana_arg,
            aes(x = origen, y = media, label = paste0("Media: ", scales::comma(round(media, 0)))),
            color = "darkred", vjust = -0.5, hjust = 0.05) +
  geom_text(data = ingresos_media_mediana_arg,
            aes(x = origen, y = mediana, label = paste0("Mediana: ", scales::comma(round(mediana, 0)))),
            color = "darkblue", vjust = 4.9, hjust = 0.4) +
  coord_flip(ylim = c(0, 1400000)) +
  theme_minimal() +
  theme(legend.position = "none")


#GRÁFICO N° 5: Distribución de los ingresos totales individuales de los Extranjeros
ingresos_extranjeros <-(extranjeros_entre_18y65 %>% filter(P47T >= 0))

# Calculo media y mediana
ingresos_media_mediana_ext <- ingresos_extranjeros %>%
  summarise(
    origen = "Extranjeros",
    media = mean(P47T, na.rm = TRUE),
    mediana = median(P47T, na.rm = TRUE)
  )

ggplot(ingresos_extranjeros, aes(x = origen, y = P47T, fill = origen)) +
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color = "darkred") +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(
    title = "Ingresos totales individuales - Extranjeros",
    x = "",
    y = "Ingreso total individual (P47T)"
  ) +
  geom_text(data = ingresos_media_mediana_ext,
            aes(x = origen, y = media, label = paste0("Media: ", scales::comma(round(media, 0)))),
            color = "darkred", vjust = -0.5, hjust = 0.15) +
  geom_text(data = ingresos_media_mediana_ext,
            aes(x = origen, y = mediana, label = paste0("Mediana: ", scales::comma(round(mediana, 0)))),
            color = "darkblue", vjust = 4.9, hjust = 0.4) +
  coord_flip(ylim = c(0, 1764300)) +
  theme_minimal() +
  theme(legend.position = "none")

# GRÁFICO N° 6: Comparación de edad entre argentinos y extranjeros residentes.

# Calcular media y mediana
edades_media_mediana <- datos_combinados_entre_18y65 %>%
  group_by(origen) %>%
  summarise(
    media = mean(CH06, na.rm = TRUE),
    mediana = median(CH06, na.rm = TRUE)
  )

# Asegurar orden de los factores
edades_media_mediana$origen <- factor(edades_media_mediana$origen,
                                      levels = c("Argentinos", "Extranjeros"))

# Gráfico
ggplot(datos_combinados_entre_18y65,
       aes(y = factor(origen, levels = c("Argentinos", "Extranjeros")),
           x = CH06, fill = origen)) +
  geom_boxplot(width = 0.5, outlier.shape = NA, alpha = 0.7) +
  
  # Punto para la media
  geom_point(data = edades_media_mediana,
             aes(x = media, y = origen),
             color = "red", size = 3) +
  
  # Etiqueta de la media con 1 decimal
  geom_text(data = edades_media_mediana,
            aes(x = media, y = origen,
                label = paste0("Media: ", format(round(media, 1), nsmall = 1))),
            color = "darkred", nudge_y = 0.15) +
  
  # Etiqueta de la mediana sin decimales
  geom_text(data = edades_media_mediana,
            aes(x = mediana, y = origen,
                label = paste0("Mediana: ", round(mediana, 0))),
            color = "blue", nudge_y = -0.35) +
  
  scale_x_continuous(breaks = seq(18, 70, 5)) +
  labs(title = "Comparación de Edad entre Argentinos y Extranjeros",
       x = "Edad (años)",
       y = "Origen") +
  theme_minimal() +
  theme(legend.position = "none")


# GRÁFICO N° 7: Distribución relativa del nivel de educación por origen.
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
datos_combinados_entre_18y65

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
