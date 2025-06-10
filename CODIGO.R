install.packages("eph")
install.packages("tidyverse")
library(eph)
library(tidyverse)




# Descargar los microdatos del primer trimestre de 2024
datos <- get_microdata(year = 2024, trimester = 1, type = "individual")




# -------------------------------------------------------------------
#  Filtramos los datos para incluir solo personas en edad laboral (18-65 años)
# -------------------------------------------------------------------
datos_edad_18a65 <- datos %>%
  filter(CH06 >= 18 & CH06 <= 65)




# -------------------------------------------------------------------
# Creamos un subconjuntos para argentinos y extranjeros
# -------------------------------------------------------------------


argentinos_18a65 <- datos_edad_18a65 %>%
  filter(CH15 %in% c("1", "2", "3")) %>%
  mutate(origen = "Argentinos")




extranjeros <- datos_edad_18a65 %>%
  filter(CH15 %in% c("4", "5")) %>%
  mutate(origen = "Extranjeros")




# -------------------------------------------------------------------
# Visualizamos la distribución del nivel educativo por origen
# -------------------------------------------------------------------


par(mfrow = c(1, 2))  # Mostrar gráficos lado a lado
barplot(table(argentinos_18a65$NIVEL_ED),
        main = expression("Nivel Educativo de Argentinos (" * pi[1] * ")"),
        horiz = TRUE,
        col = "blue")
barplot(table(extranjeros$NIVEL_ED),
        main = expression("Nivel Educativo de Extranjeros (" * pi[2] * ")"),
        horiz = TRUE,
        col = "red")
par(mfrow = c(1,1))  # Resetear el layout gráfico


# -------------------------------------------------------------------
# Calculamos estadísticas descriptivas del ingreso total individual (P47T)
# -------------------------------------------------------------------


ingresos_validos <- datos_edad_18a65 %>%
  filter(P47T > 0)


# Resumen y cálculo de la media
print(summary(ingresos_validos$P47T))
mean_ingreso <- mean(ingresos_validos$P47T, na.rm = TRUE)
print(paste("Ingreso promedio:", mean_ingreso))


# -------------------------------------------------------------------
# Visualizacion avanzada con ggplot2
# -------------------------------------------------------------------
library(ggplot2)




# Combinar ambos subconjuntos
datos_combinados <- bind_rows(argentinos_18a65, extranjeros)




# Agrupar el nivel educativo en categorías generales
datos_combinados <- datos_combinados %>%
  mutate(NIVEL_ED_group = case_when(
    NIVEL_ED %in% c("1", "2", "3", “7”) ~ "Inferior",
    NIVEL_ED %in% c("4", "5") ~ "Medio",
    NIVEL_ED %in% c("6") ~ "Superior",
    TRUE ~ "Otro"
  ))




# Filtrar ingresos válidos (P47T > 0, asumiendo que -9 es inválido)
datos_ingresos <- datos_combinados %>%
  filter(P47T > 0)




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


resumen


### PORPORCION RELATIVA DE NIVEL ED POR ORIGEN
datos_ingresos %>%
  group_by(origen, NIVEL_ED_group) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(origen) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = NIVEL_ED_group, y = prop, fill = origen)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
	title = "Distribución relativa del nivel educativo por origen",
	x = "Nivel Educativo (agrupado)",
	y = "Proporción dentro de cada grupo (%)",
	fill = "Origen"
  ) +
  theme_minimal()




library(scales)  # Para formato de números en el eje Y


# Filtrar ingresos válidos y eliminar outliers (percentil 99)
limite_superior <- quantile(datos_ingresos$P47T, 0.99, na.rm = TRUE)
datos_filtrados <- datos_ingresos %>% filter(P47T > 0 & P47T < limite_superior)


# Gráfico ajustando los valores del eje Y
ggplot(datos_filtrados, aes(x = origen, y = P47T, fill = origen)) +
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color = "red") +  # Media en rojo
  scale_y_continuous(labels = scales::comma_format()) +  # Formato con comas para facilitar lectura
  labs(
	title = "Comparación de ingresos entre argentinos y extranjeros",
	x = "Origen",
	y = "Ingreso total individual (P47T)"
  ) +
  theme_minimal()




##### MAS CONCENTRADO EN LA MEDIA Y MEDIANA


# Calcular media y mediana por cada grupo (origen)
estadisticas <- datos_filtrados %>%
  group_by(origen) %>%
  summarise(
    media = mean(P47T, na.rm = TRUE),
    mediana = median(P47T, na.rm = TRUE)
  )


# Graficar con anotaciones de media y mediana
ggplot(datos_filtrados, aes(x = origen, y = P47T, fill = origen)) +
  geom_boxplot(width = 0.5, alpha = 0.7) +  # Ajusta el ancho de las cajas
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +  # Media en rojo
  geom_hline(yintercept = median(datos_filtrados$P47T, na.rm = TRUE),
             linetype = "dashed", color = "blue") +  # Mediana en azul
  scale_y_continuous(labels = function(x) paste0(x / 1000, "K")) +  # Expresar en miles
  coord_cartesian(ylim = c(0, 500000)) +  # Ajuste del eje Y
  labs(
    title = "Comparación de ingresos entre argentinos y extranjeros",
    x = "Origen",
    y = "Ingreso total individual (en miles de pesos)"
  ) +
  # Agregar texto con las estadísticas
  geom_text(data = estadisticas, aes(x = origen, y = media, label = paste0("Media: ", round(media, 0))),
            color = "red", vjust = -1) + # Anotar media
  geom_text(data = estadisticas, aes(x = origen, y = mediana, label = paste0("Mediana: ", round(mediana, 0))),
            color = "blue", vjust = 1.5) + # Anotar mediana
  theme_minimal()


#--------------------REVISAR--------------------------------------
# Agrupar ESTADO en categorías generales
datos_combinados2 <- datos_combinados %>%
  mutate(ESTADO_group = case_when(
    ESTADO %in% c("0") ~ "Ns/Nc",
    ESTADO %in% c("1") ~ "Ocupado",
    ESTADO %in% c("2") ~ "Desocupado",
    ESTADO %in% c("3") ~ "Inactivo",
    TRUE ~ "Menor de 10"
  ))


########## Grafico ESTADO
datos_combinados2 %>%
  group_by(origen, ESTADO_group) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(origen) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = ESTADO_group, y = prop, fill = origen)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Distribución relativa de la condicion de actividad por origen",
    x = "Ocupacion (agrupado)",
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
            color = "red", vjust = -1) +
  # Agregar etiqueta de la mediana (en azul)
  geom_text(data = estadisticas_paraedades,
            aes(x = origen, y = mediana, label = paste0("Mediana: ", round(mediana, 0))),
            color = "blue", vjust = 1.5) +
  scale_y_continuous(breaks = seq(18, 80, 5)) +  # Eje Y con saltos de 5 años
  labs(title = "Comparación de Edad entre Argentinos y Extranjeros",
       x = "Origen",
       y = "Edad (años)",
       fill = "Origen") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
