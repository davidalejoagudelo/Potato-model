library(readxl)
mccain_savia <- read_excel("C:/Users/david/OneDrive/Escritorio/Personal/R/mccain_savia.xlsx")
head(mccain_savia)

library(ggplot2)

tema <-   theme(legend.position="bottom",panel.background = element_rect(fill = NA),panel.grid.major.y = element_line(colour = "grey90"),legend.key = element_rect(fill = "white"),axis.line = element_line(colour = "grey90"),legend.title = element_text(colour = "white"))

library(dplyr)

# Define una función para simplificar el proceso de creación de gráficos
crear_grafico <- function(datos, variedad, variable) {
  nombre_variable <- as.character(substitute(variable))  # Obtener el nombre de la variable
  
  datos %>%
    filter(Variedad == variedad) %>%
    group_by(DDS, Tratamiento) %>%
    summarise(Variable = mean({{variable}}, na.rm = TRUE),
              DDS = DDS,
              TRATAMIENTO = Tratamiento,
              SE = sd({{variable}}, na.rm = TRUE) / sqrt(sum({{variable}}))) %>%
    ggplot(aes(x = DDS, y = Variable, group = TRATAMIENTO, colour = TRATAMIENTO, shape = TRATAMIENTO)) +
    geom_errorbar(aes(ymin = Variable - 2 * SE, ymax = Variable + 2 * SE), width = 1) +
    geom_line(size = 0.5) +
    scale_y_continuous(breaks = seq(0, 10000, 500)) +
    scale_x_continuous(breaks = seq(0, 150, 15)) +
    labs(title = paste(nombre_variable, "en savia - Variedad", variedad),
         subtitle = paste("Representación gráfica de medias para la variedad", variedad),
         caption = paste("Variedad", variedad),
         x = "DDS", y = paste("Contenido de", nombre_variable, "(ppm)")) +
    geom_point(size = 2, fill = "white") +
    scale_color_manual(values = c("seagreen2", "cadetblue3", "cadetblue4", "cadetblue2")) +
    tema
}

# Llamar a la función para cada variedad
grafico_variedad_1_NO3 <- crear_grafico(mccain_savia, "CIP 1", NO3)
grafico_variedad_1_K <- crear_grafico(mccain_savia, "CIP 1", K)
grafico_variedad_1_Ca <- crear_grafico(mccain_savia, "CIP 1", Ca)
grafico_variedad_39_NO3 <- crear_grafico(mccain_savia, "CIP 39", NO3)
grafico_variedad_39_K <- crear_grafico(mccain_savia, "CIP 39", K)
grafico_variedad_39_Ca <- crear_grafico(mccain_savia, "CIP 39", Ca)
grafico_variedad_102_NO3 <- crear_grafico(mccain_savia, "CIP 102", NO3)
grafico_variedad_102_K <- crear_grafico(mccain_savia, "CIP 102", K)
grafico_variedad_102_Ca <- crear_grafico(mccain_savia, "CIP 102", Ca)

grafico_variedad_1_NO3
grafico_variedad_1_K
grafico_variedad_1_Ca
grafico_variedad_39_NO3
grafico_variedad_39_K
grafico_variedad_39_Ca
grafico_variedad_102_NO3
grafico_variedad_102_K
grafico_variedad_102_Ca
