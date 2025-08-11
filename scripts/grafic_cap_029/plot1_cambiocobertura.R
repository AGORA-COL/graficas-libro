library(ggplot2)
library(scales)
library(dplyr)
library(readr)

rm(list=ls())

# Datos
datosfi1 <- read_csv("data/data_cap_029/datosfi1.csv")

#Gráfico
cambio_cob <- ggplot(datosfi1, aes(x = CDS)) +
 geom_line(aes(y = distancia), color = "#FF7F00", linewidth = 1.2) +
  geom_point(aes(y = distancia), color = "#FF7F00", size = 2.5) +
  geom_label(aes(y = distancia, label = round(distancia, 0)), 
             fill = "#FF7F00", color = "black", label.size = 0.4, size = 3.5) +
 geom_line(aes(y = cobertura * 3.5), color = "#000080", linewidth = 1.2) +
  geom_point(aes(y = cobertura * 3.5), color = "#000080", size = 2.5) +
  geom_label(aes(y = cobertura * 3.5, label = paste0(round(cobertura, 0), "%")), 
             fill = "#000080", color = "white", label.size = 0.5, size = 4) +
  scale_x_continuous(breaks = seq(0, 50, 5)) +
  scale_y_continuous(
    name = "Distancia promedio (km)",
    breaks = seq(0, 350, 50),
    sec.axis = sec_axis(~./3.5, name = "Cobertura (% <180 km)", labels = function(x) paste0(x, "%"),
                        breaks = seq(0, 100, 10))
  ) +
  labs(
    x = "Número de puntos de acopio",
    title = "Cambio cobertura según número de centros de acopio"
  ) +
  theme_classic(base_family = "Arial") +
  theme(
    axis.title.y.left = element_text(color = "#FF7F00", size = 13),
    axis.text.y.left = element_text(color = "#FF7F00",size = 11),
    axis.title.y.right = element_text(color = "#000080", size = 13),
    axis.text.y.right = element_text(color = "#000080",size = 11),
    axis.text.x = element_text(size = 11),
    axis.title.x = element_text(size = 13),
    plot.title = element_text(hjust = 0.5, size = 13)
  )


cambio_cob

#Guardar gráfica
ggsave("outputs/grafic_cap_029/f1_cambio_cob.jpg",
       plot = cambio_cob, 
       width = 7, 
       height = 5, 
       dpi = 300)

