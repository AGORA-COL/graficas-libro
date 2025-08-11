library(readxl)
library(tidyverse)
library(ggplot2)
library(ggrepel)

rm(list=ls())

#Datos
datosfig7 <- read_excel("data/data_cap_014/datosfig7.xlsx")

# Datos en formato largo
datosfig7_long <- datosfig7 |>
  pivot_longer(cols = -Mes, names_to = "Año", values_to = "Casos") |>
  filter(Año %in% c("2019", "2020")) |>
  mutate(
    Año = factor(Año, levels = c("2020", "2019")),
    Mes = factor(Mes, levels = c(
      "Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
      "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"
    ))
  )



# Gráfica
cambiotar <- ggplot(datosfig7_long, aes(x = Mes, y = Casos, group = Año, color = Año)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_label_repel(
    aes(label = Casos, fill = Año),
    color = "white",
    size = 3.5,
    label.size = 0,
    show.legend = FALSE,
    max.overlaps = 20,
    box.padding = 0.2,
    point.padding = 0.3,
    direction = "both",
    segment.color = "grey80",
    force = 0.01
  )+
  scale_y_continuous(
    breaks = seq(0, 200, by=20)
    ) +
  scale_color_manual(values = c("2020" = "gray50", "2019" = "#1E90FF")) +
  scale_fill_manual(values = c("2020" = "gray50", "2019" = "#1E90FF")) +
  labs(
    x = "",
    y = "Casos prevalentes con cambio de la TAR",
    title = NULL,
    color = NULL
  ) +
  theme_classic(base_family = "Arial") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    legend.position = "bottom",
    legend.text = element_text(size = 13)
  )

cambiotar

#Guardar gráfica
ggsave("outputs/grafic_cap_014/f7_cambiotar.jpg",
       plot = cambiotar, 
       width = 7, 
       height = 5, 
       dpi = 300)




