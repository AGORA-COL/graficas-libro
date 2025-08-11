library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggrepel) 

rm(list=ls())

#Datos
datosfig1 <- read_excel("data/data_cap_014/datosfig1.xlsx") |>
  filter(Nivel %in% c("Nacional", "Contributivo", "Subsidiado"))

#Datos en formato ancho a largo
datos_long <- datosfig1 |>
  pivot_longer(
    cols = starts_with("20"),
    names_to = "Año",
    values_to = "Porcentaje"
  ) |>
  mutate(Año = as.integer(Año))


#Gráfica
brechasdet <- ggplot(datos_long, aes(x = Año, y = Porcentaje, color = Nivel)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_label_repel(
    data = subset(datos_long, Nivel %in% c("Contributivo", "Subsidiado")),
    aes(label = round(Porcentaje, 0), fill = Nivel),
    color = "white",
    size = 2.5,
    label.size = 0,
    show.legend = FALSE,
    max.overlaps = 20,
    box.padding = 0.2,
    point.padding = 0.3,
    direction = "both",
    segment.color = "grey80",
    force = 2
  ) +
  facet_wrap(~ Evento, ncol = 1, scales = "free_y") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  scale_color_manual(values = c(
    "Nacional" = "#1E90FF", 
    "Contributivo" = "#EE6A50", 
    "Subsidiado" = "grey50"
  )) +
  scale_fill_manual(values = c(
    "Contributivo" = "#EE6A50", 
    "Subsidiado" = "grey50"
  )) +
  theme_classic(base_family = "Arial") +
  theme(
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_text(
      size = 10,
      hjust = 0,
      margin = margin(t = 0, b = 5, l = 5)
    ),
    legend.title = element_blank()
  ) +
  labs(
    x = "", y = "Porcentaje de casos (%)",
    color = "Régimen",
    fill = "Régimen"
  ) +
  coord_cartesian(clip = "off")

brechasdet



#Guardar gráfica
ggsave("outputs/grafic_cap_014/f1_brechasdet.jpg",
       plot = brechasdet, 
       width = 7, 
       height = 5, 
       dpi = 300)


