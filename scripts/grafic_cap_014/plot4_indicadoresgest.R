library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggrepel)

rm(list=ls())

#Datos
datosfig4 <- read_excel("data/data_cap_014/datosfig4.xlsx")

#Organizar la estructura de datos
datosfig4_long <- datosfig4 %>%
  pivot_longer(cols = starts_with("20"),
               names_to = "Año",
               values_to = "Porcentaje") |>
  mutate(
    Año = factor(Año),
    Tipo = factor(Tipo, levels = c("4a.", "4b.", "4c.", "4d."))
  )


# Convertir Año a numérico y crear etiquetas
datosfig4_long <- datosfig4_long |>
  mutate(
    Año_num = as.numeric(as.character(Año)),
    label_pct = paste0(round(Porcentaje, 1), "%")
  )

# Gráfico
gfig4 <- ggplot(datosfig4_long, aes(x = Año_num, y = Porcentaje, color = Nivel, group = Nivel)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_label_repel(
    data = datosfig4_long %>% filter(Nivel != "Nacional"),
    aes(label = ifelse(is.na(as.numeric(Porcentaje)), "", round(as.numeric(Porcentaje), 0)), fill = Nivel),
    color = "white",
    size = 3.5,
    label.size = 0,
    show.legend = FALSE,
    max.overlaps = 20,
    box.padding = 0.2,
    point.padding = 0.3,
    direction = "both",
    segment.color = "grey80",
    force = 2
  ) +
  facet_wrap(~ Tipo, ncol = 1, scales = "free_y") +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.3))) +
  scale_color_manual(
    values = c(
      "Nacional" = "#1E90FF",
      "Contributivo" = "#EE6A50",
      "Subsidiado" = "grey50"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Nacional" = "#1E90FF",
      "Contributivo" = "#EE6A50",
      "Subsidiado" = "grey50"
    )
  ) +
  labs(
    x = NULL,
    y = "Porcentaje de casos (%)",
    color = NULL,
    fill = NULL
  ) +
  theme_classic(base_family = "Arial") +
  theme(
    strip.placement = "outside",
    strip.text = element_text(size = 13, hjust = 0, margin = margin(t = 0, b = 5, l = 5)),
    strip.background = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    legend.text = element_text(size = 12),
    panel.spacing = unit(1, "lines"),
    plot.margin = margin(15, 15, 15, 15)
  ) +
  coord_cartesian(clip = "off")

gfig4





ggsave("outputs/grafic_cap_014/f4_indicadores.jpg",
       plot = gfig4, 
       width = 8, 
       height = 7, 
       dpi = 300)
