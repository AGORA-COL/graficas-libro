library(ggplot2)
library(dplyr)
library(scales)
library(readxl)

rm(list=ls())

#Datos
datosfig2 <- read_excel("data/data_cap_008/datosfig2.xlsx")

#Crear gráfico
gr_ghs <- ggplot(datosfig2, aes(x = `GHS Index`, y = (`Sensibilidad...6`*100), size = `Población...2`, label = País)) +
  geom_point(color = "#00688B", alpha = 0.7) +
  geom_text_repel(size = 4, max.overlaps = 10) +
  scale_size(range = c(3, 15), 
             breaks = c(20000000, 50000000, 100000000),
             name = "Población", 
             labels = scales::comma) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0)),
    breaks = seq(0, 30, by = 5)  # Asegura breaks cada 5%
  ) +
  scale_x_continuous(breaks=seq(30, 90, by=10),
                     expand = expansion(mult = c(0,0))) +
  coord_cartesian(ylim = c(0, 30), xlim = c(30, 90)) +
  labs(
    x = "Global Health Security Index (JHU)",
    y = "Sensibilidad del sistema de vigilancia"
  ) +
  theme_classic(base_family = "Arial") +
  theme(
    plot.caption = element_text(size = 10, hjust = 1.6),
    axis.text = element_text(size = 10),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10, b = 10)),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

gr_ghs

#Guardar gráfica

ggsave("outputs/grafic_cap_008/f2_ghs.jpg",
       width = 8,
       height = 5,
       dpi=300)
