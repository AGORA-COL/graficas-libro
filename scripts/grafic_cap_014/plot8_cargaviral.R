library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)

rm(list=ls())

#Datos
datosfig8 <- read_excel("data/data_cap_014/datosfig8.xlsx")

# Transformar a formato largo
datosfig8_long <- datosfig8 |>
  pivot_longer(cols = c(`2020`, `2019`), 
               names_to = "Año", 
               values_to = "Valor") |>
  mutate(`Mes realización CV` = factor(`Mes realización CV`,
                                       levels = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
                                                  "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")),
         Año = as.factor(Año))

# Gráfico
cargaviral <- ggplot(datosfig8_long, aes(x = `Mes realización CV`, y = Valor, group = Año, color = Año)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  # geom_label(aes(label = Valor, fill = Año), 
  #            color = "white", 
  #            vjust = -1.2, 
  #            label.size = 0.3,
  #            size = 3,
  #            show.legend = FALSE) +
  facet_wrap(~ tipo, ncol = 1) +
  scale_color_manual(values = c("2020" = "#ff7f0e", "2019" = "grey50")) +
  scale_y_continuous(breaks = seq(0, 22000, by=3000), limits = c(0, 22000))+
  scale_fill_manual(values = c("2020" = "#ff7f0e", "2019" = "grey50")) +
  labs(x = "Mes", y = "Casos prevalentes") +
  theme_classic(base_family = "Arial") +
  theme(
    strip.placement = "outside",
    strip.text = element_text(size = 13, hjust = 0, margin = margin(t = 0, b = 5, l = 5)),
    strip.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13, margin = margin(r=5)),
    legend.text = element_text(size = 12),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(color = "black"),
    panel.spacing.y = unit(1, "lines")
  ) 

cargaviral

#Guardar gráfico
ggsave("outputs/grafic_cap_014/f8_cargaviral.jpg",
       plot = cargaviral, 
       width = 7, 
       height = 5, 
       dpi = 300)
