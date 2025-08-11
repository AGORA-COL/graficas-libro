library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(lubridate)
library(readxl)
library(tidyverse)

rm(list=ls())

#Datos
df_flujo_vacunacionxciudad <- read_excel("data/data_cap_029/datosfi4_flujovacunacion.xlsx")

# Convertir a formato largo
df_long <- df_flujo_vacunacionxciudad |>
  pivot_longer(cols = c(Aplicación, Recibos, `Promedio de inventario`),
               names_to = "Variable",
               values_to = "Valor")

df_long <- df_long |>
  mutate(Fecha = factor(Fecha, levels = unique(Fecha))) 

colores <- c(
  "Aplicación" = "#6DA544", 
  "Recibos" = "#003399",    
  "Promedio de inventario" = "#FFA500"  
)

# gráfico 
fvac_ciudades <- ggplot(df_long, aes(x = Fecha, y = Valor, group = Variable, color = Variable)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~Ciudad, ncol=1) +
  scale_color_manual(values = colores) +
  labs(
    title = "Flujo de vacunación",
    y = "Dosis",
    x = "Fecha",
    color = NULL
  ) +
  scale_y_continuous(
    breaks = seq(0,1000000, by=100000),
    labels = label_number()
  )+
  scale_x_discrete(
    breaks = levels(df_long$Fecha)[c(TRUE, FALSE)]  
  )+
  theme_classic(base_family = "Arial") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 13, margin = margin(t=5)),
    axis.title.y = element_text(size = 13,margin = margin(l=5)),
    legend.text =  element_text(size = 12),
    strip.text = element_text(size = 12),
    legend.position = "top",
    plot.title = element_text(hjust=0.5, margin = margin(t=10)),
    plot.margin = margin(r=15),
    panel.spacing.x = unit(0.7,"cm")
  )

fvac_ciudades


ggsave("outputs/grafic_cap_029/f4_fvac_ciudades.jpg",
       fvac_ciudades,
       height = 8,
       width = 7,
       dpi = 300)
