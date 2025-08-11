library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(lubridate)
library(readxl)

rm(list=ls())

#Datos
df_flujo_vacunacion <- read_excel("data/data_cap_029/datosfi3_flujovacunacion.xlsx")
df_flujo_vacunacion$Fecha <- as.Date(df_flujo_vacunacion$Fecha)

# Convertir a formato largo
df_long <- df_flujo_vacunacion |>
  pivot_longer(
    cols = c("Arribos", "Aplicaciones realizadas", "Inventario"),
    names_to = "Variable",
    values_to = "Dosis"
  )

#Gráfica
flujovac <- ggplot(df_long, aes(x = Fecha, y = Dosis, color = Variable)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) + 
  scale_color_manual(
    values = c(
      "Arribos" = "#002060",
      "Aplicaciones realizadas" = "#00CD66",
      "Inventario" = "#FFA500"
    ),
    guide = guide_legend(override.aes = list(size = 3))) +  
  scale_y_continuous(
    labels = label_comma(big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0, 0.05))  
  ) +
  scale_x_date(
    breaks = seq(min(df_long$Fecha),max(df_long$Fecha),by="2 week"),  
    date_labels = "%d-%b") +
  scale_y_continuous(
    breaks = seq(0,16000000, by=2000000),
    limits =  c(0,16000000),
    labels = label_number(),
    expand = c(0,0)
  )+
  labs(
    title = "Flujo nacional de vacunación (marzo - oct 2021)",
    x = "Semana",
    y = "Dosis",
    color = NULL
  ) +
  theme_classic(base_family = "Arial") +
  theme(
  axis.text.x = element_text(angle = 90, hjust = 1, size = 12),
  axis.text.y = element_text(size = 12),
  axis.title.x = element_text(size = 13),
  axis.title.y = element_text(size = 13),
  legend.text =  element_text(size = 12),
  plot.title = element_text(hjust = 0.5, face = "plain", size = 15, margin = margin(b = 10)),
  legend.position = c(0.43, 0.65),
  legend.justification = c(1, 0),
  legend.background = element_rect(color = "gray70", fill = "white", size = 0.5),
  legend.box = "vertical",
  legend.spacing.y = unit(2, "mm"), 
  legend.key = element_rect(fill = "white")) +
  coord_cartesian(clip = "off")


flujovac

#Guardar gráfica
ggsave("outputs/grafic_cap_029/f3_flujovac.jpg",
       flujovac,
       width = 7, 
       height = 5, 
       dpi = 300)
