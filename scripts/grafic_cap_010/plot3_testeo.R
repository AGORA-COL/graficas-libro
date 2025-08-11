library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)
library(readxl)
library(lubridate)
library(grid)
library(gtable)
library(gridExtra)
library(ggh4x)
library(tidyverse)

rm(list=ls())

# Datos
datos_pruebas <- read_excel("data/data_cap_010/datosfig3.xlsx")

# Crear variable de tiempo
datos <- datos_pruebas |>
  mutate(Fecha = as.Date(paste(Año, Mes, "01"), format="%Y %B %d"))


# Crear variable de tiempo
datos <- datos |> mutate(Fecha = as.Date(paste(Año, Mes, "01"), format="%Y %B %d"))

# Transformar los datos a formato largo
datos_long <- datos |>
  select(Fecha, incremento_men_capac_testeo, capacidad_acum_testeo) |>
  pivot_longer(cols = c(capacidad_acum_testeo, incremento_men_capac_testeo),
               names_to = "Tipo",
               values_to = "Valor") |>
  mutate(Tipo = factor(Tipo, levels = c("incremento_men_capac_testeo","capacidad_acum_testeo")))  


# Crear gráfico
gr_testeo<-ggplot() +
  geom_bar(data = datos_long, aes(x = Fecha, y = Valor, fill = Tipo),
           stat = "identity", position = "stack", alpha = 0.7, color="black") +
  geom_line(data = datos, aes(x = Fecha, y = Num_pruebas_posit, group = 1, color = "Pruebas positivas"),
            size = 1.2) +
  scale_fill_manual(values = c("capacidad_acum_testeo" = "yellow", 
                               "incremento_men_capac_testeo" = "#8B3A62"),
                    labels = c("Capacidad acumulada", "Incremento mensual")) +
  scale_color_manual(values = c("Pruebas positivas" = "blue")) +
  scale_y_continuous(name = "Histórico de pruebas procesadas acumuladas",
                     labels = comma_format(),  
                     sec.axis = sec_axis(~ ., 
                                         name = "Histórico de pruebas positivas acumuladas", 
                                         labels = NULL)) +  
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%b %y", 
               expand = c(0, 0)) +
  theme_minimal() +
  theme(axis.text.x.bottom = element_text(angle = 90, vjust = 1, hjust = 1, size = 12), 
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = -2.5, size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 13, face="bold"),
        axis.title.x = element_blank(),
        axis.title.y.right = element_text(size = 13, color = "blue", face="bold"),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        panel.spacing = unit(0.05, "cm"))


gr_testeo

#Guardar gráfica
ggsave("outputs/grafic_cap_010/f3_testeo.jpg",
       plot = gr_testeo,
       width=8,
       height=7,
       dpi=300)
