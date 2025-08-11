library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggrepel) 
library(scales)

rm(list=ls())

#Datos
datosfig3 <- read_excel("data/data_cap_014/datosfig3.xlsx")


#Transformar a formato largo
datosfig3_long <- datosfig3 |>
  pivot_longer(cols = starts_with("C-") | starts_with("S-"),
               names_to = "Grupo",
               values_to = "Casos") |>
  mutate(
    Mes = factor(Mes, levels = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
                                 "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")),
    Panel = factor(Panel, levels = c("3a.", "3b.", "3c.","3d."))
  )




# Gráfica
gparaclinicos <- ggplot(datosfig3_long, aes(x = Mes, y = Casos, color = Grupo, group = Grupo)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ Panel, ncol = 1, scales = "free_y", strip.position = "top") + 
  scale_color_manual(
    values = c(
      "C-2019" = "#1E90FF",   
      "C-2020" = "#EE6A50",   
      "S-2019" = "grey50", 
      "S-2020" = "#EEC900"    
    )
  ) +
  scale_y_continuous(labels = label_number())+
  labs(
    x = "Meses",
    y = "Número de casos",
    color = NULL
  ) +
  theme_classic(base_family = "Arial") +
  theme(
    strip.placement = "outside",              
    strip.text = element_text(                
      size = 13,                              
      hjust = 0,                              
      margin = margin(t = 0, b = 5, l = 5)    
    ),
    strip.background = element_blank(),  
    axis.text.x = element_text(angle = 45, hjust = 1, size=12),
    axis.text.y = element_text(size=12),
    axis.title.x = element_text(size=13),
    axis.title.y = element_text(size=13),
    legend.text = element_text(size=12),
    panel.spacing.y = unit(1, "lines")
  )


gparaclinicos

#Guardar Gráfica
ggsave("outputs/grafic_cap_014/f3_paraclinicos.jpg",
       plot = gparaclinicos, 
       width = 8, 
       height = 7, 
       dpi = 300)
