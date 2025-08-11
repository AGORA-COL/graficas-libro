library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggrepel) 

rm(list=ls())

#Datos
datosfig2 <- read_excel("data/data_cap_014/datosfig2.xlsx")

#Convertir los datos a formato largo
# Convertir los datos a formato largo
datosfig2_long <- datosfig2 |>
  pivot_longer(
    cols = starts_with(c("C-", "S-")),
    names_to = "Grupo",
    values_to = "Fallecidos"
  ) |>
  mutate(
    Grupo = factor(Grupo, levels = c("C-2019", "C-2020", "S-2019", "S-2020")),
    Mes = factor(Mes, levels = c(
      "Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
      "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"
    ))
  )


#Ordenar los niveles de las facetas
datosfig2_long$fallecidos <- factor(
  datosfig2_long$fallecidos,
  levels = c("Diagnóstico de HTA", "Diagnóstico de DM", "Diagnóstico de ERC")
)

#Gráfica
gfallecidosdx <- ggplot(datosfig2_long, aes(x = Mes, y = Fallecidos, color = Grupo, group = Grupo)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ fallecidos, ncol = 1, scales = "free_y", strip.position = "left") +
  scale_color_manual(
    values = c(
      "C-2019" = "#1E90FF",   
      "C-2020" = "#EE6A50",   
      "S-2019" = "grey50", 
      "S-2020" = "#EEC900"    
    )
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.1, 0.3))
  )+
  labs(
    x = "Meses",
    y = "Número de casos fallecidos con",
    color = NULL
  ) +
  theme_classic(base_family = "Arial") +
  theme(
    strip.placement = "outside",
    strip.text = element_text(size = 13),
    strip.background = element_blank(),          
    axis.text.x = element_text(angle = 45, hjust = 1, size=12),
    axis.text.y = element_text(size=12),
    axis.title.x = element_text(size=13),
    axis.title.y = element_text(size=13),
    legend.text = element_text(size=12),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(color = "black"),
    panel.spacing.y = unit(1, "lines")
    )+
  coord_cartesian(clip = "off")


gfallecidosdx

#Guardar gráfica
ggsave("outputs/grafic_cap_014/f2_fallecidosdx.jpg",
       plot = gfallecidosdx, 
       width = 8, 
       height = 7, 
       dpi = 300)
