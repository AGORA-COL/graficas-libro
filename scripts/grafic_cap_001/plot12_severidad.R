library(ggplot2)
library(scales)
library(tidyverse)

rm(list=ls())

df_severidad_olas <- read_excel("data/data_cap_001/df_severidad_olas.xlsx")


# Convertir a formato largo
df_long <- df_severidad_olas |>
  pivot_longer(cols = starts_with("Ola"),
               names_to = "Ola",
               values_to = "Valor") |>
  mutate(
    Grupo_edad = factor(Grupo_edad, levels = unique(df_severidad_olas$Grupo_edad)),
    Evento = factor(Evento, levels = unique(df_severidad_olas$Evento)),
    Ola = factor(Ola, levels = paste("Ola", 1:4))
  )


# Gráfica
g_severidad<-ggplot(df_long, aes(x = Grupo_edad, y = Valor * 100, color = Ola, group = Ola)) +
  geom_line(size = 1) +
  geom_point(size = 1.8) +
  facet_wrap(~ Evento, scales = "free_y", ncol = 2) +
  scale_y_continuous(
    breaks = breaks_pretty(n = 5),  
    labels = function(x) paste0(round(x), "%"), 
    expand = expansion(mult = c(0.05, 0.05))  
  ) +
  scale_color_manual(values = c("Ola 1" = "goldenrod", "Ola 2" = "seagreen3",
                                "Ola 3" = "magenta", "Ola 4" = "royalblue3")) +
  labs(x = "Grupo de edad", y = "", color = NULL) +
  theme_classic(base_family = "Arial") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = c(0.1, 0.88),
    legend.key.height = unit(0.8, "lines"),
    panel.background = element_rect(colour = "black", linetype = "solid")
  )


# Guardar gráfica
ggsave(filename = "outputs/grafic_cap_001/f12_severidadolas.jpg", 
       plot = g_severidad, 
       width = 6, 
       height = 5, 
       dpi = 300)






