library(readr)
library(ggplot2)
library(scales)
library(tidyverse)
library(tidyr)

rm(list=ls())

# Datos
df_letalidad_hosp_olas <- read_excel("data/data_cap_001/df_letalidad_hosp_olas.xlsx")

# Formato largo 
df_long <- df_letalidad_hosp_olas |>
  pivot_longer(cols = -c(Grupo_edad, Ola),
               names_to = "Region",
               values_to = "Riesgo") |>
  mutate(
    Region = factor(Region, levels = c("Amazonica", "Andina", "Caribe_e_Insular", "Orinoquia", "Pacifica")),
    Ola = factor(Ola, levels = c("B. Ola 1", "C. Ola 2", "D. Ola 3", "E. Ola 4")), 
    Grupo_edad = factor(Grupo_edad, levels = unique(df_letalidad_hosp_olas$Grupo_edad))
  )


df_long <- df_long %>%
  mutate(Ola = recode(Ola,
                      "B. Ola 1" = "A. Ola 1",
                      "C. Ola 2" = "B. Ola 2",
                      "D. Ola 3" = "C. Ola 3",
                      "E. Ola 4" = "D. Ola 4"))

g_letalidadreg <- ggplot(df_long, aes(x = Grupo_edad, y = Riesgo * 100, color = Region, group = Region)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  facet_wrap(~ Ola, nrow = 2) +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    breaks = seq(0, 80, by = 10),
    limits = c(0, 80)
  ) +
  # scale_y_log10(
  #   labels= label_number()
  # ) +
  scale_color_manual(
    values = c(
      "Amazonica" = "#a6d854", 
      "Andina" = "#EE3B3B", 
      "Caribe_e_Insular" = "#fc8d62",
      "Orinoquia" = "#B23AEE", 
      "Pacifica" = "#66c2a5"
    ),
    labels = c(
      "Amazonica" = "Amazónica",
      "Andina" = "Andina",
      "Caribe_e_Insular" = "Caribe e Insular",
      "Orinoquia" = "Orinoquía",
      "Pacifica" = "Pacífica"
    )
  ) +
  labs(
    x = "",
    y = "",
    color = "Región"
  ) +
  theme_classic(base_family = "Arial") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(margin = margin(r = 5)),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.spacing.x = unit(0.5, 'cm'),  
    panel.background = element_rect(colour = "black", linetype = "solid")
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) 

g_letalidadreg


# Guardar gráfica
ggsave(filename = "outputs/grafic_cap_001/f13_letalidadreg.jpg", 
       plot = g_letalidadreg, 
       width = 5, 
       height = 5, 
       dpi = 300)
