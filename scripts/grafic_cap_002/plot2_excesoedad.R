library(ggplot2)
library(ggrepel)
library(readr)
library(dplyr)

rm(list=ls())

#Datos
df_fig <- read_csv("data/data_cap_002/datosfig2.csv")

# Asegurar el orden deseado de los grupos de edad
df_fig$gredad <- factor(df_fig$gredad, levels = c("Menores de 15 años", 
                                                  "De 15 a 44 años", 
                                                  "De 45 a 64 años", 
                                                  "De 65 y más años"))

# Crear gráfico
fig2 <- ggplot(df_fig, aes(x = `6 Mar.2020 - 5 Mar. 2021`,
                           y = `6 Mar.2021 - 5 Mar. 2022`,
                           label = Departamentos)) +
  geom_abline(color = "#1E90FF", linetype = 2) +
  geom_text_repel(segment.color = "white") +
  geom_point() +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  facet_grid(gredad ~ SEXO) +  # Edad en filas, sexo en columnas
  xlab("Porcentaje de exceso de mortalidad (6 mar.2020 - 5 mar. 2021)") +
  ylab("Porcentaje de exceso de mortalidad (6 mar.2021 - 5 mar. 2022)") +
  theme_classic(base_family = "Arial") +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 13),
    axis.title.y = element_text(size = 14, margin = margin(r=10)),
    axis.title.x = element_text(size = 14),
    axis.text.x = element_text(size = 13, margin = margin(t = 5, b = 15)),
    axis.text.y = element_text(size = 13),
    strip.text = element_text(size = 13),
    panel.spacing.y = unit(0.5, "cm"),
    panel.spacing.x = unit(0.5, "cm"),
    panel.background = element_rect(colour = "black", linetype = "solid")
  )

fig2

ggsave(filename = "outputs/grafic_cap_002/f2_excesoedad.jpg", 
       plot = fig2, 
       width =7, 
       height = 10, 
       dpi = 300)


