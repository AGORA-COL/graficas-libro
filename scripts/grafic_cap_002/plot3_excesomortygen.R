library(ggplot2)
library(cowplot)
library(dplyr)
library(tidyverse)
library(readxl)
library(ggrepel)
library(readr)

rm(list=ls())

#Datos
df_fig3 <- read_csv("data/data_cap_002/datosfig3.csv")

# Reordenar los niveles del factor gredad
df_fig3$gredad <- factor(df_fig3$gredad, levels = c("Menores de 15 años", "De 15 a 44 años", "De 45 a 64 años", "De 65 y más años"))
df_fig3$CODPTORE <- ifelse(df_fig3$CODPTORE =="Archipiélago de San Andrés","San Andrés", df_fig3$CODPTORE)


# Graficar
fig3<-ggplot(df_fig3, aes(y = CODPTORE, fill = Tipo, color = Tipo)) +
  geom_errorbarh(
    aes(
      xmin = pmin(`6 Mar.2020 - 5 Mar. 2021`, `6 Mar.2021 - 5 Mar. 2022`),
      xmax = pmax(`6 Mar.2020 - 5 Mar. 2021`, `6 Mar.2021 - 5 Mar. 2022`)
    ),
    height = 0.3
  ) +
  geom_point(aes(x = `6 Mar.2020 - 5 Mar. 2021`), shape = 21) +
  geom_point(aes(x = `6 Mar.2021 - 5 Mar. 2022`), shape = 4) +
  facet_grid(SEXO ~ gredad) +
  scale_color_manual(values = c("Disminución" = "#1E90FF", "Incremento" = "#FF8247")) +
  scale_fill_manual(values = c("Disminución" = "#1E90FF", "Incremento" = "#FF8247")) +
  scale_x_continuous(labels = scales::percent) +
  xlab("Porcentaje de exceso de mortalidad") +
  ylab(" ") +
  theme_classic(base_family = "Arial") +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size=13),
    axis.title = element_text(size=13),
    axis.text.x = element_text(size=13, margin = margin(t=5,b=5)),
    axis.text.y = element_text(size=11),
    strip.text = element_text(size=13),
    panel.spacing.y = unit(1, "cm"),
    panel.background = element_rect(colour = "black", linetype = "solid")
  )
  
fig3

ggsave(filename = "outputs/grafic_cap_002/f3_excesomortygen.jpg", 
       plot = fig3, 
       width = 10, 
       height = 11, 
       dpi = 300)
