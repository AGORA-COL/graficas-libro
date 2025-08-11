library(readr)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(lubridate)

rm(list=ls())

# Datos
df_fallecidos_semanal <- read_csv("data/data_cap_001/df_fallecidos_semanal.csv")

# Cargar fechas de olas 
source("scripts/grafic_cap_001/fechas_olas.r")

y_val=4700

# Gráfico por ola
muertesxolas <- ggplot() +
  geom_col(data=df_fallecidos_semanal,aes(x = fecha_muerte, y = muertes , fill = grupo_edad), position = position_stack(reverse = TRUE) ) +
  geom_rect(aes(xmin = fechas_ola1[1], xmax = fechas_ola1[2], ymin = -Inf, ymax = Inf),
            fill = "#C1CDCD", alpha = 0.2) +
  geom_rect(aes(xmin = fechas_ola2[1], xmax = fechas_ola2[2], ymin = -Inf, ymax = Inf),
            fill = "#C1CDCD", alpha = 0.2) +
  geom_rect(aes(xmin = fechas_ola3[1], xmax = fechas_ola3[2], ymin = -Inf, ymax = Inf),
            fill = "#C1CDCD", alpha = 0.2) +
  geom_rect(aes(xmin = fechas_ola4[1], xmax = fechas_ola4[2], ymin = -Inf, ymax = Inf),
            fill = "#C1CDCD", alpha = 0.2) +
  geom_vline(data=NULL, xintercept = fechas_ola1[2], color= "#838B8B", size=1)+
  geom_vline(data=NULL, xintercept = fechas_ola2[1], color= "#838B8B", size=1)+
  geom_vline(data=NULL, xintercept = fechas_ola2[2], color= "#838B8B", size=1)+
  geom_vline(data=NULL, xintercept = fechas_ola3[1], color= "#838B8B", size=1)+
  geom_vline(data=NULL, xintercept = fechas_ola3[2], color= "#838B8B", size=1)+
  geom_vline(data=NULL, xintercept =fechas_ola4[1], color= "#838B8B", size=1)+
  geom_vline(data=NULL, xintercept = fechas_ola4[2], color= "#838B8B", size=1)+
  annotate("label", x = mean(fechas_ola1), y = y_val, label = "Ola 1", fill = "#2b83ba", color = "white") +
  annotate("label", x = mean(fechas_ola2), y = y_val, label = "Ola 2", fill = "#2b83ba", color = "white") +
  annotate("label", x = mean(fechas_ola3), y = y_val, label = "Ola 3", fill = "#2b83ba", color = "white") +
  annotate("label", x = mean(fechas_ola4), y = y_val, label = "Ola 4", fill = "#2b83ba", color = "white") +
  labs(title = "A. Fallecimientos semanales por Covid-19",
       x = "",
       y = "Número de fallecidos",
       fill = "Grupo de edad") +
  scale_x_date(
    date_breaks = "2 month", 
    date_labels = "%b %Y", 
    expand = c(0, 0),
    limits = c(as.Date("2020-02-01"), as.Date("2023-06-30"))
  ) +
  scale_y_continuous(
    breaks = seq(0, 4900, by = 500),
    expand = c(0, 0),
    limits = c(0,4900)
  )+
  scale_fill_brewer(palette = "RdYlBu", direction=-1) +  
  theme_classic(base_family = "sans")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  
        legend.position = "none")
muertesxolas

#######################################################
# porporcion
#######################################################

df_fallecidos_semanal_prop <- df_fallecidos_semanal %>%
  group_by(fecha_muerte) %>%
  mutate(proporcion = muertes / sum(muertes))


muertesxolas_por  <- ggplot() +
  geom_col(data=df_fallecidos_semanal_prop, aes(x = fecha_muerte, y = proporcion, fill = grupo_edad), position = position_stack(reverse = TRUE)) +
  geom_rect(aes(xmin = fechas_ola1[1], xmax = fechas_ola1[2], ymin = -Inf, ymax = Inf),
            fill = "#C1CDCD", alpha = 0.2) +
  geom_rect(aes(xmin = fechas_ola2[1], xmax = fechas_ola2[2], ymin = -Inf, ymax = Inf),
            fill = "#C1CDCD", alpha = 0.2) +
  geom_rect(aes(xmin = fechas_ola3[1], xmax = fechas_ola3[2], ymin = -Inf, ymax = Inf),
            fill = "#C1CDCD", alpha = 0.2) +
  geom_rect(aes(xmin = fechas_ola4[1], xmax = fechas_ola4[2], ymin = -Inf, ymax = Inf),
            fill = "#C1CDCD", alpha = 0.2) +
  geom_vline(data=NULL, xintercept = fechas_ola1[2], color= "#838B8B", size=1)+
  geom_vline(data=NULL, xintercept = fechas_ola2[1], color= "#838B8B", size=1)+
  geom_vline(data=NULL, xintercept = fechas_ola2[2], color= "#838B8B", size=1)+
  geom_vline(data=NULL, xintercept = fechas_ola3[1], color= "#838B8B", size=1)+
  geom_vline(data=NULL, xintercept = fechas_ola3[2], color= "#838B8B", size=1)+
  geom_vline(data=NULL, xintercept =fechas_ola4[1], color= "#838B8B", size=1)+
  geom_vline(data=NULL, xintercept = fechas_ola4[2], color= "#838B8B", size=1)+
  labs(title = "B. Porcentaje de fallecimientos semanales por Covid-19",
       x = "Fecha de muerte",
       y = "Porcentaje de fallecidos",
       fill = "Grupo de edad") +
  scale_x_date(
    date_breaks = "2 month", 
    date_labels = "%b %Y", 
    expand = c(0, 0),
    limits = c(as.Date("2020-02-01"), as.Date("2023-06-30"))
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1), 
    breaks = seq(0, 1, by = 0.2),
    expand = c(0, 0),
    limits = c(0,1)
  ) +
  scale_fill_brewer(palette = "RdYlBu",direction=-1) +  
  theme_classic(base_family = "Arial")+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.key = element_rect(color = "black", size = 0.3),
    legend.key.size = unit(0.7, "cm"),  
    legend.key.spacing.x = unit(0.4, "cm"),
    legend.text = element_text(size = 8),  
    legend.title = element_text(size = 9),
    legend.margin = margin(t = 5)  
  ) +
  guides(fill = guide_legend(nrow = 1, label.position = "bottom"))

muertesxolas_por

#Unir gráficas
muertes_edades <- grid.arrange(
  muertesxolas,
  muertesxolas_por,
  ncol = 1
)

# Guardar gráfica
ggsave(filename = "outputs/grafic_cap_001/f3_muertesxedad.jpg", 
       plot = muertes_edades, 
       width = 8, 
       height = 8, 
       dpi = 300)



