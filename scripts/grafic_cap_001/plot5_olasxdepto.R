library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(zoo)
library(splines)
library(gridExtra)
library(readr)

rm(list=ls())

# Datos
casos_diarios<- read_csv("data/data_cap_001/df_casos_diarios.csv")

# Agrupación por casos diarios
casos_diarios_col <- casos_diarios |> 
  filter(fecha_inicio_sintomas<="2023-06-30") |>  
  mutate(Departamento="Colombia")

casos_diarios_dept <- casos_diarios |>
  filter(fecha_inicio_sintomas<="2023-06-30")

# Unir los datos de Colombia con los datos departamentales
datos <- bind_rows(casos_diarios_dept, casos_diarios_col)
datos$Departamento <- factor(datos$Departamento, levels = c("Colombia", sort(setdiff(unique(datos$Departamento), "Colombia"))))


# Gráfica casos
source("scripts/grafic_cap_001/fechas_olas.r")

g_casos <- ggplot(data = datos, aes(x = fecha_inicio_sintomas)) +
  geom_rect(aes(xmin = fechas_ola1[1], xmax = fechas_ola1[2], ymin = -Inf, ymax = Inf),
            fill = "#C1CDCD", alpha = 0.02, inherit.aes = FALSE) +
  geom_rect(aes(xmin = fechas_ola2[1], xmax = fechas_ola2[2], ymin = -Inf, ymax = Inf),
            fill = "#C1CDCD", alpha = 0.02, inherit.aes = FALSE) +
  geom_rect(aes(xmin = fechas_ola3[1], xmax = fechas_ola3[2], ymin = -Inf, ymax = Inf),
            fill = "#C1CDCD", alpha = 0.02, inherit.aes = FALSE) +
  geom_rect(aes(xmin = fechas_ola4[1], xmax = fechas_ola4[2], ymin = -Inf, ymax = Inf),
            fill = "#C1CDCD", alpha = 0.02, inherit.aes = FALSE) +
  geom_col(aes(y = frecuencia),
           fill = "#2b83ba", color = "#2b83ba", alpha = 0.7)+
  facet_wrap(~ Departamento, 
             ncol = 3,
             scales = "free_y" 
  ) +
  labs(x = "", y = "Número de casos diarios") +
  scale_x_date(breaks = seq(as.Date("2020-02-01"), as.Date("2023-06-30"), by = "20 month"),
               date_labels = "%b %Y", expand = c(0, 0)) +
  scale_y_continuous(
    breaks = function(x) seq(0, x[2], length.out = 3),
    labels = function(x) signif(round(x, 0), digits = 2),
    expand = expansion(mult = c(0, 0.2))
  ) +
  coord_cartesian(xlim = c(as.Date("2020-02-01"), as.Date("2023-06-30"))) +
  theme_classic(base_family = "Arial") +
  theme(
    strip.text = element_text(size = 17),
    axis.text.x = element_text(size = 14, margin = margin(t = 10, b=20)),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 19, margin = margin(l=10, r=10)),
    panel.spacing.x = unit(2, "lines"),
    panel.spacing.y = unit(1, "lines"),
    panel.background = element_rect(colour = "black", linetype = "solid"),
    plot.margin = margin(r=30, t=5)
  ) 

g_casos


# Guardar la imagen
ggsave(filename = "outputs/grafic_cap_001/f5_olasxdepto.jpg", 
       plot = g_casos, 
       width = 14, 
       height = 18, 
       dpi = 300)

