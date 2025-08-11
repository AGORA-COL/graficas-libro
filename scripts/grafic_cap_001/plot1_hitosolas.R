library(dplyr)
library(ggplot2)
library(readr)
library(epitrix)
library(zoo)
library(extrafont) #fuentes de texto

font_import()  
loadfonts(device = "win") 

rm(list=ls())

#Datos
casos_diarios<- read_csv("data/data_cap_001/df_casos_diarios.csv")

#=========
# EVENTOS 
#=========

eventos <- read_csv("data/data_cap_001/eventos_version2.csv", 
                    locale = locale(encoding = "UTF-8")) |>
  rename(
    fecha = `Fecha`,
    evento = `Situacion o evento`,
    categoria = `Categoría`
  ) |>
  mutate(
    fecha = as.Date(fecha),
    etiqueta = ifelse(
      fecha == as.Date("2023-05-05"),
      paste0(format(fecha, "%Y-%m-%d"), "\nFin del uso obligatorio\nde tapabocas"),
      paste0(format(fecha, "%Y-%m-%d"), "\n", evento)
    )
  ) |>
  arrange(fecha)

# Espacios de las etiquetas (se alternan)
espacios <- c(35000, 40000, 49000, 60000, 54000, 45000, 59000)
eventos$y_pos <- rep(espacios, length.out = nrow(eventos))

# Colores por categoría
categoria_colores <- c(
  "Epidemiología" = "#1f78b4",
  "Intervenciones no farmacológicas" = "#33a02c",
  "Intervenciones farmacológicas" = "#8B3A62"
)

eventos_derecha <- eventos[-nrow(eventos), ]  
eventos_izquierda <- eventos[nrow(eventos), ]  

# Gráfico
gpositivos_hitos <- ggplot(casos_diarios, aes(x = fecha_inicio_sintomas, y = frecuencia)) +
  geom_col(na.rm = TRUE, color = "#2b83ba", fill = "#2b83ba", alpha = 0.7) +
  coord_cartesian(
    ylim = c(0, 63000),
    xlim = c(as.Date("2020-01-01"), as.Date("2023-06-30"))  
  ) +
  scale_x_date(
    breaks = "3 month", 
    date_labels = "%b %Y",
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    n.breaks = 6,
    expand = c(0, 0)
  ) +
  labs(
    x = "",
    y = "Número de casos diarios"
  ) +
  geom_segment(
    data = eventos, 
    aes(x = fecha, xend = fecha, y = 0, yend = y_pos, color = categoria), 
    linetype = "dashed", 
    linewidth = 0.7, 
    show.legend = TRUE
  ) +
  geom_label(
    data = eventos_derecha,
    aes(x = fecha, y = y_pos, label = etiqueta, color = categoria),
    label.padding = unit(0.2, "lines"),
    size = 3.1,
    hjust = 0,  
    label.size = 0.25,
    show.legend = FALSE,
    family = "Arial"
  ) +
  geom_label(
    data = eventos_izquierda,
    aes(x = fecha, y = y_pos, label = etiqueta, color = categoria),
    label.padding = unit(0.2, "lines"),
    size = 3.1,
    hjust = 1,  
    nudge_x = -5,  
    label.size = 0.25,
    show.legend = FALSE,
    family = "Arial"
  ) +
  scale_color_manual(values = categoria_colores) +
  guides(
    color = guide_legend(
      override.aes = list(linewidth = 0.5, linetype = "dashed")
    )
  ) +
  theme_classic(base_family = "Arial") +
  theme(
    text = element_text(color = "black"),
    axis.text.x = element_text(color = "black", size=11),
    axis.text.y = element_text(color = "black", size=11),
    axis.title = element_text(color = "black"),
    legend.text = element_text(size = 10),
    legend.position = c(0.90, 0.82),
    legend.title = element_blank(),
    legend.box.background = element_rect(color = "black", linewidth = 1),
    plot.margin = margin(r = 20, t = 30),
    axis.title.x = element_text(size = 12, margin = margin(t = 11)),
    axis.title.y = element_text(size = 12, margin = margin(l=5,r = 11))
  )

gpositivos_hitos

ggsave(filename = "outputs/grafic_cap_001/f1_cpositivos_hitos.jpg", 
       plot = gpositivos_hitos, 
       width = 13, 
       height = 8, 
       dpi = 300)
