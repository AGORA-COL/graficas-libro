library(readr)
library(ggplot2)
library(dplyr)
library(scales) 
library(tidyr)
library(gridExtra)
library(cowplot)

rm(list=ls())

#Datos
datos_icfes <- read_delim("data/data_cap_026/datosfig_icfes.csv", 
                          delim = "\t",
                          locale = locale(decimal_mark = ",")) |>
  mutate(anio = as.character(anio))

#Gráfica
gr_puntajes<-ggplot(datos_icfes,aes(x=anio,y=puntaje_global, group=1))+
  geom_line(color = "#5CACEE",linewidth = 1.2)+
  geom_point(color = "#5CACEE", size=3)+
  geom_text(aes(label = comma(puntaje_global, accuracy = 0.1)), 
            vjust = -0.5, 
            size = 3.5,
            color = "black") + 
  coord_cartesian(ylim = c(245,260))+
  scale_y_continuous(labels = comma, 
                     expand = expansion(mult = c(0, 0.1))) + 
  labs(title="A. Puntaje global",
       x = "",
       y = "") +
  theme_classic()+
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12, color = "black"),
    axis.text = element_text(size = 10, color = "black"),
    axis.title.y = element_text(margin = margin(r=10)),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.major.x = element_blank()
  )

gr_puntajes


#Puntajes globales
datos_icfes_long <- datos_icfes %>%
  pivot_longer(
    cols = -c(anio, numero_estudiantes, puntaje_global), 
    names_to = "materia", 
    values_to = "puntaje"
  ) %>%
  mutate(materia = case_when(
    materia == "matematicas" ~ "Matemáticas",
    materia == "lectura_critica" ~ "Lectura Crítica",
    materia == "ciencias_naturales" ~ "Ciencias Naturales",
    TRUE ~ materia
  ))

#Gráfica
gr_puntajesxmat <- ggplot(datos_icfes_long, aes(x = anio, y = puntaje, color = materia, group = materia)) +
  geom_line(linewidth = 1.2, alpha = 0.8) +
  geom_point(size = 3) +
  scale_color_manual(
    name = "Materia",
    values = c("Matemáticas" = "#CD6090", 
               "Lectura Crítica" = "#9A32CD", 
               "Ciencias Naturales" = "#458B00"),
    breaks = c("Lectura Crítica", "Matemáticas", "Ciencias Naturales")  # orden deseado
  ) +
  coord_cartesian(ylim = c(47,55)) +
  scale_y_continuous(
    labels = scales::comma_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.1))
  ) + 
  labs(
    x = "",
    y = "",
    title = "B. Puntaje promedio por materia",
    caption = "Fuente: Bases de datos del ICFES"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.caption = element_text(size = 10),
    axis.title = element_text(size = 12, color = "black"),
    axis.text = element_text(size = 10, color = "black"),
    axis.title.y = element_text(margin = margin(r = 15)),
    legend.position = "bottom",
    legend.text = element_text(size=11),
    legend.title = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.major.x = element_blank()
  )


gr_puntajesxmat


#guardar gráfica combinada
gr_puntajest <- plot_grid(
  gr_puntajes, gr_puntajesxmat, 
  ncol = 1,                
  rel_heights = c(1, 1.2) 
)

gr_puntajest

ggsave("outputs/grafic_cap_026/f2_puntajest.jpg",
       plot = gr_puntajest,
       width = 7,
       height = 7,
       dpi=300)

