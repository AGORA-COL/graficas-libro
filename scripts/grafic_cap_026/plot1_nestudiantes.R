library(readr)
library(ggplot2)
library(dplyr)
library(scales) 

rm(list=ls())

#Datos
datos_icfes <- read_delim("data/data_cap_026/datosfig1_icfes.csv", 
                          delim = "\t",
                    locale = locale(decimal_mark = ",")) |>
  mutate(anio = as.character(anio))

# Gráfica
gr_no_estudiantes<-ggplot(datos_icfes,aes(x = anio, y = numero_estudiantes)) +
  geom_col(fill = "#5CACEE", width = 0.8, color="black") + 
  geom_text(aes(label = comma(numero_estudiantes, accuracy = 1)), 
            vjust = -0.5, 
            size = 3.5,
            color = "black") + 
  scale_y_continuous(labels = comma, 
                     expand = expansion(mult = c(0, 0.1))) + 
  coord_cartesian(ylim = c(450000, 500000)) + 
  labs(#title = "Número de estudiantes por año",
       #subtitle = "Periodo 2017-2022",
       x = "",
       y = "Número de estudiantes",
       caption = "Fuente: Bases de datos del ICFES") +
  theme_classic()+
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray50"),
    plot.caption = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 12, color = "black"),
    axis.text = element_text(size = 10, color = "black"),
    axis.title.y = element_text(margin = margin(r=10)),
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.y = element_line(color = "gray90",linewidth = 0.3)
  )
gr_no_estudiantes

#Gráfica
ggsave("outputs/grafic_cap_026/f1_estudiantes.jpg",
       plot = gr_no_estudiantes,width=8,height=5,dpi=300)
