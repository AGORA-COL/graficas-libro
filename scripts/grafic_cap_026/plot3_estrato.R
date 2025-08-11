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

#Organizar datos de estrato
datos_estrato_long<-datos_icfes |> 
  pivot_longer(cols = c(estrato_bajo,estrato_medio,estrato_alto),
               names_to = "Estrato",
               values_to ="Porcentaje") %>% 
  select(anio,Estrato,Porcentaje)


# Definir el orden deseado
orden_deseado <- c("estrato_bajo", "estrato_medio","estrato_alto" )

# Transformar Estrato como factor 
datos_estrato_long <- datos_estrato_long %>%
  mutate(Estrato = factor(Estrato, levels = orden_deseado))

#Gráfica
gr_estratos<-ggplot(datos_estrato_long, aes(x = anio, y = Porcentaje, fill = factor(Estrato))) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), 
           color = "black", width = 0.7) +
  coord_cartesian(ylim = c(0.15,0.85))+
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     breaks = c(seq(0.15,0.85, by=0.10), 0.85),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(x = "", 
       y = "Porcentaje", 
       fill = "Estrato",
       caption = "Fuente: Bases de datos del ICFES") +  
  scale_fill_manual(  
    name = "Estrato",  
    labels= c("estrato_bajo"="Estrato bajo (1y2)",
              "estrato_medio"= "Estrato medio (3y4)",
              "estrato_alto"="Estrato alto (5y6)"),
    values = c("#87CEFF", "#4F94CD", "#104E8B") 
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 16, color = "black"),
    axis.text = element_text(size = 14, color = "black"),
    axis.title.y = element_text(margin = margin(r = 10),size = 14, face = "bold"),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",  
    legend.title = element_blank(),
    legend.text = element_text(size=14),
    plot.caption = element_text(size = 12, color = "black")
  )

ggsave("outputs/grafic_cap_026/f3_estratos.jpg",
       plot = ,
       width = 10,
       height = 6,
       dpi=300)
