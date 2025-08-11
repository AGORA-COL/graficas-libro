library(ggplot2)
library(ggrepel)
library(dplyr)
library(scales) 
library(readxl)

rm(list=ls())

#Datos
datosfig1 <- read_excel("data/data_cap_008/datosfig1.xlsx")


# Crear el gráfico de burbujas
gpibpoblac<-ggplot(datosfig1, aes(x = PIB, y = (`Sensibilidad...6`*100), size = `Población...2`, label = País)) +
  geom_point(color = "#00688B", alpha = 0.7) +  
  geom_text_repel(size = 4, max.overlaps = 10) +  # Etiquetas de países
  scale_size(range = c(3, 15), 
             breaks = c(20000000, 100000000, 500000000, 1000000000),
             name = "Población",
             labels = comma) +  
  scale_x_continuous(labels = comma,
                     expand = expansion(mult = c(0,0)),
                     breaks = seq(0, 70000, by=10000))+
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0,0)),
                     breaks = seq(0, 30, by=5))+
  coord_cartesian(ylim = c(0,30),
                  xlim = c(0,70000))+
  labs(x = "PIB per cápita",
       y = "Sensibilidad del sistema de vigilancia") +
  theme_classic(base_family = "Arial") +
  theme(plot.caption = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title.y = element_text(size = 12, margin = margin(r=10)),
        axis.title.x = element_text(size = 12, margin = margin(t=10)),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)
        #panel.grid.major.x = element_line(color = "gray90",linewidth = 0.3),
        #panel.grid.major.y = element_line(color = "gray90",linewidth = 0.3)
        )

gpibpoblac

#Guardar gráfica
ggsave("outputs/grafic_cap_008/f1_pibpoblac.jpg",
       width = 8,
       height = 5,
       dpi=300)
