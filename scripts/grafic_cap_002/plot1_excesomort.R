library(ggplot2)
library(cowplot)
library(dplyr)
library(tidyverse)
library(readxl)
library(ggrepel)

rm(list=ls())

#Datos
dep <- read_csv("data/data_cap_002/datosfig1_dep.csv") 

tasas <- read_excel("data/data_cap_002/datosfig1_tasas.xlsx") |>
  group_by(CODPTORE) |>
  summarise(tasas = mean(tasas)) |>
  mutate(CODPTORE=ifelse(CODPTORE=="Archipiélago de San Andrés","San Andrés",CODPTORE))


#--------------------- GRAFICA DE RANKING DE EXCESO ----------------------------
# Gráfico 1: Exceso de mortalidad absoluto
g1 = dep |>
  group_by(CODPTORE) |>
  summarise(Exceso = sum(General)-sum(pred)) |>
  ggplot(aes(y=reorder(CODPTORE,Exceso),x=Exceso))+
  geom_bar(stat = "identity", fill = "#1C86EE", color="black") +
  xlab("Exceso de mortalidad")+ylab("")+
  ggtitle("A.") +
  scale_x_continuous(breaks = seq(0, 30000, by=10000),
                     limits = c(0, 30000),
                     expand = expansion(mult = c(0, 0.1))) +
  theme_classic(base_family = "Arial") +
  theme(
    axis.title.x = element_text(margin = margin(t=5, b=15)),
    plot.title = element_text(
      hjust = 0,  
      size = 12,  
      margin = margin(b = 10)  
    ))
g1

# Gráfico 2: Porcentaje de exceso de mortalidad (Pscore)
g2 = dep |>
  group_by(CODPTORE) |>
  summarise(Exceso = mean(Pscore)) |>
  ggplot(aes(y=reorder(CODPTORE,Exceso),x=Exceso))+
  geom_bar(stat = "identity", fill = "#1C86EE", color="black") +
  xlab("Porcentaje de exceso de mortalidad")+ylab("")+ 
  scale_x_continuous(labels = scales::percent)+
  xlab("Porcentaje de exceso de mortalidad") +
  ylab("") +
  ggtitle("B.") +
  scale_x_continuous(labels = scales::percent,
                     expand = expansion(mult = c(0, 0.15))) +
  theme_classic(base_family = "Arial")+
  theme(
    axis.title.x = element_text(margin = margin(t=5, b=15)),
    plot.title = element_text(
      hjust = 0,  
      size = 12,  
      margin = margin(b = 10) 
    ))
g2


# Gráfico 3: Tasas estandarizadas por edad
g3 <- ggplot(tasas, aes(y = reorder(CODPTORE, tasas), x = tasas)) +
  geom_bar(stat = "identity", fill = "#1C86EE", color="black") +
  xlab("Tasas de exceso de mortalidad estandarizada \npor edad (por 100 mil habitantes)") +
  ylab("") +
  scale_x_continuous(breaks = seq(0, 250, by=50),
                     limits = c(0, 250),
                     expand = expansion(mult = c(0, 0.05))) +
  ggtitle("C.") +
  theme_classic(base_family = "Arial")+
  theme(
    plot.title = element_text(
      hjust = 0,  
      size = 12,   
      margin = margin(b = 10)  
    ))
g3

# Combinar las gráficas en un solo panel
fig1 <- plot_grid(g1, g2, g3, ncol = 3) 

#Guardar gráfica
ggsave(filename = "outputs/grafic_cap_002/f1_tasasexcesom.jpg", 
       plot = fig1, 
       width = 14, 
       height = 6, 
       dpi = 300)
