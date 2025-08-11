library(ggplot2)
library(gridExtra)
library(dplyr)
library(readxl)
library(patchwork)

rm(list = ls())

#Datos
datos <- read_excel("data/data_cap_013/datosfig1.xlsx")

#--------------------
#HbA1c
#--------------------
g1<-ggplot(datos, aes(x = Año)) +
  geom_line(aes(y = Hba1c), color = "black", linewidth = 1) +
  geom_line(aes(y = 7.15 + perc_registro_val_Hba1c * 0.3),  
            color = "red", linewidth = 1) +
  geom_line(aes(y = 7.15 + perc_cumplim_meta_Hba1c * 0.3),
            color = "#1874CD",  linewidth = 1) +
  scale_y_continuous(
    name = "HbA1c (mg/dL)",
    limits = c(7.15, 7.50),  
    breaks = seq(7.15, 7.47, by = 0.05),  
    sec.axis = sec_axis(
      trans = ~ (. - 7.15) / 0.3 * 100,  
      name = "",
      breaks = seq(0, 100, by = 20),  
      labels = function(x) paste0(x, "%")  
    )
  ) +
  labs(x = "") +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.major.x = element_blank(),  
    panel.grid.minor = element_blank(),    
    axis.text.y.right = element_text(size = 12, margin = margin(l = 10)),
    axis.text.y.left = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title.y = element_text(size=14, margin = margin(r = 10)),
    axis.title.x = element_text(size=14, margin = margin(t = 10)))

g1


#--------------------
#Colesterol
#--------------------
g2<-ggplot(datos, aes(x = Año)) +
  geom_line(aes(y = LDL), color = "black", linewidth = 1) +
  geom_line(aes(y = 96 + perc_registro_val_LDL * 7),  
            color = "red",  linewidth = 1) +
  geom_line(aes(y = 96 + perc_cumplim_meta_LDL * 7),
            color = "#1874CD", linewidth = 1) +
  scale_y_continuous(
    name = "Colesterol LDL (mg/dL)",
    limits = c(96, 103),  
    breaks = seq(96, 103, by = 1),  
    sec.axis = sec_axis(
      trans = ~ ((. - 96) / 7) * 85,  
      name = "",
      breaks = seq(0, 85, by = 20),  
      labels = function(x) paste0(x, "%")
    )
  ) +
  labs(x = "") +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.major.x = element_blank(),  
    panel.grid.minor = element_blank(),    
    axis.text.y.right = element_text(size = 12, margin = margin(l = 10)),
    axis.text.y.left = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title.y = element_text(size=14, margin = margin(r = 10)),
    axis.title.x = element_text(size=14, margin = margin(t = 10)))

g2

#--------------------
#IMC
#--------------------
g3<-ggplot(datos, aes(x = Año)) +
  geom_line(aes(y = IMC), color = "black", linewidth = 1) +
  geom_line(aes(y = 27.5 + perc_registro_val_IMC * 3),  
            color = "red", linewidth = 1) +
  
  geom_line(aes(y = 27.5 + perc_cumplim_meta_IMC * 3),
            color = "#1874CD",  linewidth = 1) +
  scale_y_continuous(
    name = "IMC (kg/m²)",
    limits = c(27.5, 30.5), 
    breaks = seq(27.5, 30.5, by = 0.5),  
    sec.axis = sec_axis(
      trans = ~ ((. - 27.5) / 3) * 100,  
      name = "",
      breaks = seq(0, 100, by = 20),  
      labels = function(x) paste0(x, "%")  
    )
  ) +
  labs(x = "Año") +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.major.x = element_blank(),  
    panel.grid.minor = element_blank(),    
    axis.text.y.right = element_text(size = 12, margin = margin(l = 10)),
    axis.text.y.left = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title.y = element_text(size=14, margin = margin(r = 10)),
    axis.title.x = element_text(size=14, margin = margin(t = 10)))

g3


#--------------------
#TFG
#--------------------
g4<-ggplot(datos, aes(x = Año)) +
  # TFG (línea negra con leyenda)
  geom_line(aes(y = TFG, color = "Biomarcador"), linewidth = 1) +
  geom_line(aes(y = 77 + perc_registro_val_TFG * 9, color = "% con registro válido"), linewidth = 1) +
  geom_line(aes(y = 77 + perc_cumplim_meta_TFG * 9, color = "Meta"), linewidth = 1) +
  scale_y_continuous(
    name = "TFG (ml/min/1.73m²)",
    limits = c(77, 86),
    breaks = seq(77, 86, by = 1),
    sec.axis = sec_axis(
      trans = ~ ((. - 77) / 9) * 100,
      name = "",
      breaks = seq(0, 100, by = 20),
      labels = function(x) paste0(x, "%")
    )
  ) +
  scale_color_manual(name = NULL,
                     values = c("Biomarcador" = "black", "% con registro válido" = "red", 
                                "Meta" = "#1874CD")) +
  labs(x = "Año") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y.right = element_text(size = 12, margin = margin(l = 10)),
    axis.text.y.left = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title.y = element_text(size=14, margin = margin(r = 10)),
    axis.title.x = element_text(size=14, margin = margin(t = 10))
  )


g4

# Quitar leyendas de g1, g2, g3
g1 <- g1 + theme(legend.position = "none")
g2 <- g2 + theme(legend.position = "none")
g3 <- g3 + theme(legend.position = "none")

final_plot <- (g1 + g2 + g3 + g4) +
  plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")
final_plot

#Guardar gráfica
ggsave("outputs/grafic_cap_013/f1_indicadores.png", final_plot, 
       width = 8, height = 7, dpi = 300)
