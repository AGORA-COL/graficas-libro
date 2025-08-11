library(ggplot2)
library(gridExtra)
library(dplyr)
library(readxl)
library(patchwork)
library(lubridate)
library(scales)

rm(list = ls())

#Datos
data_cantidadat <- read_excel("data/data_cap_013/datosfig2.xlsx")
data_cantidadat$Fecha <- ym(data_cantidadat$Fecha)

# Calcular un factor de escala
max_dm <- 350
max_total <- 12000
escala <- max_dm / max_total


# GRÁFICA #1
atenc1<-ggplot(data_cantidadat, aes(x = Fecha)) +
  geom_line(aes(y = Todas_atenciones * escala, color = "Todas las atenciones", group = 1), size = 1.2) +
  geom_line(aes(y = Atenciones_DiabetesMel, color = "Atenciones Diabetes Mellitus", group = 1), size = 1.2) +
  scale_y_continuous(
    name = "Miles de personas (DM)",
    labels = label_number(scale = 1e-3, suffix = "k"),
    breaks= seq(0, 500000, by = 50000),
    limits = c(0,350000),
    sec.axis = sec_axis(
      ~./escala,
      breaks= seq(0, 18000000, by = 2000000),
      name = "Miles de personas \n(Todas)",
      labels = label_number(scale = 1e-3, suffix = "k")
    )
  )+ 
  ggtitle("A. Diabetes Mellitus")+
  scale_color_manual(values = c("Atenciones Diabetes Mellitus" = "#1874CD", "Todas las atenciones" = "black")) +
  scale_x_date(
    breaks = c(seq(as.Date("2015-01-01"), as.Date("2023-12-01"), by = "6 months"), as.Date("2023-12-01")),
    date_labels = "%Y-%m",
    limits = c(as.Date("2015-01-01"), as.Date("2023-12-01")))+
  labs(x = NULL, color = NULL) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "bottom",
    axis.title.x = element_text(margin = margin(t = 10, b = 10)),
    axis.title.y.right = element_text(margin = margin(l = 10))
  )

atenc1


# GRÁFICA #2
# Calcular un factor de escala 
max_dm2 <- 140000
max_total2 <- 600
escala2 <- max_dm2 / max_total2

atenc2<-ggplot(data_cantidadat, aes(x = Fecha)) +
  geom_line(aes(y = Atenciones_codigosERC, color = "Atenciones código ERC", group = 1), size = 1.2) +
  geom_line(aes(y = Procedimientos_avascular* escala2, color = "Procedimiento acceso vascular", group = 1), size = 1.2) +
  geom_line(aes(y = Consulta_nefrologia, color = "Consulta especialista nefrología", group = 1), size = 1.2) +
  scale_y_continuous(
    name = "Número de personas",
    labels = label_number(scale = 1e-3, suffix = "k"),
    breaks= seq(0, 140000, by = 20000),
    limits = c(0,140000),
    sec.axis = sec_axis(
      ~./escala2,
      breaks= seq(0, 600, by = 100),
      name = "Número de personas \n(procedimientos)"
    )
  ) +
  ggtitle("B. Enfermedad Renal Crónica")+
  scale_color_manual(values = c("Consulta especialista nefrología" = "#FF6A6A", 
                                "Procedimiento acceso vascular" = "#9A32CD",
                                "Atenciones código ERC" = "#5F9EA0")) +
  scale_x_date(
    breaks = c(seq(as.Date("2015-01-01"), as.Date("2023-12-01"), by = "6 months"), as.Date("2023-12-01")),
    date_labels = "%Y-%m",
    limits = c(as.Date("2015-01-01"), as.Date("2023-12-01")))+
  labs(x = NULL, color = NULL) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "bottom",
    axis.title.x = element_text(margin = margin(t = 10, b = 10)),
    axis.title.y.right = element_text(margin = margin(l = 10))
  )

atenc2

# Combina los gráficos
cant_atenciones <- atenc1 / atenc2  
cant_atenciones

#Guardar gráfica
ggsave("outputs/grafic_cap_013/f2_atenciones.png", 
       plot = cant_atenciones,
       width = 10,
       height = 10,
       dpi = 300)
