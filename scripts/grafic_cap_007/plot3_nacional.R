library(tidyverse)
library(scales)
library(patchwork)
library(ggplot2)
library(readr)

rm(list = ls())

#Datos
pivot_todos <- read_csv("data/data_cap_007/pivot_todos.csv")

# Reorganizar los datos en formato largo
datos_largos <- pivot_todos %>%
  pivot_longer(-YearMonth, names_to = "Grupo", values_to = "Casos") %>%
  mutate(YearMonth = as.Date(paste0(YearMonth, "-01")))

# Calcular porcentajes mensuales
datos_porcentaje <- datos_largos %>%
  group_by(YearMonth) %>%
  mutate(Total = sum(Casos),
         Porcentaje = Casos / Total * 100) %>%
  ungroup()

# Renombrar las leyendas para simplificar
nombres_legenda <- c(
  "Enfermedades Cardiovasculares y Metabólicas" = "Cardiov./metabólicas",
  "Tumores, Enfermedades Hematopoyéticas y del sistema Inmune" = "Tumores y s.inmune",
  "Condiciones asociadas a lesiones o agresión" = "Lesiones ó agresión",
  "Enfermedades infecciosas" = "Infecciosas",
  "Enfermedades Respiratorias Crónicas o de la Piel o estructuras anexas" = "Respiratorias y piel",
  "Enfermedades de los Sistemas Digestivo o Urinario" = "Digestivo o Urinario",
  "COVID-19" = "COVID-19",
  "No/Mal definido" = "No/Mal definido",
  "Trastornos Neurológicos o mentales" = "Neurológicos o mentales",
  "Enfermedades Osteomusculares y Degenerativas" = "Osteomusculares y Degenerativas",
  "Trastornos Materno Perinatales Congenitos o Nutricionales" = "Materno, congénito o nutrición",
  "Otras" = "Otras causas"
)

# Crear vector de niveles
orden_grupos <- rev(unname(nombres_legenda))
datos_largos <- datos_largos %>%
  mutate(Grupo = recode(Grupo, !!!nombres_legenda),
         Grupo = factor(Grupo, levels = orden_grupos))

datos_porcentaje <- datos_porcentaje %>%
  mutate(Grupo = recode(Grupo, !!!nombres_legenda),
         Grupo = factor(Grupo, levels = orden_grupos))


# Fechas de cada pico 
fechas_region1 <- as.Date(c("2020-05-15", "2020-09-22"))
fechas_region2 <- as.Date(c("2020-11-20", "2021-02-09"))
fechas_region3 <- as.Date(c("2021-03-04", "2021-08-29"))
fechas_region4 <- as.Date(c("2021-12-13", "2022-02-18"))

# Colores 
colores_personalizados <- c(
  "Cardiov./metabólicas" = "#1874CD",
  "Tumores y s.inmune" = "#9ECAE1",
  "Lesiones ó agresión" = "#FF7F00",
  "Infecciosas" = "#FDAE6B",
  "Respiratorias y piel" = "#31A354",
  "Digestivo o Urinario" = "#A1D99B",
  "COVID-19" = "#E41A1C",
  "No/Mal definido" = "#FA8072",
  "Neurológicos o mentales" = "#756BB1",
  "Materno, congénito o nutrición" = "#BCBDDC",
  "Osteomusculares y Degenerativas" = "#8B5A2B",
  "Otras causas" = "#D2B48C"
)

max_val <- max(max(datos_largos$Casos, na.rm = TRUE))
alpha_t <- 0.006
color_banda <- "#8B8386"


# Gráfico de casos absolutos
g1 <- ggplot(datos_largos, aes(x = YearMonth, y = Casos, fill = Grupo)) +
  geom_area() +
  geom_rect(aes(xmin = fechas_region1[1], xmax = fechas_region1[2], ymin = -Inf, ymax = Inf),
            fill = color_banda, color = NA, alpha = alpha_t) +
  geom_rect(aes(xmin = fechas_region2[1], xmax = fechas_region2[2], ymin = -Inf, ymax = Inf),
            fill = color_banda, color = NA, alpha = alpha_t) +
  geom_rect(aes(xmin = fechas_region3[1], xmax = fechas_region3[2], ymin = -Inf, ymax = Inf),
            fill = color_banda, color = NA, alpha = alpha_t) +
  geom_rect(aes(xmin = fechas_region4[1], xmax = fechas_region4[2], ymin = -Inf, ymax = Inf),
            fill = color_banda, color = NA, alpha = alpha_t) +
  annotate("label", x = mean(fechas_region1), y = 47000 , label = "Ola 1", color = "white", 
           fill = "#00688B", size = 3.5, hjust = 0.5, vjust = 0.5, fontface = "bold") +
  annotate("label", x = mean(fechas_region2), y = 47000, label = "Ola 2", color = "white", 
           fill = "#00688B", size = 3.5, hjust = 0.5, vjust = 0.5, fontface = "bold") +
  annotate("label", x = mean(fechas_region3), y = 47000, label = "Ola 3", color = "white", 
           fill = "#00688B", size = 3.5, hjust = 0.5, vjust = 0.5, fontface = "bold") +
  annotate("label", x = mean(fechas_region4), y = 47000, label = "Ola 4", color = "white", 
           fill = "#00688B", size = 3.5, hjust = 0.5, vjust = 0.5, fontface = "bold") +
  scale_fill_manual(values = colores_personalizados) +
  labs(x = NULL, y = "Muertes mensual", fill = NULL) +
  scale_x_date(date_breaks ="6 months", date_labels = "%b %Y",expand = c(0, 0))+
  scale_y_continuous(limits = c(0,50000), expand = c(0, 0))+
  theme_classic()+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=12),
        axis.title.y = element_text(size=13))

# Gráfico de proporciones
g2 <- ggplot(datos_porcentaje, aes(x = YearMonth, y = Porcentaje, fill = Grupo)) +
  geom_area() +
  geom_rect(aes(xmin = fechas_region1[1], xmax = fechas_region1[2], ymin = -Inf, ymax = Inf),
            fill = color_banda, color = NA, alpha = alpha_t) +
  geom_rect(aes(xmin = fechas_region2[1], xmax = fechas_region2[2], ymin = -Inf, ymax = Inf),
            fill = color_banda, color = NA, alpha = alpha_t) +
  geom_rect(aes(xmin = fechas_region3[1], xmax = fechas_region3[2], ymin = -Inf, ymax = Inf),
            fill = color_banda, color = NA, alpha = alpha_t) +
  geom_rect(aes(xmin = fechas_region4[1], xmax = fechas_region4[2], ymin = -Inf, ymax = Inf),
            fill = color_banda, color = NA, alpha = alpha_t) +
  scale_fill_manual(values = colores_personalizados) +
  labs(x = NULL, y = "% mensual", fill = NULL) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y", expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  theme(
    legend.position = "bottom", 
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.box.margin = margin(0, 50, 0, 0),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.spacing = unit(0.25, "cm"),
    legend.key.size = unit(0.25, "cm"),
    axis.text.x = element_text(angle = 15, size = 12, margin = margin(t = 5)),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 13)
  ) +
  guides(fill = guide_legend(nrow = 2, reverse = TRUE))


# Unir los gráficos
gmortalidad_nal<- g1 / g2 + 
  plot_layout(heights = c(1, 1))

gmortalidad_nal


#Guardar gráfica
ggsave("outputs/grafic_cap_007/f3_mortalidad_nal.jpg",
       plot=gmortalidad_nal,
       width = 12,
       height =8, 
       dpi=300)

