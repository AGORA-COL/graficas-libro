library(readxl)
library(ggplot2)
library(dplyr)
library(scales)

rm(list=ls())

#Datos
datos_fig3 <- read_excel("data/data_cap_015/datosfig3.xlsx")

# Preparar columnas y formato de fecha
datos_fig3 <- datos_fig3 |>
  rename(
    vacunas_amazonas = `Vacunas Amazonas (% de la población)`,
    vacunas_colombia = `Vacunas Colombia (% de la población)`
  ) |>
  mutate(Meses = as.Date(paste0(Meses, "-01"))) |>
  pivot_longer(
             cols = c(vacunas_amazonas, vacunas_colombia),
             names_to = "grupo",
             values_to = "porcentaje"
           )
  
         
# Graficar
vacunasamazocol<-ggplot(datos_fig3, aes(x = Meses, y = porcentaje, color = grupo)) +
  geom_line(size = 1.2) +
  scale_color_manual(
    name = "",
    values = c(
      "vacunas_amazonas" = "red",
      "vacunas_colombia" = "black"
    ),
    labels = c(
      "vacunas_amazonas" = "Vacunas Amazonas (% de la población)",
      "vacunas_colombia" = "Vacunas Colombia (% de la población)"
    )
  ) +
  scale_y_continuous(
    name = "Vacunas administradas-al menos una dosis (%)",
    limits = c(0, 40),  
    breaks = seq(0, 40, by = 5),
    expand = expansion(mult = c(0,0)) 
  ) +
  scale_x_date(
    name = "Fecha",
    date_labels = "%b %Y", 
    date_breaks = "3 months",
    expand = expansion(mult = c(0.02, 0.02))  
  ) +
  theme_classic(base_family = "Arial") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 13),
    legend.text = element_text(size=12),
    legend.position = c(0.98, 0.7), 
    legend.justification = c("right", "bottom"),
    legend.title = element_blank(),
    legend.background = element_rect(fill = "white", color = "gray"),
    legend.margin = margin(5, 5, 5, 5),
    legend.key = element_rect(fill = "white")
  ) +
  guides(color = guide_legend(override.aes = list(size = 2)))  

vacunasamazocol

#Guardar gráfica
ggsave("outputs/grafic_cap_015/f3_vacunasamazocol.jpg",
       plot = vacunasamazocol, 
       width = 9, 
       height = 5, 
       dpi = 300)         

