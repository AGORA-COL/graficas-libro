library(ggplot2)
library(dplyr)
library(tidyr)

rm(list=ls())

# Datos
datos_box <- tribble(
  ~Figura, ~Año, ~min, ~q1, ~mediana, ~q3, ~max,
  "6a", "2019", 0, 1.14, 2.85, 5.42, 51.42,
  "6a", "2020", 0, 1.28, 2.85, 5.28, 49.85,
  "6b", "2019", 0, 3.14, 5.57, 8.71, 52,
  "6b", "2020", 0, 2.85, 5.42, 8.42, 46.42
)

# Convertir el año a factor para el orden
datos_box$Año <- factor(datos_box$Año, levels = c("2019", "2020"))

# Graficar
oporatencion <-ggplot(datos_box, aes(x = Año)) +
  geom_boxplot(
    aes(
      ymin = min,
      lower = q1,
      middle = mediana,
      upper = q3,
      ymax = max
    ),
    stat = "identity",
    fill = "#1f77b4",
    alpha = 0.5,
    width = 0.5
  ) +
  facet_wrap(~Figura, ncol = 2) +
  labs(
    y = "Tiempo desde el diagnóstico (semanas)",
    x = ""
  ) +
  theme_classic(base_family = "Arial") +
  theme(
    strip.text = element_text(size = 12),
    axis.text = element_text(size = 11),
    axis.title.y = element_text(size = 12)
  )




ggsave("outputs/grafic_cap_014/f6_oporatencion.jpg",
       plot = oporatencion, 
       width = 7, 
       height = 5, 
       dpi = 300)

