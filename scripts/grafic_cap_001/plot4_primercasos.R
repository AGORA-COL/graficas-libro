library(readr)
library(dplyr)
library(ggplot2)

rm(list=ls())

# Cargar datos 
casos_diarios<- read_csv("data/data_cap_001/df_casos_diarios.csv")

# Primeros casos por departamento
primeros_casos <- casos_diarios |>
  group_by(Departamento) |>
  summarize(fecha_primer_caso = min(fecha_inicio_sintomas, na.rm = TRUE), .groups = "drop") |>
  arrange(fecha_primer_caso) |>
  mutate(Departamento = factor(Departamento, levels = rev(unique(Departamento))))

# Grafica
primer_caso_dpto <- ggplot(primeros_casos, aes(x = Departamento, y = as.Date(fecha_primer_caso))) +
  geom_segment(aes(xend = Departamento, yend = as.Date("2020-02-01")), 
               color = "black", linewidth = 2.2) +
  geom_segment(aes(xend = Departamento, yend = as.Date("2020-02-01")), 
               color = "#2b83ba", linewidth = 1.5) +
  geom_point(shape = 21, size = 4, fill = "#2b83ba", color = "black", stroke = 1) +
  labs(
    title = "",
    x = "Departamento",
    y = "Fecha de inicio de síntomas del primer caso"
  ) +
  scale_y_date(
    limits = as.Date(c("2020-02-01", "2020-07-20")),
    date_labels = "%d %b %Y",
    expand = c(0, 0)
  ) +
  theme_classic(base_family = "Arial") +
  coord_flip() +
  theme(
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 13),
    axis.text.x = element_text(size = 11, margin = margin(t = 5)),
    axis.title.x = element_text(size = 13, 
                                margin = margin(t = 15, r = 0, b = 10, l = 0))
  )

primer_caso_dpto

#Guardar gráfica 
ggsave(filename = "outputs/grafic_cap_001/f4_primercasoxdpto.jpg", 
       plot = primer_caso_dpto, 
       width = 9, 
       height = 8, 
       dpi = 300)

