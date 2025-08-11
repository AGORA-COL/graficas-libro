library(readxl)
library(ggplot2)
library(scales)  

rm(list=ls())

#Datos
datos_fig5 <- read_excel("data/data_cap_015/datosfig5.xlsx")

# Renombrar columnas y preparar fecha
datos_fig5 <- datos_fig5 |>
  rename(
    tasa_covid = `Tasa de mortalidad x COVID-19 Colombia (x 100.000 personas)`,
    exceso_relativo = `Exc mort relativo Colombia (%)`
  ) |>
  mutate(Meses = as.Date(paste0(Meses, "-01")))

escala <- 100 / 100 

#Gráfica
colombiaexmort<-ggplot(datos_fig5, aes(x = Meses)) +
  geom_line(aes(y = exceso_relativo, color = "Exceso de mortalidad relativo"), 
            size = 1) +
  geom_line(aes(y = tasa_covid * escala, color = "Tasa de mortalidad por COVID-19"), 
            size = 1) +
  scale_y_continuous(
    name = "Exceso de mortalidad relativo (%)",
    limits = c(-10, 100),
    breaks = seq(-10, 100, by = 10),
    sec.axis = sec_axis(
      trans = ~ . / escala,
      name = "Tasa de mortalidad por COVID-19\n(x 100.000 personas)",
      breaks = seq(0, 100, by = 10)
    )
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 month") +
  scale_color_manual(
    name = "Indicadores",
    values = c("Exceso de mortalidad relativo" = "red", 
               "Tasa de mortalidad por COVID-19" = "black")
  ) +
  labs(
    x = "Fecha"
  )+
  theme_classic(base_family = "Arial") +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size=12),
    legend.position = c(0.95, 0.95), 
    legend.justification = c(1, 1), 
    legend.box.background = element_rect(color = "gray", size = 0.5), 
    legend.box.margin = margin(6, 6, 6, 6), 
    legend.key = element_rect(fill = "white"), 
    axis.text.x = element_text(angle = 45, hjust = 1, size=12),
    axis.text.y = element_text(size=12),
    axis.title.y = element_text(size=13),
    axis.title.x = element_text(size=13),
    axis.title.y.left = element_text(color = "red"),
    axis.title.y.right = element_text(color = "black")
  )

colombiaexmort


#Guardar gráfica
ggsave("outputs/grafic_cap_015/f5_colombiaexmort.jpg",
       plot = colombiaexmort, 
       width = 9, 
       height = 5, 
       dpi = 300)
