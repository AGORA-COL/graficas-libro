library(readr)
library(dplyr)
library(readxl)
library(zoo)
library(forcats)
library(ggtext)

rm(list=ls())

# Datos
tabla_vacunas_capitulo <- read_delim("data/data_cap_001/df_vacunas.csv")

# Procesar datos departamentales
vacunas_total <- tabla_vacunas_capitulo |>
  filter(CodigoDepartamentoAplicacion != 0) |>
  mutate(
    FechaAplicacion = as.Date(FechaAplicacion, format = "%d/%m/%Y"),
    anio = as.numeric(format(FechaAplicacion, "%Y"))
  ) |>
  group_by(Departamento, FechaAplicacion) |>
  summarise(
    Dosis0 = sum(Dosis0, na.rm = TRUE),
    Dosis1 = sum(Dosis1, na.rm = TRUE),
    Dosis2 = sum(Dosis2, na.rm = TRUE),
    Total_dia = Dosis0 + Dosis1 + Dosis2,
    .groups = "drop"
  ) |>
  group_by(Departamento) |>
  mutate(Dosis7d_movil = zoo::rollmean(Total_dia, k = 7, fill = NA, align = "center")) |>
  ungroup()


# Máximo por departamento
maximos_dosis <- vacunas_total |>
  group_by(Departamento) |>
  summarise(max_dosis_dia = max(Dosis7d_movil, na.rm = TRUE), .groups = "drop") |>
  select(Departamento, max_dosis_dia)


# Máximo para Colombia
colombia_max <- tabla_vacunas_capitulo |>
  #filter(CodigoDepartamentoAplicacion != 0) |>
  mutate(FechaAplicacion = as.Date(FechaAplicacion, format = "%d/%m/%Y")) |>
  group_by(FechaAplicacion) |>
  summarise(Total_dia = sum(Dosis0 + Dosis1 + Dosis2, na.rm = TRUE), .groups = "drop") |>
  mutate(Dosis7d_movil = zoo::rollmean(Total_dia, k = 7, fill = NA, align = "center")) |>
  summarise(Departamento = "Colombia", max_dosis_dia = max(Dosis7d_movil, na.rm = TRUE))

# Unión final y orden
maximos_dosis_final <- bind_rows(maximos_dosis, colombia_max) |>
  mutate(Departamento = fct_reorder(Departamento, max_dosis_dia, .desc = FALSE))


# Mejora de etiqueta colombia
color_etiquetas <- ifelse(unique(maximos_dosis_final$Departamento) == "Colombia", "#2E8B57", "gray45")
negrita_etiquetas <- ifelse(unique(maximos_dosis_final$Departamento) == "Colombia", "bold", "plain")

#Gráfica de máxima dosis diaria
maxdosisdia <- ggplot(maximos_dosis_final, aes(x = max_dosis_dia, y = reorder(Departamento, max_dosis_dia))) +
  geom_col(width = 0.7, fill = "#43CD80", color = "#2E8B57") +
  geom_label(
    aes(label = format(signif(round(max_dosis_dia, 0), digits = 2), big.mark = " ", decimal.mark = ",")),
    hjust = -0.1,
    size = 2,
    color = "black",
    fill = "#43CD80",
    alpha = 0.7,
    label.size = 0.2,  
    label.padding = unit(0.15, "lines"),
    family= "Arial"
  ) +
  scale_x_continuous(
    trans = "log10",
    expand = expansion(mult = c(0, 0.2)),  
    labels = scales::label_number()
  ) +
  labs(
    x = "",
    y = "",
    caption = ""
  ) +
  theme_classic(base_family = "Arial")+
  theme(
    axis.text.x = element_text(size = 8),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    axis.text.y = element_text(
      size = 8,
      color = color_etiquetas[match(levels(maximos_dosis_final$Departamento), unique(maximos_dosis_final$Departamento))],
      face = negrita_etiquetas[match(levels(maximos_dosis_final$Departamento), unique(maximos_dosis_final$Departamento))]
    ),
    plot.margin = margin(10, 15, 10, 10)
  )

maxdosisdia


ggsave("outputs/grafic_cap_001/f17_maxdosisdia.jpg",
       maxdosisdia,
       width = 5, 
       height = 5, 
       dpi = 300)
