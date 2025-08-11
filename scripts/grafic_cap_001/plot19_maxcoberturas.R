library(dplyr)
library(readr)
library(readxl)
library(ggplot2)
library(zoo)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(zoo)

rm(list=ls())

#Datos
tabla_vacunas_capitulo <- read_delim("data/data_cap_001/df_vacunas.csv") |> 
  filter(CodigoDepartamentoAplicacion != 0)

pobcol_departamento <- readRDS("data/data_cap_001/pobcol_2021_2023.RDS") |> 
  mutate(anio = as.numeric(anio))

# Población
pob_departamento_total <- pobcol_departamento |>
  filter(anio == 2021) |>
  group_by(dp, departamento) |>
  summarise(pob_total = sum(pob_total, na.rm = TRUE), .groups = "drop") |>
  mutate(departamento = recode(departamento,
                               "norte_santander" = "norte_de_santander",
                               "valle" = "valle_del_cauca"))

total_pob_colombia <- pob_departamento_total$pob_total |> sum(na.rm = TRUE)

# Vacunas por departamento
vacunas_departamento <- tabla_vacunas_capitulo |>
  mutate(FechaAplicacion = as.Date(FechaAplicacion, format = "%d/%m/%Y")) |>
  group_by(Departamento, departamento_limp, FechaAplicacion) |>
  summarise(across(c(Dosis0, Dosis1, Dosis2), ~sum(.x, na.rm = TRUE)), .groups = "drop") |>
  mutate(Total_completas = Dosis0 + Dosis2) |>
  left_join(pob_departamento_total, by = c("departamento_limp" = "departamento")) |>
  group_by(Departamento) |>
  arrange(FechaAplicacion) |>
  mutate(
    Acumulado_Dosis1 = cumsum(Dosis1),
    Acumulado_Dosis2 = cumsum(Dosis2),
    Acumulado_completas = cumsum(Total_completas),
    Cobertura_Dosis1 = Acumulado_Dosis1 / pob_total * 100,
    Cobertura_Dosis2 = Acumulado_Dosis2 / pob_total * 100,
    Porcentaje_vacunados_completas = Acumulado_completas / pob_total * 100
  ) |>
  ungroup() |>
  mutate(Departamento = factor(Departamento, levels = sort(unique(Departamento))))


# Vacunas a nivel nacional
vacunas_nacional <- tabla_vacunas_capitulo |>
  mutate(FechaAplicacion = as.Date(FechaAplicacion, format = "%d/%m/%Y")) |>
  group_by(FechaAplicacion) |>
  summarise(across(c(Dosis0, Dosis1, Dosis2), ~sum(.x, na.rm = TRUE)), .groups = "drop") |>
  mutate(
    Total_completas = Dosis0 + Dosis2,
    Acumulado_Dosis1 = cumsum(Dosis1),
    Acumulado_Dosis2 = cumsum(Dosis2),
    Acumulado_completas = cumsum(Total_completas),
    Cobertura_Dosis1 = Acumulado_Dosis1 / total_pob_colombia * 100,
    Cobertura_Dosis2 = Acumulado_Dosis2 / total_pob_colombia * 100,
    Porcentaje_vacunados_completas = Acumulado_completas / total_pob_colombia * 100
  )

# Último valor por departamento
cobertura_final_dpto <- vacunas_departamento |>
  group_by(Departamento) |>
  filter(FechaAplicacion == max(FechaAplicacion)) |>
  ungroup() |>
  select(Departamento, Cobertura_Dosis1, Porcentaje_vacunados_completas)

# Último valor nacional
cobertura_final_nat <- vacunas_nacional |>
  filter(FechaAplicacion == max(FechaAplicacion)) |>
  select(Cobertura_Dosis1, Porcentaje_vacunados_completas) |>
  pivot_longer(everything(), names_to = "Dosis", values_to = "Cobertura") |>
  mutate(
    Departamento = "Colombia",
    Dosis = recode(Dosis,
                   "Cobertura_Dosis1" = "A. Primera dosis",
                   "Porcentaje_vacunados_completas" = "B. Esquema completo")
  )

# Unir ambos
cobertura_final <- cobertura_final_dpto |>
  pivot_longer(cols = -Departamento, names_to = "Dosis", values_to = "Cobertura") |>
  mutate(
    Dosis = recode(Dosis,
                   "Cobertura_Dosis1" = "A. Primera dosis",
                   "Porcentaje_vacunados_completas" = "B. Esquema completo")
  ) |>
  bind_rows(cobertura_final_nat)

# Orden por cobertura
dept_order <- cobertura_final |>
  filter(Dosis == "A. Primera dosis") |>
  arrange(desc(Cobertura)) |>
  pull(Departamento) |>
  unique()

cobertura_final <- cobertura_final |>
  mutate(Departamento = factor(Departamento, levels = rev(dept_order)))

# Colores y estilos para etiquetas
color_etiquetas <- ifelse(levels(cobertura_final$Departamento) == "Colombia", "#2E8B57", "gray45")
negrita_etiquetas <- ifelse(levels(cobertura_final$Departamento) == "Colombia", "bold", "plain")

# Color diferente para Colombia
cobertura_final <- cobertura_final |>
  mutate(color_fill = ifelse(Departamento == "Colombia", "#2E8B57", "#43CD80"))

# Gráfica
cobertura_dpto <- ggplot(cobertura_final, aes(x = Departamento, y = pmin(Cobertura, 100), fill = color_fill)) +
  geom_col(width = 0.7, color = "black") +
  coord_flip() +
  facet_wrap(~ Dosis) +
  scale_fill_identity() +  
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  labs(
    title = "",
    x = "",
    y = "Cobertura acumulada (%)",
    caption = "Esquema completo: dosis únicas más segundas dosis"
  ) +
  theme_classic(base_family = "Arial") +
  theme(
    strip.text = element_text(size = 13),
    strip.key = element_rect(colour = "black", linewidth = 0.05), 
    axis.text.y = element_text(
      size = 11,
      color = color_etiquetas[match(levels(cobertura_final$Departamento), levels(cobertura_final$Departamento))],
      face = negrita_etiquetas[match(levels(cobertura_final$Departamento), levels(cobertura_final$Departamento))]
    ),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    plot.title = element_text(size = 14, hjust = 0.5),
    plot.margin = margin(r = 15),
    panel.spacing.x = unit(3, "lines"),
    legend.position = "none"
  )

cobertura_dpto

# Guardar gráfica
ggsave("outputs/grafic_cap_001/f19_coberturas.jpg",
       cobertura_dpto,
       width = 8, height = 7, dpi = 300)

