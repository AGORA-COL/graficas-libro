library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(gridExtra)

rm(list = ls())

# Datos
tabla_vacunas_capitulo <- read_delim("data/data_cap_001/df_vacunas.csv")

# Homogeneizar etiquetas de edad 
etiquetas_edad <- c(
  "0 a 9 anios" = "0 a 9 años",
  "10 a 19 anios" = "10 a 19 años",
  "20 a 29 anios" = "20 a 29 años",
  "30 a 39 anios" = "30 a 39 años",
  "40 a 49 anios" = "40 a 49 años",
  "50 a 59 anios" = "50 a 59 años",
  "60 a 69 anios" = "60 a 69 años",
  "70 a 79 anios" = "70 a 79 años",
  "80 o + anios" = "+80 años"
)

# Procesar datos de vacunación 
vacunas_acumuladas <- tabla_vacunas_capitulo |>
  mutate(
    FechaAplicacion = as.Date(FechaAplicacion, format = "%d/%m/%Y"),
    GruposEdad = recode(GruposEdad, !!!etiquetas_edad)
  ) |>
  group_by(GruposEdad, FechaAplicacion) |>
  summarise(
    Dosis0 = sum(Dosis0, na.rm = TRUE),
    Dosis1 = sum(Dosis1, na.rm = TRUE),
    Dosis2 = sum(Dosis2, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    Total_completas = Dosis0 + Dosis2,     # Dosis únicas + segundas
    Total1radosisdia = Dosis0 + Dosis1,
    Totaldosisdia = Dosis0 + Dosis1 + Dosis2
  ) |>
  arrange(GruposEdad, FechaAplicacion) |>
  group_by(GruposEdad) |>
  mutate(
    Acumulado_completas = cumsum(Total_completas),
    Acumulado_1radosis = cumsum(Total1radosisdia)
  ) |>
  ungroup()

# Cargar población 
pobcol_2021 <- readRDS("data/data_cap_001/pobcol_2021_2023.RDS") |>
  group_by(anio, GruposEdad) |>
  summarise(Total_pob = sum(pob_total, na.rm = TRUE), .groups = "drop")|> 
  filter(anio == 2021) 

# Calcular coberturas 
vacunas_porcentaje <- vacunas_acumuladas |>
  left_join(pobcol_2021, by = "GruposEdad") |>
  mutate(
    Porcentaje_vacunados = (Acumulado_completas / Total_pob) * 100,
    Porcentaje_1radosis = (Acumulado_1radosis / Total_pob) * 100
  )

# Definir orden 
orden_grupos_edad <- c(
  "+80 años", "70 a 79 años", "60 a 69 años",
  "50 a 59 años", "40 a 49 años", "30 a 39 años",
  "20 a 29 años", "10 a 19 años", "0 a 9 años"
)

vacunas_porcentaje <- vacunas_porcentaje |>
  mutate(GruposEdad = factor(GruposEdad, levels = orden_grupos_edad))

source("scripts/grafic_cap_001/fechas_olas.r")

#Graficar
g_base <- ggplot() +
  geom_rect(aes(xmin = fechas_ola1[1], xmax = fechas_ola1[2], ymin = -Inf, ymax = Inf),
            fill = "#C1CDCD", alpha = 0.5) +
  geom_rect(aes(xmin = fechas_ola2[1], xmax = fechas_ola2[2], ymin = -Inf, ymax = Inf),
            fill = "#C1CDCD", alpha = 0.5) +
  geom_rect(aes(xmin = fechas_ola3[1], xmax = fechas_ola3[2], ymin = -Inf, ymax = Inf),
            fill = "#C1CDCD", alpha = 0.5) +
  geom_rect(aes(xmin = fechas_ola4[1], xmax = fechas_ola4[2], ymin = -Inf, ymax = Inf),
            fill = "#C1CDCD", alpha = 0.5) +
  scale_x_date(
    breaks = seq(as.Date("2020-02-01"), as.Date("2023-06-30"), by = "5 months"),
    date_labels = "%b %Y",
    limits = c(as.Date("2020-02-01"), as.Date("2023-06-30")),
    expand = c(0, 0)
  ) +
  coord_cartesian(xlim = c(as.Date("2020-02-01"), as.Date("2023-06-30"))) +
  #scale_y_continuous(limits = c(0, 50000), expand = c(0, 0)) +
  theme_classic(base_family = "Arial")

g_base



#---------------
#1ra dosis
#---------------

grafvac_1radosis<-g_base +
  geom_area(data=vacunas_porcentaje, aes(x = FechaAplicacion, y = Porcentaje_1radosis, fill = GruposEdad),
            position = "stack", color="black", alpha = 0.5) +  
  annotate("label", x = mean(fechas_ola1), y = 94, label = "Ola 1", fill = "#2b83ba", color = "white") +
  annotate("label", x = mean(fechas_ola2), y = 94, label = "Ola 2", fill = "#2b83ba", color = "white") +
  annotate("label", x = mean(fechas_ola3), y = 94, label = "Ola 3", fill = "#2b83ba", color = "white") +
  annotate("label", x = mean(fechas_ola4), y = 94, label = "Ola 4", fill = "#2b83ba", color = "white") +
  scale_fill_brewer(palette = "BrBG", direction = -1) +  
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     expand = c(0, 0)) + 
  coord_cartesian(ylim = c(0, 100)) + 
    labs(
    title = "A. Cobertura acumulada de vacunación (1ra dosis) COVID-19 en Colombia",
    x = "",
    y = "",
    fill = "Grupos de edad"
  )+
  theme(
    axis.text.x = element_text(margin = margin(t = 10)),
    legend.title = element_blank(),
    #legend.position = "bottom",
    legend.position = "none",
    plot.margin = margin(10, 30, 10, 10)
  ) 

grafvac_1radosis


#------------------
#Dosis completas
#------------------

grafvac_completa<-g_base +
  geom_area(data=vacunas_porcentaje, aes(x = FechaAplicacion, y = Porcentaje_vacunados, fill = GruposEdad),
            position = "stack", color="black", alpha = 0.5) +  
  scale_fill_brewer(palette = "BrBG", direction = -1) +  
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     expand = c(0, 0)) +  
  coord_cartesian(ylim = c(0, 100)) + 
  labs(
    title = "B. Cobertura acumulada de esquema completo de vacunación COVID-19 en Colombia",
    x = "",
    y = "",
    caption = "*Esquema completo: dosis únicas más segundas dosis",
    color = "Grupos de edad"
  ) +
  theme(
    axis.text.x = element_text(margin = margin(t = 10)),,
    legend.title = element_blank(),
    legend.position = "bottom",
    plot.margin = margin(10, 30, 10, 10)
  ) 

grafvac_completa


########################
#unir gráficos
#######################
graf_vac_ambas<-grid.arrange(grafvac_1radosis,grafvac_completa, nrow=2,
                             heights = c(1, 1.2))
graf_vac_ambas

ggsave("outputs/grafic_cap_001/f18_areascoberturas.png", 
       plot = graf_vac_ambas, 
       width = 8, 
       height = 7, 
       dpi = 300)




