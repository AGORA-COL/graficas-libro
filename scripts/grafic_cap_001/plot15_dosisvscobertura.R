library(readr)
library(dplyr)
library(zoo)
library(ggplot2)
library(scales)
library(zoo)
library(readxl)

rm(list=ls())

# Datos
tabla_vacunas_capitulo <- read_delim("data/data_cap_001/df_vacunas.csv") 

pobcol <- readRDS("data/data_cap_001/pobcol_2021_2023.RDS") |>
  mutate(anio = as.numeric(anio)) |>
  group_by(anio) |>
  summarise(Total_pob = sum(pob_total, na.rm = TRUE), .groups = "drop")

# Usar población fija (2021)
pob_total_fija <- pobcol |> filter(anio == 2021) |> pull(Total_pob)

vacunas_total <- tabla_vacunas_capitulo %>%
  mutate(FechaAplicacion = as.Date(FechaAplicacion, format = "%d/%m/%Y")) %>% 
  group_by(FechaAplicacion) %>%
  summarise(
    Total_Dosis0 = sum(Dosis0, na.rm = TRUE),
    Total_Dosis1 = sum(Dosis1, na.rm = TRUE),
    Total_Dosis2 = sum(Dosis2, na.rm = TRUE),
    Totaldosisdia = sum(Dosis0 + Dosis1 + Dosis2, na.rm = TRUE),
    Total_completas = sum(Dosis0 + Dosis2, na.rm = TRUE),
    .groups = "drop"
  ) 

vacunas_total<-vacunas_total %>%
  mutate(
    Acumulado_Dosis0 = cumsum(Total_Dosis0),
    Acumulado_Dosis1 = cumsum(Total_Dosis1),
    Acumulado_Dosis2 = cumsum(Total_Dosis2),
    Acumulado_completas = cumsum(Total_completas),
    Cobertura_Dosis0 = Acumulado_Dosis0 / pob_total_fija * 100,
    Cobertura_Dosis1 = Acumulado_Dosis1 / pob_total_fija * 100,
    Cobertura_Dosis2 = Acumulado_Dosis2 / pob_total_fija * 100,
    Cobertura_Completa = (Acumulado_completas / pob_total_fija) * 100,
    moving_Totaldosisdia = rollmean(Totaldosisdia, k = 7, fill = NA, align = "center")
  )


source("scripts/grafic_cap_001/fechas_olas.r") #fecha por cada ola
y_val <-420000

#gráfica
vacunasxdia_nal <- ggplot() +
  geom_rect(aes(xmin = fechas_ola1[1], xmax = fechas_ola1[2], ymin = -Inf, ymax = Inf),
            fill = "#C1CDCD", alpha = 0.5) +
  geom_rect(aes(xmin = fechas_ola2[1], xmax = fechas_ola2[2], ymin = -Inf, ymax = Inf),
            fill = "#C1CDCD", alpha = 0.5) +
  geom_rect(aes(xmin = fechas_ola3[1], xmax = fechas_ola3[2], ymin = -Inf, ymax = Inf),
            fill = "#C1CDCD", alpha = 0.5) +
  geom_rect(aes(xmin = fechas_ola4[1], xmax = fechas_ola4[2], ymin = -Inf, ymax = Inf),
            fill = "#C1CDCD", alpha = 0.5) +
  annotate("label", x = mean(fechas_ola1), y = y_val, label = "Ola 1", fill = "#2E8B57", color = "white") +
  annotate("label", x = mean(fechas_ola2), y = y_val, label = "Ola 2", fill = "#2E8B57", color = "white") +
  annotate("label", x = mean(fechas_ola3), y = y_val, label = "Ola 3", fill = "#2E8B57", color = "white") +
  annotate("label", x = mean(fechas_ola4), y = y_val, label = "Ola 4", fill = "#2E8B57", color = "white") +
  geom_area(data=vacunas_total,
            aes(x = FechaAplicacion, y = moving_Totaldosisdia, fill = "Cantidad de dosis diarias"), 
            stat = "identity", alpha = 0.6) +
  geom_line(data=vacunas_total, 
            aes(x = FechaAplicacion, y = Cobertura_Dosis1 * 400000 / 100, color = "Primera dosis"), size = 0.8) +  
  geom_line(data=vacunas_total, 
            aes(x = FechaAplicacion, y = Cobertura_Completa * 400000 / 100, color = "Esquema completo"), size = 0.8) +  
  labs(
    x = "",
    y = "Número de dosis diaria (miles)",
    fill = "",
    color = "Cobertura acumulada (%)"
  ) +
  scale_y_continuous(
    limits = c(0, 450000),
    expand = c(0, 0),
    breaks = seq(0, 400000, by = 50000),
    labels = function(x) ifelse(x == 0, "0", paste0(x/1000, "k")),   # Convierte a formato "k" (miles)
    sec.axis = sec_axis(
      ~ . * 100 / 400000,
      name = "Cobertura acumulada (%)",
      breaks = seq(0, 100, by = 10)
    )
  )  +
  scale_fill_manual(values = c("Cantidad de dosis diarias" = "#2E8B57")) +
  scale_color_manual(values = c("Primera dosis" = "#8856a7", "Esquema completo" = "#CD8162"))+
  scale_x_date(
    breaks = seq(as.Date("2020-02-01"), as.Date("2023-06-30"), by = "5 months"),
    date_labels = "%b %Y",
    expand = c(0, 0)
  ) +
  coord_cartesian(xlim = c(as.Date("2020-02-01"), as.Date("2023-06-30"))) +
  theme_classic()+
  theme(
    text = element_text(family = "Arial"),
    axis.text.x = element_text(size=11, vjust = 0.5, hjust = 0.5, margin = margin(t=10)),
    axis.text.y = element_text(size=11),
    axis.title.x = element_text(size=11),
    axis.title.y = element_text(size=11),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size=11),
    legend.box.margin = margin(5, 5, 5, 5),
    panel.grid.minor = element_blank()
  )

vacunasxdia_nal

ggsave("outputs/grafic_cap_001/f15_vacunasvscobert.jpg",
       vacunasxdia_nal,
       height = 5,
       width = 8,
       dpi = 300)


