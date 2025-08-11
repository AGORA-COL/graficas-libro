library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)
library(ggrepel)

rm(list=ls())

#Datos
df_capacidad_camas <- read_csv("data/data_cap_001/df_capacidad_camas.csv") |>
  filter(!as.Date(date) %in% as.Date(c("2022-09-29", "2022-09-30", "2022-07-26", "2020-11-30", "2023-02-07")))

#Filtrar UCI y preparar info nacional y por depto
df_camas_uci <- df_capacidad_camas |>
  filter(capacity == "Cuidado Intensivo Adulto")

#Agrupación departamental
camas_departamento <- df_camas_uci |>
  group_by(Departamento, date) |>
  summarise(
    covid = sum(covid, na.rm = TRUE),
    non_covid = sum(non_covid, na.rm = TRUE),
    total_beds = sum(total_beds, na.rm = TRUE),
    .groups = "drop"
  )

#Agrupación nacional
camas_colombia <- df_camas_uci |>
  group_by(date) |>
  summarise(
    covid = sum(covid, na.rm = TRUE),
    non_covid = sum(non_covid, na.rm = TRUE),
    total_beds = sum(total_beds, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(Departamento = "Colombia")

#Unir departamentos + Colombia
camas <- bind_rows(camas_departamento, camas_colombia)

#Calcular media móvil de 7 días para suavizar
camas_long_uci <- camas |>
  pivot_longer(cols = c("covid", "non_covid", "total_beds"),
               names_to = "tipo_ocupacion",
               values_to = "total") |> 
  dplyr::select("date", "tipo_ocupacion", "total","Departamento") |> 
  mutate(date=as.Date(date),
         tipo_ocupacion= as.character(tipo_ocupacion),
         total=as.numeric(total)) |>
  group_by(Departamento, tipo_ocupacion) |>
  arrange(Departamento,date) |> 
  mutate(media_movil_7dias = rollmean(total, 7, fill=NA, align = "center")) 


camas_long_uci_apiladas <- camas_long_uci |> 
  filter(tipo_ocupacion %in% c("covid", "non_covid"))

camas_long_uci_total_beds <- camas_long_uci |> 
  filter(tipo_ocupacion == "total_beds")

camas_long_uci_apiladas$Departamento <- factor(camas_long_uci_apiladas$Departamento, levels = c("Colombia", sort(setdiff(unique(camas_long_uci_apiladas$Departamento), "Colombia"))))
camas_long_uci_total_beds$Departamento <- factor(camas_long_uci_total_beds$Departamento, levels = c("Colombia", sort(setdiff(unique(camas_long_uci_total_beds$Departamento), "Colombia"))))

#Fecha de olas
source("scripts/grafic_cap_001/fechas_olas.r")

#Gráfica
grafica_uci <- ggplot(data = camas_long_uci_apiladas, 
                      aes(x = date, y = media_movil_7dias, fill = tipo_ocupacion)) +
  geom_rect(aes(xmin = fechas_ola1[1], xmax = fechas_ola1[2], ymin = -Inf, ymax = Inf),
            fill = "#C1CDCD", alpha = 0.02, inherit.aes = FALSE) +
  geom_rect(aes(xmin = fechas_ola2[1], xmax = fechas_ola2[2], ymin = -Inf, ymax = Inf),
            fill = "#C1CDCD", alpha = 0.02, inherit.aes = FALSE) +
  geom_rect(aes(xmin = fechas_ola3[1], xmax = fechas_ola3[2], ymin = -Inf, ymax = Inf),
            fill = "#C1CDCD", alpha = 0.02, inherit.aes = FALSE) +
  geom_rect(aes(xmin = fechas_ola4[1], xmax = fechas_ola4[2], ymin = -Inf, ymax = Inf),
            fill = "#C1CDCD", alpha = 0.02, inherit.aes = FALSE) +
  geom_area(position = "stack", color = "black", size = 0.3) +
  geom_line(data = camas_long_uci_total_beds, 
            aes(x = date, y = media_movil_7dias, color = "Total de camas disponibles"), 
            linetype = "dashed", linewidth = 0.8) +
  facet_wrap(~ Departamento, 
             ncol = 3,
             scales = "free" 
  ) +
  scale_fill_manual(
    values = c("covid" = "#c994c7", "non_covid" = "#ccebc5"),
    labels = c("covid" = "COVID-19", "non_covid" = "No COVID-19")
  ) +
  scale_color_manual(
    values = c("Total de camas disponibles" = "#8856a7"),
    name = NULL
  ) +
  scale_x_date(
    breaks = seq(as.Date("2020-02-01"), as.Date("2023-06-30"), by = "20 months"),
    date_labels = "%b %Y",
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = function(x) c(0, x[2]/2, x[2]),  
    labels = function(x) signif(round(x, 0), digits = 2),  
    expand = expansion(mult = c(0, 0.2))
  ) +
  labs(
    title = "",
    x = "",
    y = "Número de camas diarias en UCI",
    fill = "Tipo de Ocupación",
    caption = "Sin datos de UCI para Amazonas"
  ) +
  coord_cartesian(
    xlim = c(as.Date("2020-02-01"), as.Date("2023-06-30")),
    ylim = c(0, NA)  
  ) +
  theme_classic(base_family = "Arial")+
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    axis.text.x = element_text(size = 15, margin = margin(t = 10)),
    axis.text.y = element_text(size = 15),
    axis.title.y = element_text(size = 19, margin = margin(l = 10)),
    strip.text = element_text(size = 16),
    plot.caption = element_text(size = 13), 
    panel.spacing.x = unit(3, "lines"),
    panel.spacing.y = unit(1.5, "lines"),
    panel.background = element_rect(colour = "black", linetype = "solid"),
    plot.margin = margin(r = 30)
  )

grafica_uci

# Guardar gráfica
ggsave(filename = "outputs/grafic_cap_001/f14_camasuci.jpg", 
       plot = grafica_uci, 
       width = 13, 
       height = 19, 
       dpi = 300)

