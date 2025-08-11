library(readr)
library(dplyr)
library(zoo)
library(ggplot2)
library(scales)
library(readxl)

rm(list=ls())

#Datos
tabla_vacunas_capitulo <- read_delim("data/data_cap_001/df_vacunas.csv")

# Agrupar a nivel NACIONAL 
vacunas_total_nal <- tabla_vacunas_capitulo |>
  mutate(FechaAplicacion = as.Date(FechaAplicacion, format = "%d/%m/%Y"),
         anio = as.numeric(format(FechaAplicacion, "%Y"))) |> 
  group_by(FechaAplicacion, anio) |>
  summarise(
    Total_Dosis0 = sum(Dosis0, na.rm = TRUE),
    Total_Dosis1 = sum(Dosis1, na.rm = TRUE),
    Total_Dosis2 = sum(Dosis2, na.rm = TRUE),
    Totaldosisdia = sum(Dosis0 + Dosis1 + Dosis2, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    moving_Totaldosisdia = rollmean(Totaldosisdia, k = 7, fill = NA, align = "center"),
    Departamento="Colombia"
  )


# Agrupamiento por departamento
vacunas_total_dtal <- tabla_vacunas_capitulo |>
  filter(CodigoDepartamentoAplicacion != 0) |>
  mutate(
    FechaAplicacion = as.Date(FechaAplicacion, format = "%d/%m/%Y")
  ) |>
  group_by(Departamento, FechaAplicacion) |>
  summarise(
    Total_Dosis0 = sum(Dosis0, na.rm = TRUE),
    Total_Dosis1 = sum(Dosis1, na.rm = TRUE),
    Total_Dosis2 = sum(Dosis2, na.rm = TRUE),
    Totaldosisdia = sum(Dosis0 + Dosis1 + Dosis2, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(Departamento) |>
  arrange(FechaAplicacion, .by_group = TRUE) |>
  mutate(
    moving_Totaldosisdia = rollmean(Totaldosisdia, k = 7, fill = NA, align = "center")
  ) |>
  ungroup()


#Data set con todo (Colombia + Departamentos):
vacunas_total<-bind_rows(vacunas_total_nal,vacunas_total_dtal)
vacunas_total$Departamento <- factor(vacunas_total$Departamento, levels = c("Colombia", sort(setdiff(unique(vacunas_total$Departamento), "Colombia"))))

#Fecha de olas
source("scripts/grafic_cap_001/fechas_olas.r")

# Gráfico
vacunasxdia_dtal <- ggplot(data=vacunas_total, aes(x = FechaAplicacion)) +
  geom_rect(aes(xmin = fechas_ola1[1], xmax = fechas_ola1[2], ymin = -Inf, ymax = Inf),
            fill = "#C1CDCD", alpha = 0.02, inherit.aes = FALSE) +
  geom_rect(aes(xmin = fechas_ola2[1], xmax = fechas_ola2[2], ymin = -Inf, ymax = Inf),
            fill = "#C1CDCD", alpha = 0.02, inherit.aes = FALSE) +
  geom_rect(aes(xmin = fechas_ola3[1], xmax = fechas_ola3[2], ymin = -Inf, ymax = Inf),
            fill = "#C1CDCD", alpha = 0.02, inherit.aes = FALSE) +
  geom_rect(aes(xmin = fechas_ola4[1], xmax = fechas_ola4[2], ymin = -Inf, ymax = Inf),
            fill = "#C1CDCD", alpha = 0.02, inherit.aes = FALSE) +
  geom_area(aes(y = Totaldosisdia, fill = "Dosis diarias"), stat = "identity", alpha = 0.6) +
  geom_line(aes( y = moving_Totaldosisdia, color = "Dosis diarias media movil"), size = 1) +
  labs(
    x = "",
    y = "Número de dosis diarias",
    fill = "",
    color = ""
  ) +
  scale_fill_manual(values = c("Dosis diarias" = "#43CD80")) +
  scale_color_manual(values = c("Dosis diarias media movil" = "#2E8B57")) +
  facet_wrap(~ Departamento, ncol = 3, scales = "free_y") +
  scale_x_date(breaks = seq(as.Date("2020-02-01"), as.Date("2023-06-30"), by = "20 month"),
               date_labels = "%b %Y",
               expand = c(0, 0)) +
  scale_y_continuous(
    breaks = function(x) seq(0, max(x), length.out = 3),  
    labels = function(x) signif(round(x, 0), digits = 2),  
    expand = expansion(mult = c(0, 0.2)),
    limits = c(0,NA)
  ) +
  coord_cartesian(xlim = c(as.Date("2020-02-01"), as.Date("2023-06-30"))) +
  theme_classic(base_family = "Arial")+
  theme(strip.text = element_text(size = 19),
        axis.text.x = element_text(size = 17, margin = margin(t = 10)),
        axis.text.y = element_text(size = 17),
        axis.title.y = element_text(size = 19, margin = margin(l=10, r = 10)),
        panel.spacing.x = unit(5, "lines"),  
        panel.spacing.y = unit(1, "lines"),  
        panel.background = element_rect(colour = "black", linetype = "solid"),
        plot.margin = margin(r=30,t=5),
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.position = c(0.6,0.01),
        legend.text = element_text(size=19))

vacunasxdia_dtal

ggsave("outputs/grafic_cap_001/f16_dosisdiavacunasxdpto.jpg",
       vacunasxdia_dtal,
       width = 14, 
       height = 18, 
       dpi = 300)
