library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(zoo)
library(splines)
library(gridExtra)
library(tidyr)
library(readr)

rm(list=ls())

#=======================================
# GRÁFICO BASE (FONDOS DE OLAS)
#=======================================
source("scripts/grafic_cap_001/fechas_olas.r")


g_base <- ggplot() +
  geom_rect(aes(xmin = fechas_ola1[1], xmax = fechas_ola1[2], ymin = -Inf, ymax = Inf),
            fill = "#C1CDCD", alpha = 0.4) +
  geom_rect(aes(xmin = fechas_ola2[1], xmax = fechas_ola2[2], ymin = -Inf, ymax = Inf),
            fill = "#C1CDCD", alpha = 0.4) +
  geom_rect(aes(xmin = fechas_ola3[1], xmax = fechas_ola3[2], ymin = -Inf, ymax = Inf),
            fill = "#C1CDCD", alpha = 0.4) +
  geom_rect(aes(xmin = fechas_ola4[1], xmax = fechas_ola4[2], ymin = -Inf, ymax = Inf),
            fill = "#C1CDCD", alpha = 0.4) +
  scale_x_date(
    breaks = seq(as.Date("2020-02-01"), as.Date("2023-06-30"), by = "5 months"),
    date_labels = "%b %Y",
    limits = c(as.Date("2020-02-01"), as.Date("2023-06-30")),
    expand = c(0, 0)
  ) +
  coord_cartesian(xlim = c(as.Date("2020-02-01"), as.Date("2023-06-30"))) +
  theme_classic(base_family = "Arial")

g_base



#=======================================
# GRÁFICO CASOS CONFIRMARDOS
#=======================================

# Datos
casos_diarios<- read_csv("data/data_cap_001/df_casos_diarios.csv")
casos_diarios <- casos_diarios |> 
  arrange(fecha_inicio_sintomas) 


# Gráfica casos
g_casos <- g_base +
  geom_col(data = casos_diarios, aes(x = fecha_inicio_sintomas, y = frecuencia),
           fill = "#2b83ba", color = "#2b83ba", alpha = 0.7) +
  annotate("label", x = mean(fechas_ola1), y = 55000, label = "Ola 1", fill = "#2b83ba", color = "white") +
  annotate("label", x = mean(fechas_ola2), y = 55000, label = "Ola 2", fill = "#2b83ba", color = "white") +
  annotate("label", x = mean(fechas_ola3), y = 55000, label = "Ola 3", fill = "#2b83ba", color = "white") +
  annotate("label", x = mean(fechas_ola4), y = 55000, label = "Ola 4", fill = "#2b83ba", color = "white") +
  coord_cartesian(ylim = c(0,60000))+
  ggtitle("A. Casos de COVID-19 en Colombia")+
  xlab("Fecha de inicio de síntomas") + 
  ylab("Número de casos diarios") +
  theme(axis.text.x = element_text(margin = margin(t=10, b=5)),
        axis.text.y = element_text(margin = margin(l=10, r=10)),
        axis.title.x = element_text(size=9, margin = margin(b=10)),
        axis.title.y = element_text(margin = margin(l=5)),
        plot.margin = margin(r=15),
        plot.title = element_text(margin = margin(t=5)))

g_casos


#=======================================
# GRÁFICO FALLECIDOS
#=======================================

# Datos
muertes_depto_fecha <- read_csv("data/data_cap_001/muertes_depto_fecha.csv")

# Agrupación por fecha y calculo de fallecimientos diarias
fallec_diarios <- muertes_depto_fecha |>
  group_by(fecha_muerte) |>
  summarise(frecuencia = sum(muertes), .groups = "drop") |>
  arrange(fecha_muerte) |>
  mutate(media_movil_7d = rollmean(frecuencia, k = 7, fill = NA, align = "right"))

# Gráfica fallecidos
g_muertes <- g_base +
  geom_col(data = fallec_diarios, aes(x = fecha_muerte, y = frecuencia),
           fill = "#2b83ba", color = "#2b83ba", alpha = 0.7) +
  ylim(0, (700)) + 
  xlab("Fecha de muerte") + 
  ylab("Número de muertes diarias") +
  ggtitle("B. Muertes por COVID-19 en Colombia")+
  theme(axis.text.x = element_text(margin = margin(t=10, b=5)),
        axis.text.y = element_text(margin = margin(l=10, r=12)),
        axis.title.x = element_text(size=9, margin = margin(b=15)),
        axis.title.y = element_text(margin = margin(l=5)),
        plot.margin = margin(r=10))

g_muertes

#rm(list = setdiff(ls(), c("picos_casos","picos_fallecidos")))


#=======================================
# GRÁFICO DE DOMINANCIAS
#=======================================

#Datos
df_dominancias <- read_csv("data/data_cap_001/df_dominancias.csv")

# Procesamiento para evitar espacios en curvas
conso_domin_d <- df_dominancias |>
  mutate(variante = factor(ifelse(cepa == "Otros", "Ancestral", cepa), 
                           levels = c("Ancestral", "Alpha", "Gamma", "Mu", "Delta", "Omicron"))) |> 
  complete(Date = seq(min(Date, na.rm = TRUE), max(Date, na.rm = TRUE), by = "day"), 
           variante, fill = list(valor_rep = NA, valor_est = NA)) |>
  group_by(variante) |>
  arrange(Date, .by_group = TRUE) |>
  mutate(valor_rep = na.approx(valor_rep, na.rm = FALSE),
         valor_est = na.approx(valor_est, na.rm = FALSE)) |>
  ungroup()


conso_domin_d <- conso_domin_d |> arrange(variante, Date)
conso_domin_d <- conso_domin_d |>
  group_by(variante) |>
  mutate(Date_num = as.numeric(Date), 
         valor_est_smooth = predict(smooth.spline(Date_num, valor_est, spar = 0.3), Date_num)$y) |>
  ungroup()

# Definir colores
colores <- brewer.pal(8, "Set1")[-6]


# Gráfico
g_dominancia <- g_base +
  geom_line(
    data = conso_domin_d,
    aes(x = Date, y = valor_est_smooth, color = variante, group = variante),
    size = 1.5,
    alpha = 0.7
  ) +
  geom_point(
    data = conso_domin_d,
    aes(x = Date, y = valor_rep, color = variante, group = variante),
    size = 0.4,
    alpha = 0.1
  ) +
  xlab("Fecha de registro") +
  ylab("Proporción de dominancia") +
  ggtitle("C. Dominancia de variantes de COVID-19 en Colombia") +
  scale_color_manual(values = colores) +
  scale_fill_manual(values = colores) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25), limits = c(0, 1)) +
  theme(
    legend.position = c(0.75, 0.85),
    legend.justification = c(0, 1),
    legend.box = "vertical",
    legend.spacing.y = unit(0.005, "cm"),   
    legend.key.height = unit(0.2, "lines"),
    legend.title = element_blank(),
    legend.text = element_text(size=8),
    axis.text.x = element_text(margin = margin(t=10, b=5)),
    axis.text.y = element_text(margin = margin(l=10, r=12)),
    axis.title.x = element_text(size=9, margin = margin(b=10)),
    axis.title.y = element_text(margin = margin(l=5)),
    plot.margin = margin(r=10)
  )

g_dominancia



#=======================================
# GRÁFICO DE RT
#=======================================

#Datos
rt_COL <- read_csv("data/data_cap_001/resultado_rt_COL.csv")|> 
  rename(department = country)

data_Rt<- rt_COL |> 
  group_by(department) |> 
  mutate(rt_mean_mov=rollmean(rt_mean, 7, fill = NA, align = "center"))

ymin_val <- min(data_Rt$rt_mean, na.rm = TRUE)
ymax_val <- max(data_Rt$rt_mean, na.rm = TRUE)

#Gráfica
g_rt <- g_base +
  geom_line(data=data_Rt, 
            aes(x = window_start, y = rt_mean),
            color = "#de77ae", size = 1.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "blue") +
  ggtitle(" ") +
  xlab("Fecha") +
  ylab("Valor de Rt") +
  ggtitle("D. Rt de COVID-19 en Colombia") +
  scale_y_continuous(limits = c(0,4),
                     expand = c(0, 0))+ 
  theme(
    axis.text.x = element_text(margin = margin(t=10, b=5)),
    axis.text.y = element_text(margin = margin(l=22, r=10)),
    axis.title.x = element_text(size=9, margin = margin(b=10)),
    axis.title.y = element_text(margin = margin(l=5)),
    plot.margin = margin(r=10)
  )

g_rt   


################################################
# GRÁFICA DE MOVILIDAD
################################################

#Datos
mobility_col <- readRDS("data/data_cap_001/mobility_col.RDS")

#media movil
df_mobilidad<-mobility_col |> 
  mutate(lugartrabajo=rollmean(lugartrabajo, 7, fill = NA, align = "center"),
         transporte=rollmean(transporte, 7, fill = NA, align = "center"))


#grafica
g_mobilidad <- g_base +
  geom_line(data = df_mobilidad, aes(x = date, y = lugartrabajo, color = "Trabajo presencial"), size = 1.05) +
  geom_line(data = df_mobilidad, aes(x = date, y = transporte, color = "Transporte público"), size = 1.05) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  scale_color_manual(
    name = "Lugares",
    values = c(
      "Transporte público" = "#8073ac",
      "Trabajo presencial" = "#e08214"
    )
  ) +
  labs(
    title = "E. Tendencia de movilidad en Colombia",
    x = "Fecha",
    y = "Cambio porcentual"
  ) +
  theme(
    legend.position = c(0.75, 0.38),
    legend.justification = c(0, 1),
    legend.box = "vertical",
    legend.spacing.y = unit(0.005, "cm"),
    legend.key.height = unit(0.2, "lines"),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text.x = element_text(margin = margin(t = 10, b = 5)),
    axis.text.y = element_text(margin = margin(l = 16, r = 10)),
    axis.title.x = element_text(size=9),
    axis.title.y = element_text(margin = margin(l=5)),
    plot.margin = margin(r=10)
  ) +
  guides(
    color = guide_legend(
      override.aes = list(size = 2)  
    )
  )

g_mobilidad

##############################################
# UNIR LAS 4 GRÁFICAS
##############################################
#todos_los_objetos <- ls()
#rm(list = setdiff(todos_los_objetos, c("g_casos", "g_muertes", "g_dominancia", "g_rt", "g_mobilidad")))


casosymuertesxola <- grid.arrange(
  g_casos, 
  g_muertes, 
  g_dominancia,
  g_rt,
  g_mobilidad,
  ncol = 1, 
  heights = c(1, 1, 1, 1, 1) 
)

# Guardar la imagen con más espacio para la tercera gráfica
ggsave(filename = "outputs/grafic_cap_001/f2_olascontodo.jpg", 
       plot = casosymuertesxola, 
       width = 8, 
       height = 11, 
       dpi = 300)





