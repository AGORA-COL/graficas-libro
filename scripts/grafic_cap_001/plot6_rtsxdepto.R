library(readr)
library(dplyr)
library(ggplot2)

rm(list=ls())

# Datos
rt_COL <- read_csv("data/data_cap_001/resultado_rt_COL.csv") |> 
  rename(department = country) |> 
  mutate(Departamento="Colombia")

rt_dtos <- read_csv("data/data_cap_001/df_rt_dtos.csv") 


# Agrupación por fecha y calculo de Rt
data_Rt <- bind_rows(rt_dtos, rt_COL)

data_Rt <- data_Rt |> 
  group_by(Departamento) |> 
  mutate(rt_mean_mov=rollmean(rt_mean, 7, fill = NA, align = "center"))

# Filtrar departamentos no estables
dptos_excluir <- c("arauca","caqueta", "casanare", "choco","guainia", 
                   "guaviare", "la_guajira", "san_andres", "sucre","vaupes", "vichada")

data_Rt <- data_Rt |>
  filter(!departamento_limp %in% dptos_excluir) 

# Unir los datos de Colombia con los datos departamentales
data_Rt$Departamento <- factor(data_Rt$Departamento, levels = c("Colombia", sort(setdiff(unique(data_Rt$Departamento), "Colombia"))))

ymin_val <- min(data_Rt$rt_mean, na.rm = TRUE)
ymax_val <- max(data_Rt$rt_mean, na.rm = TRUE)

#Fecha de las Olas
source("scripts/grafic_cap_001/fechas_olas.r")

# Gráfica 
g_rts <- ggplot(data_Rt, aes(x = window_start, y = rt_mean))+
  geom_rect(aes(xmin = fechas_ola1[1], xmax = fechas_ola1[2], ymin = -Inf, ymax = Inf),
            fill = "#C1CDCD", alpha = 0.02, inherit.aes = FALSE) +
  geom_rect(aes(xmin = fechas_ola2[1], xmax = fechas_ola2[2], ymin = -Inf, ymax = Inf),
            fill = "#C1CDCD", alpha = 0.02, inherit.aes = FALSE) +
  geom_rect(aes(xmin = fechas_ola3[1], xmax = fechas_ola3[2], ymin = -Inf, ymax = Inf),
            fill = "#C1CDCD", alpha = 0.02, inherit.aes = FALSE) +
  geom_rect(aes(xmin = fechas_ola4[1], xmax = fechas_ola4[2], ymin = -Inf, ymax = Inf),
            fill = "#C1CDCD", alpha = 0.02, inherit.aes = FALSE) +
  geom_line(aes(color = rt_mean), size = 1.2) +
  geom_hline(yintercept = 1, linetype= "dashed", color="blue")+
  facet_wrap(~ Departamento, scales = "free_y",  ncol = 3)+
  labs(x = "", 
       y = "Número de casos diarios",
       caption = "Indicador no estimable en Arauca, Caquetá, Casanare, Chocó, Guainía, Guaviare, La Guajira, San Andrés, Sucre, Vaupés y Vichada"
  ) +
  scale_x_date(breaks = seq(as.Date("2020-02-01"), as.Date("2023-06-30"), by = "20 month"),
               date_labels = "%b %Y",
               expand = c(0, 0)) +
  scale_y_continuous(
    breaks = function(x) seq(0, 6, length.out = 3),  
    labels = function(x) signif(round(x, 0), digits = 2),  
    expand = expansion(mult = c(0, 0.2)),
    limits = c(0,6)
  ) +
  coord_cartesian(xlim = c(as.Date("2020-02-01"), as.Date("2023-06-30"))) +
  theme_classic(base_family = "Arial")+
  theme(strip.text = element_text(size = 18),
        axis.text.x = element_text(size = 16, margin = margin(t = 15, b=20)),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 19, margin = margin(l=10, r = 10)),
        panel.spacing.x = unit(5, "lines"),  
        panel.spacing.y = unit(1, "lines"),  
        panel.background = element_rect(colour = "black", linetype = "solid"),
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.position = c(0.85,0.01),
        legend.title = element_blank(),
        legend.text = element_text(size = 19, color = "black"),
        plot.margin = margin(r=30,t=5),
        plot.caption = element_text(size=14)) +
  guides(color = guide_colorbar(barwidth = 10, barheight = 0.8))+
  scale_colour_gradientn(
    colours = c("#38D020", "#B3FF1B", "yellow", "#FF9225", "red"),
    values = scales::rescale(c(0, 0.7, 0.9, 1, 5)), 
    breaks = c(0, 1, 2, 3, 4, 5),
    limits = c(0,6),
    oob = scales::squish
  )

g_rts

# Guardar gráfica
ggsave(filename = "outputs/grafic_cap_001/f6_rtxdepto.jpg", 
       plot = g_rts, 
       width = 14, 
       height = 18, 
       dpi = 300)
