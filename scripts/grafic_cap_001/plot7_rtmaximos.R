library(readr)
library(dplyr)
library(zoo)
library(lubridate)
library(readxl)
library(ggtext)

rm(list=ls())

# Cargar datos
rt_dtos <- read_csv("data/data_cap_001/df_rt_dtos.csv") 
rt_COL <- read_csv("data/data_cap_001/resultado_rt_COL.csv")|>
  rename(department = country) |>
  mutate(Departamento="Colombia")


# Unir y suavizar Rt
data_Rt <- bind_rows(rt_dtos, rt_COL) |>
  group_by(department) |>
  mutate(rt_mean_mov = rollmean(rt_mean, 7, fill = NA, align = "center")) |>
  ungroup()

# Cargar fechas de olas desde script
source("scripts/grafic_cap_001/fechas_olas.r")

bandas_olas <- tibble(
  ola = paste("Ola", 1:4),
  xmin = c(fechas_ola1[1], fechas_ola2[1], fechas_ola3[1], fechas_ola4[1]),
  xmax = c(fechas_ola1[2], fechas_ola2[2], fechas_ola3[2], fechas_ola4[2])
)

data_Rt <- data_Rt |>
  mutate(ola = case_when(
    window_start >= fechas_ola1[1] & window_start <= fechas_ola1[2] ~ "Ola 1",
    window_start >= fechas_ola2[1] & window_start <= fechas_ola2[2] ~ "Ola 2",
    window_start >= fechas_ola3[1] & window_start <= fechas_ola3[2] ~ "Ola 3",
    window_start >= fechas_ola4[1] & window_start <= fechas_ola4[2] ~ "Ola 4",
    TRUE ~ NA_character_
  )) |>
  filter(!is.na(ola))


# Obtener máximo Rt por departamento y ola
rt_max <- data_Rt |>
  group_by(Departamento, ola) |>
  filter(rt_mean == max(rt_mean, na.rm = TRUE)) |>
  slice(1) |> 
  ungroup()

rt_max <- rt_max |>
  rename(
    rt_meanmax = rt_mean,
    rt_meanmin = rt_mean_lower,
    rt_meanmaxup = rt_mean_upper
  )

# Filtrar departamentos no estables
dptos_excluir <- c("arauca","caqueta", "casanare", "choco","guainia", 
                   "guaviare", "la_guajira", "san_andres", "sucre","vaupes", "vichada")

rt_max <- rt_max |>
  filter(!departamento_limp %in% dptos_excluir) |>
  mutate(
    Departamento = factor(Departamento, levels = sort(unique(Departamento), decreasing = TRUE))  
  )
    
rt_max_mprueba<-rt_max %>% 
  filter(rt_meanmax>=3 & ola=="Ola 1")

# Promedio de Rt máximo por ola
promedio_max_rt <- rt_max |>
  group_by(ola) |>
  summarise(promedio_rt = mean(rt_meanmax, na.rm = TRUE), .groups = "drop")

# Asegura el orden correcto
niveles_departamentos <- levels(rt_max$Departamento) 

# Gráfico final 
g_rtmax <- ggplot(rt_max, aes(x = Departamento, y = rt_meanmax)) +
  geom_errorbar(aes(ymin = rt_meanmin, ymax = rt_meanmaxup), 
                width = 0.3, color = "black") +
  geom_point(size = 2, shape = 21, fill = "#2b83ba", stroke = 0.6) +
  geom_hline(
    aes(yintercept = 1, color = "Rt = 1"),  
    linetype = "dashed", linewidth = 0.5
  ) +
  geom_hline(
    data = promedio_max_rt,
    aes(yintercept = promedio_rt, color = "Rt promedio"),  
    linetype = "dashed", linewidth = 0.8
  ) +
  coord_flip(ylim = c(0, 6)) +
  facet_wrap(~ola, ncol = 4) +
  scale_color_manual(
    name = NULL,
    values = c("Rt = 1" = "red", "Rt promedio" = "#2b83ba"),
    guide = guide_legend(override.aes = list(linetype = "dashed"))
  ) +
  labs(
    x = "",
    y = "Valor máximo de Rt",
    caption = "Indicador no estimable en Arauca, Caquetá, Casanare, Chocó, Guainía, Guaviare, La Guajira, San Andrés, Sucre, Vaupés y Vichada"
  ) +
  scale_y_continuous(
    breaks = seq(0, 6, by = 1), 
    limits = c(0, 6) 
  ) +
  theme_classic(base_family = "Arial") +
  theme(
    strip.text = element_text(size = 12),
    axis.text.y = element_text(
      size = 11,
      color = ifelse(levels(rt_max$Departamento) == "Colombia", "#2b83ba", "gray40"),
      face = ifelse(levels(rt_max$Departamento) == "Colombia", "bold", "plain")
    ),
    axis.text.x = element_text(size = 10),  
    axis.title.x = element_text(margin = margin(t = 10), size = 12),
    axis.title.y = element_text(margin = margin(l=5,r = 10), size = 12),
    panel.background = element_rect(colour = "black", linewidth = 0.5),
    legend.position = "bottom",
    legend.justification = "right",
    legend.text = element_text(size=10),
    plot.caption = element_text(size = 9, hjust = 1),
    plot.margin = margin(t=5, r=5),
    panel.spacing = unit(1, "lines")  
  )

g_rtmax

#Guardar gráfica 
ggsave("outputs/grafic_cap_001/f7_rtmax.jpg",
       plot = g_rtmax, width = 9, height = 7, dpi = 300)



