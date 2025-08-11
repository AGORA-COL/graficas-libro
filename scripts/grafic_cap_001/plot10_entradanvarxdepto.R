library(readr)
library(dplyr)
library(stringr)

rm(list=ls())

options(scipen = 999)

# Datos
df_dominancias <- read_csv("data/data_cap_001/df_dominancias.csv") 

domin_rep <- df_dominancias |> 
  select(Date, Departamento, cepa, valor_rep)

# Fecha del primer caso en Colombia
primer_caso <- as.Date("2020-03-06")

# Primera aparición de cada cepa por departamento
primeras_fechas <- domin_rep |>
  filter(valor_rep > 0) |>
  mutate(
    cepa = if_else(cepa == "Otros", "Ancestral", cepa),
    cepa = factor(cepa, levels = c("Ancestral", "Alpha", "Gamma", "Mu", "Delta", "Omicron")),
    Departamento = if_else(str_starts(Departamento, "NARI"), "NARIÑO", Departamento)
  ) |>
  group_by(Departamento, cepa) |>
  summarise(primera_fecha = min(Date), .groups = "drop")

# Orden de aparición de cepas por primera vez 
orden_por_cepa <- primeras_fechas |>
  group_by(cepa) |>
  filter(primera_fecha == min(primera_fecha)) |>
  slice(1) |>
  arrange(primera_fecha) |>
  mutate(orden = row_number()) |>
  filter(cepa != "Mu")  

# Obtener departamentos restantes 
orden_departamentos_restantes <- primeras_fechas |>
  group_by(Departamento) |>
  summarise(fecha_mas_temprana = min(primera_fecha, na.rm = TRUE)) |>
  arrange(fecha_mas_temprana) |>
  filter(!Departamento %in% orden_por_cepa$Departamento) |>
  mutate(orden = row_number() + nrow(orden_por_cepa))

# Combinar orden total de departamentos
orden_departamentos_total <- bind_rows(
  orden_por_cepa |> select(Departamento, orden),
  orden_departamentos_restantes
) |>
  arrange(orden)

etiquetas_dptos <- read_excel("data/data_cap_001/etiquetas_dptos.xlsx") 

# Aplicar orden de departamentos 
primeras_fechas_plot <- primeras_fechas |>
  mutate(Departamento = factor(Departamento, levels = rev(orden_departamentos_total$Departamento)))

# Orden de departamentos para la cepa "Ancestral"
orden_departamentos <- primeras_fechas |>
  filter(cepa == "Ancestral") |>
  arrange(primera_fecha) |>
  right_join(etiquetas_dptos, by = "Departamento")

primeras_fechas_original <- primeras_fechas |>
  mutate(Departamento = factor(Departamento, levels = rev(orden_departamentos$Departamento)))


# Paleta de colores 
colores_variantes <- brewer.pal(8, "Set1")[-c(6)]

#Grafico 
ingresos_var <- ggplot(primeras_fechas_original, aes(y = Departamento)) +
  geom_point(
    aes(x = primera_fecha, fill = cepa),  
    size = 4,
    position = position_dodge(width = 0.7),
    shape = 21,  
    color = "black",  
    stroke = 0.5  
  ) +
  scale_fill_manual(  
    values = colores_variantes,
    name = "Variante"
  ) +
  scale_x_date(
    date_labels = "%b %Y",  
    date_breaks = "2 months",
    limits = c(as.Date("2020-01-01"), as.Date("2022-11-01")),
    expand = c(0, 0)
  ) +
  labs(
    x = "",
    y = "Departamento",
    title = ""
  ) +
  theme_classic(base_family = "Arial") +
  theme(
    axis.text.x = element_text(size = 13, angle = 45, hjust = 1, margin = margin(t = 10)),
    axis.text.y = element_text(size = 13),
    axis.title.y = element_text(size = 15, margin = margin(l=10)),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_blank(),
    legend.background = element_rect(fill = "white", color = "gray80", size = 0.3),
    panel.spacing.x = unit(5, "lines"),  
    panel.background = element_rect(colour = "black", linetype = "solid"),
    plot.margin = margin(r=30)
  )+
  guides(fill = guide_legend(  
    nrow = 1,
    override.aes = list(shape = 21, color = "black") 
  ))

ingresos_var


ggsave("outputs/grafic_cap_001/f10_ingresos_var.jpg",
       plot=ingresos_var,
       width=9,
       height=8,
       dpi=300)
