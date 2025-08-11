library(ggplot2)
library(dplyr)
library(readxl)
library(readr)
library(gridExtra)
library(lubridate)
library(stringr)
library(lubridate)
library(tidyr)
library(scales)
library(ggh4x)
library(epitrix)

rm(list=ls())

# Datos

casos_fall <- read_csv("data/data_cap_001/df_casos_fall.csv") 


#Poblacion nacional por departamento del año 2019:
df_poblacion2019 <- read_csv("data/data_cap_001/df_poblacion2019.csv")|> 
  mutate(departamento = case_when(
    departamento == "valle" ~ "valle_del_cauca",
    departamento == "norte_santander" ~ "norte_de_santander",
    TRUE ~ departamento
  ))|>
  group_by(departamento)|>
  mutate(poblaciondepart = sum(poblacionxedad, na.rm = TRUE))|>
  ungroup()

etiquetas_dptos <- read_excel("data/data_cap_001/etiquetas_dptos.xlsx")

# Mejorar etiquetas
poblacion_dep_2019 <- df_poblacion2019|> 
  left_join(etiquetas_dptos, by = c("departamento" = "departamento_limp"))|> 
  mutate(grupo_edad = as.numeric(grupo_edad)
  ) 

sum(poblacion_dep_2019$poblacionxedad)

# Unir los datos de casos_fallecidos con la población por grupo de edad
casos_resumidos <- casos_fall|>
  mutate(grupo_edad_ajustado = ifelse(grupo_edad >= 84, 84, grupo_edad))|>
  group_by(nombre_departamento, grupo_edad_ajustado)|>
  summarise(casos = sum(casos, na.rm = TRUE), .groups = "drop")

casos_pob<-poblacion_dep_2019%>%
  left_join(casos_resumidos,
            by = c("departamento" = "nombre_departamento", 
                   "grupo_edad" = "grupo_edad_ajustado"))


###################################################################################
#  CÁLCULO DE TASAS AJUSTADAS 
##################################################################################

#  Tasa cruda
tasa_cruda <- casos_pob|>
  group_by(Departamento, poblaciondepart)|>
  summarise(casos_totales = sum(casos, na.rm = TRUE), .groups = "drop")|>
  mutate(tasa_cruda_mortalidad = (casos_totales / poblaciondepart) * 100000)

#  Población estándar (estructura por edad de toda la población nacional)
poblacion_estandar <- poblacion_dep_2019|>
  group_by(grupo_edad)|>
  summarise(poblacion_total = sum(poblacionxedad, na.rm = TRUE))|>
  mutate(ponderador = poblacion_total / sum(poblacion_total))

sum(poblacion_estandar$poblacion_total)
sum(poblacion_estandar$ponderador)


# Tasa específica por edad por departamento
tasas_edad_dep <- casos_pob|>
  group_by(Departamento, grupo_edad)|>
  summarise(
    casos = sum(casos, na.rm = TRUE),
    poblacionxedad = sum(poblacionxedad, na.rm = TRUE),
    .groups = "drop"
  )|>
  mutate(tasa_edad = casos / poblacionxedad)


# Ajuste de la tasa por edad por departamento
tasa_ajustada <- tasas_edad_dep|>
  left_join(poblacion_estandar, by = "grupo_edad")|>
  mutate(tasa_ponderada = tasa_edad * ponderador)|>
  group_by(Departamento)|>
  summarise(tasa_ajustada_mortalidad = sum(tasa_ponderada, na.rm = TRUE) * 100000)


# Unir con población estándar y calcular tasas específicas por edad
tasas_finales <- tasa_cruda|>
  left_join(tasa_ajustada, by = "Departamento")



###################################################################################
#  GRÁFICA DE TASAS 
##################################################################################

# Obtener el orden de los departamentos por población total (menor a mayor)
orden_poblacion <- tasas_finales|>
  arrange(poblaciondepart )|>
  pull(Departamento)


# Reordenar los niveles del factor departamento según la población
tabla_resumida_long <- tasas_finales|> 
  pivot_longer(cols = c("tasa_cruda_mortalidad", "tasa_ajustada_mortalidad", "poblaciondepart"),
               names_to = "tipo_tasa",
               values_to = "valor")|> 
  filter(!is.na(Departamento))|> 
  mutate(
    tipo_tasa = factor(tipo_tasa, levels = c("poblaciondepart", "tasa_cruda_mortalidad", "tasa_ajustada_mortalidad")),
    Departamento = factor(Departamento, levels = orden_poblacion)
  )

# Nombre de etiquetas
labels_tasas <- c(
  "poblaciondepart" = "A. Población total",
  "tasa_cruda_mortalidad" = "B. Tasa cruda",
  "tasa_ajustada_mortalidad" = "C. Tasa ajustada"
)

colores_tasas <- c(
  "tasa_ajustada_mortalidad" = "#8856a7",
  "tasa_cruda_mortalidad" = "#ccebc5",
  "poblaciondepart" = "#c994c7"
)

# Gráfica
grap_tasas <- ggplot(tabla_resumida_long) +
  geom_col(aes(x = Departamento, y = valor, fill = tipo_tasa), color = "black") +
  coord_flip(clip = "off") +
  facet_wrap(~tipo_tasa, ncol = 3, scales = "free_x", labeller = labeller(tipo_tasa = labels_tasas)) +
  scale_fill_manual(
    values = colores_tasas,
    labels = labels_tasas
  ) +
  facetted_pos_scales(
    y = list(
      tipo_tasa == "poblaciondepart" ~ scale_y_continuous(
        labels = function(x) ifelse(x == 0, "0", scales::comma_format(scale = 1e-3, suffix = "K", accuracy = 1)(x)),
        breaks = scales::pretty_breaks(n = 3),
        expand = c(0,0)
      )
    )) +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1), expand = expansion(mult = c(0, 0.1))) +
  labs(x = "", y = "") +
  theme_classic(base_family = "Arial")+
  theme(
    legend.position = "none",
    legend.box = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    legend.key = element_rect(linewidth = 0.01), 
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12, vjust = 3),
    strip.text = element_text(size = 12),
    panel.spacing.x = unit(2, "lines"),
    panel.spacing.y = unit(1, "lines"),
    plot.margin = margin(10, 40, 10, 10),
    panel.background = element_rect(colour = "black", linewidth = 0.5)
  ) +
  labs(caption = "Tasa por 100.000 habitantes\nPoblación total del año 2019, fuente DANE")

grap_tasas


# Guardar gráfica
ggsave("outputs/grafic_cap_001/f9_tasas_ajustad.jpg",
       plot = grap_tasas, width = 11, height = 8, dpi = 300)



