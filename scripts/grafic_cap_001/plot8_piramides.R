library(ggh4x)
library(ggplot2)
library(ggforce)
library(dplyr)
library(scales)
library(tibble)

rm(list=ls())

# Cargar datos 
df_casos_edad_sexo <- read_csv("data/data_cap_001/df_casos_edad_sexo.csv")
df_fallec_edad_sexo <- read_csv("data/data_cap_001/df_fallecidos_edad_sexo.csv")
df_pirpoblnal_2020 <- read_csv("data/data_cap_001/df_pirpoblnal_2020.csv")


# Unir y transformar las bases
df_unido <- bind_rows(
  df_casos_edad_sexo |> mutate(tipo = "caso"),
  df_fallec_edad_sexo |> mutate(tipo = "fallecido")
) 


piram_col <- bind_rows(df_unido, df_pirpoblnal_2020) |>
  mutate(frecuencia = ifelse(Sexo == "M", -frecuencia, frecuencia))
         
# Datos con frecuencia negativa para Masculino
piram_col <- piram_col |>
  mutate(frecuencia = ifelse(Sexo == "M", -abs(frecuencia), frecuencia),
         Sexo=ifelse(Sexo=="M","Masculino","Femenino"),
         grupo_edad = factor(grupo_edad, levels = unique(grupo_edad)))

# Calcular límites por tipo
limites_tipo <- piram_col |>
  group_by(tipo) |>
  summarise(max_abs = max(abs(frecuencia), na.rm = TRUE)) |>
  deframe()

# Colores y orden
colores_sexo <- c("Femenino" = "#AB82FF", "Masculino" = "#ccebc5")
piram_col$tipo <- factor(piram_col$tipo, levels = c("poblacion", "caso", "fallecido"))

# Gráfico 
g_pobt <- ggplot(data = piram_col) +
  geom_col(aes(x = grupo_edad, y = frecuencia, fill = Sexo), color = "black") +
  coord_flip() +
  facet_wrap(
    ~tipo, 
    ncol = 3, 
    scales = "free_x",
    labeller = as_labeller(c(
      "caso" = "B. Casos confirmados de COVID-19", 
      "fallecido" = "C. Muertes por COVID-19", 
      "poblacion" = "A. Población de Colombia"
    ))
  ) +
  scale_fill_manual(
    values = colores_sexo,
    breaks = rev(levels(factor(piram_col$Sexo)))
  ) +
  facetted_pos_scales(
    y = list(
      tipo == "poblacion" ~ scale_y_continuous(
        limits = c(-limites_tipo["poblacion"], limites_tipo["poblacion"]),
        labels = function(x) label_number(scale = 1e-6, suffix = "M")(abs(x)),  
        breaks = pretty_breaks(n = 5)
      ),
      tipo == "caso" ~ scale_y_continuous(
        limits = c(-limites_tipo["caso"], limites_tipo["caso"]),
        labels = function(x) label_number()(abs(x)),  
        breaks = pretty_breaks(n = 5)
      ),
      tipo == "fallecido" ~ scale_y_continuous(
        limits = c(-limites_tipo["fallecido"], limites_tipo["fallecido"]),
        labels = function(x) label_number()(abs(x)), 
        breaks = pretty_breaks(n = 5)
      )
    )
  ) +
  scale_x_discrete(drop = FALSE) +
  labs(
    x = "Quinquenios de edad",
    y = ""
  ) +
  theme_classic(base_family = "Arial") +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key = element_rect(colour = "black", linewidth = 0.05), 
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12, face = "bold", vjust = 3),
    strip.text = element_text(size = 12),
    panel.spacing.x = unit(2, "lines"),
    panel.spacing.y = unit(1, "lines"),
    plot.margin = margin(10, 20, 10, 10),
    panel.background = element_rect(colour = "black", linetype = "solid")
  )

g_pobt

ggsave(filename = "outputs/grafic_cap_001/f8_piramidpobs.jpg", 
       plot = g_pobt, 
       width = 12, 
       height = 6, 
       dpi = 300)

