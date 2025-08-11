library(ggplot2)
library(gridExtra)
library(scales)
library(dplyr)
library(tidyverse)
library(scales)
library(ggh4x)
library(tidyverse)

#Datos
ciudades <- c("Quibdó", "Medellín", "Buenaventura", "Cali", "Timbiquí", "Popayán", "Tumaco", "Pasto")
habitantes <- c(100000, 2500000, 300000, 2200000, 20000, 350000, 250000, 500000)
penetracion <- c(14.3, 25.8, 8.7, 31.2, 0.2, 20.4, 2.8, 21.2)

# Crear dataframe
df <- data.frame(Ciudad = ciudades, Habitantes = habitantes, Penetracion = penetracion) |>
  arrange(Penetracion)

# Convertir a formato largo
df_long <- df %>%
  pivot_longer(cols = c(Penetracion, Habitantes),
               names_to = "Variable",
               values_to = "Valor") %>%
  mutate(
    Etiqueta = case_when(
      Variable == "Penetracion" ~ paste0(Valor, "%"),
      TRUE ~ format(Valor, big.mark = ".", scientific = FALSE)
    ),
    Variable = recode(Variable,
                      "Penetracion" = "Accesos fijos a internet (%)",
                      "Habitantes" = "Cantidad de habitantes")
  )

# Reordenar factor 
df_long <- df_long %>%
  mutate(Ciudad = fct_reorder(Ciudad, df$Penetracion))

# Gráfica
p_comb <- ggplot(df_long, aes(x = Valor, y = Ciudad)) +
  geom_bar(stat = "identity", aes(fill = Variable), show.legend = FALSE) +
  geom_text(aes(label = Etiqueta), hjust = ifelse(df_long$Variable == "Cantidad de habitantes", -0.05, -0.1), size = 3.5) +
  facet_wrap(
    ~Variable, 
    scales = "free_x"
  ) +
  scale_fill_manual(values = c(
    "Accesos fijos a internet (%)" = "#264653",
    "Cantidad de habitantes" = "#e76f51"
  )) +
  facetted_pos_scales(
    x = list(
      Variable == "Accesos fijos a internet (%)" ~ scale_x_continuous(
        limits = c(0, 30),
        labels = label_number(),
        breaks = seq(0, 30, by = 5),
        expand = expansion(mult = c(0, 0.1))  
      ),
      Variable == "Cantidad de habitantes" ~ scale_x_continuous(
        limits = c(0, 2600000),
        labels = label_number(scale = 1e-6, suffix = " M"),  
        breaks = seq(0, 2500000, by = 500000),
        expand = expansion(mult = c(0, 0.1))
      )
    )
  ) +
  labs(
    x = NULL, 
    y = NULL
  ) +
  theme_classic(base_family = "Alegreya") +
  theme(
    strip.text = element_text(size = 14, hjust = 0.5, face = "plain"),
    axis.text = element_text(size = 10),
    axis.text.y = element_text(margin = margin(r = 10)),  
    plot.margin = margin(r = 20),
    panel.spacing.x = unit(3, "lines"),
    plot.title = element_text(
      hjust = 0.5,
      size = 16,
      margin = margin(b = 15)
    )) +
      coord_cartesian(clip = "off") 

p_comb

#Guardar gráfica
ggsave("outputs/grafic_cap_012/f3_accesoint.jpg",
       p_comb,
       width = 10, 
       height = 5, 
       dpi = 300)
