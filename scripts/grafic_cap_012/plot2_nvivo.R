library(ggplot2)
library(dplyr)
library(tibble)

# Datos
categorias <- c(
  "Estrategias de mejora y expansión", "Recomendaciones Stakeholders", 
  "Impacto del marco legal", "Experiencia del paciente", 
  "Infraestructura y Recursos Humanos", "Estrategia Organizacional", 
  "Relacionamiento organizacional"
)

nodos <- c(5, 10, 15, 16, 21, 28, 37)

etiquetas <- c(
  "Innovación y sostenibilidad", "Proyecciones a futuro", 
  "Factores regulatorios y normativos", "Percepción del usuario/paciente", 
  "Capacidades instaladas", "Limitaciones en la atención", 
  "Ejemplos de proyectos"
)

colores <- c("#A6CEE3", "#33A02C", "#FDBF6F", "#6A3D9A", "#E31A1C", "#1F78B4", "#FEE08B")

# Crear dataframe
df <- tibble(
  Categoria = factor(categorias, levels = categorias), 
  Nodos = nodos,
  Etiqueta = etiquetas,
  Color = colores
)

# Gráfica
nvivo<-ggplot(df, aes(x = Nodos, y = reorder(Categoria, Nodos))) +
  geom_col(aes(fill = Categoria), width = 0.7, show.legend = FALSE) +
  scale_fill_manual(values = colores) +
  geom_text(aes(label = paste0(Nodos, " nodos\n(", Etiqueta, ")")), 
            hjust = -0.05, vjust = 0.5, color = "black", size = 3.8,
            fontface = "plain", lineheight = 1.05,
            label.padding = unit(0.15, "lines"),
            label.r = unit(0.1, "lines"),
            label.size = 0.3,
            fill = after_scale(fill)) +
  labs(
    title = "Análisis cualitativo de categorías codificadas (NVivo)",
    x = "Número de Nodos", y = NULL
  ) +
  annotate("text", x = sum(nodos), y = 0.5, 
           label = paste0("Total de nodos: ", sum(nodos)), 
           hjust = 1.1, vjust = 0, color = "gray40", size = 3.5) +
  scale_x_continuous(
    breaks = seq(0, 40, by=5),
    limits = c(0,50),
    expand = expansion(mult = c(0,0))
  )+
  theme_classic(base_family = "Alegreya") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14, margin = margin(b = 10)),
    axis.text = element_text(size = 10),
    axis.title.x = element_text(size = 11, margin = margin(t = 5))
  ) 

#Guardar gráfica
ggsave("outputs/grafic_cap_012/f2_nvivo.jpg",
       nvivo,
       width = 9, 
       height = 6, 
       dpi = 300)
