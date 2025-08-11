library(ggplot2)

# Datos
df <- data.frame(
  grupo = c("Menos de 35", "35 ó más"),
  confianza = c(37, 42)  
)

#Gráfica
gr_confianza<-ggplot(df, aes(x = confianza, y = reorder(grupo, confianza))) +
  geom_col(fill = "#5CACEE", width = 0.6, color="black") + 
  geom_text(aes(label = paste0(confianza, "%")),
            hjust = -0.1, color = "black", size = 4) +
  scale_x_continuous(labels = scales::percent_format(scale = 1), 
                     limits = c(0, 50),
                     expand = expansion(mult = c(0,0))) +
  labs(x = "Confianza en las noticias", y = NULL) +
  theme_classic() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(margin = margin(t = 10),
                                size = 14),
    plot.margin = margin(l=10, r=40, t=10)
  )

gr_confianza

#Guardar gráfica
ggsave("outputs/grafic_cap_025/f1_confianza.jpg",
       plot=gr_confianza,
       width = 8,
       height =5, 
       dpi=300)

