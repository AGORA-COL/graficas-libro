library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(ggplot2)
library(RColorBrewer)
library(forcats)

rm(list=ls())

#Datos
conso_domin_d <- read_csv("data/data_cap_001/df_dominancias.csv")

domin_est <- conso_domin_d |> 
  dplyr::select(Date, Departamento, cepa, valor_est)

#Traer a Colombia a la base:
conso_domin_colombia <- domin_est |>
  mutate(Departamento = "Colombia")

#Data set con todo (Colombia + Departamentos):
domin_col<- bind_rows(domin_est, conso_domin_colombia) 


# Preprocesamiento
conso_domin_d <- domin_col |>
  mutate(
    variant = factor(ifelse(cepa == "Otros", "Ancestral", cepa), 
                     levels = c("Ancestral", "Alpha", "Gamma", "Mu", "Delta", "Omicron")),
    Departamento = fct_relevel(Departamento, "Colombia")  
  )

# Paleta de colores
colores <- brewer.pal(8, "Set1")[-6]  

# Gráfico
gr_dominancia_departamental <- ggplot(conso_domin_d, aes(x = Date, y = valor_est, fill = variant)) +
  geom_area(position = "stack", alpha = 0.8) +
  facet_wrap(~ Departamento, 
             ncol = 3) +
  xlab("") + 
  ylab("Proporción de dominancia") +
  coord_cartesian(ylim = c(0,1), 
                  xlim = c(as.Date("2020-02-01"), as.Date("2023-06-30")))+
  scale_fill_manual(values = colores, breaks = levels(conso_domin_d$variant)) +
  scale_x_date(breaks = seq(as.Date("2020-02-01"), as.Date("2023-06-30"), by = "20 month"),
               date_labels = "%b %Y",
               expand = c(0, 0)) +
  scale_y_continuous(n.breaks = 3)+
  theme_classic(base_family = "Arial") +
  theme(
    strip.text = element_text(size = 18),
    axis.text.x = element_text(size = 16, margin = margin(t = 10)),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 19, margin = margin(l=10, r = 10)),
    legend.title = element_blank(),
    legend.text = element_text(size = 18),
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.position = c(0.55,0.01),
    legend.background = element_rect(fill = "white", color = "gray80", size = 0.3),
    panel.spacing.x = unit(5, "lines"),  
    panel.background = element_rect(colour = "black", linetype = "solid"),
    plot.margin = margin(r=30,t=5)
  )

gr_dominancia_departamental

ggsave("outputs/grafic_cap_001/f11_dominvarian.jpg",
       plot = gr_dominancia_departamental, 
       width = 14, 
       height = 18,
       dpi = 300)

