library(ggplot2)
library(cowplot)
library(dplyr)
library(tidyverse)
library(readxl)
library(ggrepel)
library(patchwork)


rm(list=ls())

#Datos
modelos = readRDS("data/data_cap_002/datosfig5_modelos_tasas.RDS")

# Diccionario de reemplazo
abreviaturas <- c(
  "Amazonas" = "Ama",
  "Antioquia" = "Ant",
  "Arauca" = "Ara",
  "Archipiélago de San Andrés" = "Arc",
  "Atlántico" = "Atl",
  "Bogotá, D.C." = "Bog",
  "Bolívar" = "Bol",
  "Boyacá" = "Boy",
  "Caldas" = "Cal",
  "Caquetá" = "Caq",
  "Casanare" = "Cas",
  "Cauca" = "Cau",
  "Cesar" = "Ces",
  "Chocó" = "Cho",
  "Córdoba" = "Cor",
  "Cundinamarca" = "Cun",
  "Guainía" = "Gua",
  "Guaviare" = "Gua",
  "Huila" = "Hui",
  "La Guajira" = "La",
  "Magdalena" = "Mag",
  "Meta" = "Met",
  "Nariño" = "Nar",
  "Norte de Santander" = "Nor",
  "Putumayo" = "Put",
  "Quindio" = "Qui",
  "Risaralda" = "Ris",
  "Santander" = "San",
  "Sucre" = "Suc",
  "Tolima" = "Tol",
  "Valle del Cauca" = "Val",
  "Vaupés" = "Vau",
  "Vichada" = "Vic"
)

# Aplicar el reemplazo de departamentos abreviados
modelos$CODPTORE <- abreviaturas[modelos$CODPTORE]

#Modelos
lm_eqn <- function(df, y, x){
  formula = as.formula(sprintf('%s ~ %s', y, x))
  m <- lm(formula, data=df);
  eq <- substitute(~~italic(R)^2~"="~r2*","~~p~"="~italic(pvalue),
                   list(r2 = format(summary(m)$r.squared, digits = 3),
                        pvalue = format(summary(m)$coefficients[2,'Pr(>|t|)'], digits=1)
                   )
  )
  as.character(as.expression(eq));                 
}


datos=modelos


# m1: Densidad vs. tasas
m1_data = modelos 

m1 = modelos |>
  mutate(CODPTORE=ifelse(CODPTORE=="Archipiélago de San Andrés","San Andrés",
                         ifelse(CODPTORE=="Norte de Santander","Nt. Santader",CODPTORE))) |>
  ggplot(aes(x=Densidad,y=tasas,label=CODPTORE))+
  geom_smooth(method = lm)+
  geom_point(shape=1)+
  geom_text_repel()+
  xlab("Densidad poblacional \n(personas por kilometro cuadrado de superficie)")+
  ylab("Tasa de exceso de mortalidad estandarizada \npor edad (por 100 mil habitantes)")+
  geom_text(x=6000,y=225,label=lm_eqn(datos,'Densidad','tasas'),parse = T,color="blue")+
  ggtitle("A.") +  
  theme_classic(base_family = "Arial") +
  theme(
    plot.title = element_text(
      hjust = 0,  
      size = 12,   
      margin = margin(b = 10)  
    ))

m1


# m2: IDH vs. tasas
m2_data <- modelos 

m2= modelos |>
  mutate(CODPTORE=ifelse(CODPTORE=="Archipiélago de San Andrés","San Andrés",
                         ifelse(CODPTORE=="Norte de Santander","Nt. Santader",CODPTORE))) |>
  ggplot(aes(x=IDH,y=tasas,label=CODPTORE))+
  geom_smooth(method = lm)+
  geom_point(shape=1)+
  geom_text_repel()+
  xlab("Índice de Desarrollo Humano")+
  ylab("Tasa de exceso de mortalidad estandarizada \npor edad (por 100 mil habitantes)")+
  geom_text(x=0.75,y=225,label=lm_eqn(datos,'IDH','tasas'),parse = T,color="blue")+
  ggtitle("B.") +  
  theme_classic(base_family = "Arial") +
  theme(
    plot.title = element_text(
      hjust = 0,  
      size = 12,   
      margin = margin(b = 10)  
    ))

m2


# m3: IPM2020 vs. tasas
m3_data <- modelos 

m3 = modelos |>
  mutate(CODPTORE=ifelse(CODPTORE=="Archipiélago de San Andrés","San Andrés",
                         ifelse(CODPTORE=="Norte de Santander","Nt. Santader",CODPTORE))) |>
  ggplot(aes(x=IPM2020,y=tasas,label=CODPTORE))+
  geom_smooth(method = lm)+
  geom_point(shape=1)+
  geom_text_repel()+
  xlab("Índice de Pobreza Multidimensional 2020")+
  ylab("Tasa de exceso de mortalidad estandarizada \npor edad (por 100 mil habitantes)")+
  geom_text(x=60,y=225,label=lm_eqn(datos,'IPM2020','tasas'),parse = T,color="blue")+
  ggtitle("C.") +  
  theme_classic(base_family = "Arial") +
  theme(
    plot.title = element_text(
      hjust = 0,  
      size = 12,   
      margin = margin(b = 10)  
    )) 

m3


# m4: IPM2021 vs. tasas
m4_data <- modelos 

m4 = modelos |>
  mutate(CODPTORE=ifelse(CODPTORE=="Archipiélago de San Andrés","San Andrés",
                         ifelse(CODPTORE=="Norte de Santander","Nt. Santader",CODPTORE))) |>
  ggplot(aes(x=IPM2021,y=tasas,label=CODPTORE))+
  geom_smooth(method = lm)+
  geom_point(shape=1)+
  xlab("Índice de Pobreza Multidimensional 2021")+
  ylab("Tasa de exceso de mortalidad estandarizada \npor edad (por 100 mil habitantes)")+
  geom_text_repel()+
  geom_text(x=60,y=225,label=lm_eqn(datos,'IPM2021','tasas'),parse = T,color="blue")+
  ggtitle("D.") +  
  theme_classic(base_family = "Arial") +
  theme(
    plot.title = element_text(
      hjust = 0,  
      size = 12,  
      margin = margin(b = 10)  
    )) 

m4



# m5: PM2021 vs. tasas
m5_data <- modelos 

m5 = modelos |>
  mutate(CODPTORE=ifelse(CODPTORE=="Archipiélago de San Andrés","San Andrés",
                         ifelse(CODPTORE=="Norte de Santander","Nt. Santader",CODPTORE))) |>
  ggplot(aes(x=PM2021,y=tasas,label=CODPTORE))+
  geom_smooth(method = lm)+
  geom_point(shape=1)+
  geom_text_repel()+
  xlab("Pobreza Monetaria")+
  ylab("Tasa de exceso de mortalidad estandarizada \npor edad (por 100 mil habitantes)")+
  geom_text(x=58,y=225,label=lm_eqn(datos,'PM2021','tasas'),parse = T,color="blue")+
  ggtitle("E.") +  
  theme_classic(base_family = "Arial") +
  theme(
    plot.title = element_text(
      hjust = 0, 
      size = 12,   
      margin = margin(b = 10)  
    )) 

m5



# m6: PME2021 vs. tasas
m6_data <- modelos 

m6 = modelos |>
  mutate(CODPTORE=ifelse(CODPTORE=="Archipiélago de San Andrés","San Andrés",
                         ifelse(CODPTORE=="Norte de Santander","Nt. Santader",CODPTORE))) |>
  ggplot(aes(x=PME2021,y=tasas,label=CODPTORE))+
  geom_smooth(method = lm)+
  geom_point(shape=1)+
  geom_text_repel()+
  xlab("Pobreza Monetaria Extrema")+
  ylab("Tasa de exceso de mortalidad estandarizada \npor edad (por 100 mil habitantes)")+
  geom_text(x=33,y=225,label=lm_eqn(datos,'PME2021','tasas'),parse = T,color="blue")+
  ggtitle("F.") + 
  theme_classic(base_family = "Arial") +
  theme(
    plot.title = element_text(
      hjust = 0, 
      size = 12,   
      margin = margin(b = 10)  
    )) 

m6



# m7: PIB2021 vs. tasas
m7_data <- modelos 

m7 = modelos |>
  mutate(CODPTORE=ifelse(CODPTORE=="Archipiélago de San Andrés","San Andrés",
                         ifelse(CODPTORE=="Norte de Santander","Nt. Santader",CODPTORE))) |>
  ggplot(aes(x=PIB2021,y=tasas,label=CODPTORE))+
  geom_smooth(method = lm)+
  geom_point(shape=1)+
  geom_text_repel()+
  xlab("Producto Interno Bruto per Capitá")+
  ylab("Tasa de exceso de mortalidad estandarizada \npor edad (por 100 mil habitantes)")+
  geom_text(x=3*10^(7),y=225,label=lm_eqn(datos,'PIB2021','tasas'),parse = T,color="blue")+
  ggtitle("G.") +  
  theme_classic(base_family = "Arial") +
  theme(
    plot.title = element_text(
      hjust = 0,  
      size = 12,   
      margin = margin(b = 10)  
    )) 

m7



# m8: ESTUDIO vs. tasas
m8_data <- modelos 

m8 = modelos |>
  mutate(CODPTORE=ifelse(CODPTORE=="Archipiélago de San Andrés","San Andrés",
                         ifelse(CODPTORE=="Norte de Santander","Nt. Santader",CODPTORE))) |>
  ggplot(aes(x=ESTUDIO,y=tasas,label=CODPTORE))+
  geom_smooth(method = lm)+
  geom_point(shape=1)+
  geom_text_repel()+
  scale_x_continuous(labels = scales::percent) +
  xlab("Población de 25 y más años con al menos \n estudios universitarios (Porcentaje)")+
  ylab("Tasa de exceso de mortalidad estandarizada \npor edad (por 100 mil habitantes)")+
  geom_text(x=0.2,y=225,label=lm_eqn(datos,'ESTUDIO','tasas'),parse = T,color="blue")+
  ggtitle("H.") +  
  theme_classic(base_family = "Arial") +
  theme(
    plot.title = element_text(
      hjust = 0, 
      size = 12,   
      margin = margin(b = 10) 
    ))

m8

# Combinar los 8 gráficos
fig5ab <- m1 + m2 + 
          m3 + m4 + 
          plot_layout(ncol = 2)

fig5cd <- m5 + m6 + 
          m7 + m8 + 
          plot_layout(ncol = 2)


ggsave(filename = "outputs/grafic_cap_002/f5ab_modelos.jpg", 
       plot = fig5ab, 
       width = 10, 
       height = 9, 
       dpi = 300)


ggsave(filename = "outputs/grafic_cap_002/f5cd_modelos.jpg", 
       plot = fig5cd, 
       width = 10, 
       height = 9, 
       dpi = 300)









