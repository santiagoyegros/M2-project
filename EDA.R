#Analisis Descriptivo - EDA
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales, warn.conflicts = FALSE)

if (!require('dlookr'))
  install.packages("dlookr")
library(dlookr)
if (!require('summarytools'))
  install.packages("summarytools")
library(summarytools)


# Read the data from the RDS file
transporte <- readRDS("data/202303.rds")
rutas <- read.csv("data/rutas.csv")


trans <- select(transporte, -consecutivoevento, -dia_hora, -dia_semana, 
                -fecha, -hora, -latitude, -longitude)

rut <- select(rutas, -estado, -troncal)
# Fusionar los datos utilizando la columna "idsam" como clave de unión
datos_fusionados <- inner_join(trans, rut, by = "idrutaestacion")


library(ISLR)
st_options(lang = "es") #Translations
summarytools::view(dfSummary(datos_fusionados), 
                   footnote = NA, 
                   valid.col = FALSE, 
                   file = paste("./","summario_datos_fusionados.html", sep =""))

#Groupby por producto: CR/ES/MO
datos_fusionados_p <- datos_fusionados %>%
                      filter(tipoevento == 4) %>% 
                      group_by(producto) %>%
                      summarize(cantidad_viajes = n())

#Groupby por identidad: 2. MAS - 3. JAHA
datos_fusionados_i <- datos_fusionados %>%
                      filter(tipoevento == 4) %>% 
                      group_by(identidad) %>%
                      summarize(cantidad_viajes = n())

#Groupby por tipo transporte: 1. Normal - 3. Diferencial
datos_fusionados_t <- datos_fusionados %>%
                      filter(tipoevento == 4) %>% 
                      group_by(tipotransporte, EOT, ramal) %>%
                      summarize(cantidad_viajes = sum(montoevento)) %>%
                      ungroup()
  

