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

if (!require('ISLR'))
  install.packages("ISLR")
library(ISLR)

if (!require('rio'))
  install.packages("rio")
library(rio)

if (!require('explore'))
  install.packages("explore")
library(explore)


# Leemos los datos de Marzo 2023
transporte <- readRDS("data/202303.rds")
rutas <- read.csv("data/rutas.csv")

# Seleccionamos las columnas a utilizar
trans <- select(transporte, -consecutivoevento, -dia_hora, -dia_semana, 
                -fecha, -hora, -latitude, -longitude)
rut <- select(rutas, -estado, -troncal)

# Fusionar los datos utilizando la columna "idrutaestacion" como clave de unión
datos_fusionados <- inner_join(trans, rut, by = "idrutaestacion")

# Split de Fecha y Hora
datos_fusionados$Fecha <- as.Date(datos_fusionados$fechahoraevento)
datos_fusionados$Hora <- format(datos_fusionados$fechahoraevento,"%H:%M:%S")


datos_fusionados_short <- head(datos_fusionados, 1000)

datos_fusionados_short <- datos_fusionados_short %>%
                            mutate(Hora = hms(Hora))

datos_fusionados_short$h <- as.numeric(format(datos_fusionados_short$fechahoraevento,'%H'))

datos_fusionados_short %>%
              mutate(Periodo = case_when(datos_fusionados_short$h >= 0 & datos_fusionados_short$h < 4 ~ 'Madrugada',
                                         datos_fusionados_short$h >= 4 & datos_fusionados_short$h < 8 ~ 'Mañana',
                                         datos_fusionados_short$h >= 8 & datos_fusionados_short$h < 12 ~ 'Medio Dia',
                                         datos_fusionados_short$h >= 12 & datos_fusionados_short$h < 16 ~ 'Siesta',
                                         datos_fusionados_short$h >= 16 & datos_fusionados_short$h < 20 ~ 'Tarde',
                                         datos_fusionados_short$h >= 20 & datos_fusionados_short$h < 24 ~ 'Noche',
                                        TRUE ~ 'Error'))


datos_fusionados_short$Periodo <-
              mutate(Periodo = case_when(datos_fusionados_short$h >= 20 & datos_fusionados_short$h < 24 ~ 'Noche'))


#Groupby por tipo transporte: 1. Normal - 3. Diferencial
datos_fusionados_t <- datos_fusionados %>%
                      filter(tipoevento == 4) %>% 
                      group_by(tipotransporte, EOT, ramal, Fecha) %>%
                      summarize(cantidad_viajes = sum(montoevento)) %>%
                      ungroup()

datos_fusionados_short <- head(datos_fusionados, 1000)

datos_fusionados_short %>%
  eda_paged_report(target = "EOT", subtitle = "Empresas", 
                   output_dir = "./output", output_file = "EDA.pdf", theme = "blue")

#dfSummary(datos_fusionados_short)

#explore(datos_fusionados_short)

create_notebook_explore(
  output_dir =  "./",
  output_file = "output/notebook-exploracion-EDA")


dlookr::describe(datos_fusionados)

library(ISLR)

dfSummary(datos_fusionados)
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



