#Analisis Descriptivo - EDA


###########################
#Install necessary packages
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
if (!require('dplyr'))
  install.packages("dplyr")
library(dplyr)

if (!require('magrittr'))
  install.packages("magrittr")
library(magrittr)


#################################
#Prepare the data
# Read the data from the RDS file
transporte <- readRDS("data/202303.rds")
rutas <- read.csv("data/rutas.csv")

transporte <- select(transporte, -fechahoraevento, -consecutivoevento, -latitude, -longitude, -dia_hora)
rutas <- select(rutas, -estado)

transporte %<>% 
  mutate(periodo = case_when(
    hora >= 0 & 
      hora < 4 ~ "Madrugada",
    hora >= 4 & 
      hora < 8 ~ "Mañana",
    hora >= 8 & 
      hora < 12 ~ "Mediamañana",
    hora >= 12 & 
      hora < 16 ~ "Siesta",
    hora >= 16 & 
      hora < 20 ~ "Tarde",
    hora >= 20 & 
      hora <= 23 ~ "Noche"
    ))

transporte %<>% 
  mutate(dia = case_when(
    dia_semana == "Sun" ~ "Domingo",
    dia_semana == "Mon" ~ "Lunes",
    dia_semana == "Tue" ~ "Martes",
    dia_semana == "Wed" ~ "Miércoles",
    dia_semana == "Thu" ~ "Jueves",
    dia_semana == "Fri" ~ "Viernes",
    dia_semana == "Sat" ~ "Sábado"
  ))

transporte %<>% 
  mutate(semana = stringi::stri_datetime_fields(fecha)$WeekOfMonth)

transporte <- select(transporte, -hora, -dia_semana)

# Fusionar los datos utilizando la columna "idsam" como clave de unión
datos_fusionados <- inner_join(transporte, rutas, by = "idrutaestacion")


######################
#Análisis de los Datos
#A disposición tenemos los datos del transporte (transporte), como así también los 
#fusionados con rutas (datos_fusionados)
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
  summarize(total = sum(montoevento), cantidad_viajes = n())

#Groupby por identidad: 2. MAS - 3. JAHA
datos_fusionados_i <- datos_fusionados %>%
  filter(tipoevento == 4) %>% 
  group_by(identidad) %>%
  summarize(total = sum(montoevento), cantidad_viajes = n())

#Groupby por tipo transporte: 1. Normal - 3. Diferencial
datos_fusionados_t <- datos_fusionados %>%
  filter(tipoevento == 4) %>% 
  group_by(tipotransporte, EOT, ramal) %>%
  summarize(total = sum(montoevento), cantidad_viajes = n()) %>%
  ungroup()


#Groupby por periodo:
#0-4 Madrugada / 4-8 Mañana / 8-12 Mediamañana / 12-16 Siesta / 16-20 Tarde / 20-00 Noche
datos_fusionados_p <- datos_fusionados %>%
  filter(tipoevento == 4) %>% 
  group_by(tipotransporte, periodo) %>%
  summarize(total = sum(montoevento), cantidad_viajes = n()) %>%
  ungroup()

#Groupby por dia de semana: Domingo, Lunes, Martes, Miércoles, Jueves, Viernes, Sábado
datos_fusionados_s <- datos_fusionados %>%
  filter(tipoevento == 4) %>% 
  group_by(tipotransporte, dia) %>%
  summarize(total = sum(montoevento), cantidad_viajes = n()) %>%
  ungroup()

#Groupby número de semana del mes
datos_fusionados_n <- datos_fusionados %>%
  filter(tipoevento == 4) %>% 
  group_by(tipotransporte, semana) %>%
  summarize(monto = sum(montoevento), cantidad_viajes = n()) %>%
  ungroup()


########################################
#Pre-steps
library(ggplot2)
library(RColorBrewer)
coul <- brewer.pal(5, "Set2") 

### Creacion de graficos #1 ###
datos_fusionados_p1 <- datos_fusionados_p %>% 
  select(-cantidad_viajes) %>%
  group_by(periodo) %>%
  summarize(total = sum(total) / 1000000) 

# Barplot
barplot(height=datos_fusionados_p1$total, 
        names=datos_fusionados_p1$periodo, 
        xlab="Periodos", 
        ylab="Total (en millones de Gs)", 
        main="Total de Ganancia en Millones", 
        ylim=c(0,7500),
        col=coul )

### Creacion de graficos #2 ###
# create a dataset
datos_fusionados_p2 <- datos_fusionados_p %>% 
  select(-cantidad_viajes) %>%
  mutate(total = total / 1000000) %>%
  mutate(tipotransporte = as.character(tipotransporte))

ggplot(datos_fusionados_p2, 
       aes(fill=tipotransporte, y=total, x=periodo)) + 
       geom_bar(position="dodge", stat="identity") + 
        labs(title = "Total de Ganancia en Millones - Periodo X Tipo de Transporte",
             x = "Periodo",
             y = "Total de Ganancia en Millones",
             fill = "Tipo de Trasporte")

### Creacion de graficos #3 ###
datos_fusionados_s1 <- datos_fusionados_s %>% 
  select(-cantidad_viajes) %>%
  group_by(dia) %>%
  summarize(total = sum(total) / 1000000) %>%
  ungroup()
  
  # Barplot
  barplot(height=datos_fusionados_s1$total, 
          names=datos_fusionados_s1$dia, 
          xlab="Dias", 
          ylab="Total (en millones de Gs)", 
          main="Total de Ganancia en Millones", 
          ylim=c(0,7500),
          col=coul )

### Creacion de graficos #4 ###
# create a dataset
datos_fusionados_s2 <- datos_fusionados_s %>% 
  select(-cantidad_viajes) %>%
  mutate(total = total / 1000000) %>%
  mutate(tipotransporte = as.character(tipotransporte)) %>%
  ungroup()

ggplot(datos_fusionados_s2, 
       aes(fill=tipotransporte, y=total, x=dia)) + 
  geom_bar(position="dodge", stat="identity") + 
  labs(title = "Total de Ganancia en Millones - Dia X Tipo de Transporte",
       x = "Dia de la Semana",
       y = "Total de Ganancia en Millones",
       fill = "Tipo de Trasporte")

########################################
# Cálculo de coeficiente de correlación:
library(ISLR)
transporte %>% 
  correlate() %>% 
  plot()

datos_fusionados %>% 
  correlate() %>% 
  plot()

correlate(datos_fusionados_p, tipotransporte, periodo, total, cantidad_viajes)
datos_fusionados_p %>% 
  correlate() %>% 
  plot()

correlate(datos_fusionados_s, tipotransporte, periodo, total, cantidad_viajes)
datos_fusionados_s %>% 
  correlate() %>% 
  plot()

correlate(datos_fusionados_n, tipotransporte, periodo, total, cantidad_viajes)
datos_fusionados_n %>% 
  correlate() %>% 
  plot()

#Agregar algun informe automatizado

#Cuarta unidad?
#Quizás Regresion.R
