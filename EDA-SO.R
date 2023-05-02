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
if (!require('epitools'))
  install.packages("epitools")
library(epitools)

devtools::install_github("kassambara/ggpubr")
install.packages("ggpubr")
library(ggpubr)


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

#Groupby por dia de semana: Domingo, Lunes, Martes, Miércoles, Jueves, Viernes, Sábado; y Periodo del dia
datos_fusionados_d <- datos_fusionados %>%
  filter(tipoevento == 4) %>% 
  group_by(periodo, dia) %>%
  summarize(total = sum(montoevento), cantidad_viajes = n()) %>%
  ungroup()

#Groupby por EOT, para ranking de empresas
datos_fusionados_r <- datos_fusionados %>%
  filter(tipoevento == 4) %>% 
  group_by(EOT) %>%
  summarize(cantidad_viajes = n()) %>%
  arrange(desc(cantidad_viajes))  %>%
  ungroup()


################################GRAFICOS################################
#Pre-steps
library(ggplot2)
library(RColorBrewer)
library(ggthemes)
library(scales)
coul <- brewer.pal(7, "Set2")
coul2 <- brewer.pal(7, "Set3")

### Creacion de graficos - Grupo 1 - Por total de Ganancia ###

### Creacion de graficos #1.1 ###

datos_fusionados_p1 <- datos_fusionados_p %>% 
  select(-cantidad_viajes) %>%
  group_by(periodo) %>%
  summarize(total = sum(total) / 1000000) 

#Para graficar ordenado
datos_fusionados_p1$periodo <- factor(datos_fusionados_p1$periodo, levels=c("Madrugada", "Mañana", "Mediamañana", "Siesta", "Tarde", "Noche"))
datos_fusionados_p1 <- datos_fusionados_p1[order(datos_fusionados_p1$periodo),]

# Barplot
par(mfrow = c(1:2))
g11 <- barplot(height=datos_fusionados_p1$total, 
        names=datos_fusionados_p1$periodo,
        xlab="Periodos", 
        ylab="Total (en millones de Gs)", 
        main="Total de Ganancia en Millones", 
        ylim=c(0,7500),
        col=coul )

### Creacion de graficos #1.2 ###
datos_fusionados_s1 <- datos_fusionados_s %>% 
  select(-cantidad_viajes) %>%
  group_by(dia) %>%
  summarize(total = sum(total) / 1000000) %>%
  ungroup()

# Para graficar ordenado
datos_fusionados_s1$dia <- factor(datos_fusionados_s1$dia, levels=c("Domingo", "Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado"))
datos_fusionados_s1 <- datos_fusionados_s1[order(datos_fusionados_s1$dia),]

# Barplot
g12 <- barplot(height=datos_fusionados_s1$total, 
        names=datos_fusionados_s1$dia, 
        xlab="Dias", 
        ylab="Total (en millones de Gs)", 
        main="Total de Ganancia en Millones", 
        ylim=c(0,7500),
        col=coul )

### Creacion de graficos #1.3 ###
# create a dataset
datos_fusionados_p2 <- datos_fusionados_p %>% 
  select(-cantidad_viajes) %>%
  mutate(total = total / 1000000) %>%
  mutate(tipotransporte = as.character(tipotransporte))

# Para graficar ordenado
datos_fusionados_p2$periodo <- factor(datos_fusionados_p2$periodo, levels=c("Madrugada", "Mañana", "Mediamañana", "Siesta", "Tarde", "Noche"))
datos_fusionados_p2$tipotransporte <- factor(datos_fusionados_p2$tipotransporte, levels=c("1", "3"), labels = c("Normal", "Diferencial"))
datos_fusionados_p2 <- datos_fusionados_p2[order(datos_fusionados_p2$tipotransporte, datos_fusionados_p2$periodo),]

# Ggplot
g13 <- ggplot(datos_fusionados_p2, 
         aes(fill=tipotransporte, y=total, x=periodo)) + 
         geom_bar(position="dodge", stat="identity") + 
         labs(title = "Total de Ganancia en Millones - Periodo X Tipo de Transporte",
               x = "Periodo",
               y = "Total de Ganancia en Millones",
               fill = "Tipo de Trasporte")

### Creacion de graficos #1.4 ###
# create a dataset
datos_fusionados_s2 <- datos_fusionados_s %>% 
  select(-cantidad_viajes) %>%
  mutate(total = total / 1000000) %>%
  mutate(tipotransporte = as.character(tipotransporte)) %>%
  ungroup()

# Para graficar ordenado
datos_fusionados_s2$dia <- factor(datos_fusionados_s2$dia, levels=c("Domingo", "Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado"))
datos_fusionados_s2$tipotransporte <- factor(datos_fusionados_s2$tipotransporte, levels=c("1", "3"), labels = c("Normal", "Diferencial"))
datos_fusionados_s2 <- datos_fusionados_s2[order(datos_fusionados_s2$tipotransporte, datos_fusionados_s2$dia),]

# Ggplot
g14 <- ggplot(datos_fusionados_s2, 
         aes(fill=tipotransporte, y=total, x=dia)) + 
         geom_bar(position="dodge", stat="identity") + 
         labs(title = "Total de Ganancia en Millones - Dia X Tipo de Transporte",
         x = "Dia de la Semana",
         y = "Total de Ganancia en Millones",
         fill = "Tipo de Trasporte")

### Creacion de graficos #1.5 ###
# Para ordenar
datos_fusionados_d$dia <- factor(datos_fusionados_d$dia, levels=c("Domingo", "Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado"))
datos_fusionados_d$periodo <- factor(datos_fusionados_d$periodo, levels=c("Madrugada", "Mañana", "Mediamañana", "Siesta", "Tarde", "Noche"))

# create a dataset
datos_fusionados_d1 <- datos_fusionados_d %>% 
  select(-cantidad_viajes) %>%
  mutate(total = total / 1000000) %>%
  arrange(dia, periodo) %>%
  ungroup()

# Ggplot
g15 <- ggplot(datos_fusionados_d1, 
       aes(fill=periodo, y=total, x=dia)) + 
       geom_bar(position="dodge", stat="identity") + 
       labs(title = "Total de Ganancia en Millones - Dia de Semana X Periodo del Dia",
         x = "Dia de la Semana",
         y = "Total de Ganancia en Millones",
         fill = "Periodo del Dia")

# Agrupando 3 ggplots y guardamos
arrange <- ggarrange(g13, g14, g15 + rremove("x.text"), 
          labels = c("A", "B", "C"),
          ncol = 1, nrow = 3)
ggsave("output/g13-g14-g15.png", arrange, width = 6, height = 4)

### Creacion de graficos - Grupo 2 - Por total de Eventos ###
### Creacion de graficos #2.1 ###

datos_fusionados_p3 <- datos_fusionados_p %>% 
  select(-total) %>%
  group_by(periodo) %>%
  summarize(cantidad_viajes = sum(cantidad_viajes)) 

#Para graficar ordenado
datos_fusionados_p3$periodo <- factor(datos_fusionados_p3$periodo, levels=c("Madrugada", "Mañana", "Mediamañana", "Siesta", "Tarde", "Noche"))
datos_fusionados_p3 <- datos_fusionados_p3[order(datos_fusionados_p3$periodo),]

# Barplot
par(mfrow = c(1:2))
g21 <- barplot(height=datos_fusionados_p3$cantidad_viajes, 
               names=datos_fusionados_p3$periodo,
               xlab="Periodos del Día", 
               ylab="Total de Viajes", 
               main="Total de Viajes por Periodo", 
               ylim=c(0,2500000),
               col=coul2 )

### Creacion de graficos #2.2 ###
datos_fusionados_s3 <- datos_fusionados_s %>% 
  select(-total) %>%
  group_by(dia) %>%
  summarize(cantidad_viajes = sum(cantidad_viajes)) %>%
  ungroup()

# Para graficar ordenado
datos_fusionados_s3$dia <- factor(datos_fusionados_s3$dia, levels=c("Domingo", "Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado"))
datos_fusionados_s3 <- datos_fusionados_s3[order(datos_fusionados_s3$dia),]

# Barplot
g22 <- barplot(height=datos_fusionados_s3$cantidad_viajes, 
               names=datos_fusionados_s3$dia, 
               xlab="Dias", 
               ylab="Total de Viajes", 
               main="Total de Viajes por Día", 
               ylim=c(0,2500000),
               srt=45,
               col=coul2 ) +
              theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

### Creacion de graficos #2.3 ###
# create a dataset
datos_fusionados_p4 <- datos_fusionados_p %>% 
  select(-total) %>%
  mutate(tipotransporte = as.character(tipotransporte))

# Para graficar ordenado
datos_fusionados_p4$periodo <- factor(datos_fusionados_p4$periodo, levels=c("Madrugada", "Mañana", "Mediamañana", "Siesta", "Tarde", "Noche"))
datos_fusionados_p4$tipotransporte <- factor(datos_fusionados_p4$tipotransporte, levels=c("1", "3"), labels = c("Normal", "Diferencial"))
datos_fusionados_p4 <- datos_fusionados_p4[order(datos_fusionados_p4$tipotransporte, datos_fusionados_p4$periodo),]

# Ggplot
g23 <- ggplot(datos_fusionados_p4, 
              aes(fill=tipotransporte, y=cantidad_viajes, x=periodo)) + 
              scale_fill_brewer(palette = "Pastel2") +
              geom_bar(position="dodge", stat="identity") + 
              labs(title = "Total de Viajes - Periodo X Tipo de Transporte",
                   x = "Periodo",
                   y = "Total Viajes",
                   fill = "Tipo de Trasporte") 

### Creacion de graficos #2.4 ###
# create a dataset
datos_fusionados_s4 <- datos_fusionados_s %>% 
  select(-total) %>%
  mutate(cantidad_viajes = cantidad_viajes / 1000) %>%
  mutate(tipotransporte = as.character(tipotransporte)) %>%
  ungroup()

# Para graficar ordenado
datos_fusionados_s4$dia <- factor(datos_fusionados_s4$dia, levels=c("Domingo", "Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado"))
datos_fusionados_s4$tipotransporte <- factor(datos_fusionados_s4$tipotransporte, levels=c("1", "3"), labels = c("Normal", "Diferencial"))
datos_fusionados_s4 <- datos_fusionados_s4[order(datos_fusionados_s4$tipotransporte, datos_fusionados_s4$dia),]

# Ggplot
g24 <- ggplot(datos_fusionados_s4, 
              aes(fill=tipotransporte, y=cantidad_viajes, x=dia)) + 
              scale_fill_brewer(palette = "Pastel2") +
              geom_bar(position="dodge", stat="identity") + 
              labs(title = "Total Viajes - Dia X Tipo de Transporte",
                   x = "Dia de la Semana",
                   y = "Total de Viajes (en miles)",
                   fill = "Tipo de Trasporte")

### Creacion de graficos #2.5 ###
# Para ordenar
datos_fusionados_d$dia <- factor(datos_fusionados_d$dia, levels=c("Domingo", "Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado"))
datos_fusionados_d$periodo <- factor(datos_fusionados_d$periodo, levels=c("Madrugada", "Mañana", "Mediamañana", "Siesta", "Tarde", "Noche"))

# create a dataset
datos_fusionados_d2 <- datos_fusionados_d %>% 
  select(-total) %>%
  mutate(cantidad_viajes = cantidad_viajes / 1000) %>%
  arrange(dia, periodo) %>%
  ungroup()

# Ggplot
g25 <- ggplot(datos_fusionados_d2, 
              aes(fill=periodo, y=cantidad_viajes, x=dia)) + 
              geom_bar(position="dodge", stat="identity") + 
              scale_fill_brewer(palette = "Pastel2") +
              labs(title = "Total de Viajes - Dia de Semana X Periodo del Dia",
                   x = "Dia de la Semana",
                   y = "Total de Viajes",
                   fill = "Periodo del Dia") 

# Agrupando 3 ggplots
arrange2 <- ggarrange(g23, g24, g25, 
          labels = c("X", "Y", "Z"),
          ncol = 1, nrow = 3)
ggsave("output/g23-g24-g25.png", arrange2, width = 6, height = 4)

### Creacion de graficos - Grupo 3 

### Creacion de graficos #3.1 - Viajes por Fecha###
transporte_pasajes <- datos_fusionados %>%
  filter(tipoevento == 4, producto == "MO") %>%
  group_by(fecha) %>%
  summarize(total_monto = sum(montoevento)) %>%
  ungroup()

g31 <-  ggplot(transporte_pasajes, aes(x = fecha, y = total_monto)) +
        geom_area(linetype = 3, fill="#69b3a2", alpha=0.4,
            lwd = 1.1, color = "steelblue",
            alpha = 0.6,
            size = 0.6) + 
            geom_point(size=1) +
            labs(title = "Total de Viajes por fecha",
                 x = "Fecha", y = "Monto Total Facturado",
                 fill = "Monto en Guaranies") +
            geom_vline(xintercept = as.numeric(as.Date("2023-03-05")), linetype=2, color = "red") +
            geom_vline(xintercept = as.numeric(as.Date("2023-03-12")), linetype=2, color = "red") +
            geom_vline(xintercept = as.numeric(as.Date("2023-03-19")), linetype=2, color = "red") +
            geom_vline(xintercept = as.numeric(as.Date("2023-03-26")), linetype=2, color = "red") +
            scale_y_continuous(labels = scales::comma) +
            scale_x_date(date_breaks = "1 day", date_minor_breaks = "1 day",
                 date_labels = "%b %d") +
            guides(x = guide_axis(angle = 45)) +
            theme_minimal()
ggsave("output/g31.png", plot = g31)
  
### Creacion de graficos #3.2 - Viajes por Empresas###
par(mar=c(12,6,4,3))
g32 <-  barplot(height=datos_fusionados_r$cantidad_viajes, 
          names=datos_fusionados_r$EOT, 
          main="Total de Viajes por Empresa", 
          ylim=c(0,1200000),
          srt=45,
          las=2, cex.names=0.7, cex.axis=0.7, 
          col=brewer.pal(30, name="Spectral") ) + 
          mtext('Empresas', 1, 10) + mtext('Total de Viajes', 2, 4)
  
  
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


########################################
#Variables categóricas
#Contraste de hipotesis con variables de resultado categóricas

#Instalamos los paquetes adicionales necesarios
if (!require('finalfit'))
  install.packages("finalfit")
library(finalfit)
if (!require('rio'))
  install.packages("rio")
library(rio)
if (!require('broom'))
  install.packages("broom")
library(broom)

# Los datos continuos se pueden medir,  
# mientras que los datos categóricos se pueden contar

#Transformación de datos (recodificar)
transporte <- transporte %>% 
  mutate(identidad.factor =             # Make new variable  
           identidad %>%                # from existing variable
           factor() %>%           # convert to factor
           fct_recode(            # forcats function
             "MAS" = "2",      # new on left, old on right
             "JAHA"   = "3") %>% 
           ff_label("Identidad"),       # Optional label for finalfit
         
         # same thing but more condensed code:
         tipotransporte.factor = factor(tipotransporte) %>% 
           fct_recode("Normal" = "1",
                      "Diferencial"  = "3") %>% 
           ff_label("Tipo transporte"),
         
         periodo.factor = factor(periodo) %>% 
           ff_label("Periodo"),
         
         dia.factor = factor(dia) %>% 
           ff_label("Dia"),
         
         semana.factor = factor(semana) %>% 
           ff_label("Semana"))

transporte$identidad.factor %>% levels()
transporte$tipotransporte.factor %>% levels()
transporte$periodo.factor %>% levels()
transporte$dia.factor %>% levels()
transporte$semana.factor %>% levels()

# Visualización de datos ####
# contamos el número que tomaron diferencial con cada identidad
p1 <- transporte %>% 
  ggplot(aes(x = identidad.factor, fill = tipotransporte.factor)) + 
  geom_bar() + 
  theme(legend.position = "none")

p2 <- transporte %>% 
  ggplot(aes(x = identidad.factor, fill = tipotransporte.factor)) + 
  geom_bar(position = "fill") + 
  ylab("proportion")

library(patchwork)
p1 + p2

# Este orden de esta variable en particular es inusual;
# Una opción rápida es simplemente invertir el orden de los 
# niveles en la trama.
p1 <- transporte %>% 
  ggplot(aes(x = identidad.factor, fill = tipotransporte.factor)) + 
  geom_bar(position = position_stack(reverse = TRUE)) + 
  theme(legend.position = "none")

p2 <- transporte %>% 
  ggplot(aes(x = identidad.factor, fill = tipotransporte.factor)) + 
  geom_bar(position = position_fill(reverse = TRUE)) + 
  ylab("proportion")

library(patchwork)
p1 + p2

###Resumir factores con finalfit ####
transporte %>% 
  summary_factorlist(dependent   = "tipotransporte.factor", 
                     explanatory = "identidad.factor")
s_factorlist = transporte %>% 
  summary_factorlist(dependent = "tipotransporte.factor", 
                     explanatory = 
                       c("periodo.factor", "dia.factor", "semana.factor")
  )

as.data.frame(s_factorlist)
export(as.data.frame(s_factorlist),"s_factorlist.xlsx")

# Chi-cuadrado de Pearson y pruebas exactas de Fisher ####

# Chi-cuadrado de Pearson  ####
table(transporte$identidad.factor, transporte$tipotransporte.factor) # both give same result
with(transporte, table(identidad.factor, tipotransporte.factor))

plot(table(transporte$identidad.factor, transporte$tipotransporte.factor))

# La tabla de conteos se puede pasar a prop.table() para proporciones.
transporte %$%
  table(identidad.factor, tipotransporte.factor) %>% 
  prop.table(margin = 1)     # 1: row, 2: column etc.

# se puede pasar la tabla de conteos a chisq.test() 
# para realizar la prueba de chi-cuadrado.
transporte %$%        # note $ sign here
  table(identidad.factor, tipotransporte.factor) %>% 
  chisq.test()

# Exportar los resultados en el directorio de trabajo (a fichero)
transporte %$%        # note $ sign here
  table(identidad.factor, tipotransporte.factor) %>% 
  chisq.test() %>% 
  tidy() %>% export(.,"chisq_test.xlsx")

# cuando mayor es el valor de chi cuadrado mayor es la diferencia entre lo 
# esperado y lo observado (Es una métrica que informa sobre la bondad de ajuste)

# La corrección implica restar 0,5 de la diferencia absoluta
# entre cada valor observado y esperado. Esto es particularmente útil 
# cuando los recuentos son bajos, pero puede eliminarse 
# si lo desea chisq.test(..., correct = FALSE).

# Prueba exacta de Fisher ####

# Una suposición comúnmente declarada de la prueba de chi-cuadrado 
# es el requisito de tener un recuento esperado de al menos 5 
# en cada celda de la tabla 2x2. 

# Para tablas más grandes, todos los  recuentos esperados deben ser 
# y no más del 20 % de todas las celdas deben tener recuentos esperados. 
# Si no se cumple este supuesto, una prueba alternativa es 
# la prueba exacta de Fisher.
transporte %$%        # note $ sign here
  table(periodo.factor, tipotransporte.factor)

transporte %$%        # note $ sign here
  table(periodo.factor, tipotransporte.factor) %>% 
  chisq.test()

# por la prueba exacta de Fisher
transporte %$%        # note $ sign here
  table(periodo.factor, tipotransporte.factor) %>% 
  fisher.test(simulate.p.value = TRUE)

# Chi-cuadrado / Prueba exacta de Fisher usando finalfit ####
transporte %>% 
  summary_factorlist(dependent   = "tipotransporte.factor", 
                     explanatory = "identidad.factor",
                     p = TRUE)

# más variables:
transporte %>% 
  summary_factorlist(dependent = "tipotransporte.factor", 
                     explanatory = 
                       c("periodo.factor", "dia.factor", "semana.factor"),
                     p = TRUE)

# por prueba exacta de Fisher:
transporte %>% 
  summary_factorlist(dependent = "tipotransporte.factor", 
                     explanatory = 
                       c("periodo.factor", "dia.factor", "semana.factor"),
                     p = TRUE,
                     p_cat = "fisher"
  )

# Múltiples variables por resultado con pruebas de hipótesis: 
# Incluir otros argumentos 
transporte %>% 
  summary_factorlist(dependent = "tipotransporte.factor", 
                     explanatory = 
                       c("periodo.factor", "dia.factor", "semana.factor"),
                     p = TRUE,
                     p_cat = "fisher",
                     digits = 
                       c(1, 1, 4, 2) #1: mean/median, 2: SD/IQR 
                     # 3: p-value, 4: count percentage
  )
