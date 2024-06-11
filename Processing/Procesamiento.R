#El Impacto de la Violencia Vicaria y Entornos Familiares Violentos en 
                      #Niños y Adolescentes en Chile 

# Los efectos percibidos por niños/as y adolescentes resultantes de la exposicion 
# a entornos violentos, especificamente desde una revision bibliografica referente
# a la violencia vicaria, suelen manifestarse en tres categorias: 
# sintomas internalizantes, externalizantes y psicosomaticos.La presente investigacion
# ¿En qué medida la violencia vicaria y la exposición violencia familiar afectan el 
# bienestar emocional de niños, niñas y adolescentes en Chile? Si bien, las definiciones
# de los sintomas son referentes a VV, tambien puede ser utilizado para comprender
# la totalidad de un espacio familiar violento.
# Para definir los conceptos cuantitativamente, se operacionalizara la VV a partir de indicadores
# indirectos y los Entornos Familiares Violentos (EFV), a partir de indicadores mixtos. 
# La creacion de los datos se realizara a partir de la BBDD de la "Segunda Encuesta Nacional 
# de Polivictimizacion de 2022-2023, la cual, se ha caracterizado en la comprension profunda 
# de algunas experiencias de victimización que sufren los NNA desde una perspectiva integral: 
# entorno escolar, familiar y social.
# El sondeo realizado por DESUC, está dirigido a NNA escolarizados de 
# 7º básico a 3º medio (12 a 17 años) de establecimientos educacionales de las 16 regiones del país, 
# de todas las dependencias administrativas. Mide ocho tipos de victimizaciones 
# que pueden ocurrir en forma separada, pero que en algunos casos también pueden 
# ocurrir con diversos tipos de victimización integrados y acumulados progresivamente 
# en el tiempo (polivictimización)
# 
# Sobre manipulacion de datos y variables utilizadas: 
# Experiencia de Violencia (Entornos Violentos)
# Prevalencia de vida (Alguna vez en tu vida)
###  PA_4   PA_6    PA_7    PB_1    PB_2    PB_3   PD_3, PD_6,  PE_2     PE_4    PE_5    PE_6    PE_7
# Prevalencia en el último año (Durante los últimos 12 meses)}
### PA_4_1  PA_6_1  PA_7_1  PB_1_1  PB_2_1  PB_3_1 PD_3_1, PD_6_1,   PE_2_1  PE_4_1  PE_5_1  PE_6_1  PE_7_1
# Frecuencia en el último año (Número de veces)
### PA_4_2  PA_6_2  PA_7_2  PB_1_2  PB_2_2  PB_3_2 PD_3_2, PD_6_2,    PE_2_2  PE_4_2  PE_5_2  PE_6_2  PE_7_2
# Lugar del incidente (Dónde ocurrió)  
### PA_4_3  PA_6_3  PA_7_3  PB_1_3  PB_2_3         PD_3_3, PD_6_3,    PE_2_3  PE_4_3  PE_5_3   PE_6_3  PE_7_3
# Cercania (vive contigo, no vive contigo o fue un extraño)
### PA_4_4  PA_6_4  PA_7_4  PB_1_4  PB_2_4         PD_3_4, PD_6_4,                            PE_6_4  PE_7_4
# Perpetrador del acto (Quién lo hizo)
### PA_4_5  PA_6_5  PA_7_5                         PD_3_5,  PD_6_5
 
# Experiencia de violencia (VV)
# Prevalencia de vida (Alguna vez en tu vida)
### (custodia) PB_4   
# Prevalencia en el último año (Durante los últimos 12 meses)}
### (custodia)PB_4_1  
# Frecuencia en el último año (Número de veces)
### (custodia)PB_4_2  

# Variables de bienestar emocional
#Síntomas Internalizantes: problemas emocionales y de comportamiento que se dirigen 
#hacia el interior individuo, como la depresión y la ansiedad.
#PG_3  PG_9 PH_2 PH_10  PH_17 

#Los síntomas externalizantes:conductas dirigidas hacia el exterior, 
#como la agresión, la hiperactividad y los problemas de conducta.
#PH_3  PH_12  PH_18  

#Síntomas Psicosomáticos: manifestaciones físicas de trastornos psicológicos,
#como dolores y problemas digestivos.
#PH_4  PH_6 PH_14   PH_16 PH_15 


#Preparación de datos ENCUESTA DE POLIVICTIMIZACION EN NNA 2022-2023
#Librerías principales (de R) a utilizar en el análisis

install.packages("pacman")
pacman::p_load(dplyr, sjmisc, car, sjlabelled, stargazer, haven)
pacman::p_load(haven)
#Cargar base de datos

rm(list=ls())       # borrar todos los objetos en el espacio de trabajo
options(scipen=999) # valores sin notación científica

# Leer el archivo
library(haven)

polivictima <- read_sav("Input/data-orig/ENP.sav", encoding = "UTF-8")

dim(polivictima) # dimension de la base
View(polivictima)

# Seleccionar las variables de interés
library(dplyr)

# Seleccionar las variables de interés
proc_data <- polivictima %>% select(
  # Prevalencia de vida (Alguna vez en tu vida)
  PA_4, PA_6, PA_7, PB_1, PB_2, PB_3, PD_3, PD_6,PE_2, PE_4, PE_5, PE_6, PE_7,
  
  # Prevalencia en el último año (Durante los últimos 12 meses)
  PA_4_1, PA_6_1, PA_7_1, PB_1_1, PB_2_1, PB_3_1, PD_3_1,  PD_6_1, PE_2_1, PE_4_1, PE_5_1, PE_6_1, PE_7_1,
  
  # Frecuencia en el último año (Número de veces)
  PA_4_2, PA_6_2, PA_7_2, PB_1_2, PB_2_2, PB_3_2, PD_3_2,  PD_6_2,PE_2_2, PE_4_2, PE_5_2, PE_6_2, PE_7_2,
  
  # Lugar del incidente (Dónde ocurrió)  
  PA_4_3, PA_6_3, PA_7_3, PB_1_3, PB_2_3, PD_3_3, PD_6_3, PE_2_3, PE_4_3, PE_5_3, PE_6_3, PE_7_3,
  
  # Cercania (vive contigo, no vive contigo o fue un extraño)
  PA_4_4, PA_6_4, PA_7_4, PB_1_4, PB_2_4, PD_3_4,  PD_6_4,PE_6_4, PE_7_4,
  
  # Perpetrador del acto (Quién lo hizo)
  PA_4_5, PA_6_5, PA_7_5, PD_3_5,  PD_6_5,

  # Prevalencia de vida VV (Alguna vez en tu vida)
  PB_4,
  
  # Prevalencia en el último año VV(Durante los últimos 12 meses)
  PB_4_1,
  
  # Frecuencia en el último añoVV (Número de veces)
  PB_4_2,
    # Síntomas Internalizantes
    PG_3, PG_9, PH_2, PH_10, PH_17,
    
    # Síntomas Externalizantes
    PH_3, PH_12, PH_18,
    
    # Síntomas Psicosomáticos
    PH_4, PH_6, PH_14, PH_16, PH_15)
  
  # Comprobar
  names(proc_data)

sjlabelled::get_label(proc_data)

#Procesamiento de variables
#Descriptivo
library(sjmisc)

# Prevalencia de vida (Alguna vez en tu vida)
frq(proc_data$PA_4)
frq(proc_data$PA_6)
frq(proc_data$PA_7)
frq(proc_data$PB_1)
frq(proc_data$PB_2)
frq(proc_data$PB_3)
frq(proc_data$PD_3)
frq(proc_data$PE_2)
frq(proc_data$PE_4)
frq(proc_data$PE_5)
frq(proc_data$PE_6)
frq(proc_data$PE_7)

# Prevalencia en el último año (Durante los últimos 12 meses)
frq(proc_data$PA_4_1)
frq(proc_data$PA_6_1)
frq(proc_data$PA_7_1)
frq(proc_data$PB_1_1)
frq(proc_data$PB_2_1)
frq(proc_data$PB_3_1)
frq(proc_data$PD_3_1)
frq(proc_data$PE_2_1)
frq(proc_data$PE_4_1)
frq(proc_data$PE_5_1)
frq(proc_data$PE_6_1)
frq(proc_data$PE_7_1)

# Frecuencia en el último año (Número de veces)
frq(proc_data$PA_4_2)
frq(proc_data$PA_6_2)
frq(proc_data$PA_7_2)
frq(proc_data$PB_1_2)
frq(proc_data$PB_2_2)
frq(proc_data$PB_3_2)
frq(proc_data$PD_3_2)
frq(proc_data$PE_2_2)
frq(proc_data$PE_4_2)
frq(proc_data$PE_5_2)
frq(proc_data$PE_6_2)
frq(proc_data$PE_7_2)

# Lugar del incidente (Dónde ocurrió)
frq(proc_data$PA_4_3)
frq(proc_data$PA_6_3)
frq(proc_data$PA_7_3)
frq(proc_data$PB_1_3)
frq(proc_data$PB_2_3)
frq(proc_data$PD_3_3)
frq(proc_data$PE_2_3)
frq(proc_data$PE_4_3)
frq(proc_data$PE_5_3)
frq(proc_data$PE_6_3)
frq(proc_data$PE_7_3)

# Cercania (vive contigo, no vive contigo o fue un extraño)
frq(proc_data$PA_4_4)
frq(proc_data$PA_6_4)
frq(proc_data$PA_7_4)
frq(proc_data$PB_1_4)
frq(proc_data$PB_2_4)
frq(proc_data$PD_3_4)
frq(proc_data$PE_6_4)
frq(proc_data$PE_7_4)

# Perpetrador del acto (Quién lo hizo)
frq(proc_data$PA_4_5)
frq(proc_data$PA_6_5)
frq(proc_data$PA_7_5)
frq(proc_data$PD_3_5)


# Prevalencia de vida VV (Alguna vez en tu vida)
frq(proc_data$PB_4)
frq(proc_data$PD_6)


# Prevalencia en el último año VV (Durante los últimos 12 meses)
frq(proc_data$PB_4_1)
frq(proc_data$PD_6_1)

# Frecuencia en el último año VV(Número de veces)
frq(proc_data$PB_4_2)
frq(proc_data$PD_6_2)


# Lugar del incidenteVV(Dónde ocurrió)
frq(proc_data$PD_6_3)


# Cercania VV(vive contigo, no vive contigo o fue un extraño)
frq(proc_data$PD_6_4)


# DenunciaVV
frq(proc_data$PD_6_5)


# Síntomas Internalizantes
frq(proc_data$PG_3)
frq(proc_data$PG_9)
frq(proc_data$PH_2)
frq(proc_data$PH_10)
frq(proc_data$PH_17)

# Síntomas Externalizantes
frq(proc_data$PH_3)
frq(proc_data$PH_12)
frq(proc_data$PH_18)

# Síntomas Psicosomáticos
frq(proc_data$PH_4)
frq(proc_data$PH_6)
frq(proc_data$PH_14)
frq(proc_data$PH_16)
frq(proc_data$PH_15)


#No es necesario RECODIfiCAR
#b. nombre REFERENTE A ENTORNO FAMILIAR VIOLENTO
proc_data <- proc_data %>% 
  rename(
    "prev_vida" = c(PA_4, PA_6, PA_7, PB_1, PB_2, PB_3, PD_3, PD_6, PE_2, PE_4, PE_5, PE_6, PE_7),
    "prev_ultimo_anio" = c(PA_4_1, PA_6_1, PA_7_1, PB_1_1, PB_2_1, PB_3_1, PD_3_1, PD_6_1, PE_2_1, PE_4_1, PE_5_1, PE_6_1, PE_7_1),
    "frecuencia_ultimo_anio" = c(PA_4_2, PA_6_2, PA_7_2, PB_1_2, PB_2_2, PB_3_2, PD_3_2,  PD_6_2, PE_2_2, PE_4_2, PE_5_2, PE_6_2, PE_7_2),
    "lugar_incidente" = c(PA_4_3, PA_6_3, PA_7_3, PB_1_3, PB_2_3, PD_3_3, PD_6_3, PE_2_3, PE_4_3, PE_5_3, PE_6_3, PE_7_3),
    "cercania" = c(PA_4_4, PA_6_4, PA_7_4, PB_1_4, PB_2_4, PD_3_4, PD_6_4, PE_6_4, PE_7_4),
    "perpetrador" = c(PA_4_5, PA_6_5, PA_7_5, PD_3_5, PD_6_5))
proc_data <- proc_data %>% 
  rename(
    # Prevalencia de vida (Alguna vez en tu vida)
    "prev_vida_VV_custodia" = PB_4,
    
    # Prevalencia en el último año (Durante los últimos 12 meses)
    "prev_ultimo_anio_VV_custodia" = PB_4_1,

    # Frecuencia en el último año (Número de veces)
    "frec_ultimo_anio_VV_custodia" = PB_4_2)
proc_data <- proc_data %>% 
  rename(
    # Síntomas Internalizantes
    "internalizantes" = c(PG_3,PG_9,PH_2,PH_10,PH_17),
    
    # Síntomas Externalizantes
    "externalizantes" = c(PH_3,PH_12,PH_18),
    
    # Síntomas Psicosomáticos
    "psicosomaticos" = c(PH_4,PH_6,PH_14,PH_16,PH_15))

#Etiquetado
install.packages("labelled")

library(labelled)
# Etiqueta para "prev_vida_VV_custodia"
proc_data$prev_vida_VV_custodia <- set_label(x = proc_data$prev_vida_VV_custodia, label = "Separación Familiar: Vida")

# Etiqueta para "prev_ultimo_anio_VV_custodia"
proc_data$prev_ultimo_anio_VV_custodia <- set_label(x = proc_data$prev_ultimo_anio_VV_custodia, label = "Separación Familiar: Año")

# Etiqueta para "frec_ultimo_anio_VV_custodia"
proc_data$frec_ultimo_anio_VV_custodia <- set_label(x = proc_data$frec_ultimo_anio_VV_custodia, label = "Separación Familiar: Frecuencia")

#SUMATORIA y ETIQUETADO variables entorno familiar y bienestar emocional.

proc_data$prev_vida <- rowSums(proc_data[, paste0("prev_vida", 1:13)])
summary(proc_data$prev_vida)
proc_data$prev_vida  <- set_label(x = proc_data$prev_vida, label = "Violencia Familiar: vida")

proc_data$prev_ultimo_anio <- rowSums(proc_data[, paste0("prev_ultimo_anio", 1:13)])
summary(proc_data$prev_ultimo_anio)
proc_data$prev_ultimo_anio  <- set_label(x = proc_data$prev_ultimo_anio, label = "Violencia Familiar: Año")

proc_data$frecuencia_ultimo_anio <- rowSums(proc_data[, paste0("frecuencia_ultimo_anio", 1:13)])
summary(proc_data$frecuencia_ultimo_anio)
proc_data$frecuencia_ultimo_anio  <- set_label(x = proc_data$frecuencia_ultimo_anio, label = "Violencia Familiar: Frecuencia")

proc_data$lugar_incidente <- rowSums(proc_data[, paste0("lugar_incidente", 1:12)])
summary(proc_data$lugar_incidente)
proc_data$lugar_incidente  <- set_label(x = proc_data$lugar_incidente, label = "Violencia Familiar: ¿Dónde?")

proc_data$cercania <- rowSums(proc_data[, paste0("cercania", 1:9)])
summary(proc_data$cercania)
proc_data$cercania  <- set_label(x = proc_data$cercania, label = "Violencia Familiar: Cercania")

proc_data$perpetrador <- rowSums(proc_data[, paste0("perpetrador", 1:5)])
summary(proc_data$perpetrador)
proc_data$perpetrador  <- set_label(x = proc_data$perpetrador, label = "Violencia Familiar: ¿Quién?")

proc_data$internalizantes <- rowSums(proc_data[, paste0("internalizantes", 1:5)])
summary(proc_data$internalizantes)
proc_data$internalizantes  <- set_label(x = proc_data$internalizantes, label = "Síntomas: internalizantes")

proc_data$externalizantes <- rowSums(proc_data[, paste0("externalizantes", 1:3)])
summary(proc_data$externalizantes)
proc_data$externalizantes  <- set_label(x = proc_data$externalizantes, label = "Síntomas: externalizantes")

proc_data$psicosomaticos <- rowSums(proc_data[, paste0("psicosomaticos", 1:5)])
summary(proc_data$psicosomaticos)
proc_data$psicosomaticos  <- set_label(x = proc_data$psicosomaticos, label = "Sintomas: psicosomaticos")

frq(proc_data$internalizantes1)



library(labelled)

for (i in 1:13) {
  var_name <- paste0("prev_vida", i)
  proc_data[[var_name]] <- set_labels(proc_data[[var_name]],
                                      labels = c("Sí" = 1, "No" = 2))}

for (i in 1:13) {
  var_name <- paste0("prev_ultimo_anio", i)
  proc_data[[var_name]] <- set_labels(proc_data[[var_name]],
                                      labels = c("Sí" = 1, "No" = 2))}

for (i in 1:13) {
  var_name <- paste0("frecuencia_ultimo_anio", i)
  proc_data[[var_name]] <- set_labels(proc_data[[var_name]],
                                      labels = c("Nunca" = 1,
                                                 "1 vez" = 2,
                                                 "2 o 3 veces" = 3,
                                                 "Al menos una vez al 
                                                 mes" = 4,
                                                 "Al menos una vez a la 
                                                 semana" = 5,
                                                 "Todos los días" = 6))}

for (i in 1:12) {
  var_name <- paste0("lugar_incidente", i)
  proc_data[[var_name]] <- set_labels(proc_data[[var_name]],
                                      labels = c("Tu casa" = 1,
"Colegio" =2,
"Cerca de tu casa, en tu 
barrio" = 3,
"Lejos de tu casa, en 
algún lugar o en la 
calle" = 4))}

for (i in 1:9) {
  var_name <- paste0("cercania", i)
  proc_data[[var_name]] <- set_labels(proc_data[[var_name]],
                                      labels = c("Alguien cercano, que 
                                                 vive contigo" = 1,
                                                 "Alguien cercano pero 
                                                 que no vive contigo" = 2,
                                                 "Un extraño" = 3))}

for (i in 1:5) {
  var_name <- paste0("perpetrador", i)
  proc_data[[var_name]] <- set_labels(proc_data[[var_name]],
                                      labels = c("Un adulto" = 1,
                                                 "Una adulta" = 2,
                                                 "Un adolescente o niño" = 3,
                                                 "Una adolescente o niña" = 4))}

proc_data$prev_vida_VV_custodia <- set_labels(proc_data$prev_vida_VV_custodia,
                                   labels=c( "Sí"=1,
                                             "No"=2))

proc_data$frec_ultimo_anio_VV_custodia <- set_labels(proc_data$frec_ultimo_anio_VV_custodia,
                                   labels=c("Nunca" = 1,
                                              "1 vez" = 2,
                                              "2 o 3 veces" = 3,
                                              "Al menos una vez al 
                                                 mes" = 4,
                                              "Al menos una vez a la 
                                                 semana" = 5,
                                              "Todos los días" = 6))

#Recodificar bienestar emocional
install.packages("dplyr")
library(dplyr)
library(haven)

# Recodificar las variables de haven_labelled
proc_data$internalizantes1 <- haven::as_factor(proc_data$internalizantes1)
proc_data$internalizantes1 <- car::recode(proc_data$internalizantes1, "'1'=1; '2'=1; '3'=2; '4'=3; '5'=3")

frq(proc_data$internalizantes1)




proc_data$conf_jud <- set_labels(proc_data$conf_jud,
                                 labels=c( "Ninguna"=0,
                                           "Poca"=1,
                                           "Algo"=2,
                                           "Mucha"=3))

proc_data$conf_partpol <- set_labels(proc_data$conf_partpol,
                                     labels=c( "Ninguna"=0,
                                               "Poca"=1,
                                               "Algo"=2,
                                               "Mucha"=3))