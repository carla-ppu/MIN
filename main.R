setwd("C:/Users/carla/OneDrive/Escritorio/uni4/MIN/Trabajos")
#setwd(C:/Users/noe_2_000/Downloads/Rdef)
#setwd("C:/Users/Alvaro/Desktop/UNI/MIN/Definitivos")


rm(list=ls()) 

#CSV utilizados
datos_accidentes2<-read.csv("2017_Accidentalidad.csv", sep =";", stringsAsFactors = TRUE)
datos_accidentes<-read.csv("Listado_accidentes_2017_filtrado.csv", sep =";", stringsAsFactors = FALSE)
calendario<- read.csv("calendario_solo_2017.csv", sep = ";")

#archivos R
source("librerias.R")
source("limpieza.R")
source("calendarioConAccidentes.R")




