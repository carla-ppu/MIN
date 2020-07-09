#Limpieza de listado_accidentes_2017.csv
datos_accidentes$Fecha[is.na(datos_accidentes$Fecha)] = 0
datos_accidentes$Fecha<-as.Date(datos_accidentes$Fecha,"%d/%m/%Y")
datos_accidentes$Fecha
sum(is.na(datos_accidentes$Fecha))
datos_accidentes$Hora[is.na(datos_accidentes$Hora)] = 0
datos_accidentes$Dia[datos_accidentes$Dia == "Lunes" | datos_accidentes$Dia == "lunes"] = 1
datos_accidentes$Dia[datos_accidentes$Dia == "Martes" | datos_accidentes$Dia == "martes"] = 2
datos_accidentes$Dia[datos_accidentes$Dia == "Miércoles" | datos_accidentes$Dia == "miércoles"] = 3
datos_accidentes$Dia[datos_accidentes$Dia == "Jueves" | datos_accidentes$Dia == "jueves"] = 4
datos_accidentes$Dia[datos_accidentes$Dia == "Viernes" | datos_accidentes$Dia == "viernes"] = 5
datos_accidentes$Dia[datos_accidentes$Dia == "Sábado" | datos_accidentes$Dia == "sábado"] = 6
datos_accidentes$Dia[datos_accidentes$Dia == "Domingo" | datos_accidentes$Dia == "domingo"] = 7
datos_accidentes$Dia<-as.numeric(datos_accidentes$Dia)
class(datos_accidentes$Dia)
datos_accidentes$Dia[is.na(datos_accidentes$Dia ) | datos_accidentes$Dia == ""] = 0

datos_accidentes$Carretera[datos_accidentes$Carretera == ""] = "NN"

datos_accidentes$Total_Implicados[is.na(datos_accidentes$Total_Implicados)] = 0
datos_accidentes$Turismos[is.na(datos_accidentes$Turismos)] = 0
datos_accidentes$Motocicletas[is.na(datos_accidentes$Motocicletas)] = 0
datos_accidentes$Pesados[is.na(datos_accidentes$Pesados)] = 0

datos_accidentes$HL<-as.numeric(datos_accidentes$HL)
datos_accidentes$HL[is.na(datos_accidentes$HL) | datos_accidentes$HL == ""] = 0
sum(is.na(datos_accidentes$HL))
class(datos_accidentes$HL)
datos_accidentes$HG<-as.numeric(datos_accidentes$HG)
datos_accidentes$HG[is.na(datos_accidentes$HG)| datos_accidentes$HG] = 0
class(datos_accidentes$HG)
datos_accidentes$VM<-as.numeric(datos_accidentes$VM)
datos_accidentes$VM[is.na(datos_accidentes$VM) | datos_accidentes$VM == ""] = 0
sum(is.na(datos_accidentes$VM))

datos_accidentes$Daños_mob_urbano[datos_accidentes$Daños_mob_urbano == "No"] = "NO"
datos_accidentes$Daños_mob_urbano[datos_accidentes$Daños_mob_urbano == "Si"] = "SI"
datos_accidentes$Daños_mob_urbano[is.na(datos_accidentes$Daños_mob_urbano) | datos_accidentes$Daños_mob_urbano == ""] = "NN"

datos_accidentes$Corte_Carril[datos_accidentes$Corte_Carril == "No"] = "NO"
datos_accidentes$Corte_Carril[datos_accidentes$Corte_Carril == "Si"] = "SI"
datos_accidentes$Corte_Carril[is.na(datos_accidentes$Corte_Carril) | datos_accidentes$Corte_Carril == ""] = "NN"

datos_accidentes$Vertido[datos_accidentes$Vertido == "No"] = "NO"
datos_accidentes$Vertido[datos_accidentes$Vertido == "Si"] = "SI"
datos_accidentes$Vertido[is.na(datos_accidentes$Vertido) | datos_accidentes$Vertido == ""] = "NN"


datos_accidentes$suelo[is.na(datos_accidentes$Suelo) | datos_accidentes$Suelo == ""] = "Indefinido"
datos_accidentes$suelo<-as.factor(datos_accidentes$Suelo)

#Limpieza calendario_solo_2017.csv
class(calendario$Fecha)
calendario$Fecha<-as.character(calendario$Fecha)
calendario$Fecha[is.na(calendario$Fecha)]="NO ASIGNADA"
calendario$Fecha<-as.Date(calendario$Fecha,"%d/%m/%Y")

#Limpieza de 2017_Accidentalidad.csv
datos_accidentes2$LESIVIDAD<- gsub(" ","", datos_accidentes2$LESIVIDAD)
datos_accidentes2$DISTRITO<- gsub(" ","", datos_accidentes2$DISTRITO)
datos_accidentes2$Tipo_Vehiculo<- gsub(" ","", datos_accidentes2$Tipo_Vehiculo)
datos_accidentes2$TIPO.PERSONA<- gsub(" ","", datos_accidentes2$TIPO.PERSONA)
datos_accidentes2$tipo_accidente<-gsub(" ","", datos_accidentes2$tipo_accidente)

datos_accidentes2$FECHA[is.na(datos_accidentes2$FECHA)] = 0
datos_accidentes2$FECHA<-as.Date(datos_accidentes2$FECHA,"%d/%m/%Y")

datos_accidentes2$Edad[is.na(datos_accidentes2$Edad) | datos_accidentes2$Edad== ""] = "DESCONOCIDA"
datos_accidentes2$Tipo_Vehiculo[is.na(datos_accidentes2$Tipo_Vehiculo) | datos_accidentes2$Tipo_Vehiculo ==""] = "DESCONOCIDA"
datos_accidentes2$TIPO.PERSONA[is.na(datos_accidentes2$TIPO.PERSONA) | datos_accidentes2$TIPO.PERSONA ==""] = "DESCONOCIDA"
datos_accidentes2$LESIVIDAD[datos_accidentes2$LESIVIDAD == "NOASIGNADA"] = "NO ASIGNADA"
datos_accidentes2$Tipo_Vehiculo[datos_accidentes2$Tipo_Vehiculo == "NOASIGNADO"] = "NO ASIGNADO"
datos_accidentes2$TIPO.PERSONA<-as.factor(datos_accidentes2$TIPO.PERSONA)

datos_accidentes2$Tipo_Vehiculo[datos_accidentes2$Tipo_Vehiculo=="CICLOMOTOR"]="MOTOCICLETA"

datos_accidentes2$tipo_accidente[datos_accidentes2$tipo_accidente == "COLISIÓNDOBLE"] = "COLISIÓN DOBLE"
datos_accidentes2$tipo_accidente[datos_accidentes2$tipo_accidente == "CHOQUECONOBJETOFIJO"] = "CHOQUE CON OBJETO FIJO"
datos_accidentes2$tipo_accidente[datos_accidentes2$tipo_accidente == "CAÍDABICICLETA"] = "CAÍDA BICICLETA"
datos_accidentes2$tipo_accidente[datos_accidentes2$tipo_accidente == "CAÍDACICLOMOTOR"] = "CAÍDA MOTOCICLETA"
datos_accidentes2$tipo_accidente[datos_accidentes2$tipo_accidente == "CAÍDAMOTOCICLETA"] = "CAÍDA MOTOCICLETA"
datos_accidentes2$tipo_accidente[datos_accidentes2$tipo_accidente == "CAÍDAVEHÍCULO3RUEDAS"] = "CAÍDA VEHÍCULO 3 RUEDAS"
datos_accidentes2$tipo_accidente[datos_accidentes2$tipo_accidente == "CAÍDAVIAJEROBUS"] = "CAÍDA VIAJERO BUS"
datos_accidentes2$tipo_accidente[datos_accidentes2$tipo_accidente == "COLISIÓNMÚLTIPLE"] = "COLISIÓN MÚLTIPLE"
datos_accidentes2$tipo_accidente[datos_accidentes2$tipo_accidente == "OTRASCAUSAS"] = "OTRAS CAUSAS"
datos_accidentes2$tipo_accidente<-as.factor(datos_accidentes2$tipo_accidente)

levels(datos_accidentes2$RANGO.HORARIO)
datos_accidentes2$RANGO.HORARIO<-as.character(datos_accidentes2$RANGO.HORARIO)
datos_accidentes2$RANGO.HORARIO[datos_accidentes2$RANGO.HORARIO == "DE 00:00 A 00:59"] = "MADRUGADA"
datos_accidentes2$RANGO.HORARIO[datos_accidentes2$RANGO.HORARIO == "DE 1:00 A 1:59"] = "MADRUGADA" 
datos_accidentes2$RANGO.HORARIO[datos_accidentes2$RANGO.HORARIO == "DE 2:00 A 2:59"] = "MADRUGADA"
datos_accidentes2$RANGO.HORARIO[datos_accidentes2$RANGO.HORARIO == "DE 3:00 A 3:59"] = "MADRUGADA"
datos_accidentes2$RANGO.HORARIO[datos_accidentes2$RANGO.HORARIO == "DE 4:00 A 4:59"] = "MADRUGADA"
datos_accidentes2$RANGO.HORARIO[datos_accidentes2$RANGO.HORARIO == "DE 5:00 A 5:59"] = "MADRUGADA"
datos_accidentes2$RANGO.HORARIO[datos_accidentes2$RANGO.HORARIO == "DE 6:00 A 6:59"] = "MADRUGADA"
datos_accidentes2$RANGO.HORARIO[datos_accidentes2$RANGO.HORARIO == "DE 7:00 A 7:59"] = "MAÑANA"
datos_accidentes2$RANGO.HORARIO[datos_accidentes2$RANGO.HORARIO == "DE 8:00 A 8:59"] = "MAÑANA"
datos_accidentes2$RANGO.HORARIO[datos_accidentes2$RANGO.HORARIO == "DE 9:00 A 9:59"] = "MAÑANA"
datos_accidentes2$RANGO.HORARIO[datos_accidentes2$RANGO.HORARIO == "DE 10:00 A 10:59"] = "MAÑANA"
datos_accidentes2$RANGO.HORARIO[datos_accidentes2$RANGO.HORARIO == "DE 11:00 A 11:59"] = "MAÑANA"
datos_accidentes2$RANGO.HORARIO[datos_accidentes2$RANGO.HORARIO == "DE 12:00 A 12:59"] = "MEDIODIA"
datos_accidentes2$RANGO.HORARIO[datos_accidentes2$RANGO.HORARIO == "DE 13:00 A 13:59"] = "MEDIODIA"
datos_accidentes2$RANGO.HORARIO[datos_accidentes2$RANGO.HORARIO == "DE 14:00 A 14:59"] = "MEDIODIA"
datos_accidentes2$RANGO.HORARIO[datos_accidentes2$RANGO.HORARIO == "DE 15:00 A 15:59"] = "MEDIODIA"
datos_accidentes2$RANGO.HORARIO[datos_accidentes2$RANGO.HORARIO == "DE 16:00 A 16:59"] = "TARDE"
datos_accidentes2$RANGO.HORARIO[datos_accidentes2$RANGO.HORARIO == "DE 17:00 A 17:59"] = "TARDE"
datos_accidentes2$RANGO.HORARIO[datos_accidentes2$RANGO.HORARIO == "DE 18:00 A 18:59"] = "TARDE"
datos_accidentes2$RANGO.HORARIO[datos_accidentes2$RANGO.HORARIO == "DE 19:00 A 19:59"] = "TARDE"
datos_accidentes2$RANGO.HORARIO[datos_accidentes2$RANGO.HORARIO == "DE 20:00 A 20:59"] = "NOCHE"
datos_accidentes2$RANGO.HORARIO[datos_accidentes2$RANGO.HORARIO == "DE 21:00 A 21:59"] = "NOCHE"
datos_accidentes2$RANGO.HORARIO[datos_accidentes2$RANGO.HORARIO == "DE 22:00 A 22:59"] = "NOCHE"
datos_accidentes2$RANGO.HORARIO[datos_accidentes2$RANGO.HORARIO == "DE 23:00 A 23:59"] = "NOCHE"
datos_accidentes2$RANGO.HORARIO<-as.factor(datos_accidentes2$RANGO.HORARIO)

datos_accidentes2$LESIVIDAD[is.na(datos_accidentes2$LESIVIDAD) | datos_accidentes2$LESIVIDAD == ""] = "NO ASIGNADA"
datos_accidentes2$Tipo_Vehiculo[is.na(datos_accidentes2$Tipo_Vehiculo) | datos_accidentes2$Tipo_Vehiculo == ""] = "NO ASIGNADO"
datos_accidentes2$Tipo_Vehiculo<-as.factor(datos_accidentes2$Tipo_Vehiculo)
datos_accidentes2$LESIVIDAD<-as.factor(datos_accidentes2$LESIVIDAD)
datos_accidentes2$DIA.SEMANA<-as.character(datos_accidentes2$DIA.SEMANA)
datos_accidentes2$DIA.SEMANA[datos_accidentes2$DIA.SEMANA == "LUNES"] = 1
datos_accidentes2$DIA.SEMANA[datos_accidentes2$DIA.SEMANA == "MARTES"] = 2
datos_accidentes2$DIA.SEMANA[datos_accidentes2$DIA.SEMANA == "MIERCOLES"] = 3
datos_accidentes2$DIA.SEMANA[datos_accidentes2$DIA.SEMANA == "JUEVES"] = 4
datos_accidentes2$DIA.SEMANA[datos_accidentes2$DIA.SEMANA == "VIERNES"] = 5
datos_accidentes2$DIA.SEMANA[datos_accidentes2$DIA.SEMANA == "SABADO"] = 6
datos_accidentes2$DIA.SEMANA[datos_accidentes2$DIA.SEMANA == "DOMINGO"] = 7
datos_accidentes2$DIA.SEMANA<-as.numeric(datos_accidentes2$DIA.SEMANA)

