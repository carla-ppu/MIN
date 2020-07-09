datos_accidentes2<-subset(datos_accidentes2,TIPO.PERSONA!="TESTIGO")
data1<-data.frame(distrito=datos_accidentes2$DISTRITO, hora=datos_accidentes2$RANGO.HORARIO, edad=datos_accidentes2$Edad,lesividad=datos_accidentes2$LESIVIDAD,mojada=datos_accidentes2$Mojada,tipo_vehiculo=datos_accidentes2$Tipo_Vehiculo,
                  tipo_persona=datos_accidentes2$TIPO.PERSONA,tipo_accidente=datos_accidentes2$tipo_accidente)


rules.all<-apriori(data1)
rules.all
inspect(rules.all)

trans <- as(data1, "transactions")
summary(trans)

# Cálculo de las reglas de asociación.
rules <- apriori(trans,
                 parameter = list(minlen=2, supp=0.05, conf=0.8))
quality(rules) <- round(quality(rules), digits=3)
rules

#Ordenamiento y visualización de las reglas creadas.
rules.sorted <- sort(rules, by="confidence")
inspect(rules.sorted[1:20])


#Supresión de las reglas redundantes
subset.matrix <- is.subset(rules.sorted, rules.sorted, sparse=FALSE)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1

#Supresión efectiva
rules.pruned <- rules.sorted[!redundant]
#Ordenamos por confianza
rules.sorted <- sort(rules.pruned, by="confidence")
rules_reducidas<-inspect(rules.sorted[1:50])

#inspect(rules.pruned)
####DIAGRAMAS###
plot(rules.all, jitter=0)
plot(rules.all[1:20], method="graph")
#ESTE ES EL GRAFICO GUAY
plot(rules.all[1:20], method="graph", engine="interactive")
#EJEMPLO
#{lesividad=IL,  tipo_vehiculo=CAMION } => {sexo=HOMBRE}

#Se buscan causas directas de la lesividad en los accidentes
rules1 <- apriori(trans,
                 parameter = list(minlen=2, supp=0.05, conf=0.8),
                 appearance = list(rhs=c("lesividad=HL","lesividad=HG","lesividad=MT")))
quality(rules1) <- round(quality(rules1), digits=3)
rules1.sorted <- sort(rules1, by="confidence")
inspect(rules1.sorted)
#{tipo_vehiculo=MOTOCICLETA,tipo_accidente=COLISIÓN DOBLE} => {lesividad=HL}   0.092      0.844 1.815  2405
#{tipo_vehiculo=MOTOCICLETA,tipo_persona=CONDUCTOR}        => {lesividad=HL}   0.131      0.841 1.808  3419
#{edad=DE MAS DE 74 AÑOS ,tipo_persona=PEATON}  => {tipo_vehiculo=TURISMO}   

#Causas directas de mortalidad
rules2 <- apriori(trans,
                  parameter = list(minlen=2, supp=0.00005, conf=0.3),
                  appearance = list(rhs=c("lesividad=MT")))
quality(rules2) <- round(quality(rules2), digits=3)
rules2.sorted <- sort(rules2, by="confidence")
inspect(rules2.sorted)

#Lesividad en turismo dependiendo de la hora y el estado de la carretera
rules1 <- apriori(trans,
                  parameter = list(minlen=2, supp=0.000005, conf=0.5),
                  appearance = list(rhs=c("lesividad=HL","lesividad=HG","lesividad=MT"),lhs=c("tipo_vehiculo=TURISMO","hora=MAÑANA","hora=MEDIODIA"
                  ,"hora=TARDE","hora=NOCHE","hora=MADRUGADA","mojada=SI","mojada=NO"),default="none"))
quality(rules1) <- round(quality(rules1), digits=3)
rules1.sorted <- sort(rules1, by="confidence")
inspect(rules1.sorted)

#Patrones de atropello segun la hora
rules1 <- apriori(trans,
                  parameter = list(minlen=2, supp=0.005, conf=0.1),
                  appearance = list(rhs=c("hora=MAÑANA","hora=MEDIODIA"
                                          ,"hora=TARDE","hora=NOCHE","hora=MADRUGADA"),lhs=c("tipo_accidente=ATROPELLO"),
                                    default="none"))
quality(rules1) <- round(quality(rules1), digits=3)
rules1.sorted <- sort(rules1, by="confidence")
inspect(rules1.sorted)

#Tipo de accidente por distrito
rules1 <- apriori(trans,
                  parameter = list(minlen=2, supp=0.00000005, conf=0.01),
                  appearance = list(rhs=c("tipo_accidente=ATROPELLO","tipo_accidente=COLISIÓN DOBLE","tipo_accidente=COLISIÓN MÚLTIPLE","tipo_accidente=CAÍDA MOTOCICLETA"),lhs=c("distrito=ARGANZUELA","distrito=BARAJAS","distrito=CARABANCHEL","distrito=CENTRO","distrito=CHAMARTIN","distrito=CHAMBERI",
                                                                                                                         "distrito=CIUDADLINEAL","distrito=FUENCARRAL-ELPARDO","distrito=HORTALEZA","distrito=LATINA","distrito=MONCLOA-ARAVACA","distrito=MORATALAZ",
                                                                                                                         "distrito=PUENTEDEVALLECAS","distrito=RETIRO","distrito=SALAMANCA","distrito=SANBLAS","distrito=TETUAN","distrito=USERA"
                                                                                                                         ,"distrito=VICALVARO", "distrito=VILLADEVALLECAS","distrito=VILLAVERDE"),default="none"))
quality(rules1) <- round(quality(rules1), digits=3)
rules1.sorted <- sort(rules1, by="confidence")
inspect(rules1.sorted)

#Estado de la carretera en atropellos
rules1 <- apriori(trans,
                  parameter = list(minlen=2, supp=0.005, conf=0.5),
                  appearance = list(rhs=c("mojada=SI","mojada=NO"),lhs=c("tipo_accidente=ATROPELLO"),default="none"))
quality(rules1) <- round(quality(rules1), digits=3)
rules1.sorted <- sort(rules1, by="confidence")
inspect(rules1.sorted)

#Lesividad estado de la carretera
rules1 <- apriori(trans,
                  parameter = list(minlen=1, supp=0.00005, conf=0.1),
                  appearance = list(rhs=c("lesividad=HL","lesividad=HG","lesividad=MT","lesividad=IL"),lhs=c("mojada=SI","mojada=NO"),default="none"))
quality(rules1) <- round(quality(rules1), digits=3)
rules1.sorted <- sort(rules1, by="confidence")
inspect(rules1.sorted)

#Accidentes motocicleta
rules1 <- apriori(trans,
                  parameter = list(minlen=2, supp=0.0005, conf=0.03),
                  appearance = list(rhs=c("tipo_accidente=ATROPELLO","tipo_accidente=COLISIÓN DOBLE","tipo_accidente=COLISIÓN MÚLTIPLE","tipo_accidente=CAÍDA MOTOCICLETA"),lhs=c("tipo_vehiculo=MOTOCICLETA"),default="none"))
quality(rules1) <- round(quality(rules1), digits=3)
rules1.sorted <- sort(rules1, by="confidence")
inspect(rules1.sorted)

#Mojado motocicleta
rules1 <- apriori(trans,
                  parameter = list(minlen=2, supp=0.0005, conf=0.03),
                  appearance = list(rhs=c("mojada=SI","mojada=NO"),lhs=c("tipo_vehiculo=MOTOCICLETA"),default="none"))
quality(rules1) <- round(quality(rules1), digits=3)
rules1.sorted <- sort(rules1, by="confidence")
inspect(rules1.sorted)

#Lesividad en turismo dependiendo de la hora y el estado de la carretera
rules1 <- apriori(trans,
                  parameter = list(minlen=2, supp=0.000005, conf=0.5),
                  appearance = list(rhs=c("tipo_vehiculo=TURISMO","tipo_vehiculo=MOTOCICLETA"),lhs=c("hora=MAÑANA","hora=MEDIODIA"
                                                                                              ,"hora=TARDE","hora=NOCHE","hora=MADRUGADA"),default="none"))
quality(rules1) <- round(quality(rules1), digits=3)
rules1.sorted <- sort(rules1, by="confidence")
inspect(rules1.sorted)

