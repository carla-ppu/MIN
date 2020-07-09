#Utilizado para juntar dos daframe, con el dia, heridos leves y el dia en formato string
df <- data.frame(dia=datos_accidentes$Dia,HL= datos_accidentes$HL,
                 HG=datos_accidentes$HG, VM=datos_accidentes$VM)
df2<-data.frame(dia_semana=format(datos_accidentes$Fecha,"%A"))
df<-cbind(df, df2)

##dataframe para las reglas de asociacion
df2<-data.frame(distrito=datos_accidentes2$DISTRITO, hora=datos_accidentes2$RANGO.HORARIO, edad=datos_accidentes2$Edad,lesividad=datos_accidentes2$LESIVIDAD,mojada=datos_accidentes2$Mojada,tipo_vehiculo=datos_accidentes2$Tipo_Vehiculo,
tipo_persona=datos_accidentes2$TIPO.PERSONA,tipo_accidente=datos_accidentes2$tipo_accidente)
##dataframe para sacar graficas con los accidentes que hay en dias festivos
df3<-data.frame(calendario_accidentes)


##GRAFICA 1 (PARA EL CLUSTERING)
p1 <-ggplot(data=df, aes(x=df$dia_semana,y=df$HL, fill = ""))+geom_bar(stat = "identity")
p1 <- p1 + ggtitle("Gráfica: Dia-Heridos leves") # title
p1 <- p1 + xlab("Dia semana") + ylab("Heridos leves")
p1 <- p1 + scale_fill_discrete(name = "Accidentes")
p1 <-p1 + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
p1


##GRAFICA 2(PARA EL CLUSTERING)
p2 <-ggplot(data=df, aes(x=df$dia_semana,y=df$HG, col="red"))+geom_point()
p2 <- p2 + ggtitle("Gráfica: Dia-Heridos graves") # title
p2 <- p2 + xlab("Dia semana") + ylab("Heridos graves")
p2 <-p2 + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
p2


#GRAFICA 3(PARA EL CLUSTERING)
p3 <-ggplot(data=df, aes(x=df$dia_semana,y=df$VM,fill=""))+geom_bar(stat = "identity")
p3 <- p3 + ggtitle("Gráfica: Dia-Muertos") # title
p3 <- p3 + xlab("Dia semana") + ylab("Muertos")
p3 <- p3 + scale_fill_discrete(name = "Muertos")
p3 <-p3 + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
p3


##GRAFICA PARA REGLAS DE ASOCIACIÓN
###########################################
#PARA SABER QUE HAY MAS PERSONAS QUE SALEN ILESAS
p4 <-ggplot(data=df2, aes(x=df2$lesividad,fill=""))+geom_bar()
p4 <- p4 + ggtitle("Grado de lesividad") # title
p4 <- p4 + ylab("Numero de accidentes")
p4 <- p4 + xlab("Lesividad")
p4 <- p4 + scale_fill_discrete(name = "Accidentes")
p4 <-p4 + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
p4

p5 <-ggplot(data=df2, aes(x=df2$distrito, fill=""))+geom_bar()
p5 <- p5 + ggtitle("Accidentes por distritos") # title
p5 <- p5 + xlab("Distritos")
p5 <- p5 +ylab("Número de accidentes")
p5 <- p5 + scale_fill_discrete(name = "Accidentes")
p5 <-p5 + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
p5

p6<-ggplot(data=df2, aes(x=df2$hora))+geom_bar(aes(fill=df2$tipo_accidente),
                                               position = position_stack(reverse = TRUE))+
                                                coord_flip() +theme(legend.position = "top")
p6 <- p6 + ggtitle("Gráfica franja horaria-tipo de accidente") # title
p6 <- p6 + xlab("Franja horaria")
p6 <- p6 + ylab("Número de accidentes")
p6 <-p6 + scale_fill_discrete(name = "Tipo de vehiculo")
p6 <-p6 + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
p6

p7<-ggplot(data=df2, aes(x=df2$lesividad))+geom_bar(aes(fill=df2$mojada),
                                                    position = position_stack(reverse = TRUE)) +theme(legend.position = "top")
p7 <- p7 + ggtitle("Gráfica lesividad-mojada") # title
p7 <- p7 + xlab("Lesividad")
p7 <- p7 + ylab("Número de accidentes")
p7 <- p7 + scale_fill_discrete(name = "Mojado")
p7 <- p7 + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
p7
p8<-ggplot(data=df2, aes(x=df2$edad))+geom_bar(aes(fill=df2$tipo_vehiculo),
                                               position = position_stack(reverse = TRUE))+
                                                coord_flip() +theme(legend.position = "top")
p8 <- p8 + ggtitle("Gráfica edad-tipo de vehiculo") # title
p8 <- p8 + xlab("Edad")
p8 <- p8 + ylab("Número de accidentes")
p8 <- p8 + scale_fill_discrete(name = "Tipo de vehiculo")
p8 <- p8 + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
p8


#####Diagrama con los dias festivos, laborables...  y los accidentes
p9<-ggplot(data=df3, aes(x=df3$calendario.laboral, y= df3$num_accidentes, fill=("")))+geom_bar(stat = "identity")
p9 <- p9 + ggtitle("Gráfica dias-número de accidentes") # title
p9 <- p9 + xlab("Dias")+ylab("Número de accidentes")
p9 <- p9 + scale_fill_discrete(name = "Accidentes")
p9 <- p9 + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
p9

p10<-ggplot(data=df3, aes(x=format(df3$fecha,"%b"), y= df3$num_accidentes, fill=("")))+geom_bar(stat = "identity")
p10 <- p10 + ggtitle("Gráfica meses-número de accidentes") # title
p10 <- p10 + xlab("Meses")+ylab("Número de accidentes")
p10 <- p10 + scale_fill_discrete(name = "Accidentes")
p10 <- p10 + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
p10

p11 <- ggplot(data=grafico1,aes(x=c("laborable","festivo","domingo","sabado"),y=grafico1$accidentespordia,fill=""))+geom_bar(stat = "identity")
p11 <- p11 + ggtitle("Accidentes/Dia de la semana") # title
p11 <- p11 + xlab("Dias")+ylab("Número de accidentes")
p11 <- p11 + scale_fill_discrete(name = "Accidentes/dia")
p11 <- p11 + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
p11

p12 <- ggplot(data=grafic,aes(x=c("laborable","festivo","domingo","sabado"),y=grafic$accidentesdia,fill=""))+geom_bar(stat = "identity")
p12 <- p12 + ggtitle("Accidentes/Dia de la semana") # title
p12 <- p12 + xlab("Dias")+ylab("Número de accidentes")
p12 <- p12 + scale_fill_discrete(name = "Accidentes/dia")
p12 <- p12 + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
p12

