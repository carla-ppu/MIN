df<-data.frame(calendario$Fecha, calendario$laboral)
df2<-data.frame(datos_accidentes$Fecha)
df3<-data.frame(datos_accidentes2$FECHA)
#p le asigna el "laboral" a los dias que hay accidentes
p <- merge(df, df2, by.x = "calendario.Fecha", by.y = "datos_accidentes.Fecha")
q <- merge(df, df3, by.x = "calendario.Fecha", by.y = "datos_accidentes2.FECHA")
#esto saca la cuenta al agrupar los dias iguales p.e. el 1 de enero hay 3 accidentes
t <- table(p$calendario.Fecha)
t<- data.frame(t)
colnames(t)<-c("fecha", "num_accidentes")
t$fecha<- as.Date(t$fecha)
t$num_accidentes<-as.numeric(t$num_accidentes)

w<-table(calendario$laboral)
w<-data.frame(w)
#esto junta las tres cosas en a tenemos la fecha el laboral y cuantos accidentes hubo ese dia del
#csv de ListadoAccidentes

calendario_accidentes<- merge(t, df, by.x = "fecha", by.y = "calendario.Fecha", all.x = TRUE)

domingo<-sum(calendario_accidentes$num_accidentes[calendario_accidentes$calendario.laboral=="domingo"])
laborable<-sum(calendario_accidentes$num_accidentes[calendario_accidentes$calendario.laboral=="laborable"])
festivo<-sum(calendario_accidentes$num_accidentes[calendario_accidentes$calendario.laboral=="festivo"])
sabado<-sum(calendario_accidentes$num_accidentes[calendario_accidentes$calendario.laboral=="sabado"])
domingo<-domingo/(w$Freq[w$Var1=="domingo"])
laborable<-laborable/(w$Freq[w$Var1=="laborable"])
sabado<-sabado/(w$Freq[w$Var1=="sabado"])
festivo<-festivo/(w$Freq[w$Var1=="festivo"])
grafic<-data.frame("accidentesdia"=c(laborable,festivo,domingo,sabado))

n <- table(q$calendario.Fecha)
n<- data.frame(n)
colnames(n)<-c("fecha", "num_accidentes")
n$fecha<- as.Date(n$fecha)
n$num_accidentes<-as.numeric(n$num_accidentes)

calendario_accidentes2<-merge(n, df, by.x = "fecha", by.y = "calendario.Fecha", all.x = TRUE)

domingo1<-sum(calendario_accidentes2$num_accidentes[calendario_accidentes2$calendario.laboral=="domingo"])
laborable1<-sum(calendario_accidentes2$num_accidentes[calendario_accidentes2$calendario.laboral=="laborable"])
festivo1<-sum(calendario_accidentes2$num_accidentes[calendario_accidentes2$calendario.laboral=="festivo"])
sabado1<-sum(calendario_accidentes2$num_accidentes[calendario_accidentes2$calendario.laboral=="sabado"])
domingo1<-domingo1/(w$Freq[w$Var1=="domingo"])
laborable1<-laborable1/(w$Freq[w$Var1=="laborable"])
sabado1<-sabado1/(w$Freq[w$Var1=="sabado"])
festivo1<-festivo1/(w$Freq[w$Var1=="festivo"])
grafico1<-data.frame("accidentespordia"=c(laborable1,festivo1,domingo1,sabado1))

