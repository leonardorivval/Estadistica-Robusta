weight=c(90,76,90,64,86,51,72,90,95,78,73,102,118,104,81,107,100,87,117,111,107,95,97,80,98,74,74,67,89,58,98,75,56,111,95,88,82,77,86,92)
source=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2)
amount=c(1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2)
peso=data.frame(weight,source,amount)

##Plot de interacción en ANOVA de 2 vías o factores
##Primero va nivel de factor 1 y luego el del 2; luego la respuesta
interaction.plot(peso[,2],peso[,3],peso[,1], trace.label = "Amount",xlab = "Source",ylab = "Trimmed means", fun=tmean)

##Prueba de ANOVA de 2 vías con interacción

anova(aov(retinol[,10]~as.factor(retinol[,2])*as.factor(retinol[,3]),data = retinol))

##ANOVA de 2 vías con interacciones usando medias recortadas
t2way(x=peso[,1],IV1 =peso[,2],IV2 = peso[,3])
t2way(x=retinol[,10],IV1 = retinol[,2],IV2 = retinol[,3])

##ANOVA 2 vías bootstrap
goggles=read.csv(choose.files())
goggles$row<-rep(1:8, 6)
goggles<-melt(goggles, id = c("row", "gender", "alcohol"), measured =
                    c("attractiveness"))
goggles<-cast(goggles, row ~ gender + alcohol)
goggles$row<-NULL

pbad2way(2,3, goggles)

retinol<-read.table(file.choose())
retinol2=fac2list(retinol[ ,10],retinol[ ,3:2])
pbad2way(2,3,retinol2)

z=fac2list(retinol[,10],retinol[,3])
anova1(z)
z=fac2list(retinol[,10],retinol[,2:3])

##La versión del pbad2way del WRS2, más simple MUCHO MEJOR QUE EN WRS
goggles=read.csv(choose.files())
pbad2way(V10~V2*V3, data = retinol)
pbad2way(V14~V2*V3,data=retinol, est = "mom")

goggles[,1]<-as.factor(goggles[,1])
goggles[,2]<-as.factor(goggles[,2])

pbad2way(attractiveness~gender*alcohol,data = goggles, est = "mom")


#ANOVA 2 Vías boot para TR
t2waybt(2,3,z)

##ANOVA para mediana
m2way(2,3,z)
pbad2way(attractiveness~gender*alcohol,data = goggles,est = "median")

##ANOVA de 2 vías de rangos
bdm2way(2,3,z)
rimul(2,3,z)

##ANOVA de 3 vías versión clásica que asume homocedasticidad y normalidad
anova(aov(retinol[,10]~retinol[,2]*retinol[,3]*retinol[,5]))

##ANOVA de 3 vías con medias recortadas

t3way(V10~V2*V3*V5, data=retinol)
t3way(V14 ~ V2*V3*V5, data = retinol)

