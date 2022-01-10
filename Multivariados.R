##Multivariados
##Estimación de las medias y covarianzas que reducen:
covmve(eeg)##el área o el vlumen de un elipsoide 
covmcd(eeg)##la SD generalizada 

##Versiones robustas
rmba(cork) ##median ball, no bueno
tbs(cork) ##translated bi-weight, bueno si outcome menos a 5 y n mayor a 20
o=as.matrix(eeg)
ogk(o) 
out(eeg, plotit = TRUE, cov.fun = tbs) #adecuado si outcome mayor a 5

##Métodos de proyección para detectar outliers
outpro(eeg) ##usando regla de boxplot
outpro(eeg, MM=T) ##usando regla MADN

outproad(eeg) ##Usar sólo si p>9

outproMC() ##Iguales a las anteriores pero usan losmúltiples núcleos del procesador
outproadMC()

##Prueba de hipótesis para una muestra
dif=scent_dat[,7:9]-scent_dat[,10:12]
smeancrv2(dif, MC=T) #Para probar si H0: mu=0

rmdzeroOP(dif, MC=T)
rmdzeroOP(scent_dat[,7:9]) ##Para probar si H0: mu_1=mu_2...mu_n (caso dependiente)

hotel1(dif, tr=.2) #Usando tr con método de Hotelling

##Prueba de 2 muestras independiantes
#Divide datos por variables de tipo factor
xx=split(retinol, retinol$V2)
a=xx$`1`
b=xx$`2`
a1=subset.data.frame(a, select = c(11:13))
a2=subset.data.frame(b, select = c(11:13))
a3=as.matrix(a1)
a4=as.matrix(a2)
smean2(a3,a4) ##Prueba para  2 grupos independientes
smean2(a3,a4, plotit = T)

##MANOVA
manova(xx[,1:3]~as.factor(xx[,4:5])) #No tengo una base de datos pero esta es la sintaxis

##MANOVA con tmean
ccc=fac2Mlist(scent_dat,3,c(7:9)) ####LO MEJOR DE MUNDO :,D
MULtr.anova(ccc,2,3)
MULAOVp(ccc,2,3)
ccc

##Wilcoxon-Mann-Whitney con rangos
as.matrix(skull_data)
mulwmwv2(skull_data[,1:2],skull_data[,3:4], plotit = T)
##No da p-valor pero se interpreta a partir de si es mayor o menor de los 
##valores críticos el p-hat

##MANOVA de rangos
str(OCD)
OCD[,1]<-as.factor(OCD[,1])
ocd1=fac2list(OCD[,c(2,3)],OCD[,1])
ocd1
ocd2=fac2Mlist(OCD,1,c(2,3))
ocd2
OCD$row<-rep(1:10, 3)
ocdMelt<-melt(OCD, id = c("Group", "row"), measured = c("Actions","Thoughts"))
names(ocdMelt)<-c("Group", "row", "Outcome_Measure", "Frequency")
ocdRobust<-cast(ocdMelt, row ~ Group + Outcome_Measure, value = "Frequency")
ocdRobust$row<-NULL

oc=bw2list(OCD,1,c(2,3))
oc
mulrank(3,2,oc)
mulrank(3,2,ocdRobust) ##MANOVA de una vía
cmanova(3,2,ocdRobust)

##Regresión multivariada
formula1=lm(cbind(Actions,Thoughts)~Group, data = OCD)
summary.aov(formula1) #Versión del OLS

##Versión robusta de regresión multivariada

##PCA matriz de covariados
regpca(raq)

##PCA con matriz de correlaciones más eliminación de outliers
outpca(raq)

##PCA eigenvalores (los valores que explican la mayor varianza; la varianza de los PCAs)
robpca(raq)

##PCA robusta
raq1=as.matrix(raq)
read1<-read[,1:6]
read1<-as.matrix(read1)
read2<-as.matrix(read)
Ppca(read2)
Ppca.summary(read2)

regpca(read2)
robpcaS(read2)

