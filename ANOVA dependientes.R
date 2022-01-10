##Datos para ejemplos
dv=c(1,3,4,2,2,3,2,5,6,3,4,4,3,5,6)
subject=c("s1","s1","s1","s2","s2","s2","s3","s3","s3","s4","s4","s4","s5","s5","s5")
myfactor=c("1","2","3","1","2","3","1","2","3","1","2","3","1","2","3")
mydata=data.frame(dv,subject,myfactor)
str(mydata)
mydata$myfactor<-as.factor(mydata$myfactor)
str(mydata)

cork=read.table(choose.files(), header = T)
cork<-as.matrix(cork)

##ANOVA dependiente de 1 vía
blob=aov(dv~myfactor + Error(subject/myfactor),data = mydata)
summary(blob)

##ANOVA dependiente de 1 vía de medias recortadas
mat=rmdat2mat(mydata,dv.col=1,id.col=2)
mat
rmanova(mat)

#La misma pero con WRS2, más simple
rmanova(mydata$dv,mydata$myfactor,mydata$subject) #WRS2
rmanova(cork) #WRS

##Bootstrap-t ANOVA de una vía para medidas dependientes 
rmanovab(y=mydata$dv,groups=mydata$myfactor,blocks=mydata$subject)
akerd(mydata$dv)
rea

##Bootstrap percentil para una vía de medidas dependientes
bd1way(cork)
bd1way(cork, est = tmean)
bd1way(cork, est= median)

##Método de ANOVA boot de diferencias (no marginal) para una vía de medidas dependientes
rmdzero(cork) ##Mayor poder siempre y cuando no haya valores perdidos!!

#ANOVA una vía para grupos dependientes con métodos de rangos
cork1<-as.matrix(cork)
friedman.test(cork1)

##ANOVA de una vía para grupos dependientes con medidas repetidas
bprm(cork)

##ANOVA 2 vías de grupos dependientes con medias recortadas 
str(ChickWeight)
ChickWeight[,2]<-as.factor(ChickWeight[,2])

z2=fac2list(ChickWeight[,1],ChickWeight[,c(4,2)]) ##con WRS
bwtrim(4,12,z2)

bwtrim(weight~Time*Diet, Chick, data = ChickWeight) ##con WRS2

#ANOVA 2 vías con medias recortadas para dependientes por medio de bootstrap-t
tsplit(4,12,z2)
tsplit(weight~Time*Diet, Chick, data = ChickWeight)

##ANOVA 2 vías para dependientes con diferentes medidas de localización con el WRS2
#Por default usan mom pero se puede ajustar por est=
sppba(symptoms~time*group,id, data = hangover)
sppbb(symptoms~time*group,id, data = hangover)
sppbi(symptoms~time*group,id, data = hangover)

##ANOVA de 2 vías para rangos 
bwrank(4,12,z2)

#ANOVA de 3 vías para dependientes con tr
medicacion<-as.factor(c(1,2,1,2,1,2,1,2,1,2,1,2,1,2,1))
c1<-as.numeric(c(3,3,3,4,4,5,5,6,7,7,7,3,2,3,4))
c2<-as.numeric(c(5,5,5,5,6,6,6,6,7,7,7,6,6,6,7))
c3<-as.numeric(c(5,5,5,5,5,5,5,5,5,6,6,6,6,6,6))
e1<-as.numeric(c(10,10,10,10,10,10,8,8,8,9,9,9,9,10,10))
e2<-as.numeric(c(11,11,11,11,11,11,11,9,9,9,9,10,10,10,11))
e3<-as.numeric(c(11,12,12,12,12,11,12,13,13,13,13,13,13,13,11))
medica=data.frame(medicacion,c1,c2,c3,e1,e2,e3)
z3=bw2list(medica,1, lev.col = c(2,3,4,5,6,7))
bwwtrim(2,2,3,z3)
