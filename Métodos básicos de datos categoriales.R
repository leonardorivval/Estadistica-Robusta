##Goodness of fit de Chi^2
dato_cat1=c(20,30,15,22)##datos hipotéticos de frecuencias de selección de un producto
chisq.test(dato_cat1)##Chi^2 para Ho: de que no hay diferencias entre los valores esperados y los observados
pwr.chisq.test(w=.3,N=87,df=3)##Para ver si tenemos suficiente poder, en este caso no

##Caso hipotético donde se saben pas probabilidades de ocurrencia de 4 grupos
##pero se quiere saber si han cambiado las probabilidades en un año diferente

x=c(3,17,10,22,48)
p=c(.05,.1,.15,.30,.40)
chisq.test(x=x,p=p) ##Es sig por lo que se rechaza H0 y se puede ver que cambiaron las proporciones de ocurrencia

##Asumiendo que podemos tomar los casos de pertenecer a una categoría como una binomial (con paquete DescTools)
BinomCI(x=3,n=100) #IC contiene p así  que no difiere
BinomCI(x=17,n=100)#No lo contiene y cambió con el año entrante
BinomCI(x=10,n=100)##contiene
BinomCI(x=22, n=100)##contiene
BinomCI(x=48, n=100)##contiene

##Prueba de independencia de Chi^2
x1=matrix(c(8,5,67,20), ncol = 2,byrow = T)
x1
chi.test.ind(x1)

##Detectar diferencias en probabilidades marginales
data(occupationalStatus)
contab(occupationalStatus[,c(1,2)]) ##Versión simple con la función del WRS

##VErsión con la función integrada de R
set.seed(150)
data <- data.frame(before = sample(c("Positive",
                                     "Positive",
                                     "Positive",
                                     "Positive",
                                     "Negative"),
                                   300, replace = TRUE),
                   after = sample(c("Positive",
                                    "Positive",
                                    "Positive",
                                    "Positive",
                                    "Negative"),
                                  300, replace = TRUE))
mcnemar.test(data$before,data$after)

##Proporción de acuerdo
p=c(20,6,9)
sum(p)/100
binomci(35, 100)#.35 en intervalo así que hay acuerdo
##Acuerdo por Kappa
k=matrix(c(20,12,8,11,6,13,19,2,9), ncol = 3,byrow = T)
Ckappa(k)
Ckappa(k, fleiss = T)

##Logistic regression
flag=kyphosis_dat[,1]=="present"
flag
logreg(kyphosis_dat[,2],flag, plotit = T)

##ODDS IC
m=matrix(c(8,67,5,20), ncol = 2, nrow = 2)
ODDSR.CI(m)

##Gráfico de regresión con smooth
logSM(kyphosis_dat[,2],flag)

##Logistic regression para lidear con relación no monotónica
xsq=cbind(kyphosis_dat[,2], (kyphosis_dat[,2])^2)
xsq

