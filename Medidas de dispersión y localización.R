#Breves intentos de hacer medidas de localizaci�n y medidas de dispersi�n del libro de Wilcox
install.packages("tidyverse")
x= c(12,45,23,79,19,92,30,58,132)
winvar(x,tr=0.2)
mean(x)
mean(x,tr=0.2)
winmean(x,tr=0.2)
median(x)
idealf(x)
idealfIQR(x)
mad(x)
pbvar(x)
bivar(x)
tauvar(x)
y= c(2,2,3,3,3,4,4,4,100000,100000)
mean(y)
var(y)
sd(y)

(100000 - 20002.5)/42162.38 #Se puede ver que el valor 100000 no llega a las 2 SD por lo que no se considera un valor an�malo
median(y)
mad(y)

(100000-3.5)/0.7413 #Por ende ahora podemos ver que esto es un valor an�malo porque tiene un desviaci�n por encima del 2.24 que se esperar�a
out(y) #es equivalente a la f�mula de arriba pero con la funci�n out() dise�ada por Wilcox
boxplot(y)
boxplot(x)

outbox(y,mbox = T) #Este m�todo hace uso del criterio X< o > M +/- kIQR donde el IQR se calcula por cuartos ideales
count(z)
z = c(77,87,88,114,151,210,219,246,253,262,296,299,306,376,428,515,666,1310,2611)
onestep(z)#Son medidas de localizaci�n que quitan los valores extremos y luego promedian, el primero usa el MADN y el otro no s�lo del MAD-median rule
mom(z)

f= c(2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,1,1,1,1,1,1,1,7,7,7,7,7,7,7,5,5,5,5,5,5,8,8,8,8,8,8,8,8,8,8)
g= (1:1000)
splot(f)
hist(f)
akerd(f)
akerd(g)
kdplot(g)
datasets::women
akerd(women$height, xlab = "height women 30-39") #Las densidades de Kernel son buenas para poder ver la distribuci�n de los datos y son mejores que el histograma
akerd(women$weight,xlab = "weight women 30-39")  #sobretodo cuando hay muchos casos extremos en la poblaci�n y la distribuci�n no es sim�trica y en forma de campana
stem(f)

#Ejercicios

x1 = c(21,36,42,24,25,36,35,49,32)
mean(x1)
median(x1)
mean(x1, tr=0.2)

x2 = c(21,36,42,24,25,36,35,200,32)
mean(x2) # aqu� se ilustra como se afecta con un solo valor an�malo (punto de quiebre 1/n)
median(x2)
mean(x2, tr=0.2)

x3= c(1,1,1,1,1,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
      4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,
      5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5)
length(x3)
mean(x3)

x4= c(21,36,42,24,25,36,35,102,32)
var(x4)
winvar(x4, tr=0.2)

x5= c(6,3,2,7,6,5,8,9,9,11)
idealf(x5)
idealfIQR(x5)

x6= c(250,220,281,247,230,209,240,160,370,274,210,204,243,251,190,200,130,150,177,475,221,350,224,163,272,236,200,130,150,
      177,475,221,350,224,163,272,236,200,171,98)
out(x6)

x7 = c(0.10, 0.20,0.25,0.29,0.12,0.04)
x8 = 1000000 * x7
var(x8)

x9= c(1,2,3,4,5,6,7,20,20,20)
boxplot(x9)

x10= c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,2,2,3,3,3,6,8,9,
       11,11,11,12,18,32,32,41)
hist(x10)
outbox(x10)
out(x10)
akerd(x10)
length(x10)
