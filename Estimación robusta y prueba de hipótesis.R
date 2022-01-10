install.packages('jmv')
library('jmv')

#Prueba-T cuando 1 solo grupo o población
a=c(12,20,34,45,34,36,37,50,11,32,29)
akerd(a)
idealf(a)
idealfIQR(a)
boxplot(a)
out(a)
t.test(a, conf.level = .975, mu=25)

#Calcular el N necesario para un determinado poder y visceversa
power.t.test(10, delta = .3, type = "one.sample", alternative = "one.sided") #Por ende para 10 el power = 0.2216581 no es suficiente
power.t.test(50, delta = .3, type = "one.sample", alternative = "one.sided") #Por ende para 50 el power = 0.672574 no es suficiente
power.t.test(75, delta = .3, type = "one.sample", alternative = "one.sided") #Por ende para 75 el power = 0.8236462 es suficiente
power.t.test(100, delta = .3, type = "one.sample", alternative = "one.sided")  #Por ende para 75 el power = 0.9089875 es suficiente

#Método de Stein para calcular el N para un determinado poder cuando ya hemos recolectado datos
e=c(12,20,34,45,34,36,37,50,11,32,29)
stein1(e, del=3,pow=.9)# o sea que faltan N-n=209 observaciones
stein1(e,del=3, pow=.9, oneside=T)#o sea que faltan N-n=161 observaciones

#Prueba de hipótesis para una población con medias recortadas
f=c(21.4,26.6,22.7, 26,17.4,21.8,15.4,27.4,19.2,22.4,17.7,26, 29,41,38.4,24.4,25.6,21.9,18.3,13.1,27.3,29.5,-16.9)
t.test(f, conf.level = .95, mu=26.4)#Con una prueba t normal no somos capaces de encontrar una diferencia significativa
trimci(f,alpha = .05, tr=.2, null.value =26.4) #con la prueba t con media recortada sí se encuentra una diferencia significativa y tiene más poder

#Obtener el N necesario para un power de X en la media recortada
stein1.tr(f, del = -3.1,alpha = .05,tr=.2,pow=.8)#poder alcanzado dado de que N y n son iguales

#Intervalo de confianza de y prueba de hipótesis de la mediana 
sint(f, alpha = .05)#aquí nos da un IC de (20.18534 26.33127) por lo que se rechazaría la H_0 al no estar el valor en este 
sintv2(f, alpha = .05, nullval = 26.4)#bajo esta idea también se rechaza la H_0

#Métodos de bootstrap para calcular un valor crítico de una distribución no normal
set.seed(45)#se genera una semilla para que se pueda volver a obtener el mismo resultado siempre
t.valores= NA #Variable cualquiera para lamacenar los datos de la simulación
for(i in 1:1000){#Un loopsito para generar datos, obtener T y repetir este rollo 1000 veces
  x=rlnorm(25)#generar 25 valores de una distribución log normal
  t.valores[i]=sqrt(25)*(mean(x)-1.649)/sd(x)#sacar los T valores y almacenarlos en la variable t.valores
}
akerd(t.valores)
boxplot(t.valores)
orden=sort(t.valores)#ordenar el vector en ascendente
orden_sup=orden[975]#obtener el valor del vector en la posición 975 o alfa cuando ambos lados
orden_inf=orden[25]#obtener el límite inferior
##Nótese que como no es normal no se pueden usar métodos como t.test() para sacar el IC
lim_inf=2-orden_sup*(3/sqrt(25))
lim_sup=2-orden_inf*(3/sqrt(25))
CI_sim= c(lim_inf, lim_sup)
CI_sim

#Otra simulación
set.seed(46)
t.valores_2=NA
for(i in 1:1000){
  x=rnorm(40, mean = 0, sd=1)
  t.valores_2[i]=sqrt(40)*(mean(x)-0)/sd(x)
}
akerd(t.valores_2)
rnorm(40, mean = 0, sd=1)
orden_2=sort(t.valores_2)
orden_sup_2= orden_2[975]
orden_sup_2
orden_inf_2= orden_2[25]
lim_inf_2=orden_sup_2-0*(1/sqrt(40))
lim_sup_2=orden_inf_2-0*(1/sqrt(40))
CI_sim_2=c(lim_inf_2, lim_sup_2)
CI_sim_2

#Con IC de tipo simétrico
set.seed(47)
t.valores_3=NA
for(i in 1:1000){
  x=rnorm(40, mean = 0, sd=1)
  t.valores_3[i]=sqrt(40)*(mean(x)-0)/sd(x)
}
akerd(t.valores_3)
orden_3=sort(t.valores_3)
orden_sup_3= orden_3[95]
lim_inf_3=abs(orden_sup_3-3)*(1/sqrt(40))
lim_inf_3 # este es tanto el límite inf como el sup del CI aquí

#Bootstrap percentil de la media
set.seed(48)
mean_sim=NA
for (i in 1:1000){
  x=rnorm(40, mean = 6)
  mean_sim[i]=mean(x)
}
akerd(mean_sim)
b= mean_sim[x> 6]
length(b)
p_valor_1=2*(length(b)/1000)
p_valor_2=2*(1-(length(b)/1000))
p_valor_1#Se selecciona este que es más pequeño pero como > alpha se conserva H_0
p_valor_2

#Bootstrap percentiles con otras medidas de localización
j=c(3.39,3.30,2.81,3.03,3.44,3.07,3.00,3.43,3.36,3.13,3.12,2.74,2.76,2.88,2.96)
onesampb(j)#un CI basado en el M-estimador de un paso
momci(j)#un IC basado en la versión modificada del M-estimador
onesampb(j,est = tmean) # este hace uso del bootstrap para generar de cualquier parámetro un estimado
onesampb(j,est = median) #aquí con la mediana
onesampb(j,est = mean)#aquí con la media, es con cualquier parámetro
trimpb(j)

#Trimmed mean CI usando bootstrap
trimcibt(j) #simétrico con bootstrap-t
trimcibt(j, side=F) #colas iguales con bootstrap-t
trimci(j) #sin bootstrap
trimpb(j) #con percentil bootstrap

#Cálculo del poder con simulación y con un esrimado dado valores dif de delta, además de un CI para el estimado
powt1an(j, ci=T)
powt1an(j)

#Estimación del ES con bootstrap de algún parámetro Y
bootse(j)#por default el M-estimador
bootse(j, est = tmean) #pero puede ser de cualquier parámetro

#Sólo me puse a jugar con las distribuciones, no del capítulo

b=rnorm(100000, mean=0, sd=1)
kdplot(b)
c= rbinom(1000000, size = 5, prob=.3)
kdplot(c)
d=runif(n=10000000, min=0, max=1)
kdplot(d)

#Algunos ejercicios
power.t.test(25,delta = -.8,sd=5,sig.level = .99, type="one.sample",alternative = "one.sided")
power.t.test(36,delta = .375,sd=8,sig.level = .975, type="one.sample",alternative = "one.sided")
power.t.test(49,delta = -.2,sd=10,sig.level = .95, type="one.sample",alternative = "two.sided")
power.t.test(48,delta=0,sd=5,type="one.sample",alternative = "one.sided")

pt(-2.61, 99)
qt(.975, 99)
qt(.95,15)

k=c(38,44,62,72,43,40,43,42,39,41)
trimci(k, null.value =45)
t.test(k)
qt(.01, 24)

l=c(7.6,8.1,9.6,10.2,10.7,12.3,13.4,13.9,14.6,15.2)
2*(1-pnorm(mean(l),mean = 8.5, sd=sd(l)))
t.test(l, conf.level = .95)
m=c(5,12,23,24, 6,58,9,18,11,66,15,8)
out(m)
trimci(m,tr=.2)
trimci(m,tr=0)
trimcibt(m,tr = 0)
trimcibt(m,tr = .2)
trimcibt(m,tr=.2,side=F)
trimpb(m,tr=.2)

n=c(2,4,6,7,8,9,7,10,12,15,8,9,13,19,5,2,100,200,300,400)
sort(n)
length(n)
onesampb(n)
trimpb(n)
sint(n)
onesampb(n)
