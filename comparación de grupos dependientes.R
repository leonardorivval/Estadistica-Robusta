val=0
seed(1)
for (i in 1:10000) {
  y=rnorm(20)
  x=rlnorm(20)-sqrt(exp(1))
  res=t.test(x-y)
  if(res[3]$p.value<=.05)val=val+1
}
val 
#Val debería ser .05 ya que alfa debería ser esto pero la cobertura de los
#errores tipo 1 se ve que es inferior a el valor esperado cuando las Diferencias
#provienen de distribuciones no normales

a=trimcibt(x-y,tr=0)
t.test(x-y)

norte=c(72,56,32,39,37,32,54,91,79,78,39,60,39,43,60,41,30,42,33,63,47,56,81,46,
        32,35,50,48)
sur=c(76,64,35,31,31,34,60,100,70,67,34,67,39,39,66,36,34,31,27,74,52,47,68,37,
      30,48,37,57)
este=c(66,57,32,39,40,30,46,79,65,55,35,50,36,37,53,29,35,43,29,45,51,68,80,38,
       30,37,34,54)
oeste=c(77,58,36,27,25,28,52,75,61,60,37,54,31,50,63,38,26,25,36,63,53,50,58,38,32,
        39,40,43)
boxplot(norte-este,este-sur)
t.test(norte-este)
trimci(norte-este) 
#IC es diferente y esto aún cuando el boxplot muestra que aparentan ser simétricos
#ademas de cerca de centro en cero, y con distribución normal

trimci(este-sur)#El de diferencia
yuend(este,sur) #El marginal
#Aquí se aprecia que el método con mayor poder es el método de las diferencias y
#no el marginal ya sea trimed. Además de esto se aprecia ya el poder inherente al
#método por trimmed mean

data(Indometh)
b=fac2list(Indometh[,3],Indometh[,2])
trimci(b[[1]]-b[[2]])

yuend(b[[1]],b[[2]])
cor(b[[1]],b[[2]])

rmmest(norte, sur)
rmmest(norte,sur, dif=T)
#Aquí se hizo uso del estimador M y el MOM para comparar la hipotesis nula de 
#diferencias marginales en el primero  y de de diferencias en el segundo

D.akp.effect(b[[1]],b[[2]])
#Calcula el tamaño del efecto de medias recortadas basándose en la hipótesis 
#nula de las diferencias

c=as.matrix(as.numeric(c(NA,NA,14,15,16,13,14,15,17,16,15,14,13,18,17,13,14,14,15,13,14,15,17,16,16,15,14,13,18,17)))
d=as.matrix(as.numeric(c(20,20,21,19,20,22,21,22,22,NA,23,21,21,24,23,20,23,23,24,22,23,23,21,21,20,24,23,20,23,NA)))
length(d)
e=as.matrix(cbind(c,d))
install.packages("mice")
complete(mice(e, method = "mean"))
#Esto nos permite simular los datos faltantes a partir de los datos que tenemos

rmmismcp(b[[1]],b[[2]])
rmmismcp(c,d)
#Esto prueba la hipótesis de que los valores marginales son diferentes mediante
#el uso del percentil bootstrap; con est=X se puede cambiar el estimador

rm2miss(c,d)
#Prueba la hipótesis de los valores marginales son iguales cuando hay valores 
#faltantes a partir de un bootstrap-t; con est=X se puede cambiar el estimador

loc2dif(c,d)
l2drmci(c,d)
#Estos son para probar la hipótesis de que las diferencias de todos los datos, o sea
#que se contrastan las diferencias de los puntos de un conjunto A respecto de todos 
#los de B (de la mediana) y la H0 es que este estimado es cero. La primera te da el 
#estimado de este valor, la segunda gace esto, da el IC y un p-valor.

signt(norte,este, PVSD=T)
#Probamos la hipótesis de que no hay diferencias entre los valores de los grupos,
#ya sea p>.5 ó p<.5. Esta prueba se basa en la distribución binomial donde es o
#mayor a .5 o menor a .5 como éxito y fallo como igual a .5. Esto anterior viene 
#de si la diferencia es 0 o no, si es 0 entonces es un fallo y si no es un éxito.
#Si intervalo de confianza tiene .5 esntonces fallamos en rechazar

wilcox.test(norte,este, paired = T)
#Wilcoxon signed rank test 

comdvar(este,norte)
pcorhc4(este-norte, este+norte)
#Este es el método de Morgan-Pitman para comparar varianzas de 2 grupos 
#dependientes usando U y V donde uno es la diferencia y el otro la suma de los 
#datos. Lo que se pode a prueba es la H0:p=0 o que no hay correlación siendo
#equivalente a que las varianzas son iguales

eeg1=c(-0.15, -0.22, 0.07, -0.07, 0.02, 0.24, -0.60, -0.17, -0.33, 0.23, -0.69, 0.70, 1.13,
       0.38)

eeg2=c(-0-.05, -1.68, 0.44, -1.154, -0.16, -1.29, -2.49, -1.07, -0.84, -0.37, 0.01,
       -1.24, -0.33, 0.78)
rmrvar(eeg1,eeg2, plotit = T)
rmrvar(eeg1,eeg2, plotit = T, est = winvar)
rmrvar(eeg1,eeg2,plotit = T, est = mad)
#Este es un percentil bootsrap que permite comparar las diferencias entre
#medidad de variación robusta. Est= cambia la medida de variacón

lband(eeg1,eeg2, plotit= T)
#Comparar todos los cuantiles de un conjunto de datos 

akerd(eeg1-eeg2)
g2plotdifxy(eeg1,eeg2)

##Ejercicios
t.test(este,sur,paired=T)
trimci(este-sur)
yuend(este,sur)
trimcibt(este-sur,tr=0)
trimcibt(este-sur, tr=.2)
g1=c(10, 14, 15, 18, 20, 29, 30, 40)
g2=c(40, 8, 15, 20, 10, 8, 2, 3)
signt(g1,g2,PVSD=TRUE)
wilcox.test(g1,g2, parired=T)
