# first: install dependent packages
install.packages(c("MASS", "akima", "robustbase"))

# second: install suggested packages
install.packages(c("akima", "cobs", "robust", "mgcv", "scatterplot3d", "quantreg", "rrcov", "lars", "pwr", "trimcluster", "mc2d", "psych", "Rfit", "DepthProc", "class", "fda", "rankFD"))

# third: install an additional package which provides some C functions
# install.packages("devtools")
# NOTE: This seems to be stalled and not functional any more
# devtools::install_github("mrxiaohe/WRScpp")
install.packages("devtools")
# fourth: install WRS
install_github("nicebread/WRS", subdir="pkg")

#Algo de álgebra de matrices
a=cbind(c(2,2,2), c(5,5,5)) #crear una matriz
a
solve(a) #no tiene inversa o es singular
b=cbind(c(5,2), c(3,1))
b
c=solve(b) #tiene inversa por ende es no singular
b%*%c #la inversa por la matriz da I
diag(nrow = 5) #Matriz de identidad I
d=matrix(1:9,nrow = 3, ncol = 3)
solve(d) #es otra matriz singular
t(a) #Es la transoposición de a, o sea es el caso cuando aij--> aji
diag(b)#Regresa los elementos diagonales de una matriz
sum(diag(a)) #El trazo de la matriz o la sumatoria de sus elementos diagonales
eigen(b) #regresa los eigen vectores e igenvalores de una matriz
ginv(b)

#Regresión lineal múltiple con el OLS y el LM de R
summary(lm(data=lagos,Y~X1+X2)) #modelo lineal con LM
ols(cbind(lagos$X1,lagos$X2), lagos$Y, plotit = T, xout = T) #Este es la versión con el OLS del WRS
ols(cbind(lagos$X1,lagos$X2), lagos$Y, plotit = T) 
olshc4(cbind(lagos$X1,lagos$X2), lagos$Y, plotit = T, xout = T) #Hace uso del estimador HC4 para poder calcular los ES y por ende los IC
olshc4(cbind(lagos$X1,lagos$X2), lagos$Y, plotit = T) #sin los outliers fuera el modelo se fastidia y no es un buen predictor de Y
hc4test(cbind(lagos$X1,lagos$X2), lagos$Y, plotit = T, xout = T) #Prueba la H0 de que las pendientes son 0
hc4test(cbind(lagos$X1,lagos$X2), lagos$Y, plotit = T) #Así mientras no se quitan los outliers el modelo es no significativo o las pendientes no son significativamente diferentes de cero
olshc4band(lagos$X1, lagos$Y, plotit = T, xout = T) #Nos da el IC para las medias de Y estimada pero sólo acepta una VI!!
olshc4band(lagos$X1, lagos$Y, plotit = T, xout = T, CI=T) #Si CI= T entonces da los IC per se y no el gráfico

#Prueba de hipótesis de que r o p es =0
?cor.test()
cor.test(lagos$X1, lagos$Y, method = "pearson")
cor.test(lagos$X2, lagos$Y)
lm1=lm(data=lagos,Y~X1+X2)
plot(lm1) #Este es uno de los métodos para detectar homocedasticidad, ya que esume que la varianza del error de Y es independiente de X o que la varianza de Y es independiente de X

#Método de fisher de la trnasformación r a z para saber de cuánto es la N que se requiere para cierto poder y R en una correlación de Pearson
install.packages("pwr", repos="http://cran.r-project.org")
pwr.r.test(r=.5,power=.8, sig.level=.01) #El método asume normalidad así que punto de quiebre de 1 por lo que no es válido si nuestros datos no cumplen con esto

#Calcular un IC para los p, para probar H0: p=0
pcorhc4(cbind(lagos$X2,lagos$X1),lagos$Y, alpha=.05) #Es resistente a violaciones de la homocedasticidad

#Regresión de quantiles
install.packages("quantreg")
args(rqfit)
rqfit(lagos$X1, lagos$Y)
rqtest(cbind(lagos$X2, lagos$X1), lagos$Y, qval = .5) #para probar la hipótesis de que H0:b1....bn=0

#Detectar heterodasticidad
khomreg(cbind(lagos$X2, lagos$X1), lagos$Y) #Da v que es una prueba con distribución chi2 y el p valor; además permite más de una VI
plot(lm1)                                   #Aquí se quiere que sea significativo poque indica que no hay diferencias en las varianzas 

#Bootstrap de percentiles para poder calcular un CI de p, para H0: p diferente de 0
pcorb(cbind(lagos$X2,lagos$X1),lagos$Y) #se basa en el n>250
#Esta es una buena opción porque es capaz de lidiar con homocedasticidad y es capaz de evitar problemas del SE y dar IC adecuados al .95

#Método del Wild bootstrap para calcular las pendientes 
hc4wtest(cbind(lagos$X2, lagos$X1), lagos$Y) #Calcula la H0 de que las pendientes son todas = a 0
olswbtest(cbind(lagos$X2, lagos$X1), lagos$Y) #También calcula la H0 de que las pendientes son =0 pero para cada una
lsfitci(cbind(lagos$X2, lagos$X1), lagos$Y)#Para los casos en que ocurra una matriz singular y no es posible el cáculo de W se usa esta pero se limita a alfa=.95

Precio=c(510,690,365,592,1125,850,363,559,860,695,182,860,1050,675,859,435,555,525,805,369,930,375,670,290,715,365,610,1290)
Tamaño=c(2359,3397,1232,2608,4870,4225,1390,2028,3700,2949,688,3147,4000,4180,3883,1937,2565,2722,4231,1488,4261,1613,2746,1550,3000,1743,2388,4522)
Casa=as.factor(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28))
length(Precio)
Precio_casas=data.frame(Precio,Tamaño,Casa) #Un dataframe de los precios y tamaños de un conjunto de casas 

lsfitci(Precio_casas$Tamaño, Precio_casas$Precio)

#Detectar asociaciones entre variables cuando hay curvatura
lugar= c('Quito','Cape_of_good_h','Rome','Paris','Lapland')
Latitud=c(0.0000,0.2987,0.4648,0.5762,0.8386)
Largo=c(56751,57037,56979,57074,57422)
Arcos_meridianos=data.frame(lugar,Latitud,Largo)
indt(Arcos_meridianos$Latitud,Arcos_meridianos$Largo) #Te dice si hay o no hay relación pero no te da info de la naturaleza de la relación
medind(Arcos_meridianos$Latitud,Arcos_meridianos$Largo,com.pval = T) #Se basa en la mediana de en lugar de la media pero tampoco da info de la naturaleza de la relación

##Regresión de quantiles
qregci(cbind(lagos$X2, lagos$X1), lagos$Y) #Nos dan el IC,los valores estimados de las pendientes, los SE, y valores críticos
qregci(Precio_casas$Tamaño, Precio_casas$Precio) #Además lo hace para cada pendiente, por ende prueba la hipótesis por pendiente

rqtest(cbind(lagos$X2, lagos$X1), lagos$Y) #Revisa la H0: de que Bs=0
rqtest(Precio_casas$Tamaño, Precio_casas$Precio) #No sirve cuando N predictor=1, p=1 usa qregci
qregci(Precio_casas$Tamaño, Precio_casas$Precio)

#Prueba de Homocedasticidad por medio de regresión de quantiles
qhomt(Precio_casas$Tamaño, Precio_casas$Precio)#aquí se quiere aceptar la H0 que indica que no hay hetercedasticidad, y en el gráfico habría que ver a ambas lineas en paralelo (.2 y),sin pendientes diferentes y o con el cero dentro del IC
qhomt(cbind(lagos$X2, lagos$X1), lagos$Y) #no acepta más de un predictor 
qhomt(conciencia.fonologica$V3,conciencia.fonologica$V9)

#Métodos para ver que predictores son mejores 
x1=c(7,1,11,11,7,11,3,1,2,21,1,11,10)
x2=c(26,29,56,31,52,55,71,31,54,47,40,66,68)
x3=c(6,15,8,8,6,9,17,22,18,4,23,9,8)
x4=c(60,52,20,47,33,22,6,44,22,26,34,12,12)
y1=c(78.5,74.3,104.3,87.6,95.9,109.2,102.7,72.5,93.1,115.9,83.8,113.3,109.4)
cemento=data.frame(y1,x1,x2,x3,x4)
regpre(cbind(cemento$x1,cemento$x2,cemento$x3,cemento$x4),cemento$y1) #Aquí lo que importa es va.used que indica que variables, el rango de dichas variables y el error

larsR(cbind(cemento$x1,cemento$x2,cemento$x3,cemento$x4),cemento$y1) #Usa el método de lasso para poder seleccionar un predictor a diferentes pasos
a=as.matrix= cbind(conciencia.fonologica$V3,conciencia.fonologica$V12)
a
b=as.matrix=cbind(conciencia.fonologica$V7,conciencia.fonologica$V12)
b
#Compar correlaciones ya sean empalmadas o no
matrix=as.matrix(conciencia.fonologica)
TWOpov(cbind(conciencia.fonologica$V3,conciencia.fonologica$V7),conciencia.fonologica$V12) #Se indica por el CI cuál es mejor, si está la dif dentro del intervalo es mejor la primera variable y si no se asumen similares
TWOpov(cbind(cemento$x4,cemento$x3), cemento$y1) 

#Ejercicioc
x=c(5,8,9,7,14)
y=c(3,1,5,7,19)
ols(x,y)
res=c(2.530973, -4.911504, -2.725664,  2.902655,  2.203540)
sum(res^2)


ciudad=c('NY','Pitts','Boston','Columbus','New_Orl','W_DC','Omaha','Atlanta','Miami','Tampa','L_Vegas','El_Paso','Chicago','Seattle','Cleveland','Indianapolis','Nash','Salt_lake','San_Diego','Los_Angeles','Fort_Worth','Alburquerque','Honololulu','Phoenix')
proporcion_cancer=c(32.5,28,30.75,29,27,31.2,27,27,23.5,21,21.5,22.8,30.75,27.25,31,26.5,23.5,22.7,25.8,27.8,21.5,22.5,20.6,21)
calorias=c(300,280,305,340,348,357,380,397,453,456,510,535,275,270,335,342,354,394,383,450,446,513,520,520)
ols(calorias,proporcion)

SAT=c(500,530,590,660,610,700,570,640)
GPA=c(2.3,3.1,2.6,3,2.4,3.3,2.6,3.5)
ols(SAT, GPA)
rqfit(SAT,GPA)
qregci(SAT,GPA)
pcorhc4(SAT,GPA)
pcorb(SAT,GPA)
plot(GPA,SAT)
khomreg(SAT,GPA) #con heteroscedasticidad

mou=c(63.3,60.1,53.6,58.8,67.5,62.5)
time=c(241.5,249.8,246.1,232.4,237.2,288.4)
cor(mou,time)
plot(mou,time)
pcorhc4(mou,time)
khomreg(mou, time)
lm(time~mou)
plot(lm(time~mou))
pcorb(mou,time) #H0:p=0 se mantiene, IC contiene 0 con in IC de (-0.9876476  0.9994305)
cor.test(mou,time) #con t el IC es de (-0.7947124  0.8271605)
pcorhc4(mou,time) #con el HC$ se ve que en realidad es más grande debido a homocedasticidad (-1.070048  1.164960)
##Esto muestra que en realidad el IC es más grande de lo que nos indicaba el uso de T y que por ende los intervalos de este son irreales

a1=c(1,2,3,4,5,6)
b1=c(1,4,7,7,4,1)
ols(a1,b1)
plot(a1,b1)
b2=c(4,5,6,7,8,2)
plot(a1,b2)

age=c(5.2,8.8,10.5,10.6,10.4,1.8,12.7,15.6,5.8,1.9,2.2,4.8,7.9,5.2,.9,11.8,7.9,1.5,10.6,8.5,11.1,12.8,11.3,1,14.5,11.9,8.1,13.8,15.5,9.8,11,12.4,11.1,5.1,4.8,4.2,6.9,13.2,9.9,12.5,13.2,8.9,10.8)
length(age)
c_peptide=c(4.8,4.1,5.2,5.5,5,3.4,3.4,4.9,5.6,3.7,3.9,4.5,4.8,4.9,3,4.6,4.8,5.5,4.5,5.3,4.7,6.6,5.1,3.9,5.7,5.1,5.2,3.7,4.9,4.8,4.4,5.2,5.1,4.6,3.9,5.1,5.1,6,4.9,4.1,4.6,4.9,5.1)
length(c_peptide)
hc4test(age,c_peptide)#p.valor = 0.03357257, por lo que se rechaza que pendiente = a cero
khomreg(age, c_peptide) #p no sig, se asume que no hay heteroscedasticidad
plot(lm(c_peptide~age))
pcorb(age,c_peptide)
qhomt(age,c_peptide)#p no sig, se asume que no hay heterscedasticidad, además gráficos con pendiente similar, CI contien cero

CP=data.frame(age,c_peptide)
CP1=subset(CP, age>7)
CP2=subset(CP,age<7)
ols(CP1$age,CP1$c_peptide)
ols(CP2$age,CP2$c_peptide)
qhomt(CP2$age,CP2$c_peptide)
qhomt(CP1$age,CP1$c_peptide)

ols(Precio_casas$Tamaño, Precio_casas$Precio)
38.1921217+ 0.2153008*0 #extrapolación de los datos que da un sinsentido porque no puede haber una casa de tamaño 0!!

a2=c(18,20,35,16,12)
b2=c(36,29,48,64,18)
hc4test(a2,b2)
pcorb(a2,b2)
plot(a2,b2)
a3=c(34, 49, 49 ,44, 66, 48, 49, 39, 54, 57, 39, 65, 43, 43, 44, 42, 71, 40, 41,
       38 ,42, 77, 40, 38, 43, 42, 36, 55, 57, 57, 41, 66, 69, 38, 49, 51, 45, 141,
       133 ,76, 44, 40, 56, 50, 75, 44, 181, 45, 61, 15, 23, 42, 61, 146, 144, 89, 71,
       83, 49, 43, 68, 57, 60, 56, 63, 136, 49 ,57 ,64, 43, 71, 38, 74, 84, 75, 64, 48)
b3=c(129,107, 91, 110, 104, 101, 105, 125, 82 ,92 ,104, 134, 105 ,95 ,101, 104, 105, 122, 98,
     104, 95 ,93 ,105, 132 ,98 ,112 ,95 ,102, 72, 103, 102, 102 ,80 ,125, 93, 105 ,79, 125,
     102, 91, 58, 104, 58, 129, 58, 90, 108 ,95, 85, 84, 77, 85, 82, 82, 111 ,58, 99,
     77, 102, 82, 95, 95, 82, 72, 93, 114, 108, 95, 72, 95, 68, 119, 84, 75, 75, 122, 127)

ols(a3,b3)
olshc4(a3,b3)
khomreg(Arcos_meridianos$Latitud, Arcos_meridianos$Largo)
khomreg(a3,b3)
rqfit(a3,b3)
args(rqfit)
