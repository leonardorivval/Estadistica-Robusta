a=aov(retinol[,10]~retinol[,2])
summary(a)
anova(a)
?aov()
#Por lo que aov() ajusta los datos a un lm y summary() o anova() nos da los
#resultados en función de dicho modelo lineal.

data("ChickWeight")
#cargar datos de los pesos de los pollos por diferentes suplementos
b=aov(ChickWeight[,1]~ ChickWeight[,4])
anova(b)
b1=aov(ChickWeight[,1]~as.factor(ChickWeight[,2]))
anova(b1)

##Este primer caso no divide entre los 12 grupos si no se define la variable 
#como facto, por lo que hay que dividirlo uno usando as.factor()

b2=fac2list(ChickWeight[,1], ChickWeight[,2])
anova1(b2)
#La otra opción es usar fact2list() de en lugar de as.factor()


##Para estimar el tamño de la muestra en la ANOVA dado cierto poder 
##y tamaño del efecto
anova_power(groups = 4, power = 0.8, delta = 0.4)

anova_power(groups = 3, power=0.8, delta=0.4)

anova_power(groups = 4, n=40, delta =0.1)
anova_power(groups = 4, power = 0.8, delta = 0.1)


No_Schiz=c(0.49959, 0.23457, 0.26505, 0.27910, 0.00000, 0.00000, 0.00000, 0.14109, 0.00000, 1.34099)
Schizotypal=c(0.24792,0.000000, 0.00000,0.39062,0.34841,0.00000,0.20690,0.44428,0.00000,0.31802)
Schiz_Neg=c(0.25089,0.00000,0.00000,0.00000,0.11459,0.79480,0.17655,0.00000,0.15860,0.00000)
Schiz_Pos=c(0.37667,0.43561,0.72968,0.26285,0.22526,0.34903,0.24482,0.41096,0.08679,0.87532)
Skin=data.frame(No_Schiz,Schizotypal,Schiz_Neg, Schiz_Pos)

##Cuando se usa la media recortada con ANOVA tenemos resultados sig
t1way(Skin)
t1wayv2(Skin)

##Cuando lo hacemos con el método de Welch, o sea con tr=o, entonces no hay sig
##Esto es porque sigue asumiendo normalidad y tiene menos poder
t1way(Skin, tr=0)

#También, podemos tener casos donde lo contrario sea el caso, cuando los aiutliers
#sean pocos o nulos
t1way(smoke)
t1way(smoke, tr=0)
boxplot(smoke)
install.packages("nortest")
shapiro.test()
g5plot(smoke)

##Para hacer el ANOVA por medianas
med1way(smoke)

##Métodos de bootstrap-t para ANOVA
t1waybt(Skin, tr=.1) #Versión con media recortada, no recomendable si tr> ó = 20
BFBANOVA(Skin) #Versión con medias

#Métods de percentil Bootstrap, mejores cuando se usan diferentes estimadores a
##la media o tr>.20
b1way(Skin, est = tmean) #SHPB
pbadepth(Skin, est=tmean, op=3) #LSPB
install.packages("parallel")


prof1=c(3,5,2,4,8,4,3,9)
prof2=c(4,4,3,8,7,4,2,5)
prof3=c(6,7,8,6,7,9,10,9)
profes=data.frame(prof1,prof2,prof3)

##ANOVA para efectos aleatorios; pa dar cuenta de variación normal de la población
rananova(profes)
##El rho nos dice qupe tanta de la variación de las medias winzorizadas de cuanta
##de la varianza de las observaciones; en nuestro caso de arriba es 37% y las dif
##entre los grupos son sognificativas con p=0.027

##ANOVA de rangos
##Versión de Kruskall-Wallis
ozono= data("airquality")
kruskal.test(Ozone~Month, data = airquality)

##ANOVA de rangos de BDM
bdm(Skin)
bdmP(Skin)

##EJ
x=c(3,5,2,4,8,4,3,9,4,4,3,8,7,4,2,5,6,7,8,6,7,9,10,9)
y=c(1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3)
data_ej=data.frame(x,y)
summary(aov(x~as.factor(y)))

d=c(3,5,2,4,8,4,3,9)
e=c(4,4,3,8,7,4,2,5)
f=c(6,7,8,6,7,9,10,9)
data_ej1=data.frame(d,e,f)
#con el de Welch
t1way(data_ej1, tr=0)
#con tr
t1way(data_ej1)

bdanova1(Skin, delta = 0.03, power = 0.9)
pbadepth(Skin)
pbadepth(Skin, est = tmean)

pbadepth(Skin, op=3)
pbadepth(Skin, est = tmean,op=3)
