suj1=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
ganancia1=c(190,80,80,75,50,40,30,20, 20,10,10,10,0,0,-10,-25,-30,-45,-60,-85)
suj2=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36)
ganancia2=c(140,100,100,70,25,20,10,0,-10,-10,-25,-25,-25,-30,-30,-30,45,-45,-45,-50,-50,-50,-60,-75,-75,-85,-85,-100,-110,
            -130,-130,-155,-155,-180,-240,-290)
length(suj2)
length(ganancia2)
ganancia_peso1= data.frame(suj1,ganancia1)
ganancia_peso2= data.frame(suj2,ganancia2)

mean(ganancia_peso2$ganancia2)
t.test(ganancia_peso1$ganancia1, ganancia_peso2$ganancia2) #comparación de dos medias usando el método de Welch; N diferentes
boxplot(ganancia_peso1$ganancia1,ganancia_peso2$ganancia2) #visualización por boxplot
t.test(ganancia_peso1$ganancia1, ganancia_peso2$ganancia2, var.equal = T) #comparación de dos medias usando el método T-test; N diferentes


power.t.test(delta = .5, power=.8) #estimación de N para un poder de .8
#así N es de 64 para cada grupo!!

t.test(retinol[,10]~factor(retinol[,2])) #comparación por Welch cuando se parten los grupos en función de la variable 2

sex_att1=na.omit(sex_attitude)
a=fac2list(na.omit(sex_att1[,3]),na.omit(sex_att1[,1])) #Partir los datos en función de la variable sexc
yuen(a[[1]],a[[2]]) #Método de Yuen para comparar medias recortadas entre 2 grupos

msmed(ganancia_peso1$ganancia1, ganancia_peso2$ganancia2)#Método de Mckean-Schrader para comparar dos medianas
#No funciona si hay valores duplicados, alfa es inadecuada

Control_eeg=c(-0.15, -0.22, 0.07, -0.07, 0.02, 0.24, -0.60, -0.17, -0.33, 0.23, -0.69,
              0.70, 1.13, 0.38)
Asesino_eeg=c(-0.26, 0.25, 0.61, 0.38, 0.87, -0.12, 0.15, 0.93, 0.26, 0.83, 0.35, 1.33, 0.89, 0.58)

pb2gen(Control_eeg,Asesino_eeg, est = median)#Prueba de percentil-boot para comparar dos medianas
median(Control_eeg)#Estos valores son necesarios porque la diferencia estadística por sí misma es irrelevante y poco informativa
median(Asesino_eeg)

male_sex_parners=c(6 ,  1,    1,    3,    1,    1 ,   1 ,   1 ,   1 ,   1 ,   6 ,   1 ,   1 ,   1,    4,
                  5,    3,    9,    1,    1,    1,    5,   12,   10,    4,    2,    1,    1,    4,   45,
                  8,    5,    0,    1,  150,   13,   19,    2,    1,   18,    3,    1,    3,    1  , 11,
                  1,    2,    1,    1,    1  , 12  ,  1,    1,    2,    6  ,  1  ,  1  ,  1,    1,    4,
                  1  ,150,    6,   40,    4,   30,   10,    1,    1,    0,    3,    4,    1,    4,    7,
                  1  , 10,    0,   19  ,  1  ,  9  ,  1  ,  1,    1,    5  ,  0  ,  1  ,  1  , 15,    4,
                   1,    4,    1  ,  1,   11  ,  1,    1,   30  , 12, 6000,    1,    0,    1,    1   ,15)
female_sex_partners=c(1 , 4  ,1 , 5,  1  ,3 , 1,  1 , 1,  1 , 1,  1 , 1 ,15 , 1 , 1,  1 , 1 , 1 , 1 , 1 , 4 , 1,  1 , 1,
                      2, 12,  1,  0,  1,  1,  1, 1  ,2,  1,  1,  5,  5,  1,  1,  1 ,30,  3,  1,  3 , 1,  1,  5,  1,  1,
                       8,  1,  0, 11,  1,  1  ,2  ,1  ,1  ,1  ,3  ,3  ,1  ,1  ,5  ,2  ,4  ,1  ,1  ,1  ,1  ,1  ,1  ,2  ,1,
                       8, 1 ,10,  3,  1,  1,  1,  5,  1,  1,  1, 10,  1,  1,  1,  1,  1  ,3  ,1,  1,  2,  1,  5  ,1,  8,
                       1 , 4  ,1  ,5  ,1  ,1,  1,  1,  1,  1,  5,  1,  5, 10,  1,  6,  1,  2,  1,  2,  1, 30, 15,  1,  2,
                       1,  1  ,3  ,1  ,4  ,1  ,3  ,1 ,20, 2  ,5 , 1 , 2 , 1 , 1,  1,  1,  1,  1,  1,  7,  1,  1,  1,  3,
                       1, 10,  1,  1,  1,  1)

quantile(female_sex_partners,probs = .75)
quantile(male_sex_parners,probs = .75)
pb2gen(male_sex_parners, female_sex_partners, est=quantile, probs=.75)#Prueba de percentil-boot para comparar percentil.75

grupo1=c(3.73624506, 2.10039320, -3.56878819, -0.26418493,  #mixed normal distribution
         -0.27892175, 0.87825842, -0.70582571, -1.26678127,
         -0.30248530, 0.02255344, 14.76303893,-0.78143390,
         -0.60139147, -4.46978177, 1.56778991, -1.14150660,
         -0.20423655, -1.87554928, -1.62752834, 0.26619836)
grupo2= c(-1.1404168, -0.2123789, -1.7810069, -1.2613917,
          -0.3241972, 1.4550603, -0.5686717, -1.7919242,
          -0.6138459, -0.1386593, -1.5451134, -0.8853377,
           0.3590016, 0.4739528,-0.2557869)  #standard normal distribution
t.test(grupo1,grupo2)#Mediante Welch
yuenbt(grupo1,grupo2)#Mediante boot-t comparación de medias

grupo3=c(0, 32, 9, 0, 2, 0, 41, 0, 0, 0,
         6, 18, 3, 3, 0, 11, 11, 2, 0, 11)
grupo4=c(0, 0, 0, 0, 0, 0, 0, 0, 1, 8,
         0, 3, 0, 0, 32, 12, 2, 0, 0, 0)
trimpb2(grupo3,grupo4)#Método por percentil-boot usando tr al .20
yuenbt(grupo3,grupo4)#Método por percentil-t usando tr al .20
t.test(grupo3,grupo4)#Método por Welch

read1=c(34,49,49,44,66,48,49,39,54,57,39,65,43,43,44,42,71,40,41,38,42,77,40,38,43,42,36,55,
        57,57,41,66,69,38,49,51,45,141,133,76,44,40,56,50,75,44,181,45,61,15,23,42,61,146,144,
        89,71,83,49,43,68,57,60,56,63,136,49,57,64,43,71,38,74,84,75,64,48)
read2=c(129,107,91,110,104,101,105,125,82,92,104,134,105,95,101,104,105,122,98,104,95,93,105,132,
        98,112,95,102,72,103,102,102,80,125,93,105,79,125,102,91,58,104,58,129,58,90,108,95,85,84,
        77,85,82,82,111,58,99,77,102,82,95,95,82,72,93,114,108,95,72,95,68,119,84,75,75,122,127)
powest(read1,read2, delta = 10)#Estimación de poder cuando tr=.20
pow2an(read1,read2, ci=T)#curva de poder para los valores diferentes de delta cuando tr=.20

#Métodos de rangos para comparar dos grupos
wilcox.test(ganancia_peso1$ganancia1,ganancia_peso2$ganancia2) #Método de Wilcoxon-Mann-Whitney
wmw(ganancia_peso1$ganancia1,ganancia_peso2$ganancia2) #Método Wilcoxon-Mann-Whitney

cid(ganancia_peso1$ganancia1,ganancia_peso2$ganancia2) #Método de bootstrap the Cliff para rangos (mejor que el de Wilcoxón-Mann-Whitney)
#El método calcula P como una medida del tamaño del efecto o diferencia entre las distribuciones de los dos grupos
cidv2(ganancia_peso1$ganancia1,ganancia_peso2$ganancia2, plotit = T) #Calcula el p-valor del método de Cliff

bmp(grupo3, grupo4)#Método de Brunner-Munzel para estimar las diferencias entre rangos
#El método calcula P como una medida del tamaño del efecto o diferencia entre las distribuciones de los dos grupos
loc2dif.ci(grupo3,grupo4) #Estima las diferencias de la mediana de los dos grupos y da su IC (complementa al método anterior)
median(grupo3)
median(grupo4)

ks(grupo3,grupo4)#Prueba de Kolmogrov-Smirnov para comparar si las distribuciones son diferentes

sband(male_sex_parners,female_sex_partners, flag=T) #Método modificado de Kolmogrov-Smirnov para comparar todos los quantiles y obtener IC
#numsig da el número de IC que no contienen 0; pc indica el alfa de los IC

#Métodos de visualización de los datos para comparar grupos
a<--egyptian_skull_data[egyptian_skull_data[,5]==-3300,]
b<--egyptian_skull_data[egyptian_skull_data[,5]==-200,]
t.test(a$V1,b$V1) #método de Welch da visto bueno pero barra de errores no!
install.packages('plotrix')
ebarplot(a$V1,b$V1) #Se ve el traslape aunque no hay diferencia significativa! Lo cual en principio sería una contradicción
ebarplot(a$V1,b$V1, tr=.2) #Los mismo pero con medias recortadas al tr=.2 (el mejor)
ebarplot.med(a$V1,b$V1) #Lo mismo pero con la mediana

sband(a$V1,b$V1,plotit = T) #Método de visualización de shift function para visualizar las diferencias 
                            #entre los diferentes cuantiles;+indica mediana y o el cuantil .25 y .75;
                            #linea dura el estimado de las diferencia y las punteadas las diferencias reales con alfa=.05
sband(ozono_rata$V1,ozono_rata$V2)

g2plot(ozono_rata$V1,ozono_rata$V2) #Plot de la distribución de cada grupo op=4 si pocos datos usando kernel, else si más datos
boxplot(ozono_rata$V1,ozono_rata$V2) #boxplot en paralelo

sumplot2g(ozono_rata$V1,ozono_rata$V2) #Saca todos los métodos de visualización previos!!!

#Métodos para comparar la varianza
comvar2(ozono_rata$V1,ozono_rata$V2) #Métodos de bootstrap para comparar varinzas usando los IC (si cero H0 de varianzas iguales)

pb2gen(ozono_rata$V1,ozono_rata$V2, est =pbvar) #El percentil bootstrap puede comparar cualquier estimador de localización o variación
pb2gen(ozono_rata$V1,ozono_rata$V2, est =bivar)

#Midiendo el tamño del efecto
yuenv2(ozono_rata$V1,ozono_rata$V2) #Igual a yuen sólo que computa también la medida robuta del tamaño del efecto
yuen(ozono_rata$V1,ozono_rata$V2)

med.effect(ozono_rata$V1,ozono_rata$V2)#Tamaño del efecto de las medianas

akp.effect(ozono_rata$V1,ozono_rata$V2, tr=0)#d de Cohen (Por lo general no es adecuada debido a heterocedasticidad y distribuciones con formas diferentes)
akp.effect(ozono_rata$V1,ozono_rata$V2, EQVAR = F)#Regresa los estimados de la d usando estimadores robustos y las varianzas winzorizadas de ambos grupos

#Comparando correlaciones y pendientes de regresión
data(biopsy)
twopcor(biopsy$V2,biopsy$V9,biopsy$V3,biopsy$V9) #Pone a prueba que r1=r2; si o en IC entonces manten H0
twolsreg(biopsy$V2,biopsy$V9,biopsy$V3,biopsy$V9)#Pone a prueba que Beta1=Beta2 usando el OLS
tworegwb(biopsy$V2,biopsy$V9,biopsy$V3,biopsy$V9)#Pone a prueba que Beta1=Beta2 usando el wild-boot

#Comparando dos binomiales; todos comparan la H0 de que las probabilidades de éxito son iguales
twobinom(r1=7,n1=12,r2=22,n2=25) #Storer-Kim o da IC pero es bueno para 2
twobici(r1=7,n1=12,r2=22,n2=25) #Método de Beal, preferible si más de 2, además da IC
bi2KMS(r1=7,n1=12,r2=22,n2=25) #Supuestamente mejor pero falta evidencia

twobinom(r1=49,n1=105,r2=101,n2=156) 
twobici(r1=49,n1=105,r2=101,n2=156) 
bi2KMS(r1=49,n1=105,r2=101,n2=156) 

power.prop.test(n=50,p1=.5,p2=.75)#Comparación del poder de la comparación de dos binomiales dada la p de k, y el número de ensayos

##Ejercicios##
c=c(132,204,603,50,125,90,185,134)
d=c(92,-42,121,63,182,101,294,36)
t.test(c,d)#Usando Welch
yuen(c,d)#Usando Tr al .2

hipotálamo1=c(11.1, 12.2, 15.5, 17.6, 13.0, 7.5, 9.1, 6.6, 9.5, 18.0, 12.6)
hipotálamo2=c(18.2, 14.1, 13.8, 12.1, 34.1, 12.0, 14.1, 14.5, 12.6, 12.5, 19.8, 13.4, 16.8, 14.1, 12.9)
t.test(hipotálamo1,hipotálamo2)
yuen(hipotálamo1,hipotálamo2)
akp.effect(hipotálamo1,hipotálamo2, tr=0)#d de Cohen

t.test(ozono_rata$V1,ozono_rata$V2)
yuen(ozono_rata$V1,ozono_rata$V2)
var(ozono_rata$V1,ozono_rata$V2)
common_var_rata=(2552.81+7956.309)/(length(ozono_rata$V1)+length(ozono_rata$V2)-1)
common_var_rata
t.test(ozono_rata$V1,ozono_rata$V2,var.equal = T)
sumplot2g(ozono_rata$V1,ozono_rata$V2)
pb2gen(ozono_rata$V1,ozono_rata$V2)
yuenbt(ozono_rata$V1,ozono_rata$V2)
akerd(ozono_rata$V1)
akerd(ozono_rata$V2)
var(ozono_rata$V1)
var(na.omit(ozono_rata$V2))
out(ozono_rata$V1)
out(ozono_rata$V2)

self1=c(77,87, 88 ,114, 151, 210, 219, 246, 253,
        262, 296, 299, 306, 376, 428, 515, 666, 1310, 2611)
self2=c(59, 106, 174, 207, 219 ,237, 313, 365, 458, 497, 515,
         529, 557, 615, 625, 645, 973, 1065, 3215)
yuenbt(self1,self2)
yuenbt(self1,self2,tr=0)
sumplot2g(self1,self2)
yuen(self1,self2)
comvar2(self1,self2)

pb2gen(c,d,est=bivar)

e=c(1, 2, 1, 1, 1, 1, 1, 1 ,1, 1, 2, 4, 1, 1)
f=c(3, 3, 4, 3, 1, 2, 3, 1, 1, 5, 4)
wmw(e,f)
wilcox.test(e,f)
cid(e,f)
cidv2(e,f)
ks(e,f)
