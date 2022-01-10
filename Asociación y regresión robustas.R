#Diferentes tipos de estimadores al OLS

##Theil-Sen
tsreg(x=pubs$pubs,y=pubs$mortality, xout = T)##Theil-Sen
regplot(x=pubs$pubs,y=pubs$mortality) ##Da un gráfico de lo anterior

##Least Median Squares (LMS)
lmsreg(x=pubs$pubs,y=pubs$mortality)

##Least trimmed squares (LTS)
ltsreg(x=pubs$pubs,y=pubs$mortality)

##Least trimmed absolute
ltareg(x=pubs$pubs,y=pubs$mortality)

##M-estimador
chreg(x=pubs$pubs,y=pubs$mortality)

##MM-estimador
MMreg(x=pubs$pubs,y=pubs$mortality)

##Deepest regression line
mdepreg(x=pubs$pubs,y=pubs$mortality)

##Skipped estimators o que checan y remueven outliers y luego computan el promedio
opreg(x=pubs$pubs,y=pubs$mortality)

##TSTS
tstsreg(x=lagos$X1, y=lagos$Y)

##Pruebas de hipoótesis sobre la pendientes (MMMMMMMMUUUUUYYYYYY CLAVE!!!!!!!!!)
regtest(x=pubs$pubs,y=pubs$mortality) #general, o que no hay diferencias
regci(x=pubs$pubs,y=pubs$mortality) #con IC por cada una 
regci(x=lagos[, c(1,3)], y=lagos$Y)
regci(x=lagos[,c(1,3)], y=lagos$Y, regfun = tstsreg, xout = T) ##con regfun= cambias el estimador de las pendientes

#Inferencias sobre los valores esperados de Y dado X y sus p-valores e IC
regYci(x=pubs$pubs,y=pubs$mortality)

##Suavidad de la línea de regresión
lowess(x=lagos$X1, y=lagos$Y)
plot(x=lagos$X1, y=lagos$Y)
lines(lowess(x=lagos$X1, y=lagos$Y))

#####MUY CLAVE!!! Permite ver la versión  suave de la línea de regresión
lplot(x=lagos$X1, y=lagos$Y, eout=T, pyhat = T)##Lo anterior en una sola función pero mejor porque puede sacar outliers entre otras cosas
lplot(x=lagos[, 1], y=lagos$Y, eout=T, pyhat = T)

##Versión con medidas robustas del anterios
rplot(x=lagos[, c(1,3)], y=lagos$Y, pyhat = T, xout = T, xlab = "X1", ylab = "Y", zlab = "X2")
rplotCI(x=lagos[, 1], y=lagos$Y)

##Versión con cuantiles
qhdsm(x=lagos[, c(1,3)], y=lagos$Y, xout = T, xlab = "X1", ylab = "Y", zlab = "X2") #cuantil 0.5 (default)
qhdsm(x=lagos[, 1], y=lagos$Y, q=.25, xout = T) #cuantil 0.25
qhdsm(x=lagos[, 1], y=lagos$Y, q=.75, xout = T) #cuantil 0.75


##Cuando X es discreta pero Y no
rundis(x=lagos[, 1], y=lagos$Y) # los datos no son adecuados pero la estructura de la función sí

##Ver curvatura con más de un predictor
prplot(x=lagos[, c(3,1)], y=lagos$Y, xout = T, regfun = tstsreg) #El últmimo # de c() da la VI con la cual se predicen los residuos

##heterocedasiticidad por medio de la relación de X y residuos
rhom(x=lagos[, c(1,3)], y=lagos$Y)

#Correlación robusta

##Kendalls Tau
cor.test(x=lagos$X1, y=lagos$Y, method = "kendall") ##Hipótesis nula de que tau es igual a 0, o sea que no hay relación
##Spearmna (lo mismo que Pearson pero con rangos)
cor.test(x=lagos$X1, y=lagos$Y, method = "spearman") ##Ho es igual a tau
##Versión Winzorizada (igual a pearson pero con Winzorización)
wincor(x=lagos$X1, y=lagos$Y)
##Versión OP o por out por proyección
scor(x=lagos$X1, y=lagos$Y)

###CLAVEEEEE!!!!!
##Método de boot que permite lidiar con heterocedasiticidad y usar cualquier método anterior de correlaci+on
corb(x=lagos$X1, y=lagos$Y, corfun = scor, plotit=F) ##Si usas scor plotit debe ser F

##CLAVEEEE!!! 
##Calcular el poder explicativo, lo que en el OLS es la R^2
rplot(x=lagos$X1, y=lagos$Y, varfun = win )##Varfun para indicar el método de la varianza

##Comparar las pendientes por medio de p-boot
reg2ci(A1_dat$SF_CARE,A1_dat$MAPAGLOB,B3_dat$SF_CARE,B3_dat$MAPAGLOB)

##Prueba de que hay linealidad, si significativo se acepta que hay curvatura
lintest(x=lagos[,c(1,3)], y=lagos$Y)

##Ver cuál es el mejor predictor
###Método SA1 o por método robuso de correlación
regpord(x=lagos[, c(1,3)], y=lagos$Y)

###Método SA2 y SA3
a=as.matrix(lagos[, c(1,3)])

ts2str(xx=a,y=lagos$Y)

sm2strv7(xx=a, y=lagos$Y)

##Interacciones o moderadores
##Por OLS
olshc4.inter(x=`2019`[, c(4,5)], y=`2019`[, 6], xout = T)
ols.plot.inter(x=`2019`[, c(4,5)], y=`2019`[, 6])

##Por robustos
regci.inter(x=`2019`[, c(4,5)], y=`2019`[, 6])
reg.plot.inter(x=`2019`[, c(4,5)], y=`2019`[, 6])

##Método por funciones dentro de la función lineal
adtest(x=`2019`[, c(4,5)], y=`2019`[, 6])

olshc4.inter(x=shelley[, c(10,14)], y=shelley$totagg)
ols.plot.inter(x=shelley[, c(10,14)], y=shelley$totagg,zlab = "TOTAGG", ylab = "ENGAGE", xlab = "GPA")
lplot(x=shelley[, c(10,14)], y=shelley$totagg, zlab = "TOTAGG", ylab = "ENGAGE", xlab = "GPA")

#Métodos gráficos de interacción
kercon(x=shelley[, c(10,14)], y=shelley$totagg)

runsm2g(shelley$engage,shelley$totagg,shelley$gpa)
regi(x=shelley$engage,y=shelley$totagg,z=shelley$gpa)

##ANCOVA clásica de dos pasos
ancova_model=lm(retinol[,14]~as.factor(retinol[,2])*retinol[,7])
summary.aov(ancova_model)##Interacción es no significativa por lo que se pasa a ancova (H0 de pendientes no se rechaza)
ancova_model_1=lm(retinol[,14]~as.factor(retinol[,2])+retinol[,7])                
summary.aov(ancova_model_1)##Intercept es significativo, por lo que hipótesis de que estos son iguales se rechaza

##Métodos de ANCOVA por regresión paramétrica y smoother
##PARAMËTRICA S1 de la para métrica
retinol2=split(retinol, retinol$V2)
retinol2
retinol_a=retinol2[[1]]
retinol_b=retinol2[[2]]
##Cuando el gráfico es diferente en su intersección se ve que H0 o que las diferencias entre los Y dado X cuando se toma X covariado son significativas
ancJN(retinol_a$V7,retinol_a$V14, retinol_b$V7,retinol_b$V14, xlab = "Grasa consumida por día", ylab = "Colesterol", xout = T)

reg2plot(retinol_a$V7,retinol_a$V14, retinol_b$V7,retinol_b$V14, xlab = "Grasa consumida por día", ylab = "Colesterol", xout = T)

##Métodos por smoother 
##Método Y con tmean
ancsm(retinol_a$V7,retinol_a$V14, retinol_b$V7,retinol_b$V14, xlab = "Grasa consumida por día", ylab = "Colesterol", xout = T)
##Método Y con cuantiles usando mediana
Qancsm(retinol_a$V7,retinol_a$V14, retinol_b$V7,retinol_b$V14, xlab = "Grasa consumida por día", ylab = "Colesterol", xout = T)
##Método Y con tmean pero con protección para tipo 1
ancova(retinol_a$V7,retinol_a$V14, retinol_b$V7,retinol_b$V14, xlab = "Grasa consumida por día", ylab = "Colesterol", xout = T)
##Método Y con Mann-Whitney
ancovaWMW(retinol_a$V7,retinol_a$V14, retinol_b$V7,retinol_b$V14, xlab = "Grasa consumida por día", ylab = "Colesterol", xout = T)

##Método SPB con p-boot para sus contrastes
ancpb(retinol_a$V7,retinol_a$V14, retinol_b$V7,retinol_b$V14, xout = T)
##SPB pero con t-boot
ancboot(retinol_a$V7,retinol_a$V14, retinol_b$V7,retinol_b$V14, xlab = "Grasa consumida por día", ylab = "Colesterol", xout = T)

##Plot de las dos líneas de regresión en smooth
runmean2g(retinol_a$V7,retinol_a$V14, retinol_b$V7,retinol_b$V14, xlab = "Grasa consumida por día", ylab = "Colesterol", xout = T)

qhdsm2g()
