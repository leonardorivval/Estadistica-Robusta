##Post-hoc de Tukey (un factor)
warp=data("warpbreaks")
w=aov(breaks~tension, data = warpbreaks)
TukeyHSD(w)

##Post-hoc de tr (un factor)
g1=c(3,5,2,4,8,4,3,9)
g2=c(4,4,3,8,7,4,2,5)
g3=c(6,7,8,6,7,9,10,9)
g=data.frame(g1,g2,g3)

##Post-hoc con medianas en p-boot (un factor)
medpb(g)

##Post-hoc con tr en p-boot  (un factor)
medpb(g, est = median)

##Post-hoc con onestep mom usando p-boot (casos donde n<80;un factor)
pbmcp(g, est = mom)

##Diferentes medidas de contraste post-hoc  (un factor)
#g es un vector con los p valores a tratar, arriba es un dataframe
p.adjust(g, method = hommel)

##Post-hoc de rangos (un factor)
cidmul(g)

cidmulv2(g) #Igual a la anterior pero controla mejor P(al menos un error tipo 1)

##Post-hoc para datos discretos (MUY ÚTIL!!!)  (un factor)
smoke<-read.csv(choose.files())
smk<-split(smoke, smoke$iv)
smk1<-smk[[1]]
smk2<-smk[[2]]

binband(smk1[,2],smk2[,2]) ##sin IC
binband(smk1[,2],smk2[,2], KMS = T) ##con IC
splotg2(smk1[,2],smk2[,2]) ##Gráfico

##Post-hoc de cuantiles  (un factor)
qcomhd(smk1[,2],smk2[,2])
qcomhdMC(smk1[,2],smk2[,2]) ##igual pero aprovecha los múltiples núcleos de la PC

##Scheffe(2 factores con; B-by-B)
install.packages("DescTools")
ChickWeight[,2]<-as.factor(ChickWeight[,2])
gogg=aov(attractiveness~gender*alcohol, data=goggles)
ScheffeTest(gogg)

gm=as.matrix(goggles)
##Kaiser-Bowden con tr (2 factores con; B-by-B)
kbcon(eeg)

##Welch-Sidak con Kaiser-Bowden (para ajuste de FWE) para tmeans (2 factores con; B-by-B)
##1)Generar coeficientes de contreaste 
c=con2way(2,3)
c
##2)Generar las listas en función de los factores
r=fac2list(retinol[,10], retinol[, 2:3])
##3)contrastes para A
lincon(r,con=c$conA)
##4)contrastes para B
lincon(r,con=c$conB)
##5)contrastes para AB
lincon(r,con=c$conAB)

##En wrs2 es más simple
mcp2atm(V10~V2*V3,data=retinol)

##Contrastes por medianas en B-B
msmed(r) ##No lidia con valores repetidos
mcp2a(2,3,r, est = median) 
mcp2a(V10~V2*V3,data=retinol, est ="median") ##con WRS2

##Contrastes por medio de boot-t para B-B ANOVA con MOM
mcp2a(2,3,r)

##Contrastes por medio de boot-p para B-B ANOVA con tmean
bbmcppb(2,3,r)

##Contrastes por medio de rangos para B-B ANOVA 
rimul(2,3,r)

##Juzgar la N para hacer las comparaciones
peso1=list(beef_high,beef_low,cereal_high,cereal_low)
tamhane(peso1, cil = 1,crit = 4.3)

##Juzgar N para las comparaciones cuando hay un segundo pool de datos
hochberg(peso1, peso2,cil=1) ##peso2 es un supuesto

#Multi comparaciones con Tmean grupos dependientes (las anteriores fueron independientes)
rmmcp(eeg)

##Multi comparaciones con p-boot usando MOM, medianas, y tmean respectivamente
##MOM
rmmcppb(eeg) #si dif=T es con diferencias y si F es con marginal"
rmmcppb(eeg, dif = F)

##medianas
dmedpb(eeg)#si dif=T es con diferencias y si F es con marginal"

##tmean
dtrimpb(eeg) #si dif=T es con diferencias y si F es con marginal"

##Boot-t para múltiples comparaciones dependientes
bptd(schizo)

##Comparar todos los cuantiles de 2 grupos dependientes
Dqcomhd(smk1[,2],smk2[,2])

##BWMCP (B-W con tmean)
bwmcp(2,3,r)

##BWAMCP lo mismo pero asociado al factor A
bwamcp(2,3,r)

##BWBMCP lo mismo asociado al factor B
bwbmcp(2,3,r)

##BWIMCO lo mismo pero para las interacciones
bwimcp(2,3,r) ##if dif= F, compara las marginales else las diferencias

##si se quiere usar el MOM con Welch y ROM
go=fac2list(goggles[,3], goggles[,1:2])
spmcpa(2,3,go, est = onestep)

##si se quiere hacer usando boot-p
bwmcppb(2,3,go) ##emplea un tmean

##Diseño por W-W
mat=matrix(c(1,-1,-1,1,1,-1,-1,1))##generar matriz de contrastes
rmmcp(eeg,con=mat, dif=F)##usar matriz y datos para las combinaciones lineales

##Múltiples comparaciones para ANOVA de 3 factores
t3way(formula=aggressive~gender*degree*type, data = movie)
str(retinol)

mov=fac2list(movie[,4], movie[,c(1,2,3)])
movc=con3way(2,2,2)
mcp3atm(2,2,2,mov, con = movc) ##B-B-B

rm3mcp(2,2,2,mov) ##W-W-W

##Múltiples comparaciones usando bootstrap para 3 vías
bbwmcp(2,2,2,mov)##B-B-W boot-t con tr
bwwmcp(2,2,2,mov)##B-W-W boot-t con tr
bbbmcppb(2,2,2,mov, est = tmean)##B-B-B p-boot
bbwmcppb(2,2,2,mov)##B-B-W p-boot
bwwmcppb(2,2,2,mov) ##B-W-W p-boot
wwwmcppb(2,2,2,mov) ##W-W-W p-boot
