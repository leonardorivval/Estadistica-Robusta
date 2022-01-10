pnorm(11, mean = 14,sd=sqrt(1.636))
pnorm(45, mean = 50, sd=sqrt(100/25))#nótese que como son estimados de la distribución muestral, entonces usamos el error estándar y el error estándar cuadrado
pnorm(21, mean=30, sd=sqrt(144/16))
1 - pnorm(28, mean = 27, sd=1.166667)
pnorm(6, mean = 5, sd=sqrt(9/36)) - pnorm(4, mean = 5, sd=sqrt(9/36))

pt(2.172, df=12)
qt(.005, df=30)
a=c(12,20,34,34,45,36,37,50,11,32,29)
t.test(a,conf.level = .99)

b=c(5,60,43,56,32,43,47,79,39,41)
trimse(b, tr=.2) #trimmed mean SE
trimci(b, tr=.2, alpha = .05) #trimmed mean CI
mean(b)+ (1.96*sqrt(var(b)/10))
mean(b)- (1.96*sqrt(var(b)/10))
t.test(b, conf.level = .95) #T CI
akerd(b) #densidad de kernel 
boxplot(b) #boxplot

c=c(545,555,558,572,575,576,578,580,594,605,635,651,653,661,666)
trimci(c, tr=.2, alpha = .05)
trimci(c, tr=.2,alpha = .01)
mean(c)+ (1.96*sqrt(var(c)/10))
mean(c)- (1.96*sqrt(var(c)/10))
akerd(c)
boxplot(c)

d=c(10.4,10.9,8.8,7.8,9.5,10.4,8.4,9,22.2,8.5,9.1,8.9,10.5,8.7,10.4,9.8,
    7.7,8.2,10.3,9.1)
sint(d, alpha = .05) #IC de la mediana con alfa de .05
sint(d, alpha = .01)
median(d)
msmedse(d) #Standard squared error de la media, tiene problemas con datos repetidos de un mismo tipo (el 10.4 aquí)
qbinom(.09156,size = 15, prob = .7)

e=c(1,1,1,0,0,1,1,0,0,0,0,1)
binomci(5,25) #IC de binomial dado k=5 en m=25 
binomci(y=e) #IC de binomial dado el vector e con k=6 en n=12
binomci(0,10) #IC de k=0 en n=10

qnorm(.2, mean = 0, sd=1)
f=c(2,6,10,1,15,22,11,29)
mean(f)
se=sqrt(var(f)/8)#error estandar
se
sse=var(f)/8 #error estandar cuadrado
sse
boxplot(f) #sin outliers según el boxplot
out(f) #sin outliers basádo en MAD-median rule
akerd(f)

pnorm(29,mean=30,sd=2)
1-pnorm(30.5,mean=30,sd=2)
pnorm(31, mean = 30, sd=2)-pnorm(29,mean=30,sd=2)

g=c(250,220,281,247,230,209,240,160,370,274,210,204,243,251,190,200,130,150,177,
    475,221,350,224,163,272,236,200,171,98)
boxplot(g)
trimci(g)
229.2+(1.96*sqrt(var(g)/29))
229.2-(1.96*sqrt(var(g)/29))

binomci(0,200000)
h=0
rbinom(25,6,.9)

val= median(rbinom(25,6,.9))
for (i in 1:5000) {
  val[i]=median(rbinom(25,6,.9)
)
}
splot(val)
akerd(val)
boxplot(val)
