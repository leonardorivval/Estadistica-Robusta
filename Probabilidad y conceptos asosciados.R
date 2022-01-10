#Binomial probability
dbinom(5,size=6,prob=.01) #para la probabilidad de k exitos en n ensayos
pbinom(6,size=10,prob=.4) #para la probabilidad de K o menos éxitos en n ensayos

#Probabilidad usando la DN
pnorm(q=3000,mean=3700,sd=200)
pnorm(q=7,mean=6,sd=2)
pnorm(q=0,mean=0, sd=1)
1 - pnorm(q=240, mean = 230, sd=20) #para sacar P(X+/= 240)
pnorm(q=250, mean = 230, sd=20) - (pnorm(q=210, mean = 230, sd=20)) #para sacar el rango de P(210+/= X -/= 250)

sum(.15,.4,.9,.8,.75)
x1= c(.15,.4,.9,.8,.75)
sqrt(var(x1))
pnorm(q=3, mean = 3, sd=0.3142451)

median(c(1,2,3,4))
qnorm(p=.05, mean = 68, sd=10)

pbinom(11,size = 25,prob = .4)
pbinom(10,size = 25,prob = .4)
1-pbinom(9,size = 25,prob = .4)
1-pbinom(8,size=25,prob = .4)
