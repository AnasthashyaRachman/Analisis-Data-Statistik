#KASUS 1
#KASUS 1a
#cumulative probability dari kejadian 
n=15;
x=10
size=15
prob=0.4
pbinom(x, size, prob, lower.tail=FALSE)
dbinom(3,15,0.4)
#KASUS 1b
x1=3
x2=8
size=15
prob=0.4
dbinom(x1,size,prob)+dbinom(4,size,prob)+dbinom(5,size,prob)+dbinom(6,size,prob)+dbinom(7,size,prob)+dbinom(x2,size,prob)
#KASUS 1c
x=3
size=15
prob=0.4
dbinom(x,size,prob)
#uji hipotesis 1 sisi
x= c(183,152,178,157,194,163,144,194,163,114,178,152,118,158)
t.test(x,mu=165, alternative = 'less')

#KASUS 2
#KASIS 2a 
n = 5;
x = 0
prob = 0.4
size = 5
pbinom(x, size, prob, lower.tail=TRUE)
#KASUS 2b 
n = 5;
x = 1 #kurang dari 2 
prob = 0.4
size = 5
pbinom(x, size, prob, lower.tail=TRUE)
#KASUS 2c 
n = 5;
x = 3
prob = 0.4
size = 5
pbinom(x, size, prob, lower.tail=FALSE)

#Kasus 3 
#KASUS 3a
a = ppois (2,7, lower.tail = TRUE, log.p = FALSE)
b = 1-a
b
#KASUS 3b 
ppois(5,7, lower.tail = TRUE, log.p = FALSE)
#KASUS 3c 
ppois(6,7, lower.tail = TRUE, log.p = FALSE)

#KASUS 4
#KASUS 4a 
ppois(6,8, lower.tail=TRUE, log.p=FALSE)
#KASUS 4b
ppois(5,8, lower.tail=TRUE, log.p=FALSE)
#KASUS 4c 
a = ppois (3,8, lower.tail = TRUE, log.p = FALSE)
b = 1-a
b

#Kasus 5 
# KASUS 5a
(pnorm(834,800,40))-(pnorm(778,800,40))
#KASUS 5b
(pnorm(900,800,40))
#KASUS 5c
1-(pnorm(850,800,40))

#KASUS 6 
#KASUS 6a
(pnorm(35,30,2))
#KASUS 6b 
1- (pnorm(25,30,2))
#KASUS 6c 
(pnorm(35,30,2))*(pnorm(25,30,2))
#KASUS 6d
(pnorm (pnorm(35,30,2)))
(pnorm(1-(pnorm(25,30,2))))
(pnorm(pnorm(35,30,2))*(pnorm(25,30,2)))

#KASUS 7 
#KASUS 7a
pexp(20, rate = (1/10), log = FALSE)
#KASUS 7b
1-pexp(10, rate = (1/10), log = FALSE)
#7c
pexp(20, rate = (1/10), log = FALSE)- pexp(10, rate = (1/10), log =FALSE)

#KASUS 8 
#KASUS 8a 
1-pexp(30, rate = (1/15), log = FALSE)
#KASUS 8b 
1-pexp(5, rate = (1/15), log = FALSE)
#KASUS 8c 
pexp(60, rate = (1/15), log = FALSE)- pexp(30, rate = (1/15), log =FALSE)

#cONTOH 1 
x =c(183,152,178,157,194,163,144,194,163,114,178,152,118,158)
t.test(x,mu=165, alternative = 'less')

#CONTOH 2 
Y=c(500.2, 500.9, 500.7, 500.1, 499.8, 499.9, 500.4, 500.3, 499.8, 500.3)
t.test(Y,mu=500, alternative ="two.sided")

#Contoh Data Berpasangan 
sebelum = c(215,231,221,260,228,237,326,240,267)
sesudah = c(200,236,210,233,224,216,296,207,247)
t.test(sebelum,sesudah,alternative= c("greater"),paired=T,var.eq = T,conf.level=0.95)

#uji hipotesis proporsi 1 populasi 
prop.test(x=28,n=90,p=0.10,alternative="less", correct = FALSE)

#uji hipotesis proporsi untuk dua populasi 
prop.test(x=c(275,100), n=c(500,100), alternative = "two.sided",correct = FALSE)

#uji hipotesis 1 populasi varians diketahui
#z.test(x,mu=mean, ragam, alternative='')
#uji hipotesis 2 populasi varians tidak diketahui
Mahasiswa = c(120,122,120,138,130,128,132, 125,127,130)
Mahasiswi = c(115,120,118,130,135,126,127, 126,125,129)
t.test(Mahasiswa,Mahasiswi,alternative = c("two.sided"),paired = F,var.eq = T,conf.level
       = 0.95)

#LATIHAN 1 
#NO 1
ban_radial =c(4.2, 4.7, 6.6, 7.0, 6.7, 4.5, 5.7, 6.0, 7.4, 4.9, 6.1, 5.2)
ban_biasa = c(4.1, 4.9, 6.2, 6.9, 6.8, 4.4, 5.7, 5.8, 6.9, 4.7, 6.0, 4.9)
t.test(ban_radial, ban_biasa, alternaltive = c("two.sided"),paired=F,var.eq=T,conf.level = 0.95)
#Keputusan jika alpha 5% adalah Terima Ho karena p value > alpha yaitu 0.7379 > 0.05
t.test(ban_radial, ban_biasa, alternaltive = c("two.sided"),paired=F,var.eq=T,conf.level = 0.99)
#Keputusan jika alpha 1% adalah Terima Ho karena p value > alpha yaitu 0.7379 > 0.01
