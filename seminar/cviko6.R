#' ---
#' title: "Testy dobrej zhody"
#' author: "Jozef Barčák"
#' ---
#'
library(latexpdf)
#'# Pearsonov $\chi^2$ test dobrej zhody
#'
#' Overujeme zhodu s diskretnym rozdelenim
#'
#' Priklad: Hadzeme kockou 600x. Vysledky su v datovom
#' subore pocetnost. Overte, ci je kocka homogenna.
#' Statisticky: overte zhodu s rovnomernym diskretnym
#' rozdelenim {1,2,3,4,5,6}. Rozdelenie do tried
#' je prirodzene, 1- 6 a padalo to takto
pocetnost <- c(122,61,98,115,79,125)
#' Data musia dlzky 100
data <- rep(1:6,pocetnost)
#' Do prikazu pre $\chi^2$ idu aj teoreticke
#' pocetnosti, ktore musi pouzivatel zratat, vynimku
#' tvori rovnomerne rozdelenie, vtedy nemusime
#' nic zadat
chisq.test(pocetnost)
#' P hodnota<0.05, teda zamietame zhodu s rovnomernym
#' rozdelenim. kocka nie je homogenna.
#'
#' Pocas 100 dni sme sledovali pocty roztrhnutia
#' vlakna na pletacom stroji po generalnej oprave.
#' Overte zhodu s Poissonovym rozdelenim s parametrom
#' $\lambda=3$, t.j. v priemere tri roztrhnutia za den
#'
#' 0-8 roztrhnuti
d1 <- c(4,10,26,27,13,10,4,4,2) #pocetnosti
#' Vypocet teoretickych pocetnosti
pp <- dpois(0:7,3)
pp
sum(pp)
#'  Posledna pravdepodobnosti je pre
#'   triedu 8 a viac roztrhnuti
pp[9] <- 1-sum(pp)
pp
chisq.test(d1,p=pp)
#' Upozornujuca sprava hovori, ze nie je splneny
#' predpoklad teoreticke pocetnosti v triedach>=5
#' Za takejto sitacie Phodnota>0.05, nezamietame Ho.
#' Plati Poissonovo rozdelenie
#'ale prikaz v tvare
chisq.test(d1,p=pp,simulate.p.value = T)
#' vysledok je rovnaky ako hore.
#'
#'# Grafy pre GoF
#'
#'graf pre predosly priklad
#'
hustota <- pp
x <- 0:8
tabulka <- data.frame(hodnota=x,pravdepodbnost=pp)
barplot(tabulka$pravdepodbnost)
#' teoreticke pocetnosti
teoreticke <- 100*pp
pocetnosti <- rbind(d1,teoreticke)
barplot(pocetnosti,beside=T,names.arg=x,legend=T,
        col=c("orange","red"))
#' Kniznica EnvStats
library(EnvStats)
#' data musime dat do tvaru ako boli merane
d2 <- rep(0:8,d1)
a <- gofTest(d2,test="chisq",dist="pois",
             cut.points=c(-1,1,2,3,Inf))
plot(a)
a <- gofTest(d2,test="chisq",dist="pois",
             param.list = list(lambda=3),
             cut.points=c(-1,1,2,3,Inf))
a$p.value
#' p hodnota je>0.05, nezamietame $H_0$ zhoda
#' s Poissonovym rozdelenim
#'
#'# KS test dobrej zhody
#'
#'Testujeme meraci pristroj, 100x opakujeme
#'meranie rozmeru suciastky, ktora ma spravny rozmer
#' (stredna hodnota) 20.1. Pristroj ma smerodajnu
#' odchylku 0.9. KS testom testujte hypotezu,
#' data su normalne rozdelene s danymi parametrami.
d1 <- c(19.32, 19.57, 19.08, 20.87, 19.9, 18.92, 20.98, 20.18,
        20.67, 18.87, 21.69, 19.9, 19.16, 20.44, 21.68, 21.23,
        19.44, 19.95, 20.59, 20.62, 21.05, 20,18.96, 22.12, 19.39,
        19.85, 20.39, 18.93, 20.06,19.2)
boxplot(d1,horizontal=T)
library(ggplot2)
library(ggpubr)
ggqqplot(d1,col="red")
#' vidime aj pas spolahlivosti pre priamku,
#' data su v pase -OK
b <- gofTest(d1,test="ks",
             param.list=list(mean=20.1,sd=0.9))
b$p.value
#' Nezamietame $H_0$
plot(b)
#' Test nemozem pouzit, ked nepozname parametre
#' a parametre odhadujeme z dat
mean(d1)
sd(d1)
#' Modifikovany KS test Lillieforsov test.
#' Kniznica nortest
library(nortest)
lillie.test(d1)
#' nzamietame $H_0$
#' KS test a zhoda s exponencialnym rozdelenim
#' Zivotnost vyrobkov ma exponencialne rozdelenie
#' so strednou hodnotou 2000 dni. Exp ma parameter
#' $\lambda$, kde plati$\lambda=\frac{1}{E(X)}$.
#' Nas parameter je 1/2000. Pre data overte
#' zhodu s danym rozdelenim
zivotnost<-c(4996,976,294,7302,2420,1006,7654,2973,320,3169,3994,
             3989,2689,2215,391,4966,1959,9167,5365,2377)
ks.test(zivotnost,"pexp",1/2000)
#' Zamietame hypotezu o zhode s danym rozdelenim
mean(zivotnost)
c <- gofTest(zivotnost,test="ks",dist="exp",
             param.list =list(rate=1/3400))
c$p.value
plot(c)
c <- gofTest(zivotnost,test="ks",dist="exp",
             param.list =list(rate=1/2000))
c$p.value
plot(c)
#'# Dvojvyberove testy dobrej zhody
#'
#' testujeme dva vybery, ze maju rovnake rozdelenie.
#' kedze rozdelenie nie je specifikovane, tak dvojvyberovy
#' KS test patri k neparametrickym metodam
#'
#' Komercne laboratorium testuje pritomnost
#' protilatok na danu chorobu metodami A, B.
#' Davaju rovnake vysledky?
#'
a <- c(0.28, 0.31, 0.27, 0.33, 0.33, 0.29, 0.29, 0.3, 0.31, 0.31, 0.32,
       0.31, 0.3, 0.31, 0.3)
b <- c(0.24, 0.25, 0.28, 0.27, 0.2, 0.32, 0.13, 0.26, 0.38, 0.24, 0.36,
       0.31, 0.3, 0.2, 0.26)
qqPlot(a,b,add.line=T)
ks.test(a,b)
#' p hodnota <0.05, teda nedavaju rovnake vysledky
#'
#'# Testy urcene pre normalne rozdelenie
#'
#'Znamy Shapiro Wilk test, ma velku silu, parametre
#'nemusime poznat, data d1 meraci pristroj
shapiro.test(d1)
#' pouzitie aj Jarque Bera a Anderson Darling
#' Prvy test v kniznic tseries
library(tseries)
jarque.bera.test(d1)
ad.test(d1)