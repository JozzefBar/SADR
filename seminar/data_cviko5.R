#' ---
#' title: "Dvojvyberove testy, ANOVA"
#' author: "Jana Kalicka"
#' ---
#'
library(latexpdf)
#'# Dva nezavisle vybery
#'
#' Najrv test o rovnosti, nerovnosti disperzii
#'
#' Priklad Dva druhy plastickej hmoty (vyroba plastovych okien)
#' sme vystavili svetlu po dobu 6 mesiacov. Odolnost voci
#' svetlu v dnoch sme zaznamenali. Vyroba prveho plastu je lacnejsia.
#' Zistite, ci su rozdiely v odolnosti voci svetlu pre dva druhy
#' plastov $\alpha=0.05.$
prvy <- c(85,87,92,80,84,86)
druhy <- c(89,89,90,84,92)
#' Krabicove grafy
boxplot(prvy,druhy,col=c("red","blue"),horizontal = T)
boxplot(prvy,druhy,col=c("red","blue"),horizontal = T,range=0)
shapiro.test(prvy)#su normalne rozdelene
shapiro.test(druhy)# su normalne rozdelene
var.test(prvy,druhy)
#' Phodnota >0.05, nezamietam $H_0$ rovnost disperzii
#' Test strednych hodnot
t.test(prvy,druhy,paired = F,var.equal = T)
#' Phodnota>0.05, nezamietam hypotezu o rovnosti strednych hodnot.
#' Default pre t.test var.equal=F
#t.test(prvy,druhy,paired = F,var.equal = F)
#'
#'# Dvojvyberovy, nerovnake disperzie
#'
#'Priklad Firma odobera ochranne pracovne pomocky od dvoch
#' dodavatelov AA, BB. Zaznamenali sme tieto dlzky dodavok od
#' objednavky. Na hladine vyznamnosti $\alpha=0.05$ testujte
#' hypotezu, ze dlzky dodavok su rovnake.
aa <- c(10,12,15,25,18,20,15,25,30)
bb <- c(15,15,18,10,16,12,15)
boxplot(aa,bb,col=c("red","blue"),horizontal = T,range=0)
shapiro.test(aa)
shapiro.test(bb)
var.test(aa,bb)#test pre rovnost disperzii
#' Zamietame hypotezu o rovnosti disperzii
t.test(aa,bb,paired = F,var.equal = F)
#' Phodnota>0.05, nezamietame hypotezu o rovnosti strednych hodnot.
#'
#'# ANOVA
#'
#' Test rovnosti strednych hodnot apre viac ako dve skupiny
#'
#' Je nutne aby, boli splnene dve podmienky
#'
#' normalita dat v triedach
#'
#' rovnosti disperzi v triedach
#'
#' Priklad: Nase vyukove data. K dispozicii mame dat o priemernom
#' prijme pre tri podmnoziny(faktor vzdelanie-zs, ss, vs). Testume
#' hypotezu o rovnosti strednych hodnot, ze faktor druh vzdelania
#' je statisticky nevyznamny.
library(readxl)
library(RColorBrewer)
library(vioplot)
library(ggplot2)
library(ggpubr)
data <- read_xlsx("data/data_vyuka.xlsx")
new_data <- na.omit(data)
#' Najprv grafy
#'
boxplot(new_data$mprij~new_data$vzdelanie,col=brewer.pal(3,"Pastel2"),
        xlab="vzdelanie",ylab="prijem")
ggline(new_data,x="vzdelanie",y="mprij",add=c("mean_ci","violin",
                                              "jitter"))
#' Overenie podmienok pre ANOVA
#' normalita dat
tapply(new_data$mprij, new_data$vzdelanie, shapiro.test)
#' data v triedach su normalne rozdelene
#' Rovnost disperzii
bartlett.test(new_data$mprij, new_data$vzdelanie)
#' Phodnota>0.05, disperie v triedach sa rovnaju, ANOVA
#'
#' faktorizujeme
vzdelanie <- factor(new_data$vzdelanie)
vzdelanie
View(new_data)
anova <- aov(new_data$mprij~vzdelanie)
anova
summary.aov(anova)
#' Vystupom je standardna ANOVA tabulka, Pr(>F)-Probalility Value<
#' 0.05, teda zamietame hypotezu o rovnosti strednych hodnot.
#' Faktor vzdelanieje nenulovy, statisticky vyznamny. To jest vzdelanie
#' ma vplyv na vysku prijmu.
#'
#' Nasleduju teda post hoc testy, kde zistime, ktore dvojice
#' su odlisne a vypocet miery efektu (ci rozdiel je silny,ma prakticky
#' vyznam)
#'
#' Miera efektu
#'
library(effectsize)
eta_squared(anova)
#'Cislo je vysoke rozdiely su aj prakticky vyznamne
#'
#'Post hoc test, $H_0$ je rovnost strednych hodnot
#'
TukeyHSD(anova)
#' Triedy 1-2, nezamietame $H_0$ rovnake stredne hodnoty. Triedy 1-3,
#' 2-3 rozdielne stredne hodnoty (podla Padjust<0.05). Teraz
#' vizualne, IS, ktory obsahuje nulu-triedy niesu odlisne
plot(TukeyHSD(anova))
plot(TukeyHSD(anova),las=1,col=c("blue","red","red"))
#' Iny post test je Scheffe post test, DescTools
library(DescTools)
ScheffeTest(anova)
#'# Porusenie predpokladu rovnosti rozptylov
#'
#'Welch ANOVA-data chlesterol
#'
data <- read_xlsx("ANOVA/cholesterol.xlsx")
View(data)
#' Faktor-druh diety. Otazka - ma faktor druh diety vplyv na vysku
#' cholesterolu?
boxplot(data$Cholesterol~data$Dieta,col=brewer.pal(3,"Pastel1"),
        xlab="Typ diety",ylab="Cholesterol")
#' normalita
tapply(data$Cholesterol, data$Dieta, shapiro.test)
#' Normalita ok
#' Rovnost rozptylov
bartlett.test(data$Cholesterol,data$Dieta)
#' Nie je splnena, Welch ANOVA, faktorizujeme
Dieta <- factor(data$Dieta)
anova <- oneway.test(data$Cholesterol~data$Dieta,var.equal = F)
anova
print(anova)
#' Kedze Phodnota<0.05, zamietame hypotezu o nulovosti faktora dieta.
#' Faktor dieta ma vplyv na vysku cholesterolu.
#' Porovnanie tried, potrebujeme kniznicu rstatix
library(rstatix)
games_howell_test(Cholesterol~Dieta,data=data)
#' Rovnost keto-stredomorska, nerovnost keto-vegan, stredom-vegan.
#'
#'# Dvojfaktorova analyza rozptylu
#'
#' Traja laboranti (faktor-laborant) urobili opakovane merania deg
#' na styroch pristrojoch (faktor - pristroj), inerakcie neuvazujeme
#' su fatory vyznamne?
anova1 <- read_xlsx("ANOVA/anova1.xlsx")
laborant <- factor(anova1$laborant)
pristroj <- factor(anova1$pristroj)
tapply(anova1$deg,list(anova1$laborant,anova1$pristroj),
       function(x) shapiro.test(x)$p.value)
levene_test(deg~laborant*pristroj,data=anova1)
#' Podmienky ANOVA splnene
anova <- aov(anova1$deg~laborant+pristroj)
summary(anova)
#' Obe phodnoty<0.05, teda faktor laborant je statisticky vyznamny
#' ma vplyv na vysledok, faktor pristroj je statisticky vyznamny, ma
#' vplyv na merania
