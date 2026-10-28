#' ---
#' title: "Zadanie 4: ANOVA a dvojvyberove testy"
#' author: "Jozef Barčák"
#' ---
#'
#' Subor prijem.xlsx obsahuje cisty rocny prijem 235 osob spolu s vekovou
#' kategoriou, vzdelanim, miestom prace a sektorom. Cielom je zistit, ktore
#' z tychto premennych maju vplyv na prijem (jednofaktorova a dvojfaktorova
#' ANOVA). V priklade 3 zo suboru dieta.xlsx porovname percentualnu stratu
#' hmotnosti muzov a zien po redukcnom programe.

#+ message = FALSE
library(readxl)
library(RColorBrewer)
library(ggpubr)
library(rstatix)

#'
#'# 1. Import a uprava dat
prijem <- read_xlsx("prijem.xlsx")
str(prijem)
sum(is.na(prijem))
#' Data maju 235 riadkov, ziadne hodnoty nechybaju. Vek, vzdelanie, miesto
#' a sektor su ulozene ako cisla, su to ale kategorie, preto ich
#' faktorizujeme.
prijem$Vek <- factor(prijem$Vek)
prijem$Vzdelanie <- factor(prijem$Vzdelanie)
prijem$Miesto <- factor(prijem$Miesto)
prijem$Sektor <- factor(prijem$Sektor)
table(prijem$Vek)
table(prijem$Vzdelanie)
table(prijem$Miesto)
table(prijem$Sektor)
summary(prijem$Prijem)
#' Kodovanie: vek 1 = 18-25, 2 = 26-35, 3 = 36-45, 4 = 46-55, 5 = 56+;
#' vzdelanie 1 = zakladne alebo SS bez maturity, 2 = SS s maturitou,
#' 3 = VS 1. stupna, 4 = VS 2. stupna, 5 = VS 3. stupna; miesto 1 = mensia
#' obec, 2 = mensie mesto, 3 = okresne mesto, 4 = krajske mesto; sektor
#' 1 = sluzby, 2 = vyroba, 3 = obchod.
#'
#' Skupiny nie su rovnako velke, napr. vo vekovej kategorii 56+ je len 15
#' ludi, v kategorii 26-35 az 98. Priemerny prijem je 14058, median 14070.

#'
#'# 2. Priklad 1 - jednofaktorova ANOVA
#'
#' Pre kazdu premennu testujeme na hladine vyznamnosti $\alpha = 0.05$
#'
#' $$H_0 \quad \mu_1=\mu_2=\dots=\mu_k \qquad H_1 \quad \text{aspon dve stredne hodnoty sa lisia}$$
#'
#' $H_0$ znamena, ze faktor nema vplyv na prijem. Predpoklady ANOVA su
#' normalita dat v kazdej skupine (Shapiro-Wilk test) a rovnost disperzii
#' v skupinach (Bartlettov test). Ak by rovnost disperzii neplatila, pouzili
#' by sme Welchovu ANOVA (oneway.test) a post test Games-Howell.
par(mfrow = c(2, 2))
boxplot(prijem$Prijem ~ prijem$Vek, col = brewer.pal(5, "Pastel1"),
        main = "Prijem podla veku", xlab = "vekova kategoria", ylab = "prijem")
boxplot(prijem$Prijem ~ prijem$Sektor, col = brewer.pal(3, "Pastel1"),
        main = "Prijem podla sektora", xlab = "sektor", ylab = "prijem")
boxplot(prijem$Prijem ~ prijem$Vzdelanie, col = brewer.pal(5, "Pastel1"),
        main = "Prijem podla vzdelania", xlab = "vzdelanie", ylab = "prijem")
boxplot(prijem$Prijem ~ prijem$Miesto, col = brewer.pal(4, "Pastel1"),
        main = "Prijem podla miesta", xlab = "miesto", ylab = "prijem")
par(mfrow = c(1, 1))
#' Najvacsie rozdiely vidno pri mieste prace - cim vacsie sidlo, tym vyssi
#' prijem. Pri vzdelani ma najnizsi prijem skupina 1. Pri sektore su
#' krabice skoro rovnake.

#'
#'## Vek
tapply(prijem$Prijem, prijem$Vek, mean)
tapply(prijem$Prijem, prijem$Vek, function(x) shapiro.test(x)$p.value)
bartlett.test(prijem$Prijem, prijem$Vek)
#' Vsetky p-hodnoty Shapiro-Wilk testu su vacsie ako 0.05, normalitu
#' nezamietame. Bartlettov test ma p = 0.27 > 0.05, disperzie sa rovnaju.
#' Predpoklady su splnene, pouzijeme klasicku ANOVA.
an_vek <- aov(Prijem ~ Vek, data = prijem)
summary(an_vek)
eta_squared(an_vek)
ggline(prijem, x = "Vek", y = "Prijem", add = c("mean_ci", "violin", "jitter"),
       main = "Priemerny prijem podla veku", xlab = "vekova kategoria",
       ylab = "prijem")
#' P-hodnota je 0.093 > 0.05, $H_0$ nezamietame. Vek nema na hladine 0.05
#' statisticky vyznamny vplyv na prijem, post test preto nerobime. Miera
#' efektu $\eta^2 = 0.034$ je mala, vek vysvetluje len 3.4 % variability
#' prijmu. Z grafu vidno, ze najnizsi priemer ma skupina 56+ (12967), ale
#' je v nej len 15 ludi a jej interval je siroky.

#'
#'## Sektor
tapply(prijem$Prijem, prijem$Sektor, mean)
tapply(prijem$Prijem, prijem$Sektor, function(x) shapiro.test(x)$p.value)
bartlett.test(prijem$Prijem, prijem$Sektor)
#' Normalitu nezamietame (vsetky p > 0.05), Bartlettov test ma p = 0.55,
#' disperzie sa rovnaju. Predpoklady su splnene.
an_sektor <- aov(Prijem ~ Sektor, data = prijem)
summary(an_sektor)
eta_squared(an_sektor)
ggline(prijem, x = "Sektor", y = "Prijem", add = c("mean_ci", "violin", "jitter"),
       main = "Priemerny prijem podla sektora", xlab = "sektor",
       ylab = "prijem")
#' P-hodnota je 0.855 > 0.05, $H_0$ nezamietame. Sektor nema vplyv na
#' prijem, priemery su takmer rovnake (13983, 14152, 14046) a intervaly
#' v grafe sa prekryvaju. Miera efektu $\eta^2 = 0.001$ je zanedbatelna.

#'
#'## Vzdelanie
tapply(prijem$Prijem, prijem$Vzdelanie, mean)
tapply(prijem$Prijem, prijem$Vzdelanie, function(x) shapiro.test(x)$p.value)
bartlett.test(prijem$Prijem, prijem$Vzdelanie)
#' Normalitu nezamietame (najmensia p-hodnota 0.062 je pri skupine 4),
#' Bartlettov test ma p = 0.18, disperzie sa rovnaju. Predpoklady su
#' splnene.
an_vzdelanie <- aov(Prijem ~ Vzdelanie, data = prijem)
summary(an_vzdelanie)
eta_squared(an_vzdelanie)
ggline(prijem, x = "Vzdelanie", y = "Prijem", add = c("mean_ci", "violin", "jitter"),
       main = "Priemerny prijem podla vzdelania", xlab = "vzdelanie",
       ylab = "prijem")
#' P-hodnota je 0.0025 < 0.05, $H_0$ zamietame. Vzdelanie ma statisticky
#' vyznamny vplyv na prijem. Miera efektu $\eta^2 = 0.069$ je stredna.
#' Ktore skupiny sa lisia, zistime post testom, $H_0$ je rovnost strednych
#' hodnot dvojice:
TukeyHSD(an_vzdelanie)
plot(TukeyHSD(an_vzdelanie), las = 1)
#' Lisia sa len dvojice 3-1 (p = 0.006) a 4-1 (p = 0.004), ich intervaly
#' neobsahuju nulu. Ludia s VS 1. stupna zarabaju v priemere o 1358 viac
#' a ludia s VS 2. stupna o 1552 viac ako ludia bez maturity. Ostatne
#' dvojice sa preukazne nelisia.

#'
#'## Miesto
tapply(prijem$Prijem, prijem$Miesto, mean)
tapply(prijem$Prijem, prijem$Miesto, function(x) shapiro.test(x)$p.value)
bartlett.test(prijem$Prijem, prijem$Miesto)
#' Normalitu nezamietame (vsetky p > 0.05), Bartlettov test ma p = 0.11,
#' disperzie sa rovnaju. Predpoklady su splnene.
an_miesto <- aov(Prijem ~ Miesto, data = prijem)
summary(an_miesto)
eta_squared(an_miesto)
ggline(prijem, x = "Miesto", y = "Prijem", add = c("mean_ci", "violin", "jitter"),
       main = "Priemerny prijem podla miesta", xlab = "miesto",
       ylab = "prijem")
#' P-hodnota je 4.2e-09 < 0.05, $H_0$ zamietame. Miesto prace ma
#' statisticky vyznamny vplyv na prijem a miera efektu $\eta^2 = 0.166$ je
#' velka - zo vsetkych styroch premennych je najvacsia.
TukeyHSD(an_miesto)
plot(TukeyHSD(an_miesto), las = 1)
#' Lisia sa dvojice 3-1, 4-1, 3-2 a 4-2. Okresne a krajske mesto maju
#' vyssi prijem ako mensia obec aj mensie mesto (napr. krajske mesto
#' o 2048 viac ako mensia obec). Medzi mensou obcou a mensim mestom (2-1)
#' ani medzi okresnym a krajskym mestom (4-3) preukazny rozdiel nie je.

#'
#'# 3. Priklad 2 - dvojfaktorova ANOVA
#'
#' Testujeme na hladine vyznamnosti $\alpha = 0.1$. Pre faktory A a B su
#' hypotezy
#'
#' $$H_0 \quad \alpha_i = 0 \qquad H_1 \quad \text{aspon jedno } \alpha_i \neq 0$$
#' $$H_0 \quad \beta_j = 0 \qquad H_1 \quad \text{aspon jedno } \beta_j \neq 0$$
#'
#' a pri modeli s interakciami este
#'
#' $$H_0 \quad (\alpha\beta)_{ij} = 0 \qquad H_1 \quad \text{aspon jedna interakcia je nenulova}$$
#'
#' Predpoklady su normalita v kazdej triede (kombinacii urovni oboch
#' faktorov) a rovnost disperzii v triedach (Leveneov test), overujeme ich
#' na hladine 0.05. Post test je Tukey, dvojica sa lisi, ak p adj < 0.1.

#'
#'## Miesto a vzdelanie
table(prijem$Miesto, prijem$Vzdelanie)
tapply(prijem$Prijem, list(prijem$Miesto, prijem$Vzdelanie),
       function(x) shapiro.test(x)$p.value)
levene_test(Prijem ~ Miesto * Vzdelanie, data = prijem)
#' Vo vsetkych 20 triedach je p-hodnota vacsia ako 0.05, normalitu
#' nezamietame. Leveneov test ma p = 0.76, disperzie sa rovnaju.
#' Predpoklady su splnene.
an_mv <- aov(Prijem ~ Miesto + Vzdelanie, data = prijem)
summary(an_mv)
eta_squared(an_mv)
#' Miesto (p = 2.3e-09) aj vzdelanie (p = 0.018) maju p-hodnotu mensiu ako
#' 0.1, oba faktory maju statisticky vyznamny vplyv na prijem. Miera efektu
#' je 0.166 pre miesto (velka) a 0.042 pre vzdelanie (mala).
TukeyHSD(an_mv)
#' Pri mieste sa lisia dvojice 3-1, 4-1, 3-2 a 4-2, rovnako ako v priklade 1.
#' Pri vzdelani sa lisia 3-1 (p = 0.021) a 4-1 (p = 0.043), teda VS
#' 1. a 2. stupna oproti zakladnemu vzdelaniu.
an_mv2 <- aov(Prijem ~ Miesto * Vzdelanie, data = prijem)
summary(an_mv2)
interaction.plot(prijem$Vzdelanie, prijem$Miesto, prijem$Prijem,
                 type = "b", col = 1:4, pch = 19, trace.label = "miesto",
                 main = "Interakcia miesta a vzdelania", xlab = "vzdelanie",
                 ylab = "priemerny prijem")
#' Interakcia ma p = 0.47 > 0.1, $H_0$ nezamietame, interakcie su nulove.
#' Vplyv vzdelania je teda v kazdom type sidla podobny. V grafe sa ciary
#' krizuju, ale triedy su male (4 az 29 ludi), takze priemery kolisu
#' nahodne.

#'
#'## Vek a miesto
table(prijem$Vek, prijem$Miesto)
levene_test(Prijem ~ Vek * Miesto, data = prijem)
#' Trieda vek 3 a miesto 3 ma len 2 ludi, Shapiro-Wilk test sa na nu
#' pouzit neda, preto normalitu v triedach overit nevieme. Leveneov test ma
#' p = 0.084 > 0.05, disperzie sa rovnaju.
an_vm <- aov(Prijem ~ Vek + Miesto, data = prijem)
summary(an_vm)
eta_squared(an_vm)
#' Vek ma p = 0.053 < 0.1 a miesto p = 1.1e-08 < 0.1, oba faktory maju
#' na hladine 0.1 vyznamny vplyv. Miera efektu je 0.034 pre vek (mala)
#' a 0.156 pre miesto (velka).
TukeyHSD(an_vm)
#' Pri veku sa lisia dvojice 5-3 (p = 0.084) a 5-4 (p = 0.070), ludia nad
#' 56 rokov maju nizsi prijem ako ludia vo veku 36-45 a 46-55. Pri mieste
#' sa lisia dvojice 3-1, 4-1, 3-2 a 4-2, rovnako ako v priklade 1.
an_vm2 <- aov(Prijem ~ Vek * Miesto, data = prijem)
summary(an_vm2)
interaction.plot(prijem$Vek, prijem$Miesto, prijem$Prijem,
                 type = "b", col = 1:4, pch = 19, trace.label = "miesto",
                 main = "Interakcia veku a miesta", xlab = "vekova kategoria",
                 ylab = "priemerny prijem")
#' Interakcia ma p = 0.71 > 0.1, interakcie su nulove. Krajske mesto ma
#' najvyssi prijem v kazdej vekovej skupine. Ostatne ciary sa miestami
#' krizuju, ale triedy maju len 2 az 32 ludi, priemery preto kolisu.

#'
#'## Vek a sektor
table(prijem$Vek, prijem$Sektor)
tapply(prijem$Prijem, list(prijem$Vek, prijem$Sektor),
       function(x) shapiro.test(x)$p.value)
levene_test(Prijem ~ Vek * Sektor, data = prijem)
#' Vo vsetkych 15 triedach je p-hodnota vacsia ako 0.05, normalitu
#' nezamietame. Leveneov test ma p = 0.73, disperzie sa rovnaju.
#' Predpoklady su splnene.
an_vs <- aov(Prijem ~ Vek + Sektor, data = prijem)
summary(an_vs)
eta_squared(an_vs)
#' Vek ma p = 0.095 < 0.1, na hladine 0.1 ma vyznamny vplyv. Sektor ma
#' p = 0.90, vplyv nema. Miera efektu je pri veku 0.034 (mala) a pri
#' sektore 0.001 (zanedbatelna).
TukeyHSD(an_vs)
#' Post test pri veku nenasiel ziadnu dvojicu s p adj < 0.1, najblizsie su
#' 5-4 (p = 0.118) a 5-3 (p = 0.137). Pri sektore sa ziadne dvojice
#' nelisia.
an_vs2 <- aov(Prijem ~ Vek * Sektor, data = prijem)
summary(an_vs2)
interaction.plot(prijem$Vek, prijem$Sektor, prijem$Prijem,
                 type = "b", col = 1:3, pch = 19, trace.label = "sektor",
                 main = "Interakcia veku a sektora", xlab = "vekova kategoria",
                 ylab = "priemerny prijem")
#' Interakcia ma p = 0.925 > 0.1, interakcie su nulove. V modeli
#' s interakciou ma vek p = 0.102, teda tesne nad 0.1. Ciary troch
#' sektorov idu az po vek 46-55 blizko seba. Pri veku 56+ sa rozidu, ale
#' v tejto skupine su len 4 az 6 ludi na sektor.

#'
#'## Vek a vzdelanie
table(prijem$Vek, prijem$Vzdelanie)
levene_test(Prijem ~ Vek * Vzdelanie, data = prijem)
#' Tu su triedy velmi nevyrovnane: dve su prazdne (vek 1 a 3 so vzdelanim
#' 5) a dalsie maju len 1 az 2 ludi. Shapiro-Wilk test sa na ne pouzit
#' neda, normalitu v triedach overit nevieme. Leveneov test ma p = 0.71,
#' disperzie sa rovnaju. ANOVA spravime, ale vysledky treba brat opatrne,
#' na spolahlive overenie predpokladov by bolo treba viac ludi v kazdej
#' triede.
an_vz <- aov(Prijem ~ Vek + Vzdelanie, data = prijem)
summary(an_vz)
eta_squared(an_vz)
#' Vek ma p = 0.078 < 0.1 a vzdelanie p = 0.0023 < 0.1, oba faktory maju
#' na hladine 0.1 vyznamny vplyv. Miera efektu je 0.034 pre vek a 0.068
#' pre vzdelanie.
TukeyHSD(an_vz)
#' Pri veku sa lisi len dvojica 5-4 (p = 0.098), ludia nad 56 rokov maju
#' nizsi prijem ako ludia vo veku 46-55. Pri vzdelani sa lisia 3-1
#' (p = 0.008) a 4-1 (p = 0.013).
an_vz2 <- aov(Prijem ~ Vek * Vzdelanie, data = prijem)
summary(an_vz2)
interaction.plot(prijem$Vek, prijem$Vzdelanie, prijem$Prijem,
                 type = "b", col = 1:5, pch = 19, trace.label = "vzdelanie",
                 main = "Interakcia veku a vzdelania", xlab = "vekova kategoria",
                 ylab = "priemerny prijem")
#' Interakcia ma p = 0.163 > 0.1, interakcie su nulove. Ciary v grafe su
#' velmi rozkolisane, pretoze niektore priemery su len z 1 az 2 ludi.

#'
#'# 4. Priklad 3 - dieta
dieta <- read_xlsx("dieta.xlsx")
str(dieta)
sum(is.na(dieta))
table(dieta$Pohlavie)
#' Data maju 202 klientov (103 zien a 99 muzov), hmotnost pred a po
#' programe, ziadne hodnoty nechybaju. Percentualna strata hmotnosti
#' (100 % je povodna hmotnost):
dieta$strata <- (dieta$pred - dieta$po) / dieta$pred * 100
tapply(dieta$strata, dieta$Pohlavie, summary)
sum(dieta$strata < 0)
boxplot(dieta$strata ~ dieta$Pohlavie, col = c("red", "blue"),
        main = "Percentualna strata hmotnosti", xlab = "pohlavie (F = zeny, M = muzi)",
        ylab = "strata hmotnosti (%)")
#' Zeny schudli v priemere 9.53 %, muzi 9.10 %. Krabice su skoro rovnake.
#' 39 klientov ma stratu zapornu, teda po programe vazili viac ako pred nim.
muzi <- dieta$strata[dieta$Pohlavie == "M"]
zeny <- dieta$strata[dieta$Pohlavie == "F"]

#'
#'## Rozdiel medzi muzmi a zenami
#'
#' Normalitu podla zadania predpokladame. Najprv test rovnosti disperzii:
#'
#' $$H_0 \quad \sigma^2_M=\sigma^2_F \qquad H_1 \quad \sigma^2_M \neq \sigma^2_F$$
var.test(muzi, zeny)
#' P-hodnota je 0.62 > 0.05, rovnost disperzii nezamietame. Pouzijeme
#' dvojvyberovy t-test s rovnakymi disperziami:
#'
#' $$H_0 \quad \mu_M=\mu_F \qquad H_1 \quad \mu_M \neq \mu_F$$
t.test(muzi, zeny, paired = F, var.equal = T)
#' P-hodnota je 0.756 > 0.05, $H_0$ nezamietame. Medzi muzmi a zenami nie je
#' statisticky vyznamny rozdiel v percentualnej strate hmotnosti. 95% IS
#' pre rozdiel priemerov (-3.13 az 2.28) obsahuje nulu.

#'
#'## Predpoklad poradkyne pre zeny
#'
#' Poradkyna predpokladala, ze zeny stratia v priemere 13 %:
#'
#' $$H_0 \quad \mu_F=13 \qquad H_1 \quad \mu_F \neq 13$$
t.test(zeny, mu = 13)
#' P-hodnota je 0.0006 < 0.05, $H_0$ zamietame. Predpoklad poradkyne nebol
#' spravny. Zeny schudli v priemere len 9.53 % a 95% IS (7.58 az 11.48 %)
#' hodnotu 13 neobsahuje - skutocna strata je nizsia, ako poradkyna cakala.

#'
#'# 5. Zaver
#'
#' - Na hladine 0.05 ma na prijem vplyv miesto prace (velky efekt,
#'   $\eta^2 = 0.17$) a vzdelanie (stredny efekt, $\eta^2 = 0.07$). Vek
#'   (p = 0.093) a sektor (p = 0.855) vplyv nemaju.
#' - Vo vacsich mestach (okresne, krajske) je prijem vyssi ako v mensej obci
#'   a mensom meste. Ludia s VS 1. a 2. stupna zarabaju viac ako ludia bez
#'   maturity.
#' - V dvojfaktorovej ANOVA na hladine 0.1 je miesto vyznamne vo vsetkych
#'   kombinaciach, kde vystupuje, a vzdelanie tiez. Sektor vyznamny nie je.
#' - Vek je na hladine 0.1 vyznamny vo vsetkych troch kombinaciach
#'   (p = 0.053, 0.095 a 0.078), jeho vplyv je ale slaby (efekt 0.034).
#'   Post test ukazal, ze nizsi prijem ma hlavne skupina 56+.
#' - Ziadna interakcia nie je vyznamna, faktory posobia na prijem nezavisle.
#' - Pri kombinaciach vek a miesto a vek a vzdelanie su niektore triedy
#'   prazdne alebo maju len 1 az 2 ludi, normalitu v nich nevieme overit,
#'   vysledky preto berieme opatrne.
#' - Dieta: rozdiel v percentualnej strate hmotnosti medzi muzmi (9.1 %)
#'   a zenami (9.5 %) nie je statisticky vyznamny.
#' - Predpoklad poradkyne, ze zeny stratia 13 %, sa nepotvrdil, stratili
#'   menej (9.5 %).
