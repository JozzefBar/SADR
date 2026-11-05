#' ---
#' title: "Zadanie 5: Citlivost testov normality na symetricke a nesymetricke data"
#' author: "Jozef Barčák"
#' output: html_document
#' ---
#'
#' Cielom je zistit, ako rychlo styri testy normality, Shapiro-Wilk (SW),
#' Anderson-Darling (AD), Jarque-Bera (JB) a Lilliefors (Lill), zamietnu
#' hypotezu
#'
#' $$H_0 \quad \text{data pochadzaju z normalneho rozdelenia}$$
#'
#' Data generujeme z dvoch symetrickych rozdeleni, rovnomerneho $U(0,1)$
#' a Cauchyho $C(0,1)$, a z nesymetrickeho exponencialneho rozdelenia
#' $Exp(1)$. Hladina vyznamnosti je $\alpha = 0.05$, $H_0$ zamietame, ked je
#' p-hodnota mensia ako 0.05.

#+ message = FALSE
library(nortest)
library(tseries)
library(moments)

#'
#'# 1. Vlastnosti rozdeleni
#'
#' Z kazdeho rozdelenia vygenerujeme jednu vzorku s n = 300 a zistime jej
#' tvar, sikmost a spicatost. Pri normalnom rozdeleni je sikmost 0
#' a spicatost 3. Na zaciatku nastavime set.seed(2026), aby sa vysledky
#' dali zopakovat.
set.seed(2026)
rovn <- runif(300)
cauchy <- rcauchy(300)
expo <- rexp(300)

par(mfrow = c(1, 3))
hist(rovn, main = "U(0,1)", xlab = "x", ylab = "pocetnost")
hist(cauchy, main = "C(0,1)", xlab = "x", ylab = "pocetnost")
hist(expo, main = "Exp(1)", xlab = "x", ylab = "pocetnost")
par(mfrow = c(1, 1))

par(mfrow = c(1, 3))
qqnorm(rovn, main = "U(0,1)",
       xlab = "teoreticke kvantily", ylab = "vyberove kvantily")
qqline(rovn)
qqnorm(cauchy, main = "C(0,1)",
       xlab = "teoreticke kvantily", ylab = "vyberove kvantily")
qqline(cauchy)
qqnorm(expo, main = "Exp(1)",
       xlab = "teoreticke kvantily", ylab = "vyberove kvantily")
qqline(expo)
par(mfrow = c(1, 1))

skewness(rovn)
kurtosis(rovn)
skewness(cauchy)
kurtosis(cauchy)
range(cauchy)
skewness(expo)
kurtosis(expo)
#' Rovnomerne rozdelenie ma plochy histogram a hodnoty len medzi 0 a 1.
#' Sikmost 0.06 je takmer 0 (teoreticky 0), rozdelenie je symetricke ako
#' normalne. Spicatost 1.78 (teoreticky 1.8) je ale mensia ako 3,
#' rozdelenie nema chvosty. V Q-Q grafe lezia body v strede na priamke, na
#' oboch koncoch sa od nej odklonia: najmensie hodnoty su vacsie a najvacsie
#' mensie, ako by boli pri normalnom rozdeleni.
#'
#' Cauchyho rozdelenie je symetricke okolo 0, ale ma velmi tazke chvosty.
#' Takmer vsetky hodnoty (286 z 300) su v dvoch stlpcoch medzi -20 a 20,
#' niekolko hodnot je ale extremne daleko (od -64.8 do 176.8). V Q-Q grafe je
#' vacsina bodov na vodorovnej ciare okolo 0 a na koncoch je par bodov
#' daleko od priamky. Cauchyho rozdelenie nema strednu hodnotu ani
#' disperziu, preto teoreticka sikmost a spicatost neexistuju a vyberove
#' hodnoty vyjdu pri kazdej vzorke ine. V tejto vzorke je sikmost 6.20
#' a spicatost 60.3, urcuje ich niekolko extremnych hodnot.
#'
#' Exponencialne rozdelenie ma len kladne hodnoty a dlhy pravy chvost.
#' Sikmost 1.81 a spicatost 6.71 (teoreticky 2 a 9) su daleko od 0 a 3.
#' V Q-Q grafe tvoria body obluk, najvacsie hodnoty su vysoko nad priamkou.

#'
#'# 2. Postup simulacie
#'
#' Velkosti vzoriek su n = 20, 40, ..., 300:
n <- seq(20, 300, by = 20)
n
#' Pre kazde n vygenerujeme z kazdeho rozdelenia jednu vzorku a vypocitame
#' p-hodnoty vsetkych styroch testov. P-hodnoty ukladame do tabulky
#' (data.frame), jeden riadok je jedno n. Potom vypiseme, pri ktorych n je
#' p-hodnota mensia ako 0.05 (TRUE znamena, ze $H_0$ zamietame).
#'
#' Test zamieta $H_0$ konzistentne, ked je p-hodnota mensia ako 0.05 pri
#' danom n aj pri vsetkych vacsich n az po 300. Za $n_{min}$ preto berieme
#' najmensie n, od ktoreho je v tabulke TRUE az do konca. Ak test ani pri
#' n = 300 konzistentne nezamieta, piseme $n_{min} > 300$.
#'
#' Pre kazde rozdelenie nakreslime styri grafy vyvoja p-hodnoty v zavislosti
#' od n, jeden pre kazdy test. Cervena ciara je hladina 0.05.

#'
#'# 3. Rovnomerne rozdelenie U(0,1)
p_rovn <- data.frame(n = n, SW = 0, AD = 0, JB = 0, Lill = 0)
for (i in 1:length(n)) {
  x <- runif(n[i])
  p_rovn[i, 2:5] <- c(shapiro.test(x)$p.value, ad.test(x)$p.value,
                      jarque.bera.test(x)$p.value, lillie.test(x)$p.value)
}
round(p_rovn, 4)
p_rovn[, 2:5] < 0.05

par(mfrow = c(2, 2))
plot(p_rovn$n, p_rovn$SW, type = "b", ylim = c(0, 1),
     main = "U(0,1): Shapiro-Wilk", xlab = "n", ylab = "p-hodnota")
abline(h = 0.05, col = "red")
plot(p_rovn$n, p_rovn$AD, type = "b", ylim = c(0, 1),
     main = "U(0,1): Anderson-Darling", xlab = "n", ylab = "p-hodnota")
abline(h = 0.05, col = "red")
plot(p_rovn$n, p_rovn$JB, type = "b", ylim = c(0, 1),
     main = "U(0,1): Jarque-Bera", xlab = "n", ylab = "p-hodnota")
abline(h = 0.05, col = "red")
plot(p_rovn$n, p_rovn$Lill, type = "b", ylim = c(0, 1),
     main = "U(0,1): Lilliefors", xlab = "n", ylab = "p-hodnota")
abline(h = 0.05, col = "red")
par(mfrow = c(1, 1))
#' Pri n = 20 su p-hodnoty 0.167 (SW), 0.210 (AD), 0.495 (JB) a 0.177
#' (Lill), pri n = 40 a 60 su vsetky vacsie ako 0.24. Do n = 60 teda ziadny
#' test $H_0$ nezamieta. Pri n = 80 klesnu pod 0.05 SW (0.0046), AD (0.0084)
#' aj Lill (0.0265) a SW a AD tam uz zostanu. JB ma pri n = 80 este 0.086,
#' pod 0.05 klesne az pri n = 100 (0.025) a zostane tam. Lilliefors
#' nezamieta konzistentne: pri n = 120 ma p-hodnotu 0.064 a pri n = 160
#' az 0.144, na grafe je to vidno ako skoky nad cervenu ciaru. Pod 0.05
#' zostane az od n = 180.
#'
#' $n_{min}$: SW 80, AD 80, JB 100, Lill 180.

#'
#'# 4. Cauchyho rozdelenie C(0,1)
p_cauchy <- data.frame(n = n, SW = 0, AD = 0, JB = 0, Lill = 0)
for (i in 1:length(n)) {
  x <- rcauchy(n[i])
  p_cauchy[i, 2:5] <- c(shapiro.test(x)$p.value, ad.test(x)$p.value,
                        jarque.bera.test(x)$p.value, lillie.test(x)$p.value)
}
round(p_cauchy, 4)
p_cauchy[, 2:5] < 0.05

par(mfrow = c(2, 2))
plot(p_cauchy$n, p_cauchy$SW, type = "b", ylim = c(0, 1),
     main = "C(0,1): Shapiro-Wilk", xlab = "n", ylab = "p-hodnota")
abline(h = 0.05, col = "red")
plot(p_cauchy$n, p_cauchy$AD, type = "b", ylim = c(0, 1),
     main = "C(0,1): Anderson-Darling", xlab = "n", ylab = "p-hodnota")
abline(h = 0.05, col = "red")
plot(p_cauchy$n, p_cauchy$JB, type = "b", ylim = c(0, 1),
     main = "C(0,1): Jarque-Bera", xlab = "n", ylab = "p-hodnota")
abline(h = 0.05, col = "red")
plot(p_cauchy$n, p_cauchy$Lill, type = "b", ylim = c(0, 1),
     main = "C(0,1): Lilliefors", xlab = "n", ylab = "p-hodnota")
abline(h = 0.05, col = "red")
par(mfrow = c(1, 1))
#' Uz pri n = 20 su p-hodnoty SW (0.0153), AD (0.0139) a JB (0.0058)
#' mensie ako 0.05. Lilliefors ma pri n = 20 p-hodnotu 0.0532, tesne nad
#' 0.05. Od n = 40 su vsetky p-hodnoty najviac 0.0034 (JB pri n = 60), na
#' grafoch lezia body na nule.
#'
#' $n_{min}$: SW 20, AD 20, JB 20, Lill 40.

#'
#'# 5. Exponencialne rozdelenie Exp(1)
p_expo <- data.frame(n = n, SW = 0, AD = 0, JB = 0, Lill = 0)
for (i in 1:length(n)) {
  x <- rexp(n[i])
  p_expo[i, 2:5] <- c(shapiro.test(x)$p.value, ad.test(x)$p.value,
                      jarque.bera.test(x)$p.value, lillie.test(x)$p.value)
}
round(p_expo, 4)
p_expo[, 2:5] < 0.05

par(mfrow = c(2, 2))
plot(p_expo$n, p_expo$SW, type = "b", ylim = c(0, 1),
     main = "Exp(1): Shapiro-Wilk", xlab = "n", ylab = "p-hodnota")
abline(h = 0.05, col = "red")
plot(p_expo$n, p_expo$AD, type = "b", ylim = c(0, 1),
     main = "Exp(1): Anderson-Darling", xlab = "n", ylab = "p-hodnota")
abline(h = 0.05, col = "red")
plot(p_expo$n, p_expo$JB, type = "b", ylim = c(0, 1),
     main = "Exp(1): Jarque-Bera", xlab = "n", ylab = "p-hodnota")
abline(h = 0.05, col = "red")
plot(p_expo$n, p_expo$Lill, type = "b", ylim = c(0, 1),
     main = "Exp(1): Lilliefors", xlab = "n", ylab = "p-hodnota")
abline(h = 0.05, col = "red")
par(mfrow = c(1, 1))
#' Uz pri n = 20 su vsetky p-hodnoty mensie ako 0.05 (najvacsia je Lill
#' 0.0042) a pri vsetkych vacsich n tam zostanu. Najvacsia p-hodnota v celej
#' tabulke je 0.0170 (JB pri n = 80).
#'
#' $n_{min}$: SW 20, AD 20, JB 20, Lill 20, teda uz najmensia skumana
#' velkost vzorky.

#'
#'# 6. Zdovodnenie
#'
#' Rozdelenia:
#'
#' - Exponencialne rozdelenie je nesymetricke (sikmost 1.81) a ma dlhy pravy
#'   chvost. Od normalneho sa lisi uz tvarom, preto ho vsetky testy odhalili
#'   pri n = 20.
#' - Cauchyho rozdelenie je symetricke, ale ma tazke chvosty a extremne
#'   hodnoty. Aj v malej vzorke sa objavi par hodnot daleko od ostatnych,
#'   preto ho testy odhalili pri n = 20, Lilliefors pri n = 40.
#' - Rovnomerne rozdelenie je symetricke ako normalne a nema extremne
#'   hodnoty. Lisi sa len tym, ze je ploche a nema chvosty (spicatost 1.8
#'   namiesto 3). V malej vzorke to nevidno, preto sa odhaluje najtazsie
#'   a testy potrebovali n od 80 do 180.
#'
#' Testy:
#'
#' - Shapiro-Wilk porovnava usporiadane data s hodnotami, ktore by sme cakali
#'   pri normalnom rozdeleni, podobne ako Q-Q graf. Pri rovnomernom
#'   rozdeleni sa body v Q-Q grafe na koncoch odklonia od priamky. SW mal
#'   pri n = 20 az 60 zo styroch testov najmensiu p-hodnotu a $n_{min}$ = 80.
#' - Anderson-Darling porovnava distribucnu funkciu dat s normalnou a viac
#'   vazi chvosty. Rovnomerne rozdelenie sa od normalneho lisi prave na
#'   koncoch, kde nema chvosty, preto mal AD rovnake $n_{min}$ ako SW (80).
#' - Lilliefors je KS test, pri ktorom strednu hodnotu a smerodajnu odchylku
#'   odhadujeme z dat (obycajny ks.test potrebuje zname parametre). Berie len
#'   najvacsi rozdiel distribucnych funkcii. Pri rovnomernom rozdeleni je
#'   tento rozdiel maly, preto Lilliefors potreboval najvacsie n (180)
#'   a jeho p-hodnoty preskakovali okolo 0.05.
#'
#' Jarque-Bera pracuje iba so sikmostou S a spicatostou K,
#'
#' $$JB = \frac{n}{6}\left(S^2 + \frac{(K-3)^2}{4}\right),$$
#'
#' a $H_0$ zamieta, ked je JB vacsie ako kvantil chi-kvadrat rozdelenia
#' s 2 stupnami volnosti.
jarque.bera.test(rovn)
300 / 6 * (skewness(rovn)^2 + (kurtosis(rovn) - 3)^2 / 4)
qchisq(0.95, 2)
(kurtosis(rovn) - 3)^2 / 4 / 6
#' Vzorec dava pre vzorku s n = 300 rovnaku hodnotu ako jarque.bera.test
#' (18.9). Pri rovnomernom rozdeleni je S priblizne 0 a K priblizne 1.78,
#' teda JB je len okolo 0.0625 krat n. Kriticku hodnotu 5.99 prekroci az pri
#' n okolo 96 (5.99 / 0.0625), s teoretickou spicatostou 1.8 pri n okolo 100.
#' Preto mal JB pri n = 20 az 60 p-hodnoty 0.40 az 0.50, pri n = 80 este
#' 0.086 a konzistentne zamietal az od n = 100. Pri Cauchyho
#' a exponencialnom rozdeleni su S a K velke uz v malej vzorke, JB tam
#' zamietol hned pri n = 20.

#'
#'# 7. Zaver
#'
#' - Minimalne velkosti vzorky $n_{min}$, od ktorych test zamieta $H_0$
#'   konzistentne:
#'   - U(0,1): SW 80, AD 80, JB 100, Lill 180.
#'   - C(0,1): SW 20, AD 20, JB 20, Lill 40.
#'   - Exp(1): SW 20, AD 20, JB 20, Lill 20.
#' - Nesymetricke exponencialne rozdelenie a Cauchyho rozdelenie s tazkymi
#'   chvostami testy odhalia uz pri malych vzorkach (n = 20 az 40).
#' - Najtazsie sa odhaluje rovnomerne rozdelenie: je symetricke a nema
#'   chvosty, do n = 60 ho nezamietol ziadny test.
#' - Najcitlivejsi bol Shapiro-Wilk, pri rovnomernom rozdeleni mal najmensie
#'   p-hodnoty a spolu s AD najmensie $n_{min}$ (80).
#' - Najslabsi bol Lilliefors (pri rovnomernom rozdeleni 180, pri Cauchyho
#'   40). JB potreboval pri rovnomernom rozdeleni 100, lebo pracuje len so
#'   sikmostou a spicatostou a pri malom n je jeho statistika mala.
#' - Pre kazde n mame len jednu vzorku, presne hodnoty $n_{min}$ preto
#'   zavisia od vygenerovanych dat.
