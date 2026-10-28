#' ---
#' title: "Zadanie 3: Testy pre parametre normalneho rozdelenia - modely pretlacenia"
#' author: "Jozef Barčák"
#' output: html_document
#' ---
#'
#' Subor Modely_pretlacenie.xlsx obsahuje pre styri modely odolnosti dosiek
#' voci pretlaceniu (Eurokod, MC LoA I, MC LoA II, MC LoA III) modelove
#' hodnoty $V_{model}$ a experimentalne hodnoty $V_{exp}$. Presnost modelu
#' posudzujeme podielom $x_i = V_{exp} / V_{model}$. Cielom je overit, ci su
#' modely bezpecne (podiel aspon 1) a konzistentne (mala disperzia podielov).

#+ message = FALSE
library(readxl)
library(EnvStats)

#'
#'# 1. Import dat a vypocet podielov
data <- read_xlsx("Modely_pretlacenie.xlsx")
str(data)
sum(is.na(data))
#' Data maju 55 riadkov a 8 stlpcov, ziadne hodnoty nechybaju. Pre kazdy
#' model je jeden stlpec s modelovou a jeden s experimentalnou hodnotou.
podiely <- data.frame(Eurokod = data$Eexp / data$Eurokod,
                      LoAI = data$LoAIexp / data$MCLoAI,
                      LoAII = data$LoAIIexp / data$MCLoAII,
                      LoAIII = data$LoAIIIexp / data$MCLoAIII)
head(podiely)
summary(podiely)
#' Priemerny podiel je pri vsetkych modeloch nad 1 (Eurokod 1.190, LoA I
#' 1.178, LoA II 1.118, LoA III 1.201). Minimum nad 1 ma ale len LoA I
#' (1.001), ostatne modely maju aj podiely pod 1, LoA II dokonca 0.50.

#'
#'# 2. Priklad 1 - struktura dat
boxplot(podiely, main = "Podiely Vexp / Vmodel podla modelu",
        xlab = "model", ylab = "Vexp / Vmodel")
abline(h = 1, col = "red")

par(mfrow = c(2, 2))
hist(podiely$Eurokod, main = "Eurokod", xlab = "Vexp / Vmodel", ylab = "pocetnost")
hist(podiely$LoAI, main = "MC LoA I", xlab = "Vexp / Vmodel", ylab = "pocetnost")
hist(podiely$LoAII, main = "MC LoA II", xlab = "Vexp / Vmodel", ylab = "pocetnost")
hist(podiely$LoAIII, main = "MC LoA III", xlab = "Vexp / Vmodel", ylab = "pocetnost")
par(mfrow = c(1, 1))

par(mfrow = c(2, 2))
qqnorm(podiely$Eurokod, main = "Eurokod",
       xlab = "teoreticke kvantily", ylab = "vyberove kvantily")
qqline(podiely$Eurokod)
qqnorm(podiely$LoAI, main = "MC LoA I",
       xlab = "teoreticke kvantily", ylab = "vyberove kvantily")
qqline(podiely$LoAI)
qqnorm(podiely$LoAII, main = "MC LoA II",
       xlab = "teoreticke kvantily", ylab = "vyberove kvantily")
qqline(podiely$LoAII)
qqnorm(podiely$LoAIII, main = "MC LoA III",
       xlab = "teoreticke kvantily", ylab = "vyberove kvantily")
qqline(podiely$LoAIII)
par(mfrow = c(1, 1))

shapiro.test(podiely$Eurokod)
shapiro.test(podiely$LoAI)
shapiro.test(podiely$LoAII)
shapiro.test(podiely$LoAIII)
#' Pocet podielov pod 1, teda skusok, kde model odolnost precenil:
sum(podiely$Eurokod < 1)
sum(podiely$LoAI < 1)
sum(podiely$LoAII < 1)
sum(podiely$LoAIII < 1)
#' Cervena ciara v boxplote je hranica bezpecnosti 1. LoA I lezi cely nad
#' nou a ma najuzsi box, jeho podiely su najmenej rozptylene. Eurokod ma
#' vacsinu podielov nad 1, pod 1 je 8 z 55. LoA II a LoA III maju najvacsi
#' rozptyl a pod 1 maju az 16 z 55 podielov. Histogramy su priblizne
#' symetricke a body v Q-Q grafoch lezia blizko priamky. Shapiro-Wilk test
#' ma vsade p-hodnotu vacsiu ako 0.05 (0.68, 0.15, 0.49, 0.14), hypotezu
#' o normalite nezamietame. Podiely mozeme brat ako normalne rozdelene,
#' co suhlasi so zadanim.

#'
#'# 3. Priklad 1 - test bezpecnosti modelov
#'
#' Model je bezpecny, ak je stredna hodnota podielu vacsia ako 1. Podiely su
#' normalne rozdelene a $\sigma$ nepozname, preto pouzijeme jednovyberovy
#' t-test na hladine vyznamnosti $\alpha = 0.05$. Pre kazdy model testujeme
#'
#' $$H_0 \quad \mu=1 \qquad H_1 \quad \mu>1$$
#'
#' Ak $H_0$ zamietneme, prijimame $H_1$ a model je bezpecny.
t.test(podiely$Eurokod, mu = 1, alternative = "g")
t.test(podiely$LoAI, mu = 1, alternative = "g")
t.test(podiely$LoAII, mu = 1, alternative = "g")
t.test(podiely$LoAIII, mu = 1, alternative = "g")
#' Vsetky p-hodnoty su mensie ako 0.05 (Eurokod 1.0e-11, LoA I < 2.2e-16,
#' LoA II 0.0005, LoA III 1.3e-07). Pri vsetkych styroch modeloch zamietame
#' $H_0$ a prijimame $H_1$: stredna hodnota podielu je vacsia ako 1, modely
#' su v priemere bezpecne. Dolne hranice jednostrannych 95% IS su 1.152,
#' 1.155, 1.061 a 1.144, vsetky su nad 1.
#'
#' Test pre LoA II aj graficky. Ak plati $H_0$, statistika t ma Studentovo
#' rozdelenie s 54 stupnami volnosti. Test je pravostranny, kriticka hodnota
#' je kvantil t-rozdelenia pre 0.95.
q <- qt(0.95, 54)
q
tstat <- t.test(podiely$LoAII, mu = 1, alternative = "g")$statistic
tstat
plot(function(x) dt(x, 54), from = -4, to = 4, main = "t-test pre MC LoA II",
     xlab = "t", ylab = "hustota")
abline(v = q, col = "red")
abline(v = tstat, col = "blue")
#' Cervena ciara je kriticka hodnota 1.674, modra je statistika t = 3.48.
#' Statistika lezi napravo od kritickej hodnoty, teda v kritickom obore,
#' preto $H_0$ zamietame. Je to rovnaky zaver ako podla p-hodnoty 0.0005.
#'
#' Ktory model je najlepsi? Idealny podiel je 1. Najblizsie k 1 je priemer
#' LoA II (1.118), lenze tento model ma velky rozptyl a 16 z 55 podielov pod 1.
#' Najlepsie vychadza LoA I: priemer 1.178 je podobny ako pri Eurokode
#' a LoA III, ale ziadny podiel nie je pod 1 a podiely su najmenej
#' rozptylene. LoA I je jediny model, ktory bol bezpecny pri kazdej skuske.

#'
#'# 4. Priklad 2 - nutna poziadavka na disperziu
#'
#' Vyberove disperzie podielov:
var(podiely$Eurokod)
var(podiely$LoAI)
var(podiely$LoAII)
var(podiely$LoAIII)
#' Eurokod 0.0279, LoA I 0.0104, LoA II 0.0635, LoA III 0.0644.
#'
#' Nutna poziadavka je, aby disperzia nepresiahla 0.05. Tvrdenie, ktore
#' chceme preukazat, dame do alternativnej hypotezy, $\alpha = 0.05$:
#'
#' $$H_0 \quad \sigma^2=0.05 \qquad H_1 \quad \sigma^2<0.05$$
varTest(podiely$Eurokod, sigma.squared = 0.05, alternative = "less")
varTest(podiely$LoAI, sigma.squared = 0.05, alternative = "less")
varTest(podiely$LoAII, sigma.squared = 0.05, alternative = "less")
varTest(podiely$LoAIII, sigma.squared = 0.05, alternative = "less")
#' Eurokod (p = 0.0035) a LoA I (p = 6.5e-11) maju p-hodnotu mensiu ako 0.05,
#' zamietame $H_0$ a prijimame $H_1$: disperzia je mensia ako 0.05, nutnu
#' poziadavku splnaju. LoA II (p = 0.91) a LoA III (p = 0.92): $H_0$
#' nezamietame, nutnu poziadavku nesplnaju. Ich vyberova disperzia je
#' dokonca vacsia ako 0.05.

#'
#'# 5. Priklad 2 - idealna poziadavka na disperziu
#'
#' Idealne ma byt disperzia 0.01, $\alpha = 0.05$:
#'
#' $$H_0 \quad \sigma^2=0.01 \qquad H_1 \quad \sigma^2 \neq 0.01$$
varTest(podiely$Eurokod, sigma.squared = 0.01)
varTest(podiely$LoAI, sigma.squared = 0.01)
varTest(podiely$LoAII, sigma.squared = 0.01)
varTest(podiely$LoAIII, sigma.squared = 0.01)
#' LoA I: p = 0.80 > 0.05, $H_0$ nezamietame. Disperzia sa od idealnej 0.01
#' preukazne nelisi, 95% IS pre $\sigma^2$ je 0.0073 az 0.0157 a hodnota 0.01
#' v nom lezi. Eurokod (p = 9.0e-11), LoA II a LoA III (p = 0): $H_0$
#' zamietame, ich disperzia sa od 0.01 lisi a je vacsia (dolne hranice IS
#' su 0.0198, 0.0450 a 0.0457).

#'
#'# 6. Zaver
#'
#' - Podiely $V_{exp} / V_{model}$ vsetkych styroch modelov su normalne
#'   rozdelene, preto sme pouzili t-test pre $\mu$ a chi-kvadrat test
#'   pre $\sigma^2$.
#' - Vsetky styri modely su v priemere bezpecne, stredna hodnota podielu je
#'   preukazne vacsia ako 1.
#' - Eurokod, LoA II a LoA III maju aj skusky s podielom pod 1 (8, 16 a 16
#'   z 55). Iba LoA I ma vsetky podiely aspon 1.
#' - Nutnu poziadavku na disperziu (najviac 0.05) splnaju Eurokod a LoA I.
#'   LoA II a LoA III ju nesplnaju.
#' - Idealnu poziadavku (disperzia 0.01) splna iba LoA I.
#' - Najlepsi je model MC LoA I: je bezpecny, ma najmensi rozptyl a splna obe
#'   poziadavky na konzistentnost.
