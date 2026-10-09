#' ---
#' title: "Testy hypotez pre parametre normálneho rozdelenia"
#' author: "Jozef Barčák"
#' ---

library(latexpdf) #pouzitie tex-u, grafy
library(latex2exp) # bezne vzorce lat-tex

#'# Testy hypotez pre parametre normalneho rozdelenia 
#' 
#'# Testy pre strednu hodnotu $\mu$ a $\sigma$ pozname
#'
#'priklad> udaje o dlzke casu v sekundach
#'ktory strávia pouzivatelia na www stránke, disperzia je znama
#'$\sigma=3.8$
#'1. Testujte hypotezu, ze klient stravi na stranke v priemere
#' 55s. Hladina významnosti $\alpha=0.05$
#' Tvar hypotéz, overenie normality
#' $$H_0 \quad \mu=55 \qquad H_1 \quad \mu \neq 55$$.   # takto aj na skuske
#' Grafy na overenie normality zhody rozdelenia, default normalne 

cas <- c(48, 55, 51, 62, 53, 58, 60, 50, 49, 57, 52, 61, 54, 56, 59,53,
         50, 58, 55, 53)
qqnorm(cas) #kvantil kvantil graf
qqline(cas)#pridat ciaru
#'Shapiro  wilk test
shapiro.test(cas)
#' P hodnota = 0.74>0.05, nezamietame hypotezu o normalite dat
#' Pozname disperziu, teda Z test kniznica DescTools
library(DescTools)
ZTest(cas,mu=58,sd_pop=3.8)
ZTest(cas,mu=58,sd_pop=3.8)$p.value
#' Rozhodnutie podla Pvalue=0.72>0,05, nezamietame $H_0$. Ano, klient
#' strávi na stránke v priemere 55s
#' kniznica broom, ktora umozni dat prehladnejsi vystup
broom::tidy(ZTest(cas,mu=58,sd_pop=3.8))
#' Nová Hypoteza:
#' Otazka v skutocnosti znela, ci zotrvaju na stranke aspon 58s.
#' $$H_0 \quad \mu=58 \qquad \mu>58$$
ZTest(cas, mu=58,alternative="g",sd_pop=3.8)
#' Phodnota=0.99>0.05, nezamietame $H_0$, nie je pravda, ze zotrvaju
#' viac ako 58 sekund
#' Ak by sme testovali napriklad na hladine vyznamnosti
#' $\alpha=0.1$, aj tak rozhodujeme : ak Phodnota >0.1, tak nezamietame nulovu hypotezu
#' Test graficky, ten prvy test
plot(function(x)dnorm(x,0,1),from=3, to=3, main = TeX("Text pre $\\mu$"),
     ylab="hustota")
q <- qnorm(0.975, 0.1)#kvantil normalneho rozdelenia, hranica
z <- -0.35#statistika z vystupu testu
abline(v=z,col="blue")
abline(v=q,col="red")
abline(v=-q,col="red")
#'# Test pre $\mu$ ak $\sigma$ nepozname
#' Testujeme testy z predosleho prikladu
#' za predpokladu, $\sigma$ nepozname
t.test(cas,mu=55)
t.test(cas,mu=55)$p.value
#' phodnota> 0.05 nezamietame $H_O$ tvrdenie plati.
#' Druha hypoteza
t.test(cas,mu=58,alternative="g")
#' Phodnota>0.05, nezamietame $H_O$, teda nie je pravda,
#' ze vydrzia viac ako 58 sekúnd
#'# Plnicka jablkovej stavy
#'
#'Firma predava jablkovu stavu v 0.5l baleniach. Plniaca
#'linka presla servisom. Potom sme namerali tieto hodnoty plneneho objemu v ml. Na hladine
#'vyznamnosti $\alpha=0.1$, testujte hypotezu, ze linka je dobre nastavena
#'
#'$$H_O \quad \mu=500 \qquad H_1 \quad \mu \neq 500$$
#'
jablko <- c(499.2,496.8,502.1,498.5,501,503,500.7,
            501.5,501.8,499.1,500.9,502.2,501.7, 500.4,
            500.2,501.1,499.9,500.2,501.1,500.8,499.3)

shapiro.test(jablko)#normalne rozdelene, ak
t.test(jablko,mu=500)# nemusim dat hladinu vyznamnosti
#'Kedze $\alpha=0.1$ a Phodnota=0.09<0.1 zamietame H_0
#' Ak by bola 0.05, .09>0:05 tak by sme nezamietali
#' 
#'# Zlozene alternativy
#'
#'Firma, ktora vyraba baterie do netebookov tvrdí, ze
#'jednu vyrobí v tom procese do 13 minút. Overte toto tvrdenie, ak
#'mate k dispozicii 20 merani, $\alpha=0.05$. Stanovte spravne 
#'nulovu a alternativnu hypotezu

baterie <- c(12,19,16,13,15,12,14,20,15,19,17,20,
             13,9,11,20,12,19,8,13)
shapiro.test(baterie)#normalne rozdelene
#' $$ H_O \quad \mu=13 \qquad H_1 \quad \mu<13$$
t.test(baterie, mu=13,alternative="l")
#' phodnota=0.97>0.05, nezamierame nulovu hypotezu,
#' neprijimame alternativy, trva mu to viac, odhad strednej hodnoty je 14.8$
#' 
#' Vyrobca tvrdi, ze baterky do elekronickych pristrojov
#' vydrzia aspon 19 hodin nepretrzitej prevádzky. Testujte,
#' ak máme k dispozicii dátový súbor baterie1. Ak
#' $\alpha$ dalej neuvedieme, bude 0.05.
#' 

baterie1<- c(18.2,19.6,18.6,19.4,17,18.5,18,18.4,19,
             18,17.9,18.1)
shapiro.test(baterie1)#normalne rozdelene
#' $$H_O \quad \mu=19 \qquad H_1 \quad \mu>19$$
t.test(baterie1, mu=19, alternative="g")
#' Phodnota>0.05, nezamietame nulovu hypotezu
#' nevydrzia viac ako 19 hodin
t.test(baterie1,mu=19,alternative="l")#len ci dojde k sporu
#' Tu HO zamietame prijme H0, co nie je spor
#' Vydrzia menej ako 19
#' 
#' 
#' Priklad
#' 
#' Pizzeria ABC ma na letaku uveden, ze pizzu dovezu do 30 min. Casy dodavok su v datovom subore pizza.
#' Testujte na hladine vyznamnosti $\aplha=0.05$. Vyslovte zaver 
#' 
pizza <- c(27,25,30,28,29,24,30,26,28,32,
           24,32,31,29,28,29,35,34,30,31,18,22)

shapiro.test(pizza)# overenie normality
#' Ak Phodnota>0.05, nezamietame hypotezu o normalite dat
#' $\sigma$ nepozname, teda t test
#' $$H_0 \quad \mu=30 \qquad H_1 \quad \mu<30$$
t.test(pizza, mu=30, alternative="l")
t.test(pizza, mu=30, alternative="l")$p.value
#' Ak Phodnota<0.05: zamietame $H_0$, prijimame $H_1$,
#' pizzu dovezu v priemere do 30 minut, tvrdenie na letaku plati.
#' Ak Phodnota>0.05: nezamietame $H_0$,
#' nepreukazalo sa, ze pizzu dovezu do 30 minut.

#'#testy pre disperziu normalneho rozdelenia
#'
#'V prvom priklade sme tvrdili, ze $\sigma=3.8$, datovy subor cas.
#'Na hladine vyznamnosti $\aplha=0.05$ overte toto tvrdenie
#'
#' $$H_0 \quad \sigma^2=3.8^2 \qquad H_1 \quad  \sigma^2 \neq 3.8^2
#' Kniznica DescTool, EnvStats
EnvStats::varTest(cas,sigma.squared = 3.8^2)
EnvStats::varTest(cas,sigma.squared = 3.8^2)$p.value
#'Nezamietame $H_0
#'
#'Dvojvyberove testy
#'
#'Merania idu v pare, vytvorime diferencie a zie zmysluplne testujeme
#'Su dane casy v sekundach, pred a po, pocas ktorych riesili ziaci kontrolne ulohy
#'pred a po cviceniach z pamatoveho pocitania. Zlepsili cvicenia schopnost ziakov riesit ulohy?
#'
#'
#' $$H_0 \quad \mu_{pred}-\mu_{po}=0 \qquad H_1 \mu_{pred}-\mu_{po}>0$$
pred <- c(87,61,98,90,93,74,83,72,81,75,83)
po <- c(50,45,79,90,88,65,52,79,84,61,52)
d <- pred-po
shapiro.test(d)
t.test(d,alternative = "g")
#' Phodnota=0.005<0.05 zamietame H0 teda prijimame H1, zlepsili sa
t.test(pred,po,alternative="g", paired=T)



