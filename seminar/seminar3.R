#' ---
#' title: "Intervaly spoľahlivosti"
#' author: "Jozef Barčák"
#' ---

library(latexpdf) #pouzitie tex-u, grafy
library(latex2exp) # bezne vzorce lat-tex

#' priklad prednaska.udaje o dlzke casu v sekundach
#' ktory stravia pouzivatelia na www stranke
x <-c(48,55,51,62,53,58,60,50,49,57,52,61,54,56,59,53,50,58,55,53)
length(x)
mean(x)
boxplot(x,horizontal=T)
#' test na ooverenie normality
shapiro.test(x)
#' kedze P hodnota >0,05, nezamietame hypotezu o normalite
#'
#'# IS pre $\mu$ ak $\sigma^2$ poznanie
#'Vypocitajte 95% IS pre $\mu$ ak $\sigma=3.8$, obojstranny aj jednostranne
#'Najpr podla vzorca a potom pomocou prikazov R 
#'
alfa <- 0.05
n <- length(x)
sigma <- 3.8
#' prislusne kvantily
qnorm(1-alfa/2)
IS <- mean(x)+c(-1,1)*qnorm(1-alfa/2)*sigma/sqrt(n)
IS
ISJ <- c(mean(x)-qnorm(1-alfa)*sigma/sqrt(n),Inf)
ISJ
ISJ <- c(-Inf, mean(x)+qnorm(1-alfa)*sigma/sqrt(n))
ISJ
#' prikazmi R, IS su prepojenie s testami, takze zvycajne
#' ma prikaz tvar....test a vo vystupe
#' najdeme IS
library(DescTools)
#' Defaultne $\alpha=0.05$
ZTest(x, sd_pop=3.8)
ZTest(x, sd_pop=3.8)$conf.int
#' jednostranne
ZTest(x, sd_pop=3.8, alternative="less")$conf.int
ZTest(x, sd_pop=3.8, alternative="greater")$conf.int
#' zmena alfy, kontruujeme 90%,  $\alpha=0.1$

ZTest(x, sd_pop=3.8, conf.level=0.9)$conf.int
#'# IS pre $\mu$ ak $\sigma$ nepozname
#'
#' najcastejsie pouzivany interval, t test v standardnej kniznic
#' Rieste predosle ulohy za predpokladu
#' ze $\sigma$ nepozname
t.test(x) #95%
t.test(x)$conf.int
#' tieto IS sa daju ratat aj pomocou inych
#' kniznic, napr. mosaic
library(mosaic)
t.test(x)
#' spravme aj jednostranne a 90% IS
t.test(x, alternative="l")$conf.int
t.test(x, alternative="g")$conf.int
t.test(x,conf.level=0.9)
#'Nakerslime is pomocou plotrix, nie celkom nazorne
#'lavy a pravy bod IS vlozime do premennych
dd <- t.test(x)$conf.int[1]
dd
hh <- t.test(x)$conf.int[2]
library(plotrix)
plotCI(1,mean(x),li=dd,ui=hh,main="IS pre strednu hodnotu")
#' V praxi vsak chcem casto ratat IS pre rozne podmnoziny
data <- readxl::read_xlsx("~/Sadr/cvicenia/data/data_vyuka.xlsx")
new_data <- na.omit(data)
#'# Rmisc
#' IS pre podmnoziny dat
library(Rmisc)
CI(x)#95%
CI(x,ci=0.9)#90#
CI_pohlavie <- group.CI(mprij~pohlavie,data=new_data)
CI_pohlavie
group.CI(mprij~vzdelanie,data=new_data)
#' cez apply
tapply(new_data$mprij,new_data$vzdelanie,t.test)
#' kniznica ggplot2
#' priprava dat
library(ggplot2)
data <- data.frame(skupina=c("muzi","zeny"),priemer=c(1067.045,1157.609),
                   dolna=c(933.66,1008.71),horna=c(1200.4,1306.49))
data
ggplot(data,aes(x=skupina,y=priemer,ymin=dolna,ymax=horna))+geom_pointrange(color="blue",size=0.8,fatten=3)+
         theme_minimal(base_size = 14)
#'# IS pre $\sigma^2$
#'Nemozno pouzit standartny var.test namiesto tohto prikaz
VarTest(x)#%95
VarTest(x,conf.level=0.9)#90#
VarTest(x,alternative="l")
VarTest(x,alternative="g")
#'# Overenie spolahlivosti metody
#'
#'Simulujeme 100x nahodny vyber z normalneho rozdelenia
#' $n=30, \mu=3, \sigma=1$
k <- 0
for(i in 1:100){a <- rnorm(30,mean=3,sd=1)
    is <- t.test(a)$conf.int
    if (3 <is[[1]]||3>is[[2]]){
      k <- k + 1
      }
    }
k
#'# Vyukova kniznica TeachingDemos
library(TeachingDemos)
ci.examp()
run.ci.examp()

#'# Bootstrapove IS
#'
#'Mnozina, ktora nema normalne rozdelenie, nie standartny CI pre $\mu$
data <- c(2,2,3,4,5,11,3,12,15,21)
shapiro.test(data)
#' P hodnota <0.05, zamietam hypotezu o normalite dat 
hist(data)
moments:skewness(data)
set.seed(2026)
resamble(data)#jedna
mean(data)
mean(resample(data))
boot_data <- do(10000*mean(resamble(data)))
boot_data
hist(boot_data$mean)
confint(boot_data)

