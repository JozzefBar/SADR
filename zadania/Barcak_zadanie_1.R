#' ---
#' title: "Zadanie 1: Analyza morfologie antarktickych tucniakov (EDA)"
#' author: "Jozef Barčák"
#' output: html_document
#' ---
#'
#' Cielom je preskumat telesne proporcie tucniakov z datasetu palmerpenguins,
#' najst rozdiely medzi podskupinami (druh, pohlavie) a overit vztahy medzi
#' meranymi velicinami.

#+ message = FALSE
# install.packages("palmerpenguins")
library(palmerpenguins)
library(mice)
library(psych)
library(moments)
library(DescTools)

#'
#'# 1. Zakladny pohlad na data
#'
#' Dataset ma 344 tucniakov a 8 premennych. Kategoricke (factor) su species,
#' island a sex, ciselne su bill_length_mm, bill_depth_mm, flipper_length_mm,
#' body_mass_g a rok merania (year).
str(penguins)
summary(penguins)

#'
#'# 2. Chybajuce data
sum(is.na(penguins$sex))
sum(is.na(penguins$bill_length_mm))
sum(is.na(penguins$body_mass_g))
md.pattern(penguins, rotate.names = TRUE)
#' Pohlavie chyba u 11 tucniakov, ciselne miery u 2.
#' Prislusne riadky vynechame a pracujeme s uplnym datasetom.
tucniaky <- na.omit(penguins)
str(tucniaky)
#' Ostalo 333 tucniakov.

#'
#'# 3. Pocetnosti v skupinach
table(tucniaky$species)
prop.table(table(tucniaky$species))
table(tucniaky$island)
table(tucniaky$sex)
table(tucniaky$species, tucniaky$island)
plot(tucniaky$species, main = "Pocet tucniakov podla druhu",
     xlab = "druh", ylab = "pocet")
#' Adelie je 146, Gentoo 119, Chinstrap 68. Samcov a samic je skoro rovnako
#' (168 a 165). Gentoo zije len na ostrove Biscoe, Chinstrap len na Dream,
#' Adelie na vsetkych troch.

#'
#'# 4. Charakteristiky ciselnych premennych
describe(tucniaky[, c("bill_length_mm", "bill_depth_mm",
                      "flipper_length_mm", "body_mass_g")])
#' Variacny koeficient:
CoefVar(tucniaky$bill_length_mm)
CoefVar(tucniaky$bill_depth_mm)
CoefVar(tucniaky$flipper_length_mm)
CoefVar(tucniaky$body_mass_g)
#' Sikmost a spicatost:
moments::skewness(tucniaky$body_mass_g)
moments::kurtosis(tucniaky$body_mass_g)
moments::skewness(tucniaky$flipper_length_mm)
moments::kurtosis(tucniaky$flipper_length_mm)
#' Priemerny tucniak vazi 4207 g, ma plutvu 201 mm a zobak dlhy 44 mm
#' a hlboky 17 mm. Hmotnost ma kladnu sikmost, je teda mierne zosikmena
#' doprava (viac lahsich tucniakov, menej velmi tazkych). Spicatost hmotnosti
#' (2.26) aj dlzky plutvy (2.04) je mensia ako 3, obe rozdelenia su teda
#' plochsie ako normalne rozdelenie.

#'
#'# 5. Histogramy
hist(tucniaky$bill_length_mm, main = "Dlzka zobaka", xlab = "mm")
hist(tucniaky$bill_depth_mm, main = "Hlbka zobaka", xlab = "mm")
hist(tucniaky$flipper_length_mm, main = "Dlzka plutvy", xlab = "mm")
hist(tucniaky$body_mass_g, main = "Hmotnost", xlab = "g")
#' Histogram dlzky plutvy ma dva vrcholy - v datach su zmiesane druhy
#' s roznou velkostou.

#'
#'# 6. Rozdiely medzi druhmi
tapply(tucniaky$body_mass_g, tucniaky$species, summary)
tapply(tucniaky$flipper_length_mm, tucniaky$species, mean)
tapply(tucniaky$bill_length_mm, tucniaky$species, mean)
tapply(tucniaky$bill_depth_mm, tucniaky$species, mean)

boxplot(tucniaky$body_mass_g ~ tucniaky$species,
        main = "Hmotnost podla druhu", xlab = "druh", ylab = "g")
boxplot(tucniaky$flipper_length_mm ~ tucniaky$species,
        main = "Dlzka plutvy podla druhu", xlab = "druh", ylab = "mm")
boxplot(tucniaky$bill_length_mm ~ tucniaky$species,
        main = "Dlzka zobaka podla druhu", xlab = "druh", ylab = "mm")
boxplot(tucniaky$bill_depth_mm ~ tucniaky$species,
        main = "Hlbka zobaka podla druhu", xlab = "druh", ylab = "mm")
#' Gentoo je najvacsi (priemer 5092 g, plutva 217 mm), Adelie (3706 g)
#' a Chinstrap (3733 g) vazia skoro rovnako. Gentoo ma ale najplytsi zobak
#' (15 mm). Adelie a Chinstrap sa lisia hlavne dlzkou zobaka (39 mm vs 49 mm).

#'
#'# 7. Rozdiely medzi pohlaviami
tapply(tucniaky$body_mass_g, tucniaky$sex, mean)
tapply(tucniaky$body_mass_g, list(tucniaky$species, tucniaky$sex), mean)
boxplot(tucniaky$body_mass_g ~ tucniaky$sex,
        main = "Hmotnost podla pohlavia", xlab = "pohlavie", ylab = "g")
boxplot(tucniaky$body_mass_g ~ tucniaky$sex + tucniaky$species,
        main = "Hmotnost podla pohlavia a druhu",
        xlab = "pohlavie a druh", ylab = "g", cex.axis = 0.6)
#' Samce su tazsie ako samice (4546 g vs 3862 g) a plati to v kazdom druhu
#' zvlast (vidno z tabulky druh x pohlavie aj z boxplotu).

#'
#'# 8. Vztahy medzi velicinami
plot(tucniaky$flipper_length_mm, tucniaky$body_mass_g,
     xlab = "dlzka plutvy (mm)", ylab = "hmotnost (g)")
cor(tucniaky$flipper_length_mm, tucniaky$body_mass_g)
cor(tucniaky$bill_length_mm, tucniaky$body_mass_g)
cor(tucniaky$bill_depth_mm, tucniaky$body_mass_g)
#' Dlzka plutvy a hmotnost spolu silno suvisia (korelacia 0.87) - cim dlhsia
#' plutva, tym tazsi tucniak. Dlzka zobaka s hmotnostou suvisi stredne (0.59),
#' hlbka zobaka ma s hmotnostou zapornu korelaciu (-0.47).
#'
#' Dlzka a hlbka zobaka - najprv vsetci spolu:
plot(tucniaky$bill_length_mm, tucniaky$bill_depth_mm,
     xlab = "dlzka zobaka (mm)", ylab = "hlbka zobaka (mm)")
cor(tucniaky$bill_length_mm, tucniaky$bill_depth_mm)
#' Potom kazdy druh zvlast:
adelie <- tucniaky[tucniaky$species == "Adelie", ]
chinstrap <- tucniaky[tucniaky$species == "Chinstrap", ]
gentoo <- tucniaky[tucniaky$species == "Gentoo", ]

par(mfrow = c(1, 3))
plot(adelie$bill_length_mm, adelie$bill_depth_mm, main = "Adelie",
     xlab = "dlzka zobaka (mm)", ylab = "hlbka zobaka (mm)")
plot(chinstrap$bill_length_mm, chinstrap$bill_depth_mm, main = "Chinstrap",
     xlab = "dlzka zobaka (mm)", ylab = "hlbka zobaka (mm)")
plot(gentoo$bill_length_mm, gentoo$bill_depth_mm, main = "Gentoo",
     xlab = "dlzka zobaka (mm)", ylab = "hlbka zobaka (mm)")
par(mfrow = c(1, 1))

cor(adelie$bill_length_mm, adelie$bill_depth_mm)
cor(chinstrap$bill_length_mm, chinstrap$bill_depth_mm)
cor(gentoo$bill_length_mm, gentoo$bill_depth_mm)
#' Pre vsetkych spolu je korelacia zaporna (-0.23), ale v kazdom druhu zvlast
#' je kladna (Adelie 0.39, Chinstrap 0.65, Gentoo 0.65). Zaporny vysledok vznikol
#' len preto, ze sme zmiesali druhy - preto treba druhy skumat oddelene.

#'
#'# 9. Zaver
#'
#' - Po vyhodeni NA mame 333 tucniakov troch druhov, pohlavia su vyvazene.
#' - Gentoo je vyrazne vacsi ako ostatne dva druhy. Adelie a Chinstrap vazia
#'   priblizne rovnako, odlisit ich mozno najma podla dlzky zobaka.
#' - Samce su v kazdom druhu tazsie ako samice.
#' - Dlzka plutvy a hmotnost spolu silno kladne suvisia.
#' - Ked zoberieme vsetky druhy spolu, dlhsi zobak vyzera plytsi. V ramci
#'   kazdeho druhu je to ale naopak, preto treba druhy skumat oddelene.
