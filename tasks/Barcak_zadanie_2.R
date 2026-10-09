#' ---
#' title: "Zadanie 2: Porovnanie intervalov spolahlivosti spotreby staveniska"
#' author: "Jozef Barčák"
#' output: html_document
#' ---
#'
#' Subor Kontajner_spotreba.xlsx obsahuje hodinove merania spotreby elektrickej
#' energie (kWh) kontajnerov na stavenisku. Cielom je zostavit intervaly
#' spolahlivosti (IS) pre priemernu spotrebu a porovnat ich podla hodiny,
#' dna v tyzdni, typu dna a mesiaca.

#+ message = FALSE
library(readxl)
library(Rmisc)
library(ggplot2)

#'
#'# 1. Import a uprava dat
data <- read_xlsx("Kontajner_spotreba.xlsx")
str(data)
head(data)
#' Stlpec Cas sa nacital zle - Excel uklada cas ako datum s casom, preto je
#' pri kazdej hodine vymysleny datum 1899-12-31. Zo stlpca Cas nechame len
#' hodinu a minuty, zo stlpca Datum len datum.
data$hodina <- as.numeric(format(data$Cas, "%H"))
data$Cas <- format(data$Cas, "%H:%M")
data$Datum <- as.Date(data$Datum)
sum(is.na(data))
head(data)
range(data$Datum)
#' Data su od 1.12.2022 do 6.12.2023, spolu 8904 hodinovych merani,
#' ziadne hodnoty nechybaju.

#'
#'# 2. Priprava premennych
#'
#' Den v tyzdni ako cislo (1 = pondelok, ..., 7 = nedela), typ dna
#' (pracovny / vikend) a mesiac spolu s rokom, lebo december je v datach
#' dvakrat (2022 aj 2023).
data$den <- as.numeric(format(data$Datum, "%u"))
data$typ_dna <- ifelse(data$den >= 6, "vikend", "pracovny")
data$mesiac <- format(data$Datum, "%Y-%m")
head(data)
table(data$hodina)
table(data$den)
table(data$typ_dna)
table(data$mesiac)
#' Kazda hodina ma 371 merani, len hodina 1 ma 370 a hodina 2 ma 372 - je to
#' den zmeny casu 29.10.2023. December 2023 ma len 144 merani (6 dni).

#'
#'# 3. Spotreba ako celok
summary(data$Spotreba)
hist(data$Spotreba, main = "Hodinova spotreba", xlab = "kWh")
boxplot(data$Spotreba, horizontal = T, xlab = "kWh")
#' Spotreba nema normalne rozdelenie, je silno zosikmena doprava (median 3103,
#' priemer 4073 kWh). Test shapiro.test sa neda pouzit, lebo berie najviac
#' 5000 hodnot. Kazda skupina, pre ktoru budeme ratat IS, ma ale stovky
#' merani, takze IS pre priemer cez t.test pouzit mozeme.
#'
#' 95% IS pre priemernu hodinovu spotrebu, $\sigma$ nepozname:
t.test(data$Spotreba)$conf.int
CI(data$Spotreba)
#' Priemerna hodinova spotreba je s 95% spolahlivostou medzi 3999 a 4147 kWh.

#'
#'# 4. IS podla hodiny dna
CI_hodina <- group.CI(Spotreba ~ hodina, data = data)
CI_hodina
ggplot(CI_hodina, aes(x = hodina, y = Spotreba.mean,
                      ymin = Spotreba.lower, ymax = Spotreba.upper)) +
  geom_pointrange(color = "blue") +
  labs(y = "priemerna spotreba (kWh)") +
  theme_minimal(base_size = 14)
#' V noci (hodiny 18 az 5) je priemer okolo 3200 az 3600 kWh a intervaly sa
#' navzajom prekryvaju, spotreba je teda v noci rovnaka. O 6:00 zacne rast
#' a medzi 7:00 a 11:00 je najvyssia (priemer 5100 az 5700 kWh). IS pre 7:00
#' (5208 az 6125) sa s nocnymi intervalmi vobec neprekryva, rozdiel medzi
#' pracovnou zmenou a nocou je teda preukazny. Po 12:00 spotreba postupne
#' klesa a od 17:00 je naspat na nocnej urovni.

#'
#'# 5. IS podla dna v tyzdni
CI_den <- group.CI(Spotreba ~ den, data = data)
CI_den
ggplot(CI_den, aes(x = den, y = Spotreba.mean,
                   ymin = Spotreba.lower, ymax = Spotreba.upper)) +
  geom_pointrange(color = "blue") +
  labs(x = "den (1 = pondelok, 7 = nedela)", y = "priemerna spotreba (kWh)") +
  theme_minimal(base_size = 14)
#' Pondelok az stvrtok maju priemer 4300 az 4500 kWh a ich intervaly sa
#' prekryvaju, medzi tymito dnami rozdiel nevidime. Piatok je nizsie (4009),
#' jeho IS (3813 az 4205) sa prekryva uz len so stvrtkom. Sobota (3492)
#' a nedela (3291) su najnizsie a ich intervaly lezia cele pod intervalmi
#' pracovnych dni.

#'
#'# 6. IS pre pracovne dni a vikendy
CI_typ <- group.CI(Spotreba ~ typ_dna, data = data)
CI_typ
tapply(data$Spotreba, data$typ_dna, t.test)
ggplot(CI_typ, aes(x = typ_dna, y = Spotreba.mean,
                   ymin = Spotreba.lower, ymax = Spotreba.upper)) +
  geom_pointrange(color = "blue") +
  labs(x = "typ dna", y = "priemerna spotreba (kWh)") +
  theme_minimal(base_size = 14)
#' Pracovny den: 95% IS je 4254 az 4437 kWh. Vikend: 3271 az 3512 kWh.
#' Intervaly sa neprekryvaju a je medzi nimi velka medzera, cez vikend je
#' spotreba preukazne nizsia, v priemere asi o 950 kWh za hodinu. Na nulu ale
#' neklesne - cast spotreby bezi stale, aj ked sa na stavbe nepracuje.

#'
#'# 7. IS podla mesiaca
CI_mesiac <- group.CI(Spotreba ~ mesiac, data = data)
CI_mesiac
ggplot(CI_mesiac, aes(x = mesiac, y = Spotreba.mean,
                      ymin = Spotreba.lower, ymax = Spotreba.upper)) +
  geom_pointrange(color = "blue") +
  labs(y = "priemerna spotreba (kWh)") +
  theme_minimal(base_size = 9)
#' Toto je najvacsi rozdiel zo vsetkych. V zime je spotreba najvyssia
#' (januar 8444, februar 7516 kWh), v lete najnizsia (jul, august a september
#' okolo 1100 az 1200 kWh), teda asi sedemkrat mensia. Intervaly letnych
#' mesiacov jul, august a september sa prekryvaju, medzi nimi rozdiel nie je.
#' Prekryva sa este marec s decembrom 2022 aj 2023 a april s oktobrom,
#' ostatne mesiace maju intervaly oddelene. Vysoka spotreba v zime ukazuje,
#' ze velku cast energie beru kontajnery na kurenie.

#'
#'# 8. Zaver
#'
#' - Priemerna hodinova spotreba staveniska je s 95% spolahlivostou
#'   3999 az 4147 kWh.
#' - Najviac spotrebu ovplyvnuje mesiac: v zime je priblizne sedemkrat vyssia
#'   ako v lete a intervaly zimnych a letnych mesiacov su daleko od seba.
#' - Cez den je spotreba najvyssia medzi 7:00 a 11:00, v noci je nizsia
#'   a rovnomerna.
#' - Cez vikend je spotreba preukazne nizsia ako v pracovny den, intervaly
#'   sa neprekryvaju. Pondelok az stvrtok sa medzi sebou nelisia, piatok je
#'   o nieco nizsie.
#' - Ked sa dva intervaly neprekryvaju, priemery skupin sa preukazne lisia.
#'   Ked sa prekryvaju, rozdiel z tychto dat nevieme potvrdit.
